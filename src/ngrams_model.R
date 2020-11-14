library(tidytext)
library(dplyr)
library(stringr)
library(lexicon)
library(tidyr)
library(corpustools)

# Load trigrams
trigrams_all <- read.csv(paste0("dbs/ngrams/", 3, "-grams.csv"))
trigrams_all <- arrange(trigrams_all, desc(n))

# Load bigrams
bigrams_all <- read.csv(paste0("dbs/ngrams/", 2, "-grams.csv"))
bigrams_all <- arrange(bigrams_all, desc(n))

# Load unigrams
unigrams_all <- read.csv(paste0("dbs/ngrams/", 1, "-grams.csv"))
unigrams_all <- arrange(unigrams_all, desc(n))

# Get a part of data for training and iteration
names(unigrams_all) <- c('word', 'n_w')
names(bigrams_all) <- c('bigram', 'n_bi')
trigrams <- trigrams_all[1:nrow(trigrams_all), ]

# Split 3-grams into 2-gram and the last word
trigrams$bigram <- sapply(trigrams$ngram, 
                          function(x){
                            paste(unlist(str_split(x, " "))[1:2], collapse=" ")
                            })
trigrams$word <- sapply(trigrams$ngram, 
                          function(x){unlist(str_split(x, " "))[3]})

# compute discounting d based on Good-Turing estimation
trigrams.Nc <- trigrams %>%
  group_by(n) %>%
  summarize(Nc = n())

trigrams <- trigrams %>% 
  left_join(trigrams.Nc, by='n') %>% 
  mutate(np = n + 1)

trigrams <- trigrams %>% 
  left_join(trigrams.Nc, c("np" = "n"))

trigrams <- trigrams %>%
  mutate(d = (n + 1) * Nc.y / Nc.x / n) %>%
  mutate(d = ifelse(d < 1, d, 1)) %>%
  mutate(d = replace_na(d, 1))

trigrams <- select(trigrams, c('ngram', 'n', 'bigram', 'word', 'd'))

# compute bigram count
trigrams <- trigrams %>%
  left_join(bigrams_all, by='bigram')

# compute beta
trigrams.beta <- trigrams %>%
  group_by(bigram) %>%
  summarize(beta = 1 - sum(d * n / n_bi))

trigrams <- trigrams %>%
  left_join(trigrams.beta, by='bigram')

# compute alpha -> p_bo(wi|wi-1) = p(wi-1, wi) / p(wi-1)
trigrams$bigram_lst <- sapply(trigrams$ngram, 
                          function(x){
                            paste(unlist(str_split(x, " "))[2:3], collapse=" ")
                          })

names(bigrams_all) <- c('bigram', 'n_bi_lst')
trigrams <- trigrams %>%
  left_join(bigrams_all, c("bigram_lst" = "bigram")) %>%
  left_join(unigrams_all, by='word')

trigrams <- trigrams %>%
  mutate(p_bo2 = n_bi_lst / n_w)

# compute alpha
trigrams.alpha.de <- trigrams %>%
  group_by(word) %>%
  summarize(alpha.de = sum(p_bo2))

trigrams <- trigrams %>%
  left_join(trigrams.alpha.de, by='word')

trigrams <- trigrams %>%
  mutate(alpha=beta/alpha.de)

# Katz's back-off probability
trigrams <- trigrams %>%
  mutate(p_bo_c1 = d * n / n_bi,
         p_bo_c2 = alpha * p_bo2) %>%
  select(c('ngram', 'n', 'bigram', 'bigram_lst', 'word',
           'p_bo_c1', 'p_bo_c2', 'n_w'))

# Prepare model
trigrams4m <- filter(trigrams, n > 1)

md.c1 <- trigrams4m %>%
  select(bigram, word, p_bo_c1) %>%
  group_by(bigram) %>%
  arrange(desc(p_bo_c1)) %>%
  slice(1:5)
  
md.c2 <- trigrams4m %>%
  filter (! duplicated(bigram_lst)) %>%
  select(bigram_lst, word, p_bo_c2)

md.c3 <- trigrams4m %>%
  filter (! duplicated(word)) %>%
  select(word, n_w) %>%
  slice(1:5)

model.r <- list()
model.r$c1 <- md.c1
model.r$c2 <- md.c2
model.r$c3 <- md.c3
save(model.r, file = "dbs/model_reduced.rda")

# Test
candidates <- function(input, data){
  wds <- paste(tail(unlist(str_split(trimws(input, whitespace = "[ \t\r\n]"), " ")), 2), collapse=" ")
  wds <- tolower(gsub("[[:punct:]]", "", wds))
  two <- data$c1[data$c1$bigram == wds, c('word', 'p_bo_c1')]
  if (nrow(two) == 0){
    last.word <- unlist(str_split(wds, " "))[-1]
    one <- data$c2[grep(paste0("^",last.word), data$c2$bigram_lst), c('word', 'p_bo_c2')]
    if (nrow(one) == 0){
      return(data$c3$word)
      }else{
      one <- one %>%
        filter (! duplicated(word)) %>%
        arrange(desc(p_bo_c2))
      return(one$word)
      }
  }else{
    two <- arrange(two, desc(p_bo_c1))
    return(two$word)
  }
}
candidates("MY LOVE", model.r)
