library(tidytext)
library(dplyr)
library(stringr)
library(lexicon)
library(tidyr)

tokenize <- function(var){
  f <- paste0("dbs/en_US/en_US.", var, ".txt")
  
  # Read the text file into a table
  # d <- read.delim(textConnection(readLines(f)), sep="\n", header=FALSE)
  d <- data.frame(`text`=readLines(f))
  d$source <- var
  d <- d %>%
    # Remove punctuations and convert to lower-case
    unnest_tokens(word, text) %>%
    
    # Remove words which contain numbers
    filter(grepl("^[A-Za-z]+$", word, perl = T)) %>%
    
    # Remove profanity words
    filter(!word %in% unique(tolower(lexicon::profanity_alvarez)))
  return(d)
}

tokenize_ngrams <- function(var, n){
  f <- paste0("dbs/en_US/en_US.", var, ".txt")
  
  # Read the text file into a table
  # d <- read.delim(textConnection(readLines(f)), sep="\n", header=FALSE)
  d <- data.frame(`text`=readLines(f))
  d$source <- var
  d <- d %>%
    # Remove punctuations and convert to lower-case
    unnest_tokens(ngram, text, token='ngrams', n=n)
  
  wcols <- unlist(lapply(1:n, function(x){paste0('word', x)}))
  
  d_separated <- d %>%
    # Separate n-grams
    separate(ngram, wcols, sep = " ")
  
  for (word in wcols) {
    d_separated <- d_separated %>%
      
      # Remove numbers
      filter(grepl("^[A-Za-z]+$", !!as.symbol(word), perl = T)) %>%
      
      # Remove profanity words
      filter(!(!!as.symbol(word)) %in% unique(tolower(lexicon::profanity_alvarez)))
  }
  return(d_separated)
}

phrase_frequency <- function(var, n){
  if (n == 1){
    df <- tokenize(var)
    df <- df %>%
      count(word, sort=TRUE)
    names(df) <- c('ngram', 'n')
  }else{
    df <- tokenize_ngrams(var, n=n)
    if (n == 2){
      df <- df %>%
        unite(ngram, word1, word2, sep = " ") %>%
        count(ngram, sort=TRUE)
    }else{
      df <- df %>%
        unite(ngram, word1, word2, word3, sep = " ") %>%
        count(ngram, sort=TRUE)
    }
  }
  return(df)
}

for (n in c(1, 2, 3)){
  df <- rbind(phrase_frequency(var='news', n=n),
              phrase_frequency(var='blogs', n=n),
              phrase_frequency(var='twitter', n=n))
  df <- df %>%
    group_by(ngram) %>%
    summarize(n = sum(n))
  write.csv(df, paste0("dbs/ngrams/", n, "-grams.csv"), row.names = FALSE)
  gc()
}
