#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(stringr)
library(dplyr)

shinyServer(function(input, output) {
    withProgress(message = 'Model is loading...', value = 0, {
        load(file = "data/model_reduced.rda")
        })
    candidates <- eventReactive(input$predictButton, {
        wds <- paste(tail(unlist(str_split(trimws(input$text, whitespace = "[ \t\r\n]"), " ")), 2), collapse=" ")
        wds <- tolower(gsub("[[:punct:]]", "", wds))
        two <- model.r$c1[model.r$c1$bigram == wds, c('word', 'p_bo_c1')]
        if (nrow(two) == 0){
            last.word <- unlist(str_split(wds, " "))[-1]
            one <- model.r$c2[grep(paste0("^",last.word), model.r$c2$bigram_lst), c('word', 'p_bo_c2')]
            if (nrow(one) == 0){
                guess <- model.r$c3$word
            }else{
                one <- one %>%
                    filter (! duplicated(word)) %>%
                    arrange(desc(p_bo_c2))
                guess <- one$word
            }
        }else{
            two <- arrange(two, desc(p_bo_c1))
            guess <- two$word
        }
        return(paste(paste(input$text, paste0("<span style='color: blue;'>",
                                              guess[1],
                                              '</span>'), sep=' '),
                     "   ",
                     "Or...",
                     "   ",
                     paste(input$text, paste0("<span style='color: blue;'>",
                                              guess[2],
                                              '</span>'), sep=' '),
                     paste(input$text, paste0("<span style='color: blue;'>",
                                              guess[3],
                                              '</span>'), sep=' '),
                     paste(input$text, paste0("<span style='color: blue;'>",
                                              guess[4],
                                              '</span>'), sep=' '),
                     paste(input$text, paste0("<span style='color: blue;'>",
                                              guess[5],
                                              '</span>'), sep=' '),
                     sep = "<br/>")
               )
        
    })
    hintc <- eventReactive(input$predictButton, {
            return("Do you mean...")
    })
    output$hint <- renderUI({ HTML(hintc()) })
    output$text <- renderUI({ HTML(candidates()) })
    
})
