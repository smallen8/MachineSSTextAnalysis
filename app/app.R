#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(data.table)
library(reactable)
library(htmltools)

library(dplyr)
library(tidyr)
library(widyr)

library(tidytext)
data("stop_words")

library(igraph)
library(ggraph)
library(ggplot2)

theme_set(theme_minimal())
colorPalette <<- 'Blues'
textSize <<- 15

set.seed(2022)

source('DataPrep.R')
source('UIModules.R')

# Load data.
text_df <- loadData(filename = 'surveyResponses.xlsx', textColumn = 'love_question')

# Categorical variables in metadata that can be used for grouping later
summarizeVars <<- c("Don't Summarize", 'age_group', 'gender', 'family_status', 'has_kids', 'live_in_ss', 'work_in_ss')
topWordList <<- countWords(text_df)$word[1:20]

LaunchUI <- tryCatch({
  
  source('DataPrep.R')
  source('UIModules.R')
  text_df <- loadData(filename = 'surveyResponses.xlsx', textColumn = 'love_question')
  summarizeVars <<- c("Don't Summarize", 'age_group', 'gender', 'family_status', 'has_kids', 'live_in_ss', 'work_in_ss')
  topWordList <<- countWords(text_df)$word[1:20]
  
  ui <- div(fluidPage(
    navbarPage(id = 'Main',
               title = div(icon('hot-tub', lib='font-awesome'), 'South Side Flats - Neighbor Feedback'),
               tabPanel(title = 'Response Summary', value = 'Response Summary', MetadataUI()),
               tabPanel(title = 'Frequent Phrases', value = 'Frequent Phrases', WordCountUI()),
               tabPanel(title = 'Correlated Words', value = 'Correlated Words', WordCorrelationUI()),
               tabPanel(title = 'Neighbor Feedback', value = 'Neighbor Feedback', ResponsesUI())
    )
  ))
  
})

LaunchServer <- tryCatch({
  
  server <- function(input, output, session) {
    
    source('DataPrep.R')
    source('UIModules.R')
    text_df <- loadData(filename = 'surveyResponses.xlsx', textColumn = 'love_question')
    summarizeVars <<- c("Don't Summarize", 'age_group', 'gender', 'family_status', 'has_kids', 'live_in_ss', 'work_in_ss')
    topWordList <<- countWords(text_df)$word[1:20]
    
    output$plotTopNWords <- renderPlot({
      plotTopWords(text_df, groupByVar = input$groupByVar, tokenType = 'word', myN = input$myN)
    })
    output$plotTopNBigrams <- renderPlot({
      plotTopWords(text_df, groupByVar = input$groupByVar, tokenType = 'bigram', myN = input$myN)
    })
    output$plotTopNTrigrams <- renderPlot({
      plotTopWords(text_df, groupByVar = input$groupByVar, tokenType = 'trigram', myN = input$myN)
    })
    
    output$getCorrelatedWords <- renderDataTable({
      getTopCorrelatedWords(text_df, myN = input$myN2, myCorr = input$myCorr,
                            myWords = input$corrWord)
    })
    output$plotWordMap <- renderPlot({
      plotWordCorrGraph(text_df, myCorr = input$myCorr)
    })
    # output$getCorrelatedWords2 <- reactableOutput({
    #     raw_text <- read.csv('~/Documents/SouthSideFlats/text.csv')
    #     raw_text <- raw_text %>%
    #         filter(!is.na(text), trimws(text)!='')
    #    
    #     reactable(
    #         df = getTopCorrelatedWords(text_df, myN = input$myN2, myCorr = input$myCorr,
    #                                    myWords = input$corrWord),
    #         details = function(index) {
    #             nestedData <- data.frame(Details = raw_text[intersect(grep(word1[index], raw_text$text),
    #                                                                   grep(word2[index], raw_text$text)),])
    #             htmltools::div(style = 'padding: 16px',
    #                            reactable(nestedData, outlined = T))
    #         }
    #     )
    # })
    
    output$displayRawText <- renderDataTable({
      raw_text <- readxl::read_excel('surveyResponses.xlsx')
      raw_text <- raw_text %>%
        mutate(id = row_number(),
               text = `What do you love about the South Side? (please keep it to no more than 60 words)`) %>%
        filter(!is.na(text), trimws(text)!='') %>%
        group_by(id) %>%
        summarise_all(toString)
      
      datatable(raw_text,
                options = list(searchHighlight = T))
    })
    
    # metadata summaries
    output$summarizeAgeDf <- renderDataTable({
      text_df %>% 
        count(age_group, sort=T) %>%
        select(`Age Group` = age_group,
               Count = n)
    })
    output$summarizeAgePlot <- renderPlot({
      text_df %>% 
        count(age_group, sort=T) %>%
        ggplot() + 
        geom_col(aes(reorder(age_group, n), n, fill='blue'), show.legend=F) +
        coord_flip() +
        xlab('Age Group') +
        ylab('Count') +
        scale_fill_brewer(palette = colorPalette)
    })

    output$summarizeGenderDf <- renderDataTable({
      text_df %>% 
        count(gender, sort=T) %>%
        select(`Gender` = gender,
               Count = n)
    })
    output$summarizeGenderPlot <- renderPlot({
      text_df %>% 
        count(gender, sort=T) %>%
        ggplot() + 
        geom_col(aes(reorder(gender, n), n, fill='blue'), show.legend=F) +
        coord_flip() +
        xlab('Gender') +
        ylab('Count') +
        scale_fill_brewer(palette = colorPalette)
    })
    
    output$summarizeFamilyStatusDf <- renderDataTable({
      text_df %>% 
        count(family_status, sort=T) %>%
        select(`Family Status` = family_status,
               Count = n)
    })
    output$summarizeFamilyStatusPlot <- renderPlot({
      text_df %>% 
        count(family_status, sort=T) %>%
        ggplot() + 
        geom_col(aes(reorder(family_status, n), n, fill='blue'), show.legend=F) +
        coord_flip() +
        xlab('Family Status') +
        ylab('Count') +
        scale_fill_brewer(palette = colorPalette)
    })
    
    output$summarizeLivesInSSDf <- renderDataTable({
      text_df %>% 
        count(live_in_ss, sort=T) %>%
        select(`Lives In South Side` = live_in_ss,
               Count = n)
    })
    output$summarizeLivesInSSPlot <- renderPlot({
      text_df %>% 
        count(live_in_ss, sort=T) %>%
        ggplot() + 
        geom_col(aes(reorder(live_in_ss, n), n, fill='blue'), show.legend=F) +
        coord_flip() +
        xlab('Lives In South Side') +
        ylab('Count') +
        scale_fill_brewer(palette = colorPalette)
    })
    
    
    output$summarizeWorksInSSDf <- renderDataTable({
      text_df %>% 
        count(work_in_ss, sort=T) %>%
        select(`Works In South Side` = work_in_ss,
               Count = n)
    })
    output$summarizeWorksInSSPlot <- renderPlot({
      text_df %>% 
        count(work_in_ss, sort=T) %>%
        ggplot() + 
        geom_col(aes(reorder(work_in_ss, n), n, fill='blue'), show.legend=F) +
        coord_flip() +
        xlab('Works In South Side') +
        ylab('Count') +
        scale_fill_brewer(palette = colorPalette)
    })
  }
})

shinyApp(ui = ui, server = server)
