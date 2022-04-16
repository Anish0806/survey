library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidytext)
library(dplyr)
library(data.table)
library("wordcloud")
Sys.setlocale('LC_ALL','C')


ui <- dashboardPage(
  dashboardHeader(title = tags$img(src='https://upload.wikimedia.org/wikipedia/en/e/ea/Keys_for_Kids_Ministeries.png', height = '60', width ='100')),
    ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Sentiment Calculator", icon = icon("th"), tabName = "widgets",
              badgeColor = "green")
      
      
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                fileInput("file1", "Choose CSV File",
                          multiple = FALSE,
                          accept = c("text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv")),
                
                # Horizontal line ----
                tags$hr(),
                
                # Input: Checkbox if file has header ----
                
                # Horizontal line ----
                tags$hr(),
                
                # Input: Select number of rows to display ----
                radioButtons("disp", "Display",
                             choices = c(Head = "head",
                                         All = "all"),
                             selected = "head"),
                # plotOutput("plot2"),
                #plotOutput("plot3"),
                splitLayout(cellWidths = c("50%", "50%"), plotOutput("plot2"), plotOutput("plot3")),
                
                tableOutput("contents")
              )
      ),
      
      tabItem(tabName = "widgets",
              h2("Sentiment Score of Reviews"),
              textInput("text", label = h3("Enter Review Below"), value = ""),
              
              hr(),
              
             # fluidRow(column(3, verbatimTextOutput("value_of_sentiment")))
             tableOutput("text_contents")
             

      
      )
    )
  )
  )
options(shiny.maxRequestSize=30*1024^2)
server <- function(input, output) {
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = TRUE,
                       sep = ',',
                       quote = '"')
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    

    #df$sentiment_score <- sentimentr::sentiment_by(as.character(df$Review),by = NULL)
    df$sentiment_score <- round(sentimentr::sentiment_by(as.character(df$Review))$ave_sentiment,2)
    
    df$sentiment <- ifelse(df$sentiment_score > 0, "Positive", ifelse(df$sentiment_score < 0,"Negative","Neutral"))
    
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  print(df)
  })
  

  
  data <- reactive({ 
    req(input$file1) ## ?req #  require that the input is available
    
    inFile <- input$file1 
    
    # tested with a following dataset: write.csv(mtcars, "mtcars.csv")
    # and                              write.csv(iris, "iris.csv")
    df <- read.csv(input$file1$datapath,
                   header = TRUE,
                   sep = ',',
                   quote = '"')
    
    
   
    df <- df %>% mutate(ID = row_number())
    
    tidy_text <- df %>%  
      select(Review,ID) %>%
      unnest_tokens(word,Review)
    
#    data(stop_words)
    
    tidy_text <- tidy_text %>%
      anti_join(stop_words)
    
    tidy_count <-tidy_text %>%
      count(word, sort = TRUE) 
    
    return(tidy_text)
  })
  
  word_data <- reactive({ 
  tidy_count <- data() %>%
    count(word, sort = TRUE) 
  
  return(tidy_count)
})
  
  output$contents1 <- renderTable({
    data()
  })
  
  
  
  output$plot2 <- renderPlot({
    
    data() %>%
      count(word, sort = TRUE) %>%
      filter(n > 10) %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(n, word)) +
      geom_col() +
      labs(y = NULL)
  })
   
  
  output$plot3 <- renderPlot({
    
  df  <- word_data()
    wordcloud(words = df$word, freq = df$n, min.freq = 5,
              max.words=400, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
  })
  
  
  
  output$text_contents <- renderTable({
    df1 <- data.frame (input_text_statement  = c(input$text),
                      sentiment_score_of_text = c(0)
    )
    
    #df$sentiment_score <- sentimentr::sentiment_by(as.character(df$Review),by = NULL)
    df1$sentiment_score_of_text <- round(sentimentr::sentiment_by(as.character(df1$input_text_statement))$ave_sentiment,2)
    
    #Calculating sentiment value
    df1$sentiment_text <- ifelse(df1$sentiment_score_of_text > 0, "Positive", ifelse(df1$sentiment_score_of_text < 0,"Negative","Neutral"))

    #df1$sentiment_text <- ifelse(df1$sentiment_score_of_text = 0 , "Neutral", df1$sentiment_score_of_text)
    
    print(df1)
    names(df1)[1] <- "Review Entered"
    names(df1)[2] <- "Sentiment Score"
    names(df1)[3] <- "Sentiment"
  
    
  return(df1)

    
  })

  
  
}
shinyApp(ui, server)
