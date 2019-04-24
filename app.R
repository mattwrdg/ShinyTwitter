## app.R ##
## Install Packages if not installed:
if (!require(shinydashboard)) install.packages('shinydashboard')
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(dplyr)) install.packages('dplyr')
if (!require(rtweet)) install.packages('rtweet')

## Use Librarys
library(shinydashboard)
library('ggplot2')
library(dplyr)
library("rtweet")


## UI // Frontend
ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "widgets", icon = icon("dashboard")),
      menuItem("Histogram", tabName = "histogram", icon = icon("chart-bar"))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "histogram",
              fluidRow(
                box(plotOutput("plot1", height = 250)),
                
                box(
                  title = "Controls",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Twitter activity with #esentri"),
              fluidRow(
                
              box(
                title = "Your Input",
                selectInput("twitterkpi", 
                            label = "Choose a KPI to display",
                            choices = list("Tweets", 
                                           "Likes" ,
                                           "Likerate",
                                           "Follower",
                                           "Total Tweets"),
                            selected = "tweets"),
                sliderInput("twitterslider", "Number of KPI", 1, 10, 5),
                sliderInput("generalTweets", "Number of Tweets fetched", 1, 15000, 5000),
                
                dateRangeInput('dateRange',
                               label = 'Date range input: yyyy-mm-dd',
                               start = Sys.Date() - 7, end = Sys.Date()
                )
                
              ),
              box(
                plotOutput("twitterplot", height = 250)
                )
                )

      )
    )
  )
)

## Server // Backend & Functions
server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  
  library(rtweet)
  
  
  output$twitterplot <- renderPlot({
    # search for n tweets using the esentri phrase or hashtag
    rt <- search_tweets("esentri", n = input$generalTweets, include_rts = FALSE)
    
    kpi <- switch(input$twitterkpi, 
                  "Tweets" = "tweets", 
                  "Likes" = "likes",
                  "Likerate" = "likerate",
                  "Follower" = "follower",
                  "Total Tweets" = "alltweets")
    
    rt %>%
      filter(created_at > as.POSIXct(!!input$dateRange[1],tz="UTC")) %>%
      filter(created_at < as.POSIXct(!!input$dateRange[2],tz="UTC")) %>%
      group_by(name,account_lang,) %>%
      summarize(tweets=n(),
                likes=sum(favorite_count),
                follower=max(followers_count),
                alltweets=max(statuses_count)) %>%
      #filter(!!as.symbol(kpi)>1) %>%
      mutate(likerate=likes/tweets) %>%
      arrange(desc(!!as.symbol(kpi))) %>%
      head(input$twitterslider) %>%
      ggplot(aes(x=reorder(name,-!!as.symbol(kpi)), y=!!as.symbol(kpi) , fill = account_lang 
                 )) + 
      theme(legend.position = "bottom") +
      geom_col(position="dodge")
  })
  
  output$sliderValue <- renderText({ input$twitterkpi })

  
}

shinyApp(ui, server)