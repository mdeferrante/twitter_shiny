#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(wordcloud2)
library(dplyr)
library(ggplot2)
library(scales)
library(tidytext)
library(forcats)
library(stringr)
library(readr)
library(plotly)


# The sentiment function takes a really long time so I created a new data file so you don't have to run it
#us_tweets <- us_tweets[1:100000, ]
#us_tweets <- 
#  us_tweets %>% 
#  select(date, hour, tweet_content, anger, anticipation, fear, disgust, joy, sadness, surprise, trust)

us_tweets <- read_csv("us_tweets_small.csv") 

#write.csv(us_tweets, file = "us_tweets_small.csv")
#write.csv(us_tweets2, file = "us_tweets2.csv")
#gets rid of non alphabetic characters  
us_tweets$tweet_content_stripped <- gsub("[^[:alpha:] ]", "",
                                         us_tweets$tweet_content) 

#removes all words that are 1-2 letters long
us_tweets$tweet_content_stripped <- gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ",
                                         us_tweets$tweet_content_stripped) 
#removes amp, abbreviation for ampersan
us_tweets$tweet_content_stripped <- gsub(" amp ", " ", us_tweets$tweet_content_stripped) 

#removes recurrence of jobs 
us_tweets$tweet_content_stripped <- gsub("jobs", " job ", us_tweets$tweet_content_stripped) 
us_tweets$tweet_content_stripped <- gsub("Jobs", " job ", us_tweets$tweet_content_stripped) 

#Converts to long formatt
#us_tweets_long <- gather(us_tweets, sentiment, count, anger:trust, 
#                         factor_key = TRUE)

#Converts hour to posixct format
us_tweets$hour <- as.POSIXct(us_tweets$hour, format = " %H:%M")

#generates plot of distribution of plots across time
tweets_over_time <- 
  ggplot(data = us_tweets, aes(x = hour)) +
  geom_histogram(stat = "count") +
  xlab("Time") + ylab("Number of Tweets") +
  ggtitle("Number of Tweets per Hour") +
  scale_x_datetime(labels = date_format("%H:%M"), breaks = pretty_breaks(n = 10))

tweet_words <- us_tweets %>% 
  unnest_tokens(word, tweet_content_stripped)

data(stop_words)

tweet_words <-  
  anti_join(tweet_words, stop_words)

tweet_words_df <- tweet_words$word
tweet_words_df <- as.data.frame(table(tweet_words_df))
names(tweet_words_df) <- c("word", "freq")

tweet_words_df <- tweet_words_df[tweet_words_df$freq >= 200, ]

sentimentTotals <- data.frame(colSums(us_tweets[,c(5:12)]))

names(sentimentTotals) <- "count" 


sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals),
                         sentimentTotals)

#plots distribtion of all sentiments in tweet content
sentiment_totals_plot <-
  ggplot(data = sentimentTotals, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + 
  ylab("Total Count") + 
  labs(title = "Counts of Twitter Sentiments")

#creates bar chart of 10 most frequent words
top_words <-
  tweet_words %>% 
  count(word, sort = TRUE) %>% 
  top_n(10) %>% 
  mutate(word = fct_reorder(word, n)) %>% 
  ggplot(aes(x = word, y = n)) + 
  geom_bar(stat = "identity", fill = "blue", alpha = .6) + 
  coord_flip() +
  labs(title = "10 Most Frequent Words", y = "Count", x = "Word")

# Define UI for application that draws a histogram
ui <- fluidPage(
  #theme = shinytheme("sandstone"),
  
  # Application title
  titlePanel("Twitter Visualizations"),
  
  fluidRow(
    sidebarPanel(
      #tags$head(
      #tags$style("body {background-color: teal; }"))),
      sliderInput("size",
                  "Zoom",
                  min = .5,  max = 20,  value = 5),
      width = 3,   
      img(src = 'twitter.png', align = "center" , width = 120, height = 100)),
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Word Cloud",  wordcloud2Output("wordcloud_plot")),
        tabPanel("Bar Chart", plotOutput("top_words"))
      )
    )),
  fluidRow(
    splitLayout(plotlyOutput("tweet_time"), plotOutput("sentiments")
    )
  )
)

# Define server logic required to make plots
server <- function(input, output) {
  output$wordcloud_plot <- renderWordcloud2({
    wordcloud2(tweet_words_df, size = input$size)
  })
  output$tweet_time <- renderPlotly({
    ggplotly(tweets_over_time) 
    
  })
  output$sentiments <- renderPlot({
    sentiment_totals_plot
    
  })
  output$top_words <- renderPlot({
    top_words
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

