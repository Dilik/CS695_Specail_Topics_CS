  ## ui.R ##
library(shinydashboard)

dashboardPage(
  skin = "yellow",
  dashboardHeader(title = "Unidays"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Wordcloud", tabName = "wordcloud", icon = icon("cloud")),
      menuItem("Sentiment", tabName = "sentiment", icon = icon("magnet")),
      menuItem("Network", tabName = "network", icon = icon("connectdevelop")),
      menuItem("Topic Analysis", tabName = "topic", icon = icon("star")),
      menuItem("Customer Analysis", tabName = "customer", icon = icon("user-circle"))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "wordcloud",
              fluidRow(
                
                box(
                  width = 4,
                  title = "Select number of words to show on the graph ",
                  sliderInput("bins",
                              "Number of words:",
                              min = 1,
                              max = 50,
                              value = 30)
                ),
                
                box(
                  width = 4,
                  title = "Unidays Wordcloud",     
                  plotOutput("distPlot1")
                  
                ),
                
                box(
                  width = 4, 
                  background = "olive",
                  "This word cloud describes 50 mostly used words on Unidays data
                  It visualizes what is a target of Unidays."
                )
            )
      ),
      
      # Second tab content
      tabItem(tabName = "sentiment",
              fluidRow(
                box(
                  width = 4, 
                  title = "Unidays Sentiment Analysis",     
                  plotOutput("distPlot2")
                  
                ) ,
                box(
                  width = 4, 
                  background = "olive",
                  "This graph vizualizes sentiment analysis of Unidays data. 
                  It gives general reflection of consumer attitude and feedback."
                )
            ) 
       ),
      tabItem(tabName = "network",
              fluidRow(
                box(
                  width = 10, 
                  title = "Network Graph of Unidays",     
                  img(src='net1.png', align = "center"),
                  img(src='net2.png', align = "center")
                )
              ) 
            ),
      tabItem(tabName = "topic",
              fluidRow(
                box(
                  width = 12, 
                  title = "Topic Analysis of Unidays",     
                  img(src='topic.png', align = "center")
                )  
              )
      ),
      tabItem(tabName = "customer",
              fluidRow(
                box(
                  width = 12, 
                  title = "Customer Profile of Unidays",     
                  img(src='CustomerP.png', align = "center")
                )  
              )
      )
    )
  )
  
)


