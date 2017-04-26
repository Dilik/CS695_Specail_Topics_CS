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
                  title = "Word cloud of Brooklyn Tech Triangle",     
                  plotOutput("distPlot1")
                  
                ),
                
                box(
                  width = 4, 
                  background = "olive",
                  "This text box is for your to discuss the results. 
                  You are required to provide a deep discussion of the chart/graph. 
                  What is the chart telling about the business?
                  What insights can you discover from the results?
                  What recommendation you can make to the company?"
                )
            )
      ),
      
      # Second tab content
      tabItem(tabName = "sentiment",
              fluidRow(
                box(
                  width = 4, 
                  title = "Sentiment of Brooklyn Tech Triangle",     
                  plotOutput("distPlot2")
                  
                ) ,
                box(
                  width = 4, 
                  background = "olive",
                  "This text box is for your to discuss the results. 
                  You are required to provide a deep discussion of the chart/graph. 
                  What is the chart telling about the business?
                  What insights can you discover from the results?
                  What recommendation you can make to the company?"
                )
            ) 
       ),
      tabItem(tabName = "network",
              fluidRow(
                box(
                  width = 8, 
                  title = "Network Graph of Unidays",     
                  plotOutput("distPlot3")
                  
                ),
                box(
                  width = 4, 
                  background = "olive",
                  "This text box is for your to discuss the results. 
                  You are required to provide a deep discussion of the chart/graph. 
                  What is the chart telling about the business?
                  What insights can you discover from the results?
                  What recommendation you can make to the company?"
                )
              ) 
            ),
      tabItem(tabName = "topic",
              fluidRow(
                box(
                  width = 8, 
                  title = "Network Graph of Brooklyn Tech Triangle",     
                  plotOutput("distPlot4")
                )  
              )
      ),
      tabItem(tabName = "customer",
              h2("Customer")
      )
    )
  )
  
)


