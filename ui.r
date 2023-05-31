#ui.R
library(shiny)
library(shinydashboard)
library(excelR)

#static array for genre list
genre_list <- c("Select","Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film.Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci.Fi", "Thriller", "War", "Western")
search <- read.csv("movies.csv", stringsAsFactors=FALSE)
m<-search$title
n<-search$genres
data = data.frame(
  Title=m,
  Genre=n
)
columns = data.frame(title=c('Title', 'Genre'),
                     width= c(600, 500),
                     type=c('text', 'text')) 
rows=data.frame(title=1,
                width=30
)

shinyUI(dashboardPage(
  dashboardHeader(title = "Movie Recommendation System"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Recommend Movie", tabName = "RecommendMovie", icon = icon("dashboard")),
      menuItem("List Movies", tabName = "ListMovies", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(

      tabItem(tabName = "RecommendMovie",
              fluidRow(
                
                column(4, wellPanel(h3("Select Movie Genres You Prefer (order matters):")),
                       wellPanel(
                         selectInput("input_genre", "Genre #1",
                                     genre_list),
                         selectInput("input_genre2", "Genre #2",
                                     genre_list),
                         selectInput("input_genre3", "Genre #3",
                                     genre_list)
                         #submitButton("Update List of Movies")
                       )),
                
                column(4,  wellPanel(h3("Select Movies You Like of these Genres:")),
                       wellPanel(
                         # This outputs the dynamic UI component
                         uiOutput("ui"),
                         uiOutput("ui2"),
                         uiOutput("ui3")
                         #submitButton("Get Recommendations")
                       )),
                
                column(4,
                       wellPanel(h3("You Might Like The Following Movies Too!")),
                       wellPanel(
                         tableOutput("table")
                         #verbatimTextOutput("dynamic_value")
                       ))
              )
              
      ),
      
      tabItem(tabName = "ListMovies",
              
             fluidRow( excelTable(data=data,columns = columns,pagination = 50,minDimensions = c(2,20),search = TRUE,rowHeight = rows
                                  )
                       
             )
      )
    )
  )
)
)