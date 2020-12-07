#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(plotly)

dashboardPage(
    dashboardHeader(title = "Logistic Regression"),
    dashboardSidebar(
        radioButtons("rb", "Select What to Plot:", 
                     choices = list("Age" = "age",
                                    "Estimated Salary" = "salary")
        ),
        selectInput("gender", "Select Gender: ", choices = c("Choose a Gender"=""), multiple = TRUE)
    ),
   dashboardBody(
        fluidRow(
            valueBoxOutput("n1", width = 4),
            plotOutput("logisticplot")
       )  
    )
)
