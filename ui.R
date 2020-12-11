#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(shinydashboard)

fluidPage(
    titlePanel("Logistic Regression"),
    tabsetPanel(
        tabPanel("Plotting", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(
                        radioButtons("rb", "Select Predictor Variable:", 
                        choices = list("Age" = "age1",
                                    "Estimated Salary" = "salary1"),
                        selected = FALSE
                        ),
                        conditionalPanel(condition = "input.rb == 'age1'",
                            checkboxInput("gender1", "Also plot Gender?", 
                                     value = FALSE)
                        ),
                        conditionalPanel(condition = "input.rb == 'salary1'",
                            checkboxInput("gender2", "Also plot Gender?",
                                     value = FALSE)
                        )
                    ),
                    mainPanel(
                        fluidRow(
                            plotOutput("logisticplot")  
                        )
                ))),
        tabPanel("Prediction", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(
                         sliderInput("age", "Select Age:", min = 18, max = 60, value = 18, sep = ""),
                         sliderInput("salary", "Select Estimated Salary", min = 15000, 
                                     max = 150000, value = 15000),
                         selectInput("gender", "Select Gender",  choices=c("Choose a Gender"=""))
                     ),
                    mainPanel(
                        tableOutput("one")
                    )
                )
        ),
        tabPanel("Simulation", fluid = TRUE,
                 sidebarLayout(
                   sidebarPanel(
                     sliderInput("age", "Select Age:", min = 18, max = 60, value = 18, sep = ""),
                     sliderInput("salary", "Select Estimated Salary", min = 15000, 
                                 max = 150000, value = 15000),
                     selectInput("gender", "Select Gender",  choices=c("Choose a Gender"=""))
                   ),
                   mainPanel(
                     tableOutput("one")
                   )
                 )
        )
    )
)
                        
  