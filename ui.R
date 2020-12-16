#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/[ ]
#

library(shiny)
library(plotly)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(DT)


navbarPage("Logistic Regression", theme = shinytheme("flatly"),
           header = tagList(useShinydashboard()),
           tabPanel("Plotting with LR",
                    sidebarLayout(
                        sidebarPanel(
                            radioButtons("rb", "Select Predictor Variable:", 
                                         choices = list("Age" = "AGE",
                                                        "Estimated Salary" = "Estimated.Salary"),
                                         selected = "AGE"),
                            awesomeCheckbox("gender1", "Also Plot Gender")
                        ),
                        mainPanel(
                            fluidPage(
                                box(title = "Visualization of Logistic Regression",
                                    status = "primary",
                                    solidHeader = T, color = "yellow",
                                    width = 12, plotOutput("logisticplot"))  
                            )
                        ))),
           tabPanel("Parameter Estimates", fluid = TRUE,
                    sidebarLayout(
                        sidebarPanel(
                            sliderInput("age", "Select Age:", min = 18, max = 60, value = 18, sep = ""),
                            sliderInput("salary", "Select Estimated Salary", min = 15000, 
                                        max = 150000, value = 15000),
                            selectInput("gender", "Select Gender",  choices=c("Male", "Female"))
                        ),
                        mainPanel(
                            fluidRow(
                                infoBoxOutput("prediction", width = 12)),
                            fluidRow(
                                infoBoxOutput("confinterval", width = 12)),
                            fluidRow(
                                infoBoxOutput("Sentence", width = 12)
                            )
                        )
                    )),
           tabPanel("Data Summary",
                    sidebarLayout(
                        sidebarPanel(
                            radioButtons("rb1", "Select a Variable:", 
                                         choices = list("Age" = "Age",
                                                        "Estimated Salary" = "Estimated_Salary",
                                                        "Gender" = "Gender",
                                                        "Purchased" = "Purchased"),
                                         selected = "Age")),
                        mainPanel(
                            fluidRow(
                                infoBoxOutput('description', width = 12)),
                            fluidRow(
                                box(title = "Visualization of Data", status = "primary",
                                    solidHeader = T, color = "yellow",
                                    width = 12, plotOutput("summaryplot"))),
                            fluidRow(
                                infoBoxOutput("max", width = 12)
                            )
                        ))),
           tabPanel("Raw Data",
                    fluidPage(
                        DTOutput("mydata")
                    )
           )
)

