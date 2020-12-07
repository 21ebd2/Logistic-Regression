#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(readr)
library(plotly)
library(tidyverse)


mypath <- "C:/Users/Ella Dunn/Desktop/Data Visualization/Shiny/Final Project/Logistic-Regression/socialnetworkads.csv"
data <- read_csv(mypath)
data$Purchased <- factor(data$Purchased)
gender <- data %>% select(Gender)

shinyServer(function(input, output, session) {
    
    updateSelectInput(session, "gender", choices = c("Choose a Gender"="", gender))
    
    output$n1 <- renderValueBox({
        count <- data %>% 
            summarize(sum = sum(Purchased))
        
        valueBox(
            value =  count,
            subtitle = "Total Purchased",
            icon = icon("table")
        )
    })
    
    plotdata <- reactive({
        if(input$rb == "age") {
            data <- data %>% filter(xvariable == Age, Gender == input$gender)
        }
        else
            if(input$rb == "salary") {
                data <- data %>% filter(xvariable == EstimatedSalary, Gender == input$gender)
            }
        return(data)
    })
    
    output$logisticplot <- renderPlotly({
        
        fit1 <- glm(plotdata()$Purchased ~ plotdata()$xvariable, family = binomial(link = logit))
        
        plottingdata <- tibble(
            xvariable = xvariable.new,
            log.odds = predict(fit1, newdata = data.frame(xvariable = xvariable.new), type = "link"),
            odds = exp(log.odds),
            prob = odds/(l+odds)
        )
        
        p <- plot_ly(plotdata(),
                     x = ~xvariable, y=~Purchased,
                     type = 'scatter', mode = 'markers') %>%
            layout(title = "Logistic Regression: Prob. of Purchase",
                   yaxis = list("Probability of Purchase"))
        return(p)
        
    })
    
})