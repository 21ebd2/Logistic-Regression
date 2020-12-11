#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(ggplot2)
library(tidyverse)
library(cowplot)


mypath <- "C:/Users/Ella Dunn/Desktop/Data Visualization/Shiny/Final Project/Logistic-Regression/socialnetworkads.csv"
data <- read_csv(mypath)
data$Purchased <- factor(data$Purchased)
gender <- data %>% select(Gender)

shinyServer(function(input, output, session) {
    
    updateSelectInput(session, "gender", choices = c("Choose a Gender"="", gender))
    
    predictdata <- reactive({
            data <- data %>% filter(age ==input$age, gender = input$gender,
                                    salary = input$salary)
        return(data)
    })
    
    output$one <- renderTable({
        count <- data %>% summarize(sum = sum(Purchased))
                 
                 # valueBox(
                 #     value =  count,
                 #     subtitle = "Predicted Value",
                 #     icon = icon("table")
                 # )
    })
    
    plotdata <- reactive({
        if (input$rb == "age1") {
            x.variable <- data %>% filter(x.variable == Age)
            xvariable.new <- seq(15000, 50000, 100000, 125000, 150000)
            
        }
        else
            if (input$rb == "salary1") {
                x.variable <- data %>% filter(x.variable == EstimatedSalary)
                xvariable.new <- seq(20, 30, 40, 50, 60)
            }
        return(data)
    })
    
    # plotwithgender <- reactive({
    #     if ()
    # })
    
    output$logisticplot <- renderPlot({
        
        fit1 <- glm(data$Purchased ~ data$x.variable, family = binomial(link = logit))
        
        
        predictdata <- tibble(
            Age = Age.new,
            log.odds = predict(fit1, newdata = data.frame(Age = Age.new), type = "link"),
            odds = exp(log.odds),
            prob = odds/(l+odds)
        )
        
        plot.logodds <- ggplot(predictdata, aes(x = Age, y = log.odds)) +
            geom_line(color = "blue", size = 1) + theme_bw() + 
        plot.odds <- ggplot(predictdata, aes(x = Age, y = odds)) +
            geom_line(color = "red", size = 1) + theme_bw()
        plot.probs <- ggplot(predictdata, aes(x = Age, y = prob)) +
            geom_line(color = "green", size = 1) + theme_bw()
    
        p <- plot_grid(plot.logodds, plot.odds, plot.probs, nrow = 1)
        
        return(p)
        
        })
    
})