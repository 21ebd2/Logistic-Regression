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
library(shinyWidgets)
library(readr)
library(tidyverse)
library(readr)
library(ggplot2)
library(cowplot)
library(DT)



mypath <- "https://storage.googleapis.com/kagglesdsdata/datasets/7812/11044/Social_Network_Ads.csv?X-Goog-Algorithm=GOOG4-RSA-SHA256&X-Goog-Credential=gcp-kaggle-com%40kaggle-161607.iam.gserviceaccount.com%2F20201215%2Fauto%2Fstorage%2Fgoog4_request&X-Goog-Date=20201215T162011Z&X-Goog-Expires=259199&X-Goog-SignedHeaders=host&X-Goog-Signature=8c7f8562ed534958f2aa49919e36c6b5183daae0f7e16061a9e076afc03d55b0499c7ce794fec83c80089218b73fcd56b42c8d8d7107215b84e8040deba77805bb13c755ab6272288b445faeec87fe813efa18398eeb2f7cbc492890f3030f1d7a7bdd1679e72928f1b0d0858b998f08ff3ec1bc8c55aafc6568a3931759673ab2a8092eba18c423f862c4a82c54ed9b25f19711d9aa0f8c9fb9345da980e2c736ff6f091ce758f9441bc04aa89acae890f5162feab6466cbf9498081771034981031080872fd20431850473f69c7a219761b59a603b6c38704de6fef1edb48958b3abdf292f8dd80a4573852699760988b824d588363fc61069d3bbb7ef521a"
logisticdata <- read_csv(mypath)
logisticdata$Purchased <- factor(logisticdata$Purchased)
gender <- logisticdata %>% select(Gender)

shinyServer(function(input, output, session) {
    
    predictdata <- reactive({
        fit <- glm(Purchased ~ Age + Gender + EstimatedSalary,
                   data = logisticdata, family = "binomial")
        prediction <- predict(fit,
                              newdata = data.frame(Age = input$age,
                                                   Gender = input$gender,
                                                   EstimatedSalary = input$salary),
                              type = "response")
        return(round(prediction*100,2))
    })
    
    confintervaldata <- reactive({
        fit <- glm(Purchased ~ Age + Gender + EstimatedSalary,
                   data = logisticdata, family = "binomial")
        prediction <- predict(fit,
                              newdata = data.frame(Age = input$age,
                                                   Gender = input$gender,
                                                   EstimatedSalary = input$salary),
                              se.fit = TRUE)
        log.odds <- prediction$fit
        se.log.odds <- prediction$se.fit
        ci <- log.odds + c(-1,1)*qnorm(0.975)*se.log.odds
        
        return(round(exp(ci)/(1+exp(ci)),3))
    })
    
    
    output$prediction <- renderInfoBox({
        
        infoBox(
            "Predicted Probability of Purchase", paste0(predictdata(), "%"),
            icon = icon("shopping-cart"),
            color = "aqua", fill = T
        )
    })
    
    output$confinterval <- renderInfoBox({
        
        infoBox(
            "95% Confidence Interval", paste0("(", confintervaldata()[1], ", ", confintervaldata()[2], ")"),
            icon = icon("percent"),
            color = "blue", fill = T)
    })
    
    
    output$Sentence <- renderInfoBox({
        
        infoBox(
            "In Context", paste0("A ",input$gender," that is ", input$age,
                                 " years old and has an estimated salary of ",
                                 input$salary, " dollars has a ", predictdata(),
                                 "% chance of purchasing an item after clicking on a social media ad."),
            icon = icon("ad"),
            color = "navy", fill= T)
    })
    
    output$description <- renderInfoBox({
        
        infoBox(
            value = "",
            title = "Description of Data Set",
            subtitle = "This dataset contains information on roughly 400 respondents and whether or not they 
                    purchased an item after seeing a social media ad. Information on the respondents age,
                    gender, and estimated salary is given, along with the binary outcome of whether or not
                    they made a purchase (1 = yes, 0 = no)",
            icon = icon("comment-dots"),
            fill = TRUE,
            color = "light-blue"
        )
    })
    
    summary <- reactive({
        if (req(input$rb1) == "Age") {
            summary <- logisticdata %>% mutate(x.variable = Age)
        }
        else
            if (req(input$rb1) == "Estimated_Salary") {
                summary <- logisticdata %>% mutate(x.variable = EstimatedSalary)
            }
        else
            if (req(input$rb1) == "Gender") {
                summary <- logisticdata %>% mutate(x.variable = Gender)
            }
        else
            if (req(input$rb1) == "Purchased") {
                summary <- logisticdata %>% mutate(x.variable = Purchased)
            }
        return(summary)
    })
    
    plottype <- reactive({
        if (input$rb1 == "Age" || input$rb1 == "Estimated_Salary"){
            p <- geom_histogram(fill = "light pink", bins = 10)
        }
        else
            if (input$rb1 == "Gender" || input$rb1 == "Purchased"){
                p <- geom_bar(fill = "lightpink") 
            }
        return(p)
    })
    
    output$summaryplot<- renderPlot({
        
        plot <- ggplot(data = summary(), aes(x = x.variable)) + 
            plottype() + labs( x = input$rb1, y = "Frequency") +
            theme_classic()
        return(plot)
    })
    
    output$max <- renderValueBox({
        if (input$rb1 == "Age"){
            infoBox(
                "Min and Max Values", paste0("The min value of Age is 18 and maximum value is 60"),
                icon = icon("sticky-note"),
                color = "light-blue")
        }
        else
            if (input$rb1 == 'Estimated_Salary'){
                infoBox(
                    "Min and Max Values", paste0("The min value of Estimated Salary is $15,000 and maximum value is $150,000"),
                    icon = icon("sticky-note"),
                    color = "light-blue")
            }
        else
            if (input$rb1 == 'Gender'){
                infoBox("Gender Composition", paste0("The sample is 51% female and 49% male"),
                        icon = icon("sticky-note"),
                        color = "light-blue")
            }
        else
            if (input$rb1 == 'Purchased'){
                infoBox("Purchased Composition", paste0("35.8% of respondents purchased an item after clicking on a social media ad"),
                        icon = icon("sticky-note"),
                        color = "light-blue")
            }
    })
    
    output$mydata <- renderDT({
        df <- logisticdata
        return(df)
    })
    
    newdata <- reactive({
        if (req(input$rb) == "AGE") {
            newdata <- logisticdata %>% mutate(x.variable = Age) 
        }
        else
            if (req(input$rb) == "Estimated.Salary") {
                newdata <- logisticdata %>% mutate(x.variable = EstimatedSalary)
            }
        return(newdata)
    })
    
    xvariablenew <- reactive({
        if (req(input$rb) == "Estimated.Salary") {
            xvariable.new <- seq(15000, 150000, length.out = 100)
        }
        else
            if (req(input$rb) == "AGE") {
                xvariable.new <- seq(18, 60, length.out = 100)
            }
        return(xvariable.new)
    })
    
    output$logisticplot <- renderPlot({
        
        
        if (!input$gender1) {
            fit1 <- glm(Purchased ~ x.variable,
                        family = binomial(link = logit), data = newdata())
            
            plotdata <- tibble(
                x.variable = xvariablenew(),
                log.odds = predict(fit1, newdata = data.frame(x.variable = xvariablenew()),
                                   type = "link"),
                odds = exp(log.odds),
                prob = odds/(1+odds)
            )
            
            predictions <- predict(fit1, newdata = data.frame(x.variable = xvariablenew()),
                                   type = "link", se.fit = TRUE)
            se.log.odds <- predictions$se.fit
            
            plotdata <- plotdata %>% mutate(
                log.odds.LB = log.odds - 1.96*se.log.odds,
                log.odds.UB = log.odds + 1.96*se.log.odds,
                odds.LB = exp(log.odds.LB),
                odds.UB = exp(log.odds.UB),
                prob.LB = odds.LB/(1+odds.LB),
                prob.UB = odds.UB/(1+odds.UB)
            )
            
            plot.logodds <- ggplot(plotdata, aes(x = x.variable, y = log.odds)) +
                geom_line(color = "blue", size = 1) + 
                geom_ribbon(aes(ymin = log.odds.LB, ymax = log.odds.UB), fill = "blue", alpha = .1) +
                theme_bw() + labs(x= input$rb)
            plot.odds <- ggplot(plotdata, aes(x = x.variable, y = odds)) +
                geom_line(color = "red", size = 1) + 
                geom_ribbon(aes(ymin = odds.LB, ymax = odds.UB), fill = "red", alpha = .1) +
                theme_bw() + labs(x= input$rb)
            plot.probs <- ggplot(plotdata, aes(x = x.variable, y = prob)) +
                geom_line(color = "green", size = 1) + 
                geom_ribbon(aes(ymin = prob.LB, ymax = prob.UB), fill = "green", alpha = .1) +
                theme_bw() + labs(x= input$rb)
            
            
            p <- plot_grid(plot.logodds, plot.odds, plot.probs, nrow = 1)
        }
        
        else{
            fit1 <- glm(Purchased ~ x.variable + Gender,
                        family = binomial(link = logit), data = newdata())
            
            mygrid <-expand.grid(x.variable = xvariablenew(), Gender = c("Male","Female"))
            
            predictions <- predict(fit1, newdata = mygrid, type = "link", se.fit = TRUE)
            
            plotdata <- tibble(
                x.variable = mygrid$x.variable, 
                Gender = mygrid$Gender,
                log.odds = predictions$fit,
                log.odds.LB = log.odds - 1.96*predictions$se.fit,
                log.odds.UB = log.odds + 1.96*predictions$se.fit,
                odds = exp(log.odds),
                odds.LB = exp(log.odds.LB),
                odds.UB = exp(log.odds.UB),
                prob = exp(log.odds)/(1 + exp(log.odds)),
                prob.LB = exp(log.odds.LB)/(1+exp(log.odds.LB)),
                prob.UB = exp(log.odds.UB)/(1+exp(log.odds.UB))
            )
            
            plot.logodds <- ggplot(plotdata, aes(x = x.variable, y = log.odds, color = Gender)) +
                geom_line(size = 1) +
                geom_ribbon(aes(ymin = log.odds.LB, ymax = log.odds.UB, fill = Gender), color = NA, alpha= .5) +
                scale_color_manual(values = c("navy", "dodgerblue1"), name = "Gender:") + 
                scale_fill_manual(values = c("lightskyblue3", "cadetblue1"), name = "Gender:") + 
                theme_bw() + theme(legend.position = "top") + 
                labs(x= input$rb)
            
            
            plot.odds <- ggplot(plotdata, aes(x = x.variable, y = odds, color = Gender)) +
                geom_line(size = 1) +
                geom_ribbon(aes(ymin = odds.LB, ymax = odds.UB, fill = Gender), color = NA, alpha= .2) +
                scale_color_manual(values = c("red", "purple"), name = "Gender:") + 
                scale_fill_manual(values =  c("pink", "light pink"), name = "Gender:") + 
                theme_bw() + theme(legend.position = "top") + 
                labs(x= input$rb)
            
            
            plot.probs <- ggplot(plotdata, aes(x = x.variable, y = prob, color = Gender)) +
                geom_line(size = 1) +
                geom_ribbon(aes(ymin = prob.LB, ymax = prob.UB, fill = Gender), color = NA, alpha= .2) +
                scale_color_manual(values = c("blue", "green"), name = "Gender:") + 
                scale_fill_manual(values = c("light blue", "light green"), name = "Gender:") + 
                theme_bw() + theme(legend.position = "top") + 
                labs(x= input$rb)
            
            
            p <- plot_grid(plot.logodds, plot.odds, plot.probs, nrow = 1)
        }    
        
        return(p)
        
    })
    
})