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
library(tidyverse)
library(readr)
library(ggplot2)
library(cowplot)
library(DT)



mypath <- "https://storage.googleapis.com/kagglesdsdata/datasets/7812/11044/Social_Network_Ads.csv?X-Goog-Algorithm=GOOG4-RSA-SHA256&X-Goog-Credential=gcp-kaggle-com%40kaggle-161607.iam.gserviceaccount.com%2F20201218%2Fauto%2Fstorage%2Fgoog4_request&X-Goog-Date=20201218T163352Z&X-Goog-Expires=259199&X-Goog-SignedHeaders=host&X-Goog-Signature=02d71235c4e9474056645d2066254d7827f45dc3b48c2bb6a0ea8675a3d3b68b6222219d76a98974fdaaf7790ee68cb6bb89bac7ff69a6ec36c470c0adf59075625f4d8860b71f8f4efdc8e51eb20e0f170f115012c927c82964b25e0dec8ba537d2577075ccce84c3aa8afc51426d9733f6bf004c27cd79795df2e7affb02455bebab9a857627095c5a64668a4495216dfc5ecc230d3bd6ac1bdaa5cd26ca8e2c8f0b327b56259ddcee6858cf601bd9bb1e0901b3b3d6e44f9d5bbf86e86348485df533768d5d4cd40e78abd23f103e55c4cf11b2eb315543b2f4392d92923643fb7b85729c7b511e562ea9e24a99079b9d4196cfe048fb99ec179c2ff0fca7"
logisticdata <- read_csv(mypath)
dataforsummary <- logisticdata %>% mutate(quantPurchased = Purchased)
logisticdata$Purchased <- factor(logisticdata$Purchased)

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
        
        return(round((exp(ci)/(1+exp(ci)))*100,2))
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
            "95% Confidence Interval", paste0("(", confintervaldata()[1], "%, ", confintervaldata()[2], "%)"),
            icon = icon("shopping-basket"),
            color = "blue", fill = T)
    })
    
    
    output$Sentence <- renderInfoBox({
        
        infoBox(
            "In Context", paste0("A ",input$gender," that is ", input$age,
                                 " years old and has an estimated salary of ",
                                 input$salary, " dollars has an expected probability of ", predictdata(),
                                 "% of purchasing an item after clicking on a social media ad."),
            icon = icon("shopping-bag"),
            color = "navy", fill= T)
    })
    
    output$description <- renderInfoBox({
        
        infoBox(
            value = "",
            title = "Description of Data Set",
            subtitle = "This dataset contains information on roughly 400 respondents and whether or not they 
                    purchased an item after seeing a social media ad. Information on the respondents age,
                    gender, and estimated salary is given, along with the binary outcome of whether or not
                    they made a purchase (1 = yes, 0 = no). Source: Kaggle.com",
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
    
     output$min <- renderValueBox({
         if (input$rb1 == "Age"){
             valueBox(
                 "Min", min(logisticdata$Age), color = "red")
         }
         else
             if (input$rb1 == 'Estimated_Salary'){
                 valueBox(
                     "Min", min(logisticdata$EstimatedSalary), color = "red")
             }
         else 
             if (input$rb1 == 'Gender'){
                valueBox(
                     "Min", "NA", color = "red")
         }
         else
             if (input$rb1 == 'Purchased'){
                 valueBox(
                     "Min", 0, color = "red")
             }
     })
     
     output$max <- renderValueBox({
         if (input$rb1 == "Age"){
             valueBox(
                 "Max", max(logisticdata$Age), color = "blue")
         }
         else
             if (input$rb1 == 'Estimated_Salary'){
                 valueBox(
                     "Max", max(logisticdata$EstimatedSalary), color = "blue")
             }
         else
             if (input$rb1 == 'Gender'){
                valueBox(
                 "Max", "NA", color = "blue")
         }
         else
             if (input$rb1 == 'Purchased'){
                 valueBox(
                     "Max", 1, color = "blue")
             }
     })
     
     output$mean <- renderValueBox({
         if (input$rb1 == "Age"){
             valueBox(
                 "Mean", round(mean(logisticdata$Age),2), color = "green")
         }
         else
             if (input$rb1 == 'Estimated_Salary'){
                 valueBox(
                     "Mean", round(mean(logisticdata$EstimatedSalary),2), color = "green")
             }
         else
             if (input$rb1 == 'Gender'){
                valueBox(
                 "Mean", "NA", color = "green")
         }
         else
             if (input$rb1 == 'Purchased'){
                 valueBox(
                     "Mean", round(mean(dataforsummary$quantPurchased),2), color = "green")
             }
     })
     
     output$sd <- renderValueBox({
         if (input$rb1 == "Age"){
             valueBox(
                 "S.D.", round(sd(logisticdata$Age),2), color = "yellow")
         }
         else
             if (input$rb1 == 'Estimated_Salary'){
                 valueBox(
                     "S.D.", round(sd(logisticdata$EstimatedSalary),2), color = "yellow")
             }
        else
            if (input$rb1 == 'Gender'){
                 valueBox(
                 "S.D.", "NA", color = "yellow")
         }
        else
             if (input$rb1 == 'Purchased'){
                 valueBox(
                     "S.D.", round(sd(dataforsummary$quantPurchased),2), color = "yellow")
         }
})
     
     output$categorical <- renderValueBox({
         if (input$rb1 == "Age"){
             valueBox("Age Composition", paste0("The histogram of age shows a roughly uniform distribution with 
                                                values rannging from 18 to 60"),
                      color = "light-blue")
         }
         else
             if (input$rb1 == 'Estimated_Salary'){
                 valueBox("Estimated Salary Composition", paste0("The histogram of estimated salaries is slighly
                                                                 skewed right with values ranging from 15,000 to 150,000"),
                          color = "light-blue")
             }
        else
            if (input$rb1 == 'Gender'){
                valueBox("Gender Composition", paste0("The sample is 51% female and 49% male"),
                     color = "light-blue")
         }
        else
            if (input$rb1 == 'Purchased'){
                 valueBox("Purchased Composition", paste0("35.8% of respondents purchased an item after clicking on a social media ad"),
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
                scale_color_manual(values = c("purple", "red"), name = "Gender:") + 
                scale_fill_manual(values =  c("plum1", "pink"), name = "Gender:") + 
                theme_bw() + theme(legend.position = "top") + 
                labs(x= input$rb)
            
            
            plot.probs <- ggplot(plotdata, aes(x = x.variable, y = prob, color = Gender)) +
                geom_line(size = 1) +
                geom_ribbon(aes(ymin = prob.LB, ymax = prob.UB, fill = Gender), color = NA, alpha= .2) +
                scale_color_manual(values = c("dark green", "green"), name = "Gender:") + 
                scale_fill_manual(values = c("light green", "seagreen3"), name = "Gender:") + 
                theme_bw() + theme(legend.position = "top") + 
                labs(x= input$rb)
            
            
            p <- plot_grid(plot.logodds, plot.odds, plot.probs, nrow = 1)
        }    
        
        return(p)
        
    })
    
})