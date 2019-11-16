---
# title: "CSML1000-003-O-F19 - Group 2 Project - Shiny App"
# author: "Rajiv Kaushik, Yuming Cui, Madana Bolla, Pratik Chandwani, Konstantin Krassavine"
# date: "11/15/2019"
# output: html_document
# published at https://yumingcui.shinyapps.io/HeartDiseaseCleveland/
---
  

library(shiny)
library(ggplot2)
library(reshape2)
library(ROCR)
library(factoextra)
library(gbm)
library(rpart)
library(rpart.plot)


# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    tags$style(HTML("   
     .col-sm-4 { width: 25%;}
     .col-sm-8 { width: 75%;}
    "))
  ),
    # Application title
    titlePanel("Heart Disease Cleveland"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("age","Age:", min = 29, max = 77, value = 40),
            radioButtons("sex", "Sex", inline=T, choices = list("Male" = 1, "Female" = 0),selected = 1),
            sliderInput("cp","Chest Pain:", min = 0, max = 3, value = 1),
            sliderInput("trestbps","Resting Blood Pressure:", min = 94, max = 200, value = 120),
            sliderInput("chol","Serum Cholestoral (mg/dl):", min = 126, max = 564, value = 200),
            radioButtons("fbs", "Fasting Blood Sugar", inline=T,  choices = list("> 120 mg/dl:" = 1, "< 120 mg/dl:" = 0),selected = 1),
            sliderInput("restecg","Resting Electrocardiographic:", min = 0, max = 2, value = 1),
            sliderInput("thalach","Maximum Heart Rate Achieved:", min = 71, max = 202, value = 100),
            radioButtons("exang", "Exercise Induced Angina(EIA):",  inline=T, choices = list("YES" = 1, "NO" = 0),selected = 1),
            sliderInput("oldpeak","ST depression induced by (EIA):", min = 0, max = 6.2, value = 3),
            sliderInput("slope","Slope of the peak exercise ST segment:", min = 0, max = 2, value = 1),
            sliderInput("ca","number of major vessels:", min = 0, max = 3, value = 1),
            radioButtons("thal", "THAL",  inline=T,  choices = list("Normal" = 3, "Fixed defect" = 6,"Reversable defect" = 7), selected = 3)
        ),

        # Show a plot of the generated distribution
       
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Predicted Result",  
                                 br(),
                                 h1(textOutput("text_predicted"))
                        ),
                        tabPanel("Corralation Plot",  
                                 fluidRow(
                                    splitLayout(cellWidths = c("50%", "50%"), plotOutput("plot_age"), plotOutput("plot_cp")),
                                    splitLayout(cellWidths = c("50%", "50%"), plotOutput("plot_trestbps"), plotOutput("plot_chol"))
                                )
                        ),
                        tabPanel("UnSupervised Plot",  
                            
                                 fluidRow(
                                   plotOutput("plot_princomp", width = "600px")
                                 ),
                                 fluidRow(
                                   plotOutput("plot_prcomp", width = "600px")
                                 )
                        ),
                        tabPanel("Supervised Plot",  
                                 fluidRow(
                                   plotOutput("plot_model_lr")
                                 ),
                                 fluidRow(
                                   plotOutput("plot_model_dt")
                                 )
                        ),
                        
                        tabPanel("Data", tableOutput("table"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  load("heart.RData")
  load("trainData.RData")
  load("scaled.RData")
    
  lr_model <- readRDS("./logisticRegression.rds")
  dt_model <- readRDS("./decisionTree.rds")
  ann_model <- readRDS("./artificialNeuralNetwork.rds")
  gb_model <- readRDS("./gradientBoosting.rds")
  
  lr.prob <- predict(lr_model, type="response")
  lr.predict <- ROCR::prediction(lr.prob, trainData$target)
  lr.perf <- performance(lr.predict, "tpr", "fpr")
  
  
  inputList <-reactive ({  
  
    inputData <- data.frame("age" = input$age,
                   "sex" = as.numeric(input$sex),
                   "cp" = input$cp,
                   "trestbps" = input$trestbps,
                   "chol" = input$chol,
                   "fbs" = as.numeric(input$fbs),
                   "restecg" = input$restecg,
                   "thalach" = input$thalach,
                   "exang" = as.numeric(input$exang),
                   "oldpeak" = input$oldpeak,
                   "slope" = input$slope,
                   "ca" = input$ca,
                   "thal" = input$thal)
                  return (inputData)
  })
  
  pca.princomp <- princomp(data)
  pca.prcomp <- prcomp(data)
    
  output$text_predicted <- renderText({
    age <- as.numeric(input$age)
    age <- ((age - mean(heart$age))/sd(heart$age))
    
    trestbps <- as.numeric(input$trestbps)
    trestbps <- ((trestbps - mean(heart$trestbps))/sd(heart$trestbps))
    
    chol <- as.numeric(input$chol)
    chol <- ((chol - mean(heart$chol))/sd(heart$chol))
    
    thalach <- as.numeric(input$thalach)
    thalach <- ((thalach - mean(heart$thalach))/sd(heart$thalach))
    
    oldpeak <- as.numeric(input$oldpeak)
    oldpeak <- ((oldpeak - mean(heart$oldpeak))/sd(heart$oldpeak))

    inputData <- data.frame("age" = age,
                            "sex" = as.numeric(input$sex),
                            "cp" = as.numeric(input$cp),
                            "trestbps" = trestbps,
                            "chol" = chol,
                            "fbs" = as.numeric(input$fbs),
                            "restecg" = as.numeric(input$restecg),
                            "thalach" = thalach,
                            "exang" = as.numeric(input$exang),
                            "oldpeak" = oldpeak,
                            "slope" = as.numeric(input$slope),
                            "ca" = as.numeric(input$ca),
                            "thal" = as.numeric(input$thal))
      

      lr_predcited_value <- predict(lr_model, inputData, type="response")
      
      paste("The change you will get heart disease is ", round(lr_predcited_value*100), "%")
      
    })
      

    output$plot_age <- renderPlot({
        ggplot(data=heart, aes(age, target)) + geom_jitter(height=0.03, alpha=0.2) + stat_smooth(method="loess", alpha=0.2, col="red") + ggtitle("Target By Age") + theme_bw()
    })
    
    output$plot_cp <- renderPlot({
        ggplot(data=heart, aes(cp, target)) + geom_jitter(height=0.03, alpha=0.2) + stat_smooth(method="loess", alpha=0.2, col="red") + ggtitle("Target By Chest Pain") + theme_bw()
    })
    
    output$plot_trestbps <- renderPlot({
        ggplot(data=heart, aes(trestbps, target)) + geom_jitter(height=0.03, alpha=0.2) + stat_smooth(method="loess", alpha=0.2, col="red") + ggtitle("Resting Blood Pressure") + theme_bw()
    })
    
    output$plot_chol <- renderPlot({
        ggplot(data=heart, aes(chol, target)) + geom_jitter(height=0.03, alpha=0.2) + stat_smooth(method="loess", alpha=0.2, col="red") + ggtitle("Serum Cholestoral (mg/dl)") + theme_bw()
    })
    
    
    output$plot_model_lr <- renderPlot({
        plot(lr.perf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7),main="ROC for Logistic Regression Model")
    })
    
    output$plot_model_dt <- renderPlot({
        rpart.plot(dt_model, main="Decision Tree Model")
    })
    
    output$plot_model_ann <- renderPlot({
        plot(ann_model, colorize=TRUE,  main="Artificial Neural Network Model")
    })
    
    output$plot_model_gb <- renderPlot({
      plot(gb_model, colorize=TRUE, main="Gradient Boosting Model")
    })
    
    output$plot_princomp <- renderPlot({
      fviz_pca_var(pca.princomp,
                   col.var = "contrib",
                   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                   repel = TRUE,
                   title = "Princomp PCA"
      )
    })
    
    output$plot_prcomp <- renderPlot({
      fviz_pca_var( pca.prcomp,
                   col.var = "contrib",
                   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                   repel = TRUE,
                   title = "Prcom PCA"    
      )
    })
   
    output$table <- renderTable({
        list(heart)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
