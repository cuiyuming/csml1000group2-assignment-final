#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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
                        tabPanel("Predicted Plot",  
                                 plotOutput("plot_predicted")
                        ),
                        tabPanel("Corralation Plot",  
                                 fluidRow(
                                    splitLayout(cellWidths = c("50%", "50%"), plotOutput("plot_age"), plotOutput("plot_cp")),
                                    splitLayout(cellWidths = c("50%", "50%"), plotOutput("plot_trestbps"), plotOutput("plot_chol"))
                                )
                        ),
                        tabPanel("UnSupervised Plot",  
                                 fluidRow(
                                   splitLayout(cellWidths = c("50%", "80%"), plotOutput("plot_princomp"), plotOutput("plot_prcomp"))
                              
                                 )
                        ),
                        tabPanel("Supervised Plot",  
                                 fluidRow(
                                   splitLayout(cellWidths = c("50%", "50%"), plotOutput("plot_model_lr"), plotOutput("plot_model_dt")),
                                   splitLayout(cellWidths = c("50%", "80%"), plotOutput("plot_model_ann"), plotOutput("plot_model_gb"))
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
  load("scaled.Rdata")
    
  lr_model <- readRDS("./logisticRegression.rds")
  dt_model <- readRDS("./decisionTree.rds")
  ann_model <- readRDS("./artificialNeuralNetwork.rds")
  gb_model <- readRDS("./gradientBoosting.rds")
  
  lr.prob <- predict(lr_model, type="response")
  lr.predict <- ROCR::prediction(lr.prob, trainData$target)
  lr.perf <- performance(lr.predict, "tpr", "fpr")
  
  
  inputList <-reactive ({  
    
    inputData <- data.frame(input$age,
                   as.numeric(input$sex),
                   input$cp,
                   input$trestbps,
                   input$chol,
                   as.numeric(input$fbs),
                   input$restecg,
                   input$thalach,
                   as.numeric(input$exang),
                   input$oldpeak,
                   input$slope,
                   input$ca,
                   input$thal)
  
                    
                  return (inputData)
  })
  
  pca.princomp <- princomp(data)
  pca.prcomp <- prcomp(data)
  
    
    # output$plot_predicted <- renderPlot({
    # 
    #   patientData <- trainData[1,]
    # 
    #   patientData$age = input$age
    #   patientData$sex = as.numeric(input$sex)
    #   patientData$cp =   input$cp
    #   patientData$trestbps =  input$trestbps
    #   patientData$chol = input$chol
    #   patientData$fbs =  as.numeric(input$fbs)
    #   patientData$restecg =  input$restecg
    #   patientData$thalach =  input$thalach
    #   patientData$exang =  as.numeric(input$exang)
    #   patientData$oldpeak = input$oldpeak
    #   patientData$slope =   input$slope
    #   patientData$ca = input$ca
    #   patientData$thal =  input$thal
    #   
    # 
    #   lr_predcited_value <- predict(lr_model, scale(patientData))
    #    dt_predcited_value <- predict(dt_model, items)
    #    ann_predcited_value <- predict(ann_model, items)
    #    gb_predcited_value <- predict(gb_model, items)
    # 
    #   predictedData <- c(lr_predcited_value,dt_predcited_value,ann_predcited_value,gb_predcited_value)
    #   models <- c("LR", "DT", "ANN", "GB")
    # 
    #   df1 <- data.frame(models, predictedData)
    #   df2 <- melt(df1, id.vars='models')
    # 
    # 
    #   ggplot(df2, aes(x=models, y=predictedData, fill=variable)) + geom_bar(stat='identity', position='dodge')
    # 
    # })

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
