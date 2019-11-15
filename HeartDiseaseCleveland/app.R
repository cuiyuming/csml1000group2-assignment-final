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
library(EnvStats)
library(nnet)

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
                        tabPanel("Visual Output",  
                                 fluidRow(
                                    splitLayout(cellWidths = c("50%", "50%"), plotOutput("plot_age"), plotOutput("plot_cp")),
                                    splitLayout(cellWidths = c("50%", "50%"), plotOutput("plot_trestbps"), plotOutput("plot_chol"))
                                )
                        ),
                        tabPanel("Supervised Models",  
                                 fluidRow(
                                   splitLayout(cellWidths = c("50%", "50%"), plotOutput("plot_model_lr"), plotOutput("plot_model_dt")),
                                   splitLayout(cellWidths = c("50%", "50%"), plotOutput("plot_model_ann"), plotOutput("plot_model_gb"))
                                 )
                        ),
                        # fluidRow(
                        #     splitLayout(cellWidths = c("50%", "50%"), plotOutput("plot_model_lr"), plotOutput("plot_model_dt")),
                        #     splitLayout(cellWidths = c("50%", "50%"), plotOutput("plot_model_ann"), plotOutput("plot_model_gb"))
                        # )
                        tabPanel("Data", tableOutput("table"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

   load("heart.RData")
 load("trainData.RData")
    
    lr_model <- readRDS("./logisticRegression.rds")
    dt_model <- readRDS("./decisionTree.rds")
    ann_model <- readRDS("./artificialNeuralNetwork.rds")
    gb_model <- readRDS("./gradientBoosting.rds")
    lr.prob <- predict(lr_model, type="response")
    lr.predict <- prediction(lr.prob, trainData$target)
    lr.perf <- performance(lr.predict, "tpr", "fpr")
    
  

    r <- reactive({
         age <- input$age
        sex <- input$sex
        cp <- input$cp
        trestbps <- input$trestbps
        chol <- input$chol
        fbs <- input$fbs
        restecg <- input$restecg
        thalach <- input$thalach
        exang <- input$exang
        oldpeak <- input$oldpeak
        slope <- input$slope
        ca <- input$ca
        thal <- input$thal
    
        r <- list(heart=heart)
        return(r)
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
   
    output$table <- renderTable({
        r()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
