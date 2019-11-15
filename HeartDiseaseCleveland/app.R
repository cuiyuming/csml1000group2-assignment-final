#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

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
                        tabPanel("Visual Output", plotOutput("plot1", height="500px")),
                        tabPanel("Data", tableOutput("table"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    in.age <- input$age
    in.sex <- input$sex
    in.cp <- input$cp
    in.trestbps <- input$trestbps
    in.chol <- input$chol
    in.fbs <- input$fbs
    in.restecg <- input$restecg
    in.thalach <- input$thalach
    in.exang <- input$exang
    in.oldpeak <- input$oldpeak
    in.slope <- input$slope
    in.ca <- input$ca
    in.thal <- input$thal
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
