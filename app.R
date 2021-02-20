#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Import libraries
library(shiny)
library(shinythemes)
library(data.table)
library(RCurl)
library(randomForest)


publicidad_data <- read.csv("Publicidad_Cereales_Compras.csv")#

#publicidad_data$Compraron <- ifelse(publicidad_data$Compraron=='yes',1,0)
#publicidad_data$hijos <- ifelse(publicidad_data$hijos=='yes',1,0)
#publicidad_data$VieronAnunc <- ifelse(publicidad_data$VieronAnunc=='yes',1,0)
#weather$play<- as.integer(weather$play)
publicidad_data$hijos<- as.factor(publicidad_data$hijos)
publicidad_data$VieronAnunc<- as.factor(publicidad_data$VieronAnunc)
publicidad_data$Compraron<-as.factor(publicidad_data$Compraron)
#str(publicidad_data)

model <- randomForest(Compraron ~ ., data = publicidad_data, ntree = 300, mtry = 3, importance = TRUE)
model
# Read in the RF model
#model <- readRDS("model.rds")

####################################
# User interface                   #
####################################

ui <- fluidPage(theme = shinytheme("united"),
                
                # Page header
                headerPanel('Propensión de compra '),
                
                # Input values
                sidebarPanel(
                  HTML("<h3>Input parameters</h3>"),
                  
                  selectInput("hijos", label = "hijos:", 
                              choices = list("yes" = "yes", "no" = "no"), 
                              selected = "no"),
                  sliderInput("Ingresos", "Ingresos:",
                              min = 10, max = 60,
                              value = 45),
                  selectInput("VieronAnunc", label = " VieronAnuncio:", 
                              choices = list("yes" = "yes", "no" = "no"), 
                              selected = "no"),
                  
                  actionButton("analizar", "Submit", class = "btn btn-primary")
                ),
                
                mainPanel(
                  tags$label(h3('Status/Output')), # Status/Output Text Box
                  verbatimTextOutput('contents'),
                  tableOutput('tabledata') # Prediction results table
                  
                )
)

####################################
# Server                           #
####################################

server <- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    # outlook,temperature,humidity,windy,play
    df <- data.frame(
      Name = c("hijos",
               "Ingresos",
               "VieronAnunc"),
      Value = as.character(c(input$hijos,
                             input$Ingresos,
                             input$VieronAnunc)),
      stringsAsFactors = FALSE)
    
    Compraron <- "Compraron"
    df <- rbind(df, Compraron)
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    
    test$hijos <- factor(test$hijos, levels = c("yes", "no"))
    test$VieronAnunc <- factor(test$VieronAnunc, levels = c("yes", "no"))
    
    
    Output <- data.frame(Prediction=predict(model,test), round(predict(model,test,type="prob"), 3))
    print(Output)
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$analizar>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$analizar>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)
