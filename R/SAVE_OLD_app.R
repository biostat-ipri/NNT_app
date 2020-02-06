#################
# Shiny App - NNT 
##################



library(shiny)
library(devtools)
#install_github('gilles-guillot/NNT')  #new
library(NNT)

ui <- fluidPage(
  numericInput(inputId = "cases1", 
            label = "Enter the number of cases in the first category of exposure",
            value = ""),
  numericInput(inputId = "py1", 
            label = "Enter the number of PY in the first category of exposure",
            value = ""),
  numericInput(inputId = "cases2", 
            label = "Enter the number of cases in the second category of exposure",
            value = ""),
  numericInput(inputId = "py2", 
            label = "Enter the number of PY in the first category of exposure",
            value = ""),
  actionButton(inputId = "button_compute", 
               label = "Compute NNT"),
  textOutput("NNT"),
  textOutput("ARR")
  # plotOutput("plot")
)

server <- function(input, output) {
  
    # 
  # observeEvent(input$button_compute, {
    NNT <- eventReactive(input$button_compute,{
      compute_NNT(c(input$cases1,input$cases2),c(input$py1,input$py2),0.05)[["NNT"]]
    })
    NNT_CI <- eventReactive(input$button_compute,{
      compute_NNT(c(input$cases1,input$cases2),c(input$py1,input$py2),0.05)[["CI_NNT"]]
    })
    ARR <- eventReactive(input$button_compute,{
      compute_NNT(c(input$cases1,input$cases2),c(input$py1,input$py2),0.05)[["ARR"]]
    })
    ARR_CI <- eventReactive(input$button_compute,{
      compute_NNT(c(input$cases1,input$cases2),c(input$py1,input$py2),0.05)[["CI_ARR"]]
    })
    
    output$ARR <- renderText({
      paste("The absolute risk reduction is:",round(ARR(),2),"with 95%CI",paste("[",paste(round(ARR_CI(),2),collapse="; "),"]"),sep=" ")
    })
    
    output$NNT <- renderText({
      paste("The number needed to treat is:",round(NNT()),"with 95%CI",paste("[",paste(round(NNT_CI()),collapse="; "),"]"),sep=" ")
    })
 
 # })
 
  # output$plot <- renderPlot({
  #   title <- "NNT with CI"
  #   plot(c(input$cases1,input$cases2),c(input$py1,input$py2),main=title)
  # })
  
}

shinyApp(ui = ui, server = server)