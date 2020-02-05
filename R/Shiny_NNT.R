#################
# Shiny App - NNT 
##################



library(shiny)
library(devtools)
#install_github('gilles-guillot/NNT')  #new
library(NNT)

ui <- fluidPage(
  
  fluidRow(
    column(4),
    column(5,tags$h1("NNT computation"))
          ),
  
  tags$p("This app allows you to easily compute the number needed to treat."),
 
  tags$br(),
  
  tags$head(
    tags$style(HTML(
      "label {font-weight:normal;}"
    ))
  ),
  
  fluidRow(
    column(3,tags$strong("First exposure quantile")),
    column(4,numericInput(inputId = "cases1", 
                        label = "Enter the number of cases observed in the first exposure quantile",
                        value = "")),
    column(4,numericInput(inputId = "py1", 
                          label = "Enter the number of PY in the first exposure quantile",
                          value = ""))
          ),
  
  fluidRow(
    column(3,tags$strong("Second exposure quantile")),
    column(4,numericInput(inputId = "cases2", 
                          label = "Enter the number of cases observed in the second exposure quantile",
                          value = "")),
    column(4,numericInput(inputId = "py1", 
                          label = "Enter the number of PY in the second exposure quantile",
                          value = ""))
          ),
  
  tags$br(),
  
  fluidRow(
    column(5),
    column(2,actionButton(inputId = "button_compute", 
               label = "Compute NNT"))
          ),
  
  tags$br(),
  tags$br(),
  
  textOutput("NNT"),
  
  textOutput("ARR"),
  
  textOutput("Error")
  
  # plotOutput("plot")
)

server <- function(input, output) {
  
  NNT_computation <- eventReactive(input$button_compute,{
    compute_NNT(c(input$cases1,input$cases2),c(input$py1,input$py2),0.05)
  })
  # if(NNT_computation()[["ARR"]]<0){
  #   output$Error <- renderText({
  #   "Warning: The ARR is negative, check your data"
  # })
  # }else{
    output$ARR <- renderText({
      ARR <- NNT_computation()[["ARR"]]
      ARR_CI <- NNT_computation()[["CI_ARR"]]
      paste("The absolute risk reduction is:",format(round(ARR,2),nsmall=2),"with 95%CI",paste("[",paste(format(round(ARR_CI,2),nsmall=2),collapse="; "),"]"),sep=" ")
    })
  
    output$NNT <- renderText({
      NNT <- NNT_computation()[["NNT"]]
      NNT_CI <- NNT_computation()[["CI_NNT"]]
      paste("The number needed to treat is:",round(NNT),"with 95%CI",paste("[",paste(round(NNT_CI),collapse="; "),"]"),sep=" ")
    })
    
    output$Error <- renderText({
      if(NNT_computation()[["ARR"]]<0){"Warning: The ARR is negative, check your data!"}
      else{""}
    })
  # }

 
  # output$plot <- renderPlot({
  #   title <- "NNT with CI"
  #   plot(c(input$cases1,input$cases2),c(input$py1,input$py2),main=title)
  # })
  
}

shinyApp(ui = ui, server = server)