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
    column(6,tags$h1("Number needed to treat"))
          ),
  
  tags$h5("This app provides a graphical interface to the R package NNT",
  tags$a(href= "https://github.com/gilles-guillot/NNT",tags$u("(github.com/gilles-guillot/NNT)."))),
  tags$h5("It computes the number needed to treat and the corresponding confidence interval."),
  tags$h5("It was developed at", 
          tags$a(href="https://i-pri.org/",tags$u("iPRI")),
          "for a study published as \"Efforts needed for preventing breast and colorectal cancer through changes in dietary patterns\"
          , P. Mullie, G. Guillot, C. Pizot, P. Autier, P. Boyle."),
 
  tags$br(),
  tags$br(),

  tags$head(
    tags$style(HTML(
      "label {font-weight:normal;}"
    ))
  ),
  
  fluidRow(
    column(3,tags$strong("First exposure group")),
    column(4,numericInput(inputId = "cases1", 
                        label = "Enter the number of cases observed in the first exposure group",
                        value = "",
                        min=0)),
    column(4,numericInput(inputId = "py1", 
                          label = "Enter the number of PY in the first exposure group",
                          value = "",
                          min=1))
          ),
  
  fluidRow(
    column(3,tags$strong("Second exposure group")),
    column(4,numericInput(inputId = "cases2", 
                          label = "Enter the number of cases observed in the second exposure group",
                          value = "",
                          min=0)),
    column(4,numericInput(inputId = "py2", 
                          label = "Enter the number of PY in the second exposure group",
                          value = "",
                          min=1))
          ),
  
  fluidRow(
    column(3),
    column(3,numericInput(inputId = "alpha", 
                        label = "Enter your alpha error (in %)",
                        value = "5",
                        min=0,
                        max=100))
          ),
  
  tags$br(),
  
  fluidRow(
    column(5),
    column(2,actionButton(inputId = "button_compute", label = "Compute NNT",style="color: #fff; background-color: #337ab7; border-color: #2e6da4;font-weight:bold"))
          ),
  
  tags$br(),
  tags$br(),
  
  textOutput("Error"),
  tags$head(tags$style("#Error{color: red;
                       font-weight: bold;
                       }")),
  
  textOutput("NNT"),
  
  textOutput("ARR")
  
  # plotOutput("plot")
)

server <- function(input, output) {

  NNT_computation <- eventReactive(input$button_compute,{
    torf <- NA %in% c(input$cases1,input$cases2,input$py1,input$py2,input$alpha)
    if(!torf){
      compute_NNT(c(input$cases1,input$cases2),c(input$py1,input$py2),input$alpha/100)
    }else{
      list("ARR"=NA,CI_ARR=c(NA,NA),"NNT"=c(NA),"CI_NNT"=c(NA,NA))
    }
  })
  
  output$ARR <- renderText({
    ARR <- NNT_computation()[["ARR"]]
    ARR_CI <- NNT_computation()[["CI_ARR"]]
    if(!is.na(ARR) & ARR>0){
      paste("The absolute risk reduction is: ",format(round(ARR,5),nsmall=5)," with ",100-input$alpha,"%CI ",paste("[",paste(format(round(ARR_CI,5),nsmall=5),collapse="; "),"]"),sep="")
    }
  })
  
  output$NNT <- renderText({
    ARR <- NNT_computation()[["ARR"]]
    NNT <- NNT_computation()[["NNT"]]
    NNT_CI <- NNT_computation()[["CI_NNT"]]
    if(!is.na(NNT) & ARR>0){
      paste("The number needed to treat is: ",round(NNT)," with ",100-input$alpha,"%CI ",paste("[",paste(round(NNT_CI),collapse="; "),"]"),sep="")
    }
  })
  #
  output$Error <- renderText({
    if(is.na(NNT_computation()[["ARR"]])){"Warning: Some values are missing!"}  
    else if(NNT_computation()[["ARR"]]<=0){"Warning: The absolute risk reduction is negative or null, check your data!"}
  })
  

 
  # output$plot <- renderPlot({
  #   title <- "NNT with CI"
  #   plot(c(input$cases1,input$cases2),c(input$py1,input$py2),main=title)
  # })
  
}

shinyApp(ui = ui, server = server)