#################
# Shiny App - NNT 
##################



library(shiny)
library(devtools)
# devtools::install_github('biostat-ipri/NNT')
library(NNT)


ui <- fluidPage(
  
  fluidRow(
    column(4),
    column(6,tags$h1("Number needed to treat"))
          ),
  
  tags$h5("This app provides a graphical interface to the R package NNT",
  tags$a(href= "https://github.com/biostat-ipri/NNT",tags$u("(github.com/biostat-ipri/NNT)."))),
  tags$h5("It computes the number needed to treat to save one case, and the corresponding confidence interval. For more information about how to use this App",
          tags$a(href="Help.pdf",target="_blank",tags$u("click here."))),
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
                          label = "Enter the number of person-years in the first exposure group",
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
                          label = "Enter the number of person-years in the second exposure group",
                          value = "",
                          min=1))
          ),
  
  fluidRow(
    column(3),
    column(3,numericInput(inputId = "alpha", 
                        label = "Enter 1-confidence level (alpha, by default 5%)",
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
  
)

server <- function(input, output) {
  
    NNT_computation <- eventReactive(input$button_compute,{
    empty_val <- NA %in% c(input$cases1,input$cases2,input$py1,input$py2,input$alpha)
    neg_val <- (TRUE %in% (c(input$cases1,input$cases2,input$py1,input$py2)<0) | TRUE %in% (c(input$py1,input$py2)==0))
    alpha_out <- (input$alpha>=100 | input$alpha<=0)
    moreCases <- (input$cases1>input$py1 | input$cases2>input$py2)
    if(!empty_val){
      if(!neg_val){
        if(!alpha_out){
            if(!moreCases){
              compute_NNT(c(input$cases1,input$cases2),c(input$py1,input$py2),input$alpha/100)
            }else{
              list("ARR"="more_cases",CI_ARR=c("more_cases","more_cases"),"NNT"=c("more_cases"),"CI_NNT"=c("more_cases","more_cases"))
            }
        }else{
          list("ARR"="alpha_out",CI_ARR=c("alpha_out","alpha_out"),"NNT"=c("alpha_out"),"CI_NNT"=c("alpha_out","alpha_out"))
        }
      }else{
        list("ARR"="neg",CI_ARR=c("neg","neg"),"NNT"=c("neg"),"CI_NNT"=c("neg","neg"))
      }
    }else{
      list("ARR"=NA,CI_ARR=c(NA,NA),"NNT"=c(NA),"CI_NNT"=c(NA,NA))
    }
  })
  
  
  output$ARR <- renderText({
    ARR <- NNT_computation()[["ARR"]]
    ARR_CI <- NNT_computation()[["CI_ARR"]]
    if(!is.na(ARR) & ARR!="neg" & ARR!="alpha_out" & ARR!="more_cases" & ARR>0){
      paste("The absolute risk reduction is: ",format(round(ARR,5),nsmall=5)," with ",100-isolate(input$alpha),"%CI ",paste("[",paste(format(round(ARR_CI,5),nsmall=5),collapse="; "),"]."),sep="")
    }
  })
  
  output$NNT <- renderText({
    ARR <- NNT_computation()[["ARR"]]
    NNT <- NNT_computation()[["NNT"]]
    NNT_CI <- NNT_computation()[["CI_NNT"]]
    if(!is.na(NNT) & ARR!="neg" & ARR!="alpha_out" & ARR!="more_cases" & ARR>0){
      if(length(NNT_CI)==4){
        CI2print <- paste("]",paste(round(NNT_CI)[1:2],collapse="; "),"] U [",paste(round(NNT_CI)[3:4],collapse="; "),"[.")
      }else{
        CI2print <- paste("[",paste(round(NNT_CI),collapse="; "),"].")
      }
      paste("The number needed to treat is: ",round(NNT)," with ",100-isolate(input$alpha),"%CI ",CI2print,sep="")
    }
  })
 
  output$Error <- renderText({
    if(is.na(NNT_computation()[["ARR"]])){"Warning: some values are missing!"}  
    else if(NNT_computation()[["ARR"]]=="neg"){"Warning: some values are negative or PYs are null, check your data!"} 
    else if(NNT_computation()[["ARR"]]=="alpha_out"){"Warning: your alpha is out of bound, choose an alpha in the interval ]0-100[."} 
    else if(NNT_computation()[["ARR"]]=="more_cases"){"Warning: you entered more cases than person-years, check your data!"} 
    else if(NNT_computation()[["ARR"]]<=0){"Warning: the incidence in group 2 is higher than in group 1 (or is the same), check your data!"}
  })
  
}

shinyApp(ui = ui, server = server)