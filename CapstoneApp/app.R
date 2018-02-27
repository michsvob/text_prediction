#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(stringr)
library(dplyr)
library(tm)
library(shiny)
library(flexdashboard)

ngramdb <- readRDS("ngramdb_medium.rds")
knowns <- readRDS("knowns_medium.rds")

predict_3 <- function(input,alternatives=T,c5=1,c4=1,c3=1,c2=1,c1=1){
  input=stripWhitespace(removeNumbers(removePunctuation(input)))
  input= str_to_lower(gsub("^ * | $ *","",input)) # lowercase and strip extra whitespace before and after
  input = tail(str_split(input," ")[[1]],4)
  input=replace(input,!(input %in% knowns),"<unk>")
  input=c(rep("",4-length(input)),input)
  
  gr5=ngramdb %>% 
    filter(grams==5,predictor==paste(input,collapse=" ")) 
  #ngrams are up-voted by factor c
  
  gr4=ngramdb %>% 
    filter(grams==4,predictor==paste(input[2:4],collapse=" ")) 
  
  gr3=ngramdb %>% 
    filter(grams==3,predictor==paste(input[3:4],collapse=" "))
  
  gr2=ngramdb %>% 
    filter(grams==2,predictor==paste(input[4],collapse=" "))
  
  pred <- (bind_rows(gr5,gr4,gr3,gr2) %>% 
             group_by(prediction) %>% summarise(score=sum(condprob,na.rm=TRUE)) %>% top_n(((alternatives*4)+1),score) %>% 
             arrange(desc(score)))
  #top_n is here for performance reasons
  
  scores <- pred$score
  predictions <- pred$prediction
  
  if(length(predictions)<5){
    predictions=head(append(predictions,c('the', 'to','and', 'a','of')),5)
    scores=head(append(scores,c(0,0,0,0,0)),5) 
  }
  list(predictions,scores)
}

################################-------------------------------#########################


library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Text Predictor"),
   
   # Sidebar with a slider input for number of bins 
   verticalLayout(
     splitLayout(
       cellWidths = 100,
       cellArgs = list(style = "padding: 6px"),
       gaugeOutput("gauge1",height = "auto"),
       gaugeOutput("gauge2",height = "auto"),
       gaugeOutput("gauge3",height = "auto")
     ),
      splitLayout(
        cellWidths = 100,
        cellArgs = list(style = "padding: 6px"),
         actionButton("suggestion1",label = "the"),
         actionButton("suggestion2",label = "to"),
         actionButton("suggestion3",label = "and")
      ),
      textAreaInput("textIn",label  = "Text input",
                    placeholder = "Start typing here:",
                    resize = "vertical",
                    rows = 6),
     tags$div(
       tags$p("The application predicts the text upon writing in the text input field. The suggestions are labels of the buttons and can be used by pressing the buttons.
              The gauges and numbers above the buttons show the score of the predictions - the higher the score, the more likely is the prediction to be correct.",
              style = "width: 300px"), 
       tags$p("This app was made as a final task of the Data Science Specialisation course provided by John Hopkins University on Coursera by Michal Svoboda.",
              style = "width: 300px")
     ),
     HTML('<a href="https://github.com/michsvob"> Michal Svoboda - Github</a>')
   )
)

server <- function(input, output,session) {
  prediction <- reactiveVal(c("the","to","and","a","of")) #= initial button labels
  scores <- reactiveVal(c(0,0,0,0,0)) #= initial gauge values
  
  update_buttons <- function(labels,vals){
    updateActionButton(session,inputId = "suggestion1",label =labels[1])
    updateActionButton(session,inputId = "suggestion2",label =labels[2])
    updateActionButton(session,inputId = "suggestion3",label =labels[3])

    vals=round(vals,1)
    output$gauge1 <- renderGauge(gauge(vals[1],min=0,max=4,abbreviate = TRUE, abbreviateDecimals = 1,sectors = gaugeSectors(success = c(2, 4), warning = c(1.13, 2), danger = c(0, 1.13))))
    output$gauge2 <- renderGauge(gauge(vals[2],min=0,max=4,abbreviate = TRUE, abbreviateDecimals = 1,sectors = gaugeSectors(success = c(2, 4), warning = c(1.13, 2), danger = c(0, 1.13))))
    output$gauge3 <- renderGauge(gauge(vals[3],min=0,max=4,abbreviate = TRUE, abbreviateDecimals = 1,sectors = gaugeSectors(success = c(2, 4), warning = c(1.13, 2), danger = c(0, 1.13))))
    }

  observeEvent(input$textIn,{
    pred=predict_3(input$textIn)
    prediction(pred[[1]])
    scores(pred[[2]])
    update_buttons(prediction(),scores())
  })
  
  observeEvent(input$suggestion1,{
    updateTextAreaInput(session,inputId = "textIn",value = paste(input$textIn,prediction()[1]))
  })
  
  observeEvent(input$suggestion2,{
    updateTextAreaInput(session,inputId = "textIn",value = paste(input$textIn,prediction()[2]))
  })
  
  observeEvent(input$suggestion3,{
    updateTextAreaInput(session,inputId = "textIn",value = paste(input$textIn,prediction()[3]))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

