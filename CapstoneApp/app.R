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

ngramdb <- readRDS("ngramdb_withunks.rds")
knowns <- readRDS("knowns.rds")

predict_2 <- function(text_in,alternatives=T,c5=1,c4=1,c3=1,c2=1,c1=1){
  text_in=stripWhitespace(removeNumbers(removePunctuation(text_in)))
  text_in= str_to_lower(gsub("^ * | $ *","",text_in)) # lowercase and strip extra whitespace
  
  text_in = tail(str_split(text_in," ")[[1]],4)
  text_in=replace(text_in,!(text_in %in% knowns),"<unk>")
  text_in=c(rep("",4-length(text_in)),text_in)
  
  gr5=ngramdb %>% 
    filter(grams==5,fourth==text_in[1],third==text_in[2],second==text_in[3],first==text_in[4]) %>% 
    group_by(pred) %>% summarise(score=c5*sum(condprob))
  #ngrams are up-voted by factor c
  
  score5=0
  if(nrow(gr5)>0){
    score5=top_n(gr5,1,score)[1,2]
  }
  
  gr4=ngramdb %>% 
    filter(grams==4,third==text_in[2],second==text_in[3],first==text_in[4]) %>% 
    group_by(pred) %>% summarise(score=c4*sum(condprob)) 
  
  score4=0
  if(nrow(gr4)>0){
    score4=top_n(gr4,1,score)[1,2]
  }
  
  gr3=ngramdb %>% 
    filter(grams==3,second==text_in[3],first==text_in[4]) %>% 
    group_by(pred) %>% summarise(score=c3*sum(condprob))
  
  score3=0
  if(nrow(gr3)>0){
    score3=top_n(gr3,1,score)[1,2]
  }
  
  gr2=ngramdb %>% 
    filter(grams==2,first==text_in[4]) %>% 
    group_by(pred) %>% summarise(score=c2*sum(condprob)) 
  
  score2=0
  if(nrow(gr2)>0){
    score2=top_n(gr2,1,score)[1,2]
  }
  
  #gr1=ngramdb %>% 
  #  filter(grams==1, pred %in% gr2$pred) %>% 
  #  group_by(pred) %>% summarise(score=c1*sum(condprob)) 
  
  pred <- (bind_rows(gr5,gr4,gr3,gr2) %>% #,gr1) %>%
             group_by(pred) %>% summarise(score=sum(score,na.rm=TRUE)) %>% top_n(((alternatives*2)+1),score) %>% 
             arrange(desc(score)))$pred
  
  if(length(pred)<3){
    pred=head(append(pred,c('the', 'on', 'a')),3)
  }
  pred
}


################################-------------------------------#########################


library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Text predictor"),
   
   # Sidebar with a slider input for number of bins 
   verticalLayout(
      splitLayout(
         actionButton("suggestion1",label = "the"),
         actionButton("suggestion2",label = "on"),
         actionButton("suggestion3",label = "a")
      ),
      textInput("textIn",label  = "Text input",placeholder = "Start typing here:")
   )
)

server <- function(input, output,session) {
  prediction <- reactiveVal(c("the","on","a")) #= initial button labels
  
  update_buttons <- function(labels){
    updateActionButton(session,inputId = "suggestion1",label =labels[1])
    updateActionButton(session,inputId = "suggestion2",label =labels[2])
    updateActionButton(session,inputId = "suggestion3",label =labels[3])
    }

  observeEvent(input$textIn,{
    prediction(predict_2(input$textIn))
    update_buttons(prediction())
    print(prediction())
  })
  
  observeEvent(input$suggestion1,{
    updateTextInput(session,inputId = "textIn",value = paste(input$textIn,prediction()[1]))
  })
  
  observeEvent(input$suggestion2,{
    updateTextInput(session,inputId = "textIn",value = paste(input$textIn,prediction()[2]))
  })
  
  observeEvent(input$suggestion3,{
    updateTextInput(session,inputId = "textIn",value = paste(input$textIn,prediction()[3]))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

