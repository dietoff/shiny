library(shiny)
library(googlesheets)
library(reshape2)
library(ggplot2)
library(dplyr)
library(plotly)
library(hms)
options(shiny.sanitize.errors = F)
gs_auth(token = "token.rds")

init <- function() {
  plan <- gs_title("Spring final")
  AD <- gs_read(ss = plan, 1, range = "A1:O195")
  AD$time.start <- hms(hours=as.numeric(substr(AD$Time,1,2)),minutes = as.numeric(substr(AD$Time,3,4),sep = ":"))
  AD$time.end <- hms(hours=as.numeric(substr(AD$Time,6,7)),minutes = as.numeric(substr(AD$Time,8,9),sep = ":"))
  AD1 <- AD[nchar(AD$Day)>1,]; 
  AD$Day <- substring(AD$Day,1,1)
  AD1$Day <- substring(AD1$Day,2,2)
  AD <- rbind(AD,AD1); rm(AD1)
  AD$Day <- factor(AD$Day,levels = c('M','T','W','R','F'), ordered=T)
  AD$Title <- AD$`Title Short`
  AD <- select(AD,Num,Crs,Title,Instructor,Day,time.start,time.end,Room,grad)
  return(AD)
}

AD <- init()

ui <- fillPage(tags$style(type='text/css', "#reload { margin-top: 25px;}"),
  fluidRow(style = "padding-bottom: 20px;",
           column(2, selectInput('grad', 'Level', c('Undergrad','Grad','Both'), selected = 'Both')),
           column(2, selectInput('label', 'Labels', c('Number','Description','Instructor'), selected = 'Description')),
           column(2, selectInput('color', 'Color', c('Prefix','Instructor','Level'), selected = 'Prefix')),
           column(3, checkboxGroupInput('days', 'Days', c('M','T','W','R','F'), selected = c('M','T','W','R','F'), inline=T)),
           column(3, actionButton("reload", "Reload"))
  ),
  plotOutput("distPlot", height = "90%", width = "100%")
)


server <- function(input, output) {
  observeEvent(input$reload,{
    AD <<- init()
  })
  
  lbl <- reactive({
    if (input$label == 'Number') return('Num');
    if (input$label == 'Description') return('Title');
    if (input$label == 'Instructor') return('Instructor');
  })
  
  col <- reactive({
    if (input$color == 'Prefix') return('Crs');
    if (input$color == 'Instructor') return('Instructor');
    if (input$color == 'Level') return('grad');
  })
  
  dys <- reactive({
    return(input$days)
  })
  
  output$distPlot <- renderPlot({
    input$reload
    
    if (input$grad == 'Grad') AD <- AD[AD$grad==T,]
    if (input$grad == 'Undergrad') AD <- AD[AD$grad==F,]
    
    ds <- dys()
    AD <- AD[AD$Day%in%ds,]
    
    l <- lbl()
    cl <- col()
    
    #ggplotly(
    ggplot(AD, aes(x=time.start,y=Room)) + 
      geom_point(data=AD[AD$grad==T,], aes(shape = grad), size = 4,  alpha = 0.7) + 
      geom_segment(aes_string(xend='time.end', yend = 'Room', color = cl), size = 5, alpha = 0.7) + 
      geom_text(aes_string(label=l),size=4, hjust=0) + facet_wrap(~Day) #+ scale_color_brewer(palette="Set2") 
    #)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

