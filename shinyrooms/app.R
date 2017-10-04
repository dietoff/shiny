library(shiny)
library(googlesheets)
library(reshape2)
library(ggplot2)
library(dplyr)
library(plotly)
options(shiny.sanitize.errors = F)
gs_auth(token = "token.rds")

init <- function() {
  plan <- gs_title("new Spring 2018 Schedule")
  AD <- gs_read(ss = plan, 1, range = "A1:N195")
  AD <- transform(AD, c = colsplit(AD$Course, " ", names=c('pref','nr') ))
  AD <- transform(AD, day = colsplit(AD$Day, ",", names=c('1','2') ))
  AD1 <- AD[!is.na(AD$day.2),]; AD1$day.1 <- AD1$day.2
  AD <- rbind(AD,AD1)
  AD$Day <- as.numeric(AD$day.1)
  AD <- AD[!is.na(AD$Day),]
  AD$grad <- AD$c.nr>5000
  AD <- select(AD, Course,Course.Name,Day,Room,Start,End,grad,c.pref, Ins.Last)
  return(AD)
}

AD <- init()

ui <- fillPage(tags$style(type='text/css', "#reload { margin-top: 25px;}"),
  fluidRow(style = "padding-bottom: 20px;",
           column(2, selectInput('grad', 'Level', c('Undergrad','Grad','Both'), selected = 'Both')),
           column(2, selectInput('label', 'Labels', c('Number','Description','Instructor'), selected = 'Description')),
           column(2, selectInput('color', 'Color', c('Prefix','Instructor','Level'), selected = 'Prefix')),
           column(3, checkboxGroupInput('days', 'Days', c(1,2,3,4,5), selected = c(1,2,3,4,5), inline=T)),
           column(3, actionButton("reload", "Reload"))
  ),
  plotOutput("distPlot", height = "90%", width = "100%")
)


server <- function(input, output) {
  observeEvent(input$reload,{
    AD <<- init()
  })
  
  lbl <- reactive({
    if (input$label == 'Number') return('Course');
    if (input$label == 'Description') return('Course.Name');
    if (input$label == 'Instructor') return('Ins.Last');
  })
  
  col <- reactive({
    if (input$color == 'Prefix') return('c.pref');
    if (input$color == 'Instructor') return('Ins.Last');
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
    ggplot(AD, aes(x=Start,y=Room)) + 
      geom_point(data=AD[AD$grad==T,], aes(shape = grad), size = 4,  alpha = 0.7) + 
      geom_segment(aes_string(xend='End', yend = 'Room', color = cl), size = 5, alpha = 0.7) + 
      geom_text(aes_string(label=l),size=4, hjust=0) + facet_wrap(~Day) #+ scale_color_brewer(palette="Set2") 
    #)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

