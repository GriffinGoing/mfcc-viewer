library(shiny)
library(shinyjs)
library(tidyverse)
library(seewave)
library(tuneR)
library(DT)

convert_audio <- function(filePath, filename){
  
  waveFile <- readWave(filePath)
  melTrans <- melfcc(waveFile)
  melTrans <- as.data.frame(melTrans)
  melTrans <- melTrans %>%
    mutate(frame = row_number()) %>%
    pivot_longer(!frame, names_to = "ceps", values_to = "coeff") %>%
    mutate(file = filename)
  melTrans
}

ui <- fluidPage(
  
  title = "MFCC Viewer",
  
  #h3("MFCC Viewer"),
  
  #hr(),
  
  plotOutput('plot'),
  
  hr(),
  
  fluidRow(
    column(3,
           h4("File Management"),
           fileInput('newFile', "Upload New .wav File"),
           selectInput('ceps', 'Cepstrum', c("Any")),
           
    ),
    column(4,
           sliderInput('xmin', 'X Window Min', 
                       min=0, max=0, value=0, 
                       step=1, round=0),
           sliderInput('xmax', 'X Window Max', 
                       min=0, max=0, value=0, 
                       step=1, round=0),
           sliderInput('ymin', 'Y Window Min', 
                       min=0, max=0, value=0, 
                       step=1, round=0),
           sliderInput('ymax', 'Y Window Max', 
                       min=0, max=0, value=0, 
                       step=1, round=0)
    ),
    column(4,
           dataTableOutput('datasetTable')
    )
  )
)


server <- function(input, output, session) {
  
  placeholder <- read.csv('blank.csv')
  dataset <- NULL
  
  output$plot <- renderPlot(placeholder %>%
    ggplot(mapping = aes(x = frame, y = coeff)) +
    geom_point(alpha = 0.6) +
      labs(
        x = "Frame (time)",
        y = "Mel Coefficient"
      ))
  
  output$datasetTable <- renderDataTable(placeholder)
  
  observeEvent(input$newFile, {
    newFileMels <- convert_audio(input$newFile$datapath, input$newFile$name)
    
    print(is.null(dataset))
    
    if (!is.null(dataset)) {
      print("COMBINED")
      dataset <<- dataset %>%
        bind_rows(newFileMels)
    } 
    
    else {
      print("NOT COMBINED")
      dataset <<- newFileMels
    }
    
    xMaxVal <- max(dataset$frame, na.rm = TRUE)
    yMaxVal <- ceiling(max(dataset$coeff, na.rm = TRUE)) + 5
    
    updateSliderInput(session, 'xmin', max = xMaxVal, value = 0)
    updateSliderInput(session, 'xmax', max = xMaxVal, value = xMaxVal)
    updateSliderInput(session, 'ymin', max = yMaxVal, value = 0)
    updateSliderInput(session, 'ymax', max = yMaxVal, value = yMaxVal)
    updateSelectInput(session, 'ceps', choices = c("Any", unique(dataset$ceps)))
    
    print(max(dataset$coeff, na.rm = TRUE))
    
    output$plot <- renderPlot(dataset %>% 
                                #filter(ceps == input$ceps) %>%
                                ggplot(mapping = aes(x = frame, y = coeff, color = file)) +
                                geom_point(alpha = 0.6) +
                                xlim(input$xmin, input$xmax) +
                                ylim(input$ymin, input$ymax) +
                                labs(
                                  x = "Frame (time)",
                                  y = "Mel Coefficient"
                                ))
    output$datasetTable <- renderDataTable(dataset)
  
  })
  
  
}

shinyApp(ui = ui, server = server)
