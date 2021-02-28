library(shiny)
library(shinyjs)
library(tidyverse)
library(seewave)
library(tuneR)
library(DT)
library(shinycssloaders)
library(shinyWidgets)

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
  
  plotOutput('plot') %>%
    withSpinner(type = 8, color='#037bbc', size = 2),
  
  hr(),
  
  fluidRow(
    column(3,
           h4("File Management"),
           hr(),
           fileInput('newFile', "Upload New .wav File"),
           downloadButton('downloadFeatures', "Download Features"),
           downloadButton('downloadPlot', "Download Plot"),
           hr(),
           h4("Geometry"),
           hr(),
           switchInput('smoothingLine', "Smoothing Line", value = FALSE, inline = TRUE),
           switchInput('cepstrumShapes', "Cepstrum Shapes", value = FALSE, inline = TRUE),

    ),
    column(2,
           h4("Graph Options"),
           hr(),
           selectInput('ceps', 'Cepstrum', c("All")),
           sliderInput('alpha', "Alpha", min = 0, max = 100,
                       value = 65, step = 5, ticks = FALSE),
           sliderInput('xWindow', 'X Window', 
                       min=0, max=0, value=c(0, 0), 
                       step=1, round=0),
           sliderInput('yWindow', 'Y Window', 
                       min=0, max=0, value=c(0, 0), 
                       step=1, round=0),
    ),
    column(3,
           h4("Table of Differences"),
           dataTableOutput('diffTable'),
    ),
    column(4,
           h4("Extracted Features"),
           dataTableOutput('datasetTable')
    )
  )
)


server <- function(input, output, session) {
  
  lastCeps <- "All"
  
  filter_data <- function (x, cepstrum, update = FALSE) {
    
    
    if (cepstrum != "All") {
      x <- x %>%
        filter(ceps == cepstrum)
    }
    
    if (update == TRUE) {
      # check for new min/max values, and set the current xWindow right val if necessary
      xMaxVal <- max(x$frame, na.rm = TRUE)
      yMinVal <- floor(min(x$coeff, na.rm = TRUE))
      yMaxVal <- ceiling(max(x$coeff, na.rm = TRUE))
      if (input$xWindow[2] == 0) { input$xWindow[2] <- xMaxVal } 
      
      print(c(0, xMaxVal))
      print(c(yMinVal, yMaxVal))
      
      
      # only alter values to hard min/max if we're switching cepstrums
      if (lastCeps != cepstrum) {
        updateSliderInput(session, 'xWindow', max = xMaxVal, 
                          value = c(0, xMaxVal))
        updateSliderInput(session, 'yWindow', min = yMinVal, 
                          max = yMaxVal, value = c(yMinVal, yMaxVal))
      }
      

    }
    lastCeps <<- cepstrum
    x
  }
  
  placeholder <- read.csv('blank.csv')
  diffPlacehodler <- data.frame(file=character(), ceps = character(), avg_diff = character())
  #dataset <- read.csv('blank.csv')
  dataset <- NULL
  
  output$plot <- renderPlot(placeholder %>%
    ggplot(mapping = aes(x = frame, y = coeff)) +
    geom_point(alpha = 0.6) +
      labs(
        x = "Frame (time)",
        y = "Mel Coefficient"
      ))
  
  output$datasetTable <- renderDataTable(placeholder)
  
  output$diffTable <- renderDataTable(diffPlacehodler,
                                      options = list(lengthChange = FALSE))
  
  observeEvent(input$newFile, {
    newFileMels <- convert_audio(input$newFile$datapath, input$newFile$name)
    
    print(is.null(dataset))
    
    if (!is.null(dataset)) {
      dataset <<- dataset %>%
        bind_rows(newFileMels)
    } 
    
    else {
      dataset <<- newFileMels
    }
    
    # no x min necessary, as it will always be 0
    xMaxVal <- max(dataset$frame, na.rm = TRUE)
    yMinVal <- floor(min(dataset$coeff, na.rm = TRUE))
    yMaxVal <- ceiling(max(dataset$coeff, na.rm = TRUE))
    
    updateSliderInput(session, 'xWindow', max = xMaxVal, value = c(0, xMaxVal))
    updateSliderInput(session, 'yWindow', min = yMinVal, max = yMaxVal, value = c(yMinVal, yMaxVal))
    updateSelectInput(session, 'ceps', choices = c("All", unique(dataset$ceps)))
    
    print(max(dataset$coeff, na.rm = TRUE))
    
    output$plot <- renderPlot({
                              mfccPlot <- dataset %>%
                                filter_data(input$ceps, update = TRUE) %>%
                                ggplot(mapping = aes(x = frame, y = coeff, color = file))
                              
                              if (input$cepstrumShapes) {
                                mfccPlot <- mfccPlot +
                                  geom_point(aes(shape = ceps), alpha = input$alpha/100) +
                                  scale_shape_manual(values = 1:n_distinct(dataset$ceps))
                                print("LEVELS:")
                                print(n_distinct(dataset$ceps))
                                print(length(unique(dataset$ceps)))
                                print(nlevels(dataset$ceps))
                              }
                              
                              else {
                                mfccPlot <- mfccPlot + geom_point(alpha = input$alpha/100)
                              }
                              
                              if (input$smoothingLine) {
                                mfccPlot <- mfccPlot + geom_smooth()
                              }
                              
                              mfccPlot +
                                xlim(input$xWindow[1], input$xWindow[2]) +
                                ylim(input$yWindow[1], input$yWindow[2]) +
                                labs(
                                  x = "Frame (time)",
                                  y = "Mel Coefficient",
                                  color = "File",
                                  shape = "Cepstrum"
                                )
                              })
    
    # build/update differences table
    
    
    # update featurs table
    output$datasetTable <- renderDataTable(dataset %>%
                                             drop_na() %>%
                                             mutate(
                                               coeff = round(coeff, digits = 6)
                                             )
                                          )  
    output$diffTable <- renderDataTable(
      dataset %>%
        group_by(file, ceps) %>%
        summarise(avg_diff = round(abs(mean(coeff, na.rm = TRUE)), digits = 2)),
      options = list(lengthChange = FALSE)
      )
    
  })
  

  
  
}

shinyApp(ui = ui, server = server)
