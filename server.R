
library(shiny)
library(plotly)
library(dplyr)

source("jolokiaUtils.R")

data(movies, package = "ggplot2")
minx <- min(movies$rating)
maxx <- max(movies$rating)

shinyServer(function(input, output, session) {
  autoInvalidate <- reactiveTimer(5000, session)
  
  observe({
    plotHeapUsage <- function() {
      autoInvalidate()
      
      # Get the values from the agent (HTTP request):
      responseContent <- makeHttpPostRequest(mbeanName = 'java.lang:type=Memory',
                          attributeName = 'HeapMemoryUsage')
      writeToLogFile(heapUsageLogFilename, responseContent)
      
      fileContent <- readLines(heapUsageLogFilename)
      fileLineCount <- length(fileContent)
      Timestamp <- c(fileLineCount)
      Used <- c(fileLineCount)
      for(idx in 1:length(fileContent)) {
        listFromJson <- fromJSON(fileContent[idx])
        Timestamp[idx] <- c(listFromJson$timestamp)
        Used[idx] <- c(listFromJson$value$used)
      }
      heapDataFrame <- data.frame(Timestamp, Used)
      
      p <- plot_ly(heapDataFrame, x = Timestamp, y = Used) %>%
        layout(title = "Heap Memory Usage",
               showlegend = FALSE) %>%
        dplyr::filter(Used == max(Used)) %>%
      layout(annotations = list(x = Timestamp, y = Used, text = "Peak", showarrow = T))
    }
    
    ## Render the Heap Memory Usage plot
    output$heapUsagePlot <- renderPlotly({
      plotHeapUsage()
    })
    
    plotThreadCount <- function() {
      autoInvalidate()
      
      # Get the values from the agent (HTTP request):
      responseContent <- makeHttpPostRequest(mbeanName = 'java.lang:type=Threading',
                                             attributeName = 'ThreadCount')
      writeToLogFile(threadsLogFilename, responseContent)
      
      fileContent <- readLines(threadsLogFilename)
      fileLineCount <- length(fileContent)
      Timestamp <- c(fileLineCount)
      Thread.Count <- c(fileLineCount)
      for(idx in 1:length(fileContent)) {
        listFromJson <- fromJSON(fileContent[idx])
        Timestamp[idx] <- c(listFromJson$timestamp)
        Thread.Count[idx] <- c(listFromJson$value)
      }
      heapDataFrame <- data.frame(Timestamp, Thread.Count)
      
      p <- plot_ly(heapDataFrame, x = Timestamp, y = Thread.Count) %>%
        layout(title = "Threads",
               showlegend = FALSE) 
    }
    
    ## Render the Thread Count plot
    output$threadCountPlot <- renderPlotly({
      plotThreadCount()
    })
    
    plotClassCount <- function() {
      autoInvalidate()
      
      # Get the values from the agent (HTTP request):
      responseContent <- makeHttpPostRequest(mbeanName = 'java.lang:type=ClassLoading',
                                             attributeName = 'LoadedClassCount')
      writeToLogFile(classesLogFilename, responseContent)
      
      fileContent <- readLines(classesLogFilename)
      fileLineCount <- length(fileContent)
      Timestamp <- c(fileLineCount)
      Classes.Count <- c(fileLineCount)
      for(idx in 1:length(fileContent)) {
        listFromJson <- fromJSON(fileContent[idx])
        Timestamp[idx] <- c(listFromJson$timestamp)
        Classes.Count[idx] <- c(listFromJson$value)
      }
      heapDataFrame <- data.frame(Timestamp, Classes.Count)
      
      p <- plot_ly(heapDataFrame, x = Timestamp, y = Classes.Count) %>%
        layout(title = "Classes",
               showlegend = FALSE) 
    }
    
    ## Render the Heap Memory Usage plot
    output$classCountPlot <- renderPlotly({
      plotClassCount()
    })
    
    plotCpuUsage <- function() {
      autoInvalidate()
      
      # Get the values from the agent (HTTP request):
      responseContent <- makeHttpPostRequest(mbeanName = 'java.lang:type=OperatingSystem',
                                             attributeName = 'ProcessCpuLoad')
      writeToLogFile(cpuUsageLogFilename, responseContent)
      
      fileContent <- readLines(cpuUsageLogFilename)
      fileLineCount <- length(fileContent)
      Timestamp <- c(fileLineCount)
      Cpu.Usage <- c(fileLineCount)
      for(idx in 1:length(fileContent)) {
        listFromJson <- fromJSON(fileContent[idx])
        Timestamp[idx] <- c(listFromJson$timestamp)
        Cpu.Usage[idx] <- c(listFromJson$value * 100)
      }
      heapDataFrame <- data.frame(Timestamp, Cpu.Usage)
      
      p <- plot_ly(heapDataFrame, x = Timestamp, y = Cpu.Usage) %>%
        layout(title = "CPU Usage (%)",
               showlegend = FALSE) 
    }
    
    ## Render the CPU Usage plot
    output$cpuUsagePlot <- renderPlotly({
      plotCpuUsage()
    })
    
  })
  
})
