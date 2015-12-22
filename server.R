
library(shiny)
library(plotly)
library(dplyr)

source("jolokiaUtils.R")

shinyServer(function(input, output, session) {
  
  observe({
    autoInvalidate <- reactiveTimer((input$refreshInterval) * 1000, session)
    
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
      Timestamp <- as.POSIXct(Timestamp, origin="1970-01-01")
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
      Timestamp <- as.POSIXct(Timestamp, origin="1970-01-01")
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
      Timestamp <- as.POSIXct(Timestamp, origin="1970-01-01")
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
      Timestamp <- as.POSIXct(Timestamp, origin="1970-01-01")
      heapDataFrame <- data.frame(Timestamp, Cpu.Usage)
      
      p <- plot_ly(heapDataFrame, x = Timestamp, y = Cpu.Usage) %>%
        layout(title = "CPU Usage (%)",
               showlegend = FALSE) 
    }
    
    ## Render the CPU Usage plot
    output$cpuUsagePlot <- renderPlotly({
      plotCpuUsage()
    })
    
    ## Agents management
    getSavedAgents <- function() {
      # Load the agent list from a file
      agentsDetailsDataFrame <- loadAgentListFromFile()
    }
    
    savedAgentsList <- getSavedAgents()
    
    getSavedAgentNames <- function() {
      agentList <- getSavedAgents()
      values <- as.character(agentList$Name)
    }
    
    savedAgentNames <- getSavedAgentNames()
    
    updateSelectInput(session, "agentListSelectInput",
                      choices = savedAgentNames #,
                      #selected = "None"
    )
    
    observeEvent(input$saveAgentButton, {
      agentDataList <- getSavedAgents()
      agentDataList$X <- NULL
      transformedDataFrame <- transform(agentDataList, Name = as.character(Name))
      transformedDataFrame <- transform(transformedDataFrame, Host = as.character(Host))
      transformedDataFrame <- transform(transformedDataFrame, Port = as.numeric(Port))
      
      # Remove the agen from the list in case of update
      filteredDataFrame <- transformedDataFrame[transformedDataFrame$Name == input$agentNameText, ]
      if(nrow(filteredDataFrame) > 0 ) {
        transformedDataFrame <- transformedDataFrame[transformedDataFrame$Name != input$agentNameText, ]
      }
      
      transformedDataFrame <- rbind(transformedDataFrame, data.frame("Name"=input$agentNameText, "Host"=input$agentHostText, "Port"=input$agentPortText))
      saveAgentToFile(transformedDataFrame)
    })
    
    # Fill the form after combo box selection
    observeEvent(input$agentListSelectInput, {
      if(input$agentListSelectInput != 'None') {
        agentDataList <- getSavedAgents()
        agentDataList <- agentDataList[agentDataList$Name==input$agentListSelectInput,]
        updateTextInput(session, "agentNameText", value = input$agentListSelectInput)
        updateTextInput(session, "agentHostText", value = agentDataList[1,3])
        updateTextInput(session, "agentPortText", value = agentDataList[1,4])
      }
    }
    )
    
    # Test an agent after user click on the Test button
    observeEvent(input$testAgentButton, {
      # Compose the agent URL
      agentUrl <- paste('http://', input$agentHostText, ':',
                        input$agentPortText, '/jolokia/', sep="")
      # Make a GET request to the agent
      responseContent <- testAgentConnection(agentUrl)
      # Parse the response status
      if(class(responseContent) == 'list') {
        if(responseContent$status == '200') {
          responseMessage <- "Success!"
        } else {
          responseMessage <- "Failure!"
        }
      } else {
        responseMessage <- "Failure!"
      }
      output$messageTextOutput <- renderText({ 
        responseMessage
      })
    }
    )
    
  })
  
})
