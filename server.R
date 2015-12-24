
library(shiny)
library(plotly)
library(dplyr)

source("jolokiaUtils.R")
source("dial.plot.R")

shinyServer(function(input, output, session) {
  Attributes <- c('BootClassPath', 'BootClassPathSupported', 'ClassPath', 'LibraryPath', 'Name',
                  'ProcessID', 'SpecName', 'SpecVendor', 'SpecVersion', 'StartTime', 'Uptime',
                  'VmName', 'VmVendor', 'VmVersion')
  
  observe({
    autoInvalidate <- reactiveTimer((input$refreshInterval) * 1000, session)
    
    getSavedAgents <- function() {
      # Load the agent list from a file
      agentsDetailsDataFrame <- loadAgentListFromFile()
    }
    
    savedAgentsList <- getSavedAgents()
    
    getSavedAgentNames <- function() {
      agentList <- savedAgentsList
      #values <- as.character(agentList$Name)
      values <- as.list(agentList$Name)
    }
    
    output$agentSelection <- renderUI({
      selectInput("agentSelectInput", "Agent:", 
                  choices = getSavedAgentNames(),
                  selected = "None"
                  )
    })
    
    updateSelectInput(session, "agentListSelectInput",
                      choices = getSavedAgentNames(),
                      selected = "None"
    )
    
    getAgentUrlFromList <- function() {
      agentDataList <- savedAgentsList[savedAgentsList$Name==input$agentSelectInput,]
      agentUrl <- paste('http://', agentDataList[1,3], ':', agentDataList[1,4], '/jolokia/', sep = "")
    }
    
    getHeapUsage <- function() {
      
      fileContent <- makeHttpPostRequestAndSaveToFile(agentUrl = getAgentUrlFromList(), 
                                       mbeanName = 'java.lang:type=Memory',
                                       attributeName = 'HeapMemoryUsage',
                                       logFilename = heapUsageLogFilename)
    }
    
    plotHeapUsage <- function() {
      autoInvalidate()
      
      if(input$agentSelectInput != 'None') {
        # Get the values from the agent (HTTP request):
        fileContent <- getHeapUsage()
        fileLineCount <- length(fileContent)
        Timestamp <- c(fileLineCount)
        Used <- c(fileLineCount)
        for(idx in 1:length(fileContent)) {
          listFromJson <- fromJSON(fileContent[idx])
          Timestamp[idx] <- c(listFromJson$timestamp)
          Used[idx] <- c(listFromJson$value$used)
          if(idx == length(fileContent)) {
            currentHeapUsageValue <- Used[idx]
          }
        }
        Timestamp <- as.POSIXct(Timestamp, origin="1970-01-01")
        heapDataFrame <- data.frame(Timestamp, Used)
        
        x <- list(
          title = ""
        )
        p <- plot_ly(heapDataFrame, x = Timestamp, y = Used) %>%
          layout(title = "Heap Memory Usage",
                 showlegend = FALSE) %>%
          dplyr::filter(Used == max(Used)) %>%
        layout(annotations = list(x = Timestamp, y = Used, text = "Peak", showarrow = T),
               xaxis = x)
      } 
    }
    
    ## Render the Heap Memory Usage plot
    output$heapUsagePlot <- renderPlotly({
      plotHeapUsage()
    })
    
    getThreadCount <- function() {
      fileContent <- makeHttpPostRequestAndSaveToFile(agentUrl = getAgentUrlFromList(), 
                                                      mbeanName = 'java.lang:type=Threading',
                                                      attributeName = 'ThreadCount',
                                                      logFilename = threadsLogFilename)
    }
    
    plotThreadCount <- function() {
      autoInvalidate()
      
      if(input$agentSelectInput != 'None') {
        # Get the values from the agent (HTTP request):
        fileContent <- getThreadCount()
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
        
        x <- list(
          title = ""
        )
        p <- plot_ly(heapDataFrame, x = Timestamp, y = Thread.Count) %>%
          layout(title = "Threads",
                 showlegend = FALSE,
                 xaxis = x)
      }
    }
    
    ## Render the Thread Count plot
    output$threadCountPlot <- renderPlotly({
      plotThreadCount()
    })
    
    getClassCount <- function() {
      fileContent <- makeHttpPostRequestAndSaveToFile(agentUrl = getAgentUrlFromList(),
                                                      mbeanName = 'java.lang:type=ClassLoading',
                                                      attributeName = 'LoadedClassCount',
                                                      logFilename = classesLogFilename)
    }
    
    plotClassCount <- function() {
      autoInvalidate()
      
      if(input$agentSelectInput != 'None') {
        # Get the values from the agent (HTTP request):
        fileContent <- getClassCount()
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
        
        x <- list(
          title = ""
        )
        p <- plot_ly(heapDataFrame, x = Timestamp, y = Classes.Count) %>%
          layout(title = "Classes",
                 showlegend = FALSE,
                 xaxis = x) 
      }
    }
    
    ## Render the Heap Memory Usage plot
    output$classCountPlot <- renderPlotly({
      plotClassCount()
    })
    
    getCpuUsage <- function() {
      fileContent <- makeHttpPostRequestAndSaveToFile(agentUrl = getAgentUrlFromList(),
                                                      mbeanName = 'java.lang:type=OperatingSystem',
                                                      attributeName = 'ProcessCpuLoad',
                                                      logFilename = cpuUsageLogFilename)
    }
    
    plotCpuUsage <- function() {
      autoInvalidate()
      
      if(input$agentSelectInput != 'None') {
        # Get the values from the agent (HTTP request):
        fileContent <- getCpuUsage()
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
        
        x <- list(
          title = ""
        )
        p <- plot_ly(heapDataFrame, x = Timestamp, y = Cpu.Usage) %>%
          layout(title = "CPU Usage (%)",
                 showlegend = FALSE,
                 xaxis = x) 
      }
    }
    
    ## Render the CPU Usage plot
    output$cpuUsagePlot <- renderPlotly({
      plotCpuUsage()
    })
    
    ## Agents management
#     getSavedAgents <- function() {
#       print('getSavedAgents() start')
#       # Load the agent list from a file
#       agentsDetailsDataFrame <- loadAgentListFromFile()
#     }
#     
#     savedAgentsList <- getSavedAgents()
#     
#     getSavedAgentNames <- function() {
#       print('getSavedAgentNames() start')
#       agentList <- getSavedAgents()
#       values <- as.character(agentList$Name)
#     }
#     
#     savedAgentNames <- getSavedAgentNames()
#     
#     updateSelectInput(session, "agentListSelectInput",
#                       choices = savedAgentNames #,
#                       #selected = "None"
#     )
    
    observeEvent(input$saveAgentButton, {
      agentDataList <- getSavedAgents()
      agentDataList$X <- NULL
      transformedDataFrame <- transform(agentDataList, Name = as.character(Name))
      transformedDataFrame <- transform(transformedDataFrame, Host = as.character(Host))
      transformedDataFrame <- transform(transformedDataFrame, Port = as.numeric(Port))
      
      # Remove the agent from the list in case of update
      filteredDataFrame <- transformedDataFrame[transformedDataFrame$Name == input$agentNameText, ]
      if(nrow(filteredDataFrame) > 0 ) {
        transformedDataFrame <- transformedDataFrame[transformedDataFrame$Name != input$agentNameText, ]
      }
      
      transformedDataFrame <- rbind(transformedDataFrame, data.frame("Name"=input$agentNameText, "Host"=input$agentHostText, "Port"=input$agentPortText))
      saveAgentToFile(transformedDataFrame)
      updateSelectInput(session, "agentListSelectInput",
                        choices = getSavedAgentNames(),
                        selected = input$agentNameText
      )
      
      updateSelectInput(session, "agentSelectInput",
                        choices = getSavedAgentNames() #,
                        #selected = input$agentNameText
      )
    })
    
    observeEvent(input$deleteAgentButton, {
      agentDataList <- getSavedAgents()
      agentDataList$X <- NULL
      
      # Remove the agent from the list
      agentDataList <- agentDataList[agentDataList$Name != input$agentNameText, ]
      saveAgentToFile(agentDataList)
      updateTextInput(session, "agentNameText", value = '')
      updateTextInput(session, "agentHostText", value = '')
      updateTextInput(session, "agentPortText", value = '')
      output$messageTextOutput <- renderText({ 
        'The selected agent has been removed from the list'
      })
      updateSelectInput(session, "agentListSelectInput",
                        choices = getSavedAgentNames(),
                        selected = "None"
      )
      
      updateSelectInput(session, "agentSelectInput",
                        choices = getSavedAgentNames() #,
                        #selected = input$agentNameText
      )
    })
    
#     updateSelectInput(session, "agentSelectInput",
#                       choices = getSavedAgentNames() #,
#                       #selected = input$agentNameText
#     )
    
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
    
    ## Delete history ##
    observeEvent(input$deleteHistoryButton, {
      unlink('*.json')
    }
    )
    
    ## Dashboard view
    plotFirstRowDial <- function() {
      autoInvalidate()
      if(input$agentSelectInput != 'None') {
        fileContent <- makeHttpPostRequest(agentUrl = getAgentUrlFromList(),
                                           mbeanName = 'java.lang:type=Memory',
                                           attributeName = 'HeapMemoryUsage')
        listFromJson <- fromJSON(fileContent[1])
        currentHeapMemoryUsed <- listFromJson$value$used
        maxHeapSize <- listFromJson$value$max
        cpuUsageValue <- (currentHeapMemoryUsed/maxHeapSize) * 100 
        
        fileContent <- makeHttpPostRequest(agentUrl = getAgentUrlFromList(),
                                            mbeanName = 'java.lang:type=Threading',
                                           attributeName = 'ThreadCount')
        listFromJson <- fromJSON(fileContent[1])
        currentThreadCount <- listFromJson$value
        
        opar <- par(mfrow=c(1, 2))
        dial.plot(label="Used Java Heap (%)", label.cex=1, value=round(cpuUsageValue, 2), dial.radius = 1.1)
        dial.plot(label="Thread Count", label.cex=1, value=currentThreadCount, dial.radius = 1.1)
      } 
    }
    
    output$upperRowDialPlot <- renderPlot({
      plotFirstRowDial()
    })
    
    output$lowerRowDialPlot <- renderPlot({
      if(input$agentSelectInput != 'None') {
        fileContent <- makeHttpPostRequest(agentUrl = getAgentUrlFromList(), 
                                           mbeanName = 'java.lang:type=ClassLoading',
                                           attributeName = 'LoadedClassCount')
        listFromJson <- fromJSON(fileContent[1])
        currentClassCount <- listFromJson$value
        
        fileContent <- makeHttpPostRequest(agentUrl = getAgentUrlFromList(),
                                            mbeanName = 'java.lang:type=OperatingSystem',
                                           attributeName = 'ProcessCpuLoad')
        listFromJson <- fromJSON(fileContent[1])
        currentProcessCpuLoad <- listFromJson$value
        
        opar <- par(mfrow=c(1, 2))
        dial.plot(label="Class Count (K)", label.cex=1, value=round(currentClassCount/1000, 2), dial.radius = 1.1)
        dial.plot(label="JVM CPU Usage (%)", label.cex=1, value=round(currentProcessCpuLoad*100, 2), dial.radius = 1.1)
      }
    })
    
    output$cpuDashUi <- renderUI({
      if(input$agentSelectInput != 'None') {
        column(width = 8,
               plotOutput("upperRowDialPlot", width = "100%", height = "350px"),
               plotOutput("lowerRowDialPlot", width = "100%", height = "350px")
        )
      }
    })
    
    ## JVM details
    output$jvmSummaryTable <- renderDataTable({
      if(input$agentSelectInput != 'None') {
          fileContent <- makeHttpPostRequest(agentUrl = getAgentUrlFromList(),
                                             mbeanName = 'java.lang:type=Runtime',
                                             attributeName = '')
          listFromJson <- fromJSON(fileContent[1])
          Values <- c(listFromJson$value$BootClassPath, listFromJson$value$BootClassPathSupported,
                      listFromJson$value$ClassPath, listFromJson$value$LibraryPath,
                      listFromJson$value$Name, listFromJson$value$ProcessID,
                      listFromJson$value$SpecName, listFromJson$value$SpecVendor, listFromJson$value$SpecVersion,
                      listFromJson$value$StartTime, listFromJson$value$Uptime, listFromJson$value$VmName,
                      listFromJson$value$VmVendor, listFromJson$value$VmVersion)
          jvmDf <- data.frame(Attributes, Values)
      }
      }, options = list(lengthMenu = c(5, 10, 30), pageLength = 5))
    
    
  })
  
})
