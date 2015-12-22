
library(httr)
library(jsonlite)

## HTTP requests to Jolokia agents ##

## Default Jolokia agent URL (to use for testing purposes only)
defaultAgentUrl <- 'http://127.0.0.1:8778/jolokia/'

## Log file names
heapUsageLogFilename <- paste0('HeapUsage',
                      floor(runif(1, 1e+05, 1e+06 - 1)),
                      '.json')
threadsLogFilename <- paste0('Threads',
                               floor(runif(1, 1e+05, 1e+06 - 1)),
                               '.json')
classesLogFilename <- paste0('Classes',
                             floor(runif(1, 1e+05, 1e+06 - 1)),
                             '.json')
cpuUsageLogFilename <- paste0('CpuUsage',
                             floor(runif(1, 1e+05, 1e+06 - 1)),
                             '.json')

## Agents list file name
agentListFilename <- paste0('JolokiaAgentsList', ".csv")

## Performs a HTTP POST request to Jolokia
makeHttpPostRequest <- function(agentUrl = defaultAgentUrl, operationType = 'read', mbeanName, attributeName) {
  requestBody <- list(
    type=operationType,
    mbean=mbeanName,
    attribute=attributeName
  )
  
  req <- POST(agentUrl, 
              body = requestBody, encode = "json")
  stop_for_status(req)
  json <- content(req, "text", "application/json")
}   

## Tests a connection to an agent
testAgentConnection <- function(agentUrl) {
  req <- tryCatch(GET(agentUrl),
                  warning = function(w) {
                    req <- "Error"
                  },
                  error = function(e) {
                    req <- "Error"
                  }
  )
  if(class(req) == "response") {
    stop_for_status(req)
    json <- try(content(req, "text", "application/json"));
    if(class(json) == "try-error") {
      responseData <- "Error"
    } else {
      responseData <- fromJSON(json)
    }
  } else {
    json <- req
  }
}

## Writes to log file
writeToLogFile <- function(logFilename, jsonEntry) {
  if(file.exists(logFilename) == FALSE) {
    file.create(logFilename)
  }
  cat(as.character(jsonEntry), '\n', file = logFilename,
      append = TRUE)
}

## Loads the list of saved agents from a file to a data frame
loadAgentListFromFile <- function() {
  if(file.exists(agentListFilename) == TRUE) {
    jolokiaAgentData = read.csv(agentListFilename, as.is = TRUE)
  } else {
    Name <- c('None')
    Host <- c('None')
    Port <- c('0')
    jolokiaAgentData = data.frame(Name, Host, Port)
  }
}

## Saves or updates the agent list to a CSV file
saveAgentToFile <- function(agentListDataFrame) {
  write.csv(agentListDataFrame, file = agentListFilename, 
            col.names = TRUE, quote = FALSE)
}
