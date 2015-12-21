
library(httr)
library(jsonlite)

## HTTP requests to Jolokia agents ##

## Default Jolokia agent URL (to use for testing purposes only)
defaultAgentRL <- 'http://127.0.0.1:8778/jolokia/'

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

## Performs a HTTP POST request to Jolokia
makeHttpPostRequest <- function(agentUrl = defaultAgentRL, operationType = 'read', mbeanName, attributeName) {
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


## Writes to log file
writeToLogFile <- function(logFilename, jsonEntry) {
  if(file.exists(logFilename) == FALSE) {
    file.create(logFilename)
  }
  cat(as.character(jsonEntry), '\n', file = logFilename,
      append = TRUE)
}
