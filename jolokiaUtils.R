
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
makeHttpPostRequest <- function(agentUrl = defaultAgentUrl, operationType = 'read', mbeanName, attributeName = '') {
  requestBody <- list(type=operationType,
                          mbean=mbeanName)
  if(attributeName != '') { 
    requestBody$attribute <- attributeName
  }
  req <- POST(agentUrl, 
              body = requestBody, encode = "json")
  stop_for_status(req)
  json <- content(req, "text", "application/json")
}   

## Performs a HTTP POST request to Jolokia, save the content response to file and returns the content
makeHttpPostRequestAndSaveToFile <- function(agentUrl = defaultAgentUrl, 
                                             operationType = 'read', 
                                             mbeanName, attributeName,
                                             logFilename) {
  responseContent <- makeHttpPostRequest(agentUrl, operationType,
                                         mbeanName, attributeName)
  writeToLogFile(logFilename, responseContent)
  
  fileContent <- readLines(logFilename)
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

tailfile <- function(file, n) {
  bufferSize <- 1024L
  size <- file.info(file)$size
  
  if (size < bufferSize) {
    bufferSize <- size
  }
  
  pos <- size - bufferSize
  text <- character()
  k <- 0L
  
  f <- file(file, "rb")
  on.exit(close(f))
  
  while(TRUE) {
    seek(f, where=pos)
    chars <- readChar(f, nchars=bufferSize)
    k <- k + length(gregexpr(pattern="\\n", text=chars)[[1L]])
    text <- paste0(text, chars)
    
    if (k > n || pos == 0L) {
      break
    }
    
    pos <- max(pos-bufferSize, 0L)
  }
  
  tail(strsplit(text, "\\n")[[1L]], n)
}

