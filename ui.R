
library(shiny)
library(shinydashboard)
library(plotly)

actionButton <- function(inputId, label, btn.style = "" , css.class = "") {
  if ( btn.style %in% c("primary","info","success","warning","danger","inverse","link")) {
    btn.css.class <- paste("btn",btn.style,sep="-")
  } else btn.css.class = ""
  
  tags$button(id=inputId, type="button", class=paste("btn action-button",btn.css.class,css.class,collapse=" "), label)
}

shinyUI(dashboardPage(skin = "green",
  dashboardHeader(title = "Jolokia UI" #,
                   #actionLink("documentationActionlink", "Jolokia documentation", icon = icon("book"))
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overviewTab", icon = icon("dashboard")),
      menuItem("Configuration", tabName = "configurationTab", icon = icon("wrench"),
               menuItem("General Settings", tabName = "settingsTab", icon = icon("wrench")),
               menuItem("Manage Agents", tabName = "agentMgmtTab", icon = icon("plug"))
      ),
      selectInput("agentSelectInput", "Agent:", 
                  choices = c('localhost'='http://127.0.0.1:8778/jolokia/',
                                  'None'='None'))
    )
    
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "overviewTab",
              fluidRow(
                column(width = 6,
                       plotlyOutput("heapUsagePlot")
                       ),
                column(width = 6,
                       plotlyOutput("threadCountPlot")
                )
              ),
              br(),
              fluidRow(
                column(width = 6,
                       plotlyOutput("classCountPlot")
                ),
                column(width = 6,
                       plotlyOutput("cpuUsagePlot")
                )
              )
              
      ),
      tabItem(tabName = "configurationTab"
      ),
      tabItem(tabName = "settingsTab",
              h2("General Settings"),
              br(),
              sliderInput("refreshInterval", "Refresh Interval (in seconds):", 
                          min=0, max=120, value=5)
      ),
      tabItem(tabName = "agentMgmtTab",
              h2("Jolokia Agents Management"),
              br(),
              selectInput("agentListSelectInput", "Saved Agents:", 
                          choices = c('None'='None')),
              br(),
              column(width = 6,
                wellPanel(
                  textInput("agentNameText", label = h3("Agent Name"), 
                            value = "Enter value..."),
                  textInput("agentHostText", label = h3("Agent Host"), 
                            value = "Enter value..."),
                  textInput("agentPortText", label = h3("Port"), 
                            value = "Enter value..."),
                  actionButton("testAgentButton", "Test Connection", "info"),
                  actionButton("saveAgentButton", "Save", "danger"),
                  #TODO Add the delete button
                  textOutput("messageTextOutput")
                )
              )
      ) 
    )
  )
))
