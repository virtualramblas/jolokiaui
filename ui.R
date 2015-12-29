
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
  dashboardHeader(title = "Jolokia UI" 
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overviewTab", icon = icon("line-chart")),
      menuItem("Dashboard", tabName = "dashboardTab", icon = icon("dashboard")),
      menuItem("JVM Details", tabName = "jvmDetailsTab", icon = icon("coffee")),
      menuItem("Configuration", tabName = "configurationTab", icon = icon("wrench"),
               menuItem("General Settings", tabName = "settingsTab", icon = icon("wrench")),
               menuItem("Manage Agents", tabName = "agentMgmtTab", icon = icon("plug"))
      ),
      uiOutput("agentSelection")
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
      tabItem(tabName = "dashboardTab",
              fluidRow(
                uiOutput("cpuDashUi", width = 12), 
                uiOutput("memoryDashUi", width = 12)
              )
      ),
      tabItem(tabName = "jvmDetailsTab",
              h2("Monitored JVM Details"),
              dataTableOutput(outputId="jvmSummaryTable")
      ),
      tabItem(tabName = "configurationTab"
      ),
      tabItem(tabName = "settingsTab",
              h2("General Settings"),
              br(),
              sliderInput("refreshInterval", "Refresh Interval (in seconds):", 
                          min=0, max=120, value=5),
              br(),
#               textInput("logDirectoryInput", "The directory where the JSON logs are stored:"),
#               br(),
              actionButton("deleteHistoryButton", "Delete History", "danger")
      ),
      tabItem(tabName = "agentMgmtTab",
              h2("Jolokia Agents Management"),
              br(),
              selectInput("agentListSelectInput", "Saved Agents:", choices = c('None'='None')
                          ),
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
                  actionButton("deleteAgentButton", "Delete", "danger"),
                  textOutput("messageTextOutput")
                )
              )
      ) 
    )
  )
))
