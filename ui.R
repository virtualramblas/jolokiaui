
library(shiny)
library(shinydashboard)
library(plotly)

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
              h2("Jolokia Agents Management")  
      ) 
    )
  )
))
