library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(plotly)

# Define user interface
ui <- dashboardPage(
  dashboardHeader(title = "Analysis / Predictions"),
  dashboardSidebar(
  
  # Set up sidebar menu with three tabs
      sidebarMenu(
        menuItem("Year Analysis", tabName = "analysis", icon = icon("calendar")),
        menuItem("Match Prediction", tabName = "prediction"),
        menuItem("Data", tabName = "data", icon=icon("table"))
      )
    ),
    dashboardBody(
      tabItems(
      
      # Tab content for prediction
      tabItem(
              tabName = "analysis",
              h1("FIFA World Cup Year Analysis"),
              box(textInput("year", "Enter year of World Cup:", value = ""),
              actionButton("submit", "Analyse"),
              textOutput("winner"), width=6, height = 150),
              tabsetPanel(
                  tabPanel("Winners", plotOutput("winning_percentages")),
                  tabPanel("Player Stats", plotOutput("player_statistics"))
              )),
              
      
      tabItem(tabName = "prediction",
              h1("Match Predictions"),
              box(textInput("team1", "Enter team 1:", value = ""),
              textInput("team2", "Enter team 2:", value = ""),
              actionButton("predict", "Predict"),
              textOutput("result"), width=5),
              tabsetPanel(
                tabPanel("Team 1", plotOutput("team1_plot")),
                tabPanel("Team 2", plotOutput("team2_plot")),
              )
              ),
      
      # Tab content for data
      tabItem(tabName = "data",
              h1("Data Sheet"),
              dataTableOutput("data"),
              h1("Player Stats"),
              dataTableOutput("player_stats"),
              h1("Team Stats"),
              dataTableOutput("data_pos")
              
              
      )
    )
  )
)

