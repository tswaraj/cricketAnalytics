library(shiny)

ui <- fluidPage(
  titlePanel("Cricket Clairvoyance App"),

  sidebarLayout(
    sidebarPanel(
      radioButtons("power_play", "Power Play:", choices = c("Yes" = 1, "No" = 0)),
      selectInput("batting_team", "Batting Team:", choices = NULL),
      selectInput("bowling_team", "Bowling Team:", choices = NULL),
      selectInput("venue", "Venue:", choices = NULL),
      sliderInput("balls_delivered", "Balls Delivered:", min = 1, max = 120, value = 1),
      sliderInput("first_inning_score", "First Inning Score:", min = 0, max = 720, value = 0),
      sliderInput("current_score", "Current Score:", min = 0, max = 720, value = 0),
      sliderInput("current_run_rate", "Current Run Rate:", min = 0, max = 36, value = 0),
      sliderInput("wickets_lost", "Wickets Lost:", min = 0, max = 10, value = 0),
      sliderInput("run_in_last5", "Run in Last 5:", min = 0, max = 720, value = 0),
      sliderInput("wickets_in_last5", "Wickets in Last 5:", min = 0, max = 10, value = 0)
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Win Probability", plotOutput("win_probability_plot")),
        tabPanel("Batting Team Averages", plotOutput("batting_team_averages")),
        tabPanel("Bowling Team Averages", plotOutput("bowling_team_averages")),
        tabPanel("Cumulative Runs", plotOutput("cumulative_runs_specific_match")),
        tabPanel("Boundaries", plotOutput("boundaries"))
      )
    )
  )
)
