library(shiny)

server <- function(input, output, session) {
  # Update the choices for batting_team, bowling_team, and venue
  updateSelectInput(session, "batting_team", choices = batting_team_list())
  updateSelectInput(session, "bowling_team", choices = bowling_team_list())
  updateSelectInput(session, "venue", choices = venue_list())

  # Compute the win probability and generate a pie chart
  output$win_probability_plot <- renderPlot({
    # Get input values
    test <- data.frame(
      power_play = input$power_play,
      batting_team = input$batting_team,
      bowling_team = input$bowling_team,
      venue = input$venue,
      balls_delivered = input$balls_delivered,
      first_inning_score = input$first_inning_score,
      current_score = input$current_score,
      current_run_rate = input$current_run_rate,
      wickets_lost = input$wickets_lost,
      run_in_last5 = input$run_in_last5,
      wickets_in_last5 = input$wickets_in_last5
    )

    # Calculate win probability (using your model prediction function)
    win_prob <- predict_match_outcome(test)
    lose_prob <- 1 - win_prob

    # Create a pretty pie chart with percentage
    probs <- c(win_prob, lose_prob)
    labels <- c("Win", "Lose")
    labels <- paste(labels, round(probs * 100, 1), "%")
    colors <- c("#56B4E9", "#E69F00")
    pie(probs, labels = labels, col = colors, main = "Win / Lose Probability")
  })

  # Generate the other plots
  output$batting_team_averages <- renderPlot({
    plot_batting_team_averages()
  })

  output$bowling_team_averages <- renderPlot({
    plot_bowling_team_averages()
  })

  output$cumulative_runs_specific_match <- renderPlot({
    plot_cumulative_runs_all_teams()
  })

  output$boundaries <- renderPlot({
    plot_boundaries()
  })

}
