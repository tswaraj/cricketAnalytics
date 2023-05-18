#' Load the processed cricket data
#'
#' This function loads the processed cricket data
load_data_graph <- function(){
  data("all_matches_data", package = "cricketAnalytics")
  all_matches_data
}

#' Plot Boundaries by Team
#'
#' This function creates a bar plot of the total number of boundaries (4s and 6s) scored by each team in the given dataset.
#'
#' @param data A data.frame containing the cricket match data with at least two columns: 'batting_team' and 'runs_off_bat'.
#' 'batting_team' should contain the team names, and 'runs_off_bat' should contain the number of runs scored off each ball.
#'
#' @return A ggplot2 object displaying a bar plot of the number of boundaries scored by each team.
#'
#' @examples
#' # Sample dataset
#' data <- data.frame(
#'   batting_team = c("Team A", "Team A", "Team B", "Team B", "Team A", "Team B"),
#'   runs_off_bat = c(4, 1, 6, 2, 4, 4)
#' )
#'
#' # Create a plot of boundaries by team
#' boundary_plot <- plot_boundaries(data)
#' print(boundary_plot)
#'
#' @import ggplot2
#' @export

# Function to plot the number of boundaries by each team
plot_boundaries <- function() {
  data <- load_data_graph()
  data$boundary <- ifelse(data$runs_off_bat %in% c(4, 6), 1, 0)
  team_boundaries <- aggregate(data$boundary, by = list(data$batting_team), FUN = sum)
  colnames(team_boundaries) <- c("Team", "Boundaries")
  boundary_bar_plot <- ggplot(team_boundaries, aes(x = Team, y = Boundaries, fill = Team)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(title = "Number of Boundaries by Each Team", x = "Team", y = "Boundaries")
  return(boundary_bar_plot)
}


#' Plot Total Runs Scored by Each Team at a Specific Stadium During the First Innings
#'
#' This function creates a bar plot of the total runs scored by each team at a specific stadium during the first innings.
#'
#' @param data A data.frame containing the cricket match data with at least four columns: 'batting_team', 'runs_off_bat', 'innings', and 'venue'.
#' 'batting_team' should contain the team names, 'runs_off_bat' should contain the number of runs scored off each ball, 'innings' should contain the innings number, and 'venue' should contain the stadium name.
#' @param specific_stadium A character string specifying the name of the stadium.
#'
#' @return A ggplot2 object displaying a bar plot of the total runs scored by each team at the specific stadium during the first innings.
#'
#' @examples
#' # Sample dataset
#' data <- data.frame(
#'   batting_team = c("Team A", "Team A", "Team B", "Team B", "Team A", "Team B"),
#'   runs_off_bat = c(4, 1, 6, 2, 4, 4),
#'   innings = c(1, 1, 1, 1, 1, 1),
#'   venue = rep("Sawai Mansingh Stadium, Jaipur", 6)
#' )
#'
#' # Create a plot of total runs scored by each team at a specific stadium during the first innings
#' specific_stadium <- "Sawai Mansingh Stadium, Jaipur"
#' bar_plot <- plot_total_runs(data, specific_stadium)
#' print(bar_plot)
#'
#' @export
plot_total_runs <- function(specific_stadium) {
  data <- load_data_graph()
  # Filter data for first innings and specific stadium
  first_innings_data <- subset(data, innings == 1 & venue == specific_stadium)

  if(dim(first_innings_data)[1] == 0){
    df <- data.frame(x = numeric(), y = numeric())
    p <- ggplot(df, aes(x, y))
    p <- p + annotate("text", x = 0.5, y = 0.5, label = "No data available")
    return(p)
  }

  # Bar plot of total runs scored by each team at the specific stadium during the first innings
  team_runs <- aggregate(first_innings_data$runs_off_bat, by = list(first_innings_data$batting_team), FUN = sum)
  colnames(team_runs) <- c("Team", "Total_Runs")
  bar_plot <- ggplot(team_runs, aes(x = Team, y = Total_Runs, fill = Team)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(title = paste("Total Runs Scored by Each Team at", specific_stadium, "During the First Innings"),
         x = "Team", y = "Total Runs")
  return(bar_plot)
}


#' Plot Cumulative Runs Over Balls for Each Team's Second Latest Match with Outliers Removed
#'
#' This function creates a line plot of cumulative runs over balls for each team's second latest match, with outliers removed.
#'
#' @param data A data.frame containing the cricket match data with at least four columns: 'match_id', 'batting_team', 'runs_off_bat', and 'ball'. 'match_id' should contain the match identifiers, 'batting_team' should contain the team names, 'runs_off_bat' should contain the number of runs scored off each ball, and 'ball' should contain the ball numbers.
#'
#' @return A ggplot2 object displaying a line plot of cumulative runs over balls for each team's second latest match, with outliers removed.
#' @export
plot_cumulative_runs_all_teams <- function() {
  data <- load_data_graph()
  # Calculate the cumulative runs for each team across all matches
  data$cum_runs <- with(data, ave(runs_off_bat, batting_team, match_id, FUN = cumsum))

  # Smoothed line plot of cumulative runs over balls for all teams across all matches
  line_plot <- ggplot(data, aes(x = ball, y = cum_runs, color = batting_team, linetype = batting_team)) +
    geom_smooth(se = FALSE, method = "loess") +
    theme_minimal() +
    labs(title = "Smoothed Cumulative Runs Over Balls for All Teams Across All Matches",
         x = "Ball", y = "Cumulative Runs")
  return(line_plot)
}


#' Plot Batting Averages of Teams Across Seasons
#'
#' This function creates line plots of the batting averages of teams across seasons.
#'
#' @param data A data.frame containing the cricket match data with at least five columns: 'batting_team', 'bowling_team', 'season', 'runs_off_bat', and 'player_dismissed'. 'batting_team' and 'bowling_team' should contain the team names, 'season' should contain the season identifiers, 'runs_off_bat' should contain the number of runs scored off each ball, and 'player_dismissed' should contain the names of dismissed players.
#'
#' @return A list containing a ggplot2 object displaying line plots of the batting averages of teams across seasons.
#'
#' @examples
#' # Sample dataset
#' data <- data.frame(
#'   batting_team = c("Team A", "Team A", "Team B", "Team A", "Team B", "Team B"),
#'   bowling_team = c("Team B", "Team B", "Team A", "Team B", "Team A", "Team A"),
#'   season = c(1, 1, 1, 2, 2, 2),
#'   runs_off_bat = c(4, 1, 6, 2, 4, 4),
#'   player_dismissed = c(NA, "Batsman A", NA, NA, "Batsman B", NA)
#' )
#'
#' # Create plots of batting averages for each team per season
#' batting_avg_plot <- plot_team_averages(data)
#' print(batting_avg_plot)
#'
#' @import ggplot2
#' @export
plot_batting_team_averages <- function() {
  data <- load_data_graph()
  # Calculate total runs scored and conceded by each team per season
  team_runs_scored <- aggregate(data$runs_off_bat, by = list(data$batting_team, data$season), FUN = sum)
  team_runs_conceded <- aggregate(data$runs_off_bat, by = list(data$bowling_team, data$season), FUN = sum)
  colnames(team_runs_scored) <- c("team", "season", "runs_scored")
  colnames(team_runs_conceded) <- c("team", "season", "runs_conceded")

  # Calculate total wickets taken by each team per season
  team_wickets <- data[!is.na(data$player_dismissed), ]
  team_wickets_taken <- aggregate(team_wickets$player_dismissed, by = list(team_wickets$bowling_team, team_wickets$season), FUN = length)
  colnames(team_wickets_taken) <- c("team", "season", "wickets_taken")

  # Merge the runs scored, runs conceded, and wickets taken data
  team_performance <- merge(merge(team_runs_scored, team_runs_conceded), team_wickets_taken)

  # Calculate batting averages
  team_performance$batting_avg <- team_performance$runs_scored / team_performance$wickets_taken

  batting_avg_plot <- ggplot(team_performance, aes(x = season, y = batting_avg, group = team, color = team)) +
    geom_line() +
    geom_point() +
    theme_minimal() +
    labs(title = "Batting Averages of Teams Across Seasons",
         x = "Season", y = "Batting Average")

  return(batting_avg_plot)
}


#' Plot Bowling Averages of Teams Across Seasons
#'
#' This function creates line plots of the bowling averages of teams across seasons.
#'
#' @param data A data.frame containing the cricket match data with at least five columns: 'batting_team', 'bowling_team', 'season', 'runs_off_bat', and 'player_dismissed'. 'batting_team' and 'bowling_team' should contain the team names, 'season' should contain the season identifiers, 'runs_off_bat' should contain the number of runs scored off each ball, and 'player_dismissed' should contain the names of dismissed players.
#'
#' @return A list containing a ggplot2 object displaying line plots of the bowling averages of teams across seasons.
#'
#' @examples
#' # Sample dataset
#' data <- data.frame(
#'   batting_team = c("Team A", "Team A", "Team B", "Team A", "Team B", "Team B"),
#'   bowling_team = c("Team B", "Team B", "Team A", "Team B", "Team A", "Team A"),
#'   season = c(1, 1, 1, 2, 2, 2),
#'   runs_off_bat = c(4, 1, 6, 2, 4, 4),
#'   player_dismissed = c(NA, "Batsman A", NA, NA, "Batsman B", NA)
#' )
#'
#' # Create plots of batting and bowling averages for each team per season
#' bowling_avg_plot <- plot_team_averages(data)
#' print(bowling_avg_plot)
#'
#' @import ggplot2
#' @export
plot_bowling_team_averages <- function() {
  data <- load_data_graph()
  # Calculate total runs scored and conceded by each team per season
  team_runs_scored <- aggregate(data$runs_off_bat, by = list(data$batting_team, data$season), FUN = sum)
  team_runs_conceded <- aggregate(data$runs_off_bat, by = list(data$bowling_team, data$season), FUN = sum)
  colnames(team_runs_scored) <- c("team", "season", "runs_scored")
  colnames(team_runs_conceded) <- c("team", "season", "runs_conceded")

  # Calculate total wickets taken by each team per season
  team_wickets <- data[!is.na(data$player_dismissed), ]
  team_wickets_taken <- aggregate(team_wickets$player_dismissed, by = list(team_wickets$bowling_team, team_wickets$season), FUN = length)
  colnames(team_wickets_taken) <- c("team", "season", "wickets_taken")

  # Merge the runs scored, runs conceded, and wickets taken data
  team_performance <- merge(merge(team_runs_scored, team_runs_conceded), team_wickets_taken)

  # Calculate bowling averages
  team_performance$bowling_avg <- team_performance$runs_conceded / team_performance$wickets_taken

  bowling_avg_plot <- ggplot(team_performance, aes(x = season, y = bowling_avg, group = team, color = team)) +
    geom_line() +
    geom_point() +
    theme_minimal() +
    labs(title = "Bowling Averages of Teams Across Seasons",
         x = "Season", y = "Bowling Average")
  return(bowling_avg_plot)
}
