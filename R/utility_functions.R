#' Load data for utility functions
#'
#' This function loads the processed cricket data for utility functions.
#'
#' @return A data.frame containing the processed cricket data.
load_data_for_utitlity_functions = function() {
  data("processed_cricket_data", package = "cricketAnalytics")
  processed_cricket_data
}

#' Batting team list
#'
#' This function returns a list of unique batting teams.
#'
#' @return A character vector containing the unique batting teams.
#' @export
batting_team_list = function() {
  df = load_data_for_utitlity_functions()
  unique_batting_teams = unique(df$batting_team)
  return(unique_batting_teams)
}

#' Bowling team list
#'
#' This function returns a list of unique bowling teams.
#'
#' @return A character vector containing the unique bowling teams.
#' @export
bowling_team_list = function() {
  df = load_data_for_utitlity_functions()
  unique_bowling_teams = unique(df$bowling_team)
  return(unique_bowling_teams)
}

#' Venue list
#'
#' This function returns a list of unique venues.
#'
#' @return A character vector containing the unique venues.
#' @export
venue_list = function() {
  df = load_data_for_utitlity_functions()
  unique_venues = unique(df$venue)
  return(unique_venues)
}





