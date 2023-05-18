#' Download and process CSV files (cricket matches ball by ball data)
#'
#' Fetch the latest cricket match data for Women's T20 Challenge cricket matches.
#' Process the data (extract and create features) that would help predict outcome of a match.
#'
#' @importFrom utils unzip
#' @importFrom utils download.file
#' @import dplyr
#' @import zoo
#'
load_and_process_raw_cricket_data = function() {

  # Download and unzip the file to the package's inst/extdata directory
  url = "https://cricsheet.org/downloads/wtc_female_csv2.zip"
  destfile = tempfile(fileext = ".zip")
  download.file(url, destfile)
  unzip(destfile, exdir = system.file("extdata", package = "cricketAnalytics"))
  file.remove(destfile)

  processed_cricket_data = data.frame(
    power_play = logical(),
    batting_team = character(),
    bowling_team = character(),
    venue = character(),
    balls_delivered = numeric(),
    first_inning_score = numeric(),
    current_score = numeric(),
    current_run_rate = numeric(),
    wickets_lost = numeric(),
    run_in_last5 = numeric(),
    wickets_in_last5 = numeric(),
    winner = logical()
  )

  process_match_data = function(file){
    match_df = read.csv(file)

    match_df$power_play = ifelse(match_df$ball < 7, 1, 0)

    match_df$balls_delivered = 1
    match_df$balls_delivered[match_df$innings == 1] = cumsum(match_df$balls_delivered[match_df$innings == 1])
    match_df$balls_delivered[match_df$innings == 2] = cumsum(match_df$balls_delivered[match_df$innings == 2])

    match_df$runs_scored_current_ball = match_df$runs_off_bat + match_df$extras
    first_inning_score = sum(match_df$runs_scored_current_ball[match_df$innings == 1])
    match_df$first_inning_score = first_inning_score

    match_df$current_score[match_df$innings == 1] = cumsum(match_df$runs_scored_current_ball[match_df$innings == 1])
    match_df$current_score[match_df$innings == 2] = cumsum(match_df$runs_scored_current_ball[match_df$innings == 2])

    match_df$ball = as.character(match_df$ball)
    split_ball = strsplit(match_df$ball, "\\.")
    match_df$current_over = sapply(split_ball, "[", 1)
    match_df$current_ball = sapply(split_ball, "[", 2)
    match_df$current_over = as.numeric(match_df$current_over)
    match_df$current_ball = as.numeric(match_df$current_ball)
    match_df$current_run_rate = match_df$current_score / (match_df$current_over + 1)


    match_df$wicket = ifelse(nchar(match_df$player_dismissed) > 0, 1, 0)
    match_df$wickets_lost[match_df$innings == 1] = cumsum(match_df$wicket[match_df$innings == 1])
    match_df$wickets_lost[match_df$innings == 2] = cumsum(match_df$wicket[match_df$innings == 2])

    match_df = subset(match_df, innings == 2)

    match_df = match_df %>% mutate(run_in_last5 = rollapplyr(runs_scored_current_ball, width = 30, FUN = sum, fill = NA, partial = TRUE))

    match_df = match_df %>% mutate(wickets_in_last5 = rollapplyr(wicket, width = 30, FUN = sum, fill = NA, partial = TRUE))

    if (match_df[dim(match_df)[1], c("current_score")] > match_df[dim(match_df)[1], c("first_inning_score")]){
      match_df$winner = 1
    } else {
      match_df$winner = 0
    }

    cols = c("power_play", "batting_team", "bowling_team", "venue", "balls_delivered", "first_inning_score", "current_score", "current_run_rate", "wickets_lost", "run_in_last5", "wickets_in_last5", "winner")
    return (match_df[, cols])
  }

  file_list = list.files(system.file("extdata", package = "cricketAnalytics"))
  for (file in file_list) {
    if (file == "all_matches.csv"){
      all_matches_data = read.csv(paste0(system.file("extdata", package = "cricketAnalytics"), "/", file))
    }
    if (grepl("^\\d+\\.csv$", file)) {
      match_df = process_match_data(paste0(system.file("extdata", package = "cricketAnalytics"), "/", file))
      processed_cricket_data = rbind(processed_cricket_data, match_df)
    }
  }

  # Save the processed data to .rda files in the package's data directory
  data_path = system.file("data", package = "cricketAnalytics")
  save(processed_cricket_data, file = file.path(data_path, "processed_cricket_data.rda"))
  save(all_matches_data, file = file.path(data_path, "all_matches_data.rda"))
}

#' Fetches and loads the latest data
.onLoad = function(libname, pkgname, ...){
  load_and_process_raw_cricket_data()
}
