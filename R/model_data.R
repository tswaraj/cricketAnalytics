#' Load the processed cricket data
#'
#' This function loads the processed cricket data.
load_data_model = function(){
  data("processed_cricket_data", package = "cricketAnalytics")
  processed_cricket_data
}

#' Transform data
#'
#' This function takes processed data as input and returns the transformed data.
#' Transformation include converting the output variable to factor.
#'
#' @param raw_data A data.frame containing the raw data.
#'
#' @return A data.frame containing the transformed data.
#' @import caret
transform_data = function(raw_data){
  transformed_data = data.frame(raw_data)
  transformed_data$winner = as.factor(transformed_data$winner)
  levels(transformed_data$winner) = make.names(levels(transformed_data$winner))

  return(transformed_data)
}


#' Fit model
#'
#' This function fits a random forest model to the given training data.
#'
#' @param train A data.frame containing the training data.
#'
#' @return A trained random forest model.
#' @import randomForest
#' @import caret
fit_model = function(train){
  set.seed(123)
  model = randomForest(winner ~ ., data = train, mtry = 4)
  return(model)
}

#' Predict match outcome
#'
#' This function predicts the match outcome for the given test data
#' using the best-fitted model.
#'
#' @param test A data.frame containing the test data.
#'
#' @return A vector containing the predicted win probabilities.
#' @export
predict_match_outcome = function(test){
  processed_cricket_data = load_data_model()
  df = transform_data(processed_cricket_data)
  model = fit_model(df)
  win_prob = predict(model, newdata = test, type = "prob")
  return(win_prob[, "X1"])
}
