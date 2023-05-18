# Cricket Clairvoyance - Predicting Women’s T20 Match Winners with Machine Learning

`cricketAnalytics` is a comprehensive R package developed for predicting the outcomes of Women’s T20 Challenge cricket matches utilizing machine learning. It takes into account the first innings score and predicts the winning probability of the team batting in the second innings.

The package makes use of the detailed ball-by-ball data from all Women’s T20 Challenge matches. The raw data, present in individual CSV files, is processed to form a final, comprehensive dataset used for training the machine learning model.

The final processed dataset contains 12 features:

1. Balls delivered
2. Powerplay status of the current ball
3. Batting team name
4. Bowling team name
5. Match venue
6. First innings score
7. Current score
8. Current run rate
9. Wickets lost
10. Runs scored in the last five overs
11. Wickets taken in the last five overs
12. Winner (1 if the batting team won, 0 otherwise)

I have utilized the Random Forest model for its exemplary performance with tabular data. The model uses an ensemble of decision trees trained on random subsets of data and features, thereby ensuring diversity in the trees, and collectively making the final prediction.

Apart from just making match outcome predictions, `cricketAnalytics` also provides visualization functionalities, helping users understand the past matches in depth and understand the performance trends of teams.

Lastly, `cricketAnalytics` includes a 'run_app' function that launches a Shiny app for interactive use of the model and visualization capabilities. Every time the `cricketAnalytics` package is loaded, it automatically fetches and processes the latest Women’s T20 Challenge match data from [cricsheet.org](cricsheet.org), ensuring the model remains updated.

## Installation

You can install the released version of `cricketAnalytics` from GitHub with:

```r
devtools::install_github("tswaraj/cricketAnalytics")
```
