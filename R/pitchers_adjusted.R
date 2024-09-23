#' Pitchers Adjusted Dataset
#'
#' A dataset containing adjusted statistics for baseball pitchers.
#'
#' @usage data(pitchers_adjusted)
#' @format A data frame with [number_of_rows] rows and [number_of_columns] variables:
#' \itemize{
#'   \item[name]:  a character vector containing the name of the player.
#'   \item[playerID]: The unique ID of the player.
#'   \item[age]: The age of the player during the season (e.g., integer).
#'   \item[year]: The season year (e.g., integer).
#'   \item[IP]: Innings pitched, the total number of innings the pitcher has thrown (e.g., numeric).
#'   \item[ERA]: Earned Run Average, calculated as (9 * ER)/IP (e.g., numeric).
#'   \item[SO]: Strikeouts, the number of batters struck out by the pitcher (e.g., integer).
#'   \item[ebWAR]: Era-adjusted Wins Above Replacement as computed by Baseball Reference (e.g., numeric).
#'   \item[efWAR]: Era-adjusted Wins Above Replacement as computed by FanGraphs (e.g., numeric).
#'   \item[ER]: Earned Runs, the number of runs scored off the pitcher that are not the result of errors or passed balls (e.g., integer).
#' }
"pitchers_adjusted"
