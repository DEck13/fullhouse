#' Batters Adjusted Dataset
#'
#' A dataset containing adjusted statistics for baseball batters.
#'
#' @usage data(batters_adjusted)
#' @format A data frame with [number_of_rows] rows and [number_of_columns] variables:
#' \itemize{
#'   \item[name]:  a character vector containing the name of the player.
#'   \item[playerID]: The unique ID of the player.
#'   \item[age]: The age of the player during the season (e.g., integer).
#'   \item[year]: The season year (e.g., integer).
#'   \item[PA]: Plate appearances, the total number of times the player has appeared at the plate (e.g., integer).
#'   \item[AB]: At-bats, the number of official at-bats the player has taken (e.g., integer).
#'   \item[H]: Hits, the total number of successful hits by the player (e.g., integer).
#'   \item[HR]: Home runs, the number of home runs hit by the player (e.g., integer).
#'   \item[BB]: Bases on Balls (Walks), the number of times the player was walked (e.g., integer).
#'   \item[BA]: Batting Average, calculated as H/AB (e.g., numeric).
#'   \item[OBP]: On-Base Percentage, calculated as (H + BB + HBP)/(AB + BB + HBP + SF) (e.g., numeric).
#'   \item[HBP]: Hit by Pitch, the number of times the player was hit by a pitched ball (e.g., integer).
#'   \item[SF]: Sacrifice Fly, a fly ball that allows a runner to score after the catch, but does not count as an at-bat (e.g., integer).
#'   \item[ebWAR]: Era-adjusted Wins Above Replacement as computed by Baseball Reference (e.g., numeric).
#'   \item[efWAR]: Era-adjusted Wins Above Replacement as computed by FanGraphs (e.g., numeric).
#' }
"batters_adjusted"
