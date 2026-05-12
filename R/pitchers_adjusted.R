#' Era-Adjusted Pitchers Season Dataset
#'
#' A dataset containing era-adjusted season statistics for pitchers up to 2025.
#' @details
#' These era-adjusted statistics are obtained from Full House Modeling.
#' This model computes era-adjusted statistics through a principled
#' balancing of how well a player performed "vs their peers" and the
#' size of the MLB talent pool. Under this model, great all-time
#' statistics requires that an MLB player is both better than their
#' peers and played during a time in which the talent pool is large.
#' In this way, the model constructs an even playing field that
#' extends across eras.
#'
#' @usage data(pitchers_adjusted)
#' @format A data frame with 33271 rows and 9 variables:
#' \itemize{
#'   \item name: a character vector containing the name of the player.
#'   \item playerID: The unique ID of the player.
#'   \item year: The season year (integer).
#'   \item age: The age of the player during the season (integer).
#'   \item eIP: Innings pitched, the total number of innings the pitcher has thrown (numeric).
#'   \item eERA: Earned Run Average, calculated as (9 * eER)/eIP (numeric).
#'   \item eSO: strikeout, the number of times a pitcher struck out a batter (integer).
#'   \item ebWAR: era-adjusted wins above replacement as computed by Baseball Reference (numeric).
#'   \item efWAR: era-adjusted wins above replacement as computed by FanGraphs (numeric).
#' }
#' @references
#' Shen Yan, Adrian Burgos Jr., Christopher Kinson, and Daniel J. Eck (2025). Comparing baseball players across eras via novel Full House Modeling. Annals of Applied Statistics, 19(2): 1778-1799. DOI: 10.1214/24-AOAS1992
#'
#' Website: \url{https://eckeraadjustment.web.illinois.edu/}
"pitchers_adjusted"
