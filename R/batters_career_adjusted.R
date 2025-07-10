#' Era-adjusted Batters Career Dataset
#'
#' A dataset containing era-adjusted career statistics for batters up to 2024.
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
#' @usage data(batters_career_adjusted)
#' @format A data frame with 9637 rows and 13 variables:
#' \itemize{
#'   \item playerID: The unique ID of the player.
#'   \item name:  a character vector containing the name of the player.
#'   \item PA: plate appearances, the total number of times the player has appeared at the plate (e.g., integer).
#'   \item AB: at-bats, the number of official at-bats the player has taken (e.g., integer).
#'   \item H: hits, the total number of successful hits by the player (e.g., integer).
#'   \item HR: home runs, the number of home runs hit by the player (e.g., integer).
#'   \item BB: bases on balls (Walks), the number of times the player was walked (e.g., integer).
#'   \item BA: batting average, calculated as H/AB (e.g., numeric).
#'   \item OBP: on-base percentage, calculated as (H + BB + HBP)/(AB + BB + HBP + SF) (e.g., numeric).
#'   \item HBP: hit by pitch, the number of times the player was hit by a pitched ball (e.g., integer).
#'   \item SF: sacrifice fly, a fly ball that allows a runner to score after the catch, but does not count as an at-bat (e.g., integer).
#'   \item ebWAR: era-adjusted wins above replacement as computed by Baseball Reference (e.g., numeric).
#'   \item efWAR: era-adjusted wins above replacement as computed by FanGraphs (e.g., numeric).
#' }
#' @references
#' Shen Yan, Adrian Burgos Jr., Christopher Kinson, and Daniel J. Eck (2025). Comparing baseball players across eras via novel Full House Modeling. Annals of Applied Statistics, 19(2): 1778-1799. DOI: 10.1214/24-AOAS1992
#'
#' Website: \url{https://eckeraadjustment.web.illinois.edu/}
"batters_career_adjusted"
