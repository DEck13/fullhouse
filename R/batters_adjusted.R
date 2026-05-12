#' Era-Adjusted Batters Season Dataset
#'
#' A dataset containing era-adjusted season statistics for batters up to 2025.
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
#' @usage data(batters_adjusted)
#' @format A data frame with 51656 rows and 19 variables:
#' \itemize{
#'   \item name: a character vector containing the name of the player.
#'   \item playerID: the unique ID of the player.
#'   \item year: the season year (integer).
#'   \item age: the age of the player during the season (integer).
#'   \item ePA: plate appearances, the total number of times the player has appeared at the plate (integer).
#'   \item eAB: at-bats, the number of official at-bats the player has taken (integer).
#'   \item eH: hits, the total number of successful hits by the player (integer).
#'   \item e2B: doubles, the number of doubles hit by the player (integer).
#'   \item e3B: triples, the number of triples hit by the player (integer).
#'   \item eHR: home runs, the number of home runs hit by the player (integer).
#'   \item eBB: bases on balls (Walks), the number of times the player was walked (integer).
#'   \item eBA: batting average, calculated as H/AB (numeric).
#'   \item eOBP: on-base percentage, calculated as (eH + eBB + HBP)/(eAB + eBB + HBP + SF) (numeric).
#'   \item eSLG: slugging percentage, calculated as (eH + e2B + 2\*e3B + 3\*eHR)/eAB (numeric).
#'   \item eOPS: on-base plus slugging percentage, calculated as eOBP + eSLG (numeric).
#'   \item HBP: hit by pitch, the number of times the player was hit by a pitched ball (integer).
#'   \item SF: sacrifice fly, a fly ball that allows a runner to score after the catch, but does not count as an at-bat (integer).
#'   \item ebWAR: era-adjusted wins above replacement as computed by Baseball Reference (numeric).
#'   \item efWAR: era-adjusted wins above replacement as computed by FanGraphs (numeric).
#' }
#' @references
#' Shen Yan, Adrian Burgos Jr., Christopher Kinson, and Daniel J. Eck (2025). Comparing baseball players across eras via novel Full House Modeling. Annals of Applied Statistics, 19(2): 1778-1799. DOI: 10.1214/24-AOAS1992
#'
#' Website: \url{https://eckeraadjustment.web.illinois.edu/}
"batters_adjusted"

