#' Era-adjusted Batters Career Dataset (2024)
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
#' @usage data(pitchers_adjusted)
#' @format A data frame with 9638 rows and 13 variables:
#' \itemize{
#'   \item playerID: The unique ID of the player.
#'   \item name:  a character vector containing the name of the player.
#'   \item PA: career plate appearances, the total number of times the player has appeared at the plate.
#'   \item AB: career at-bats, the number of official at-bats the player has taken.
#'   \item H: career hits, the total number of successful hits by the player.
#'   \item HR: career home runs, the number of home runs hit by the player.
#'   \item BB: career bases on balls (Walks), the number of times the player was walked.
#'   \item BA: career batting average, calculated as H/AB.
#'   \item HBP: career hit by pitch, the number of times the player was hit by a pitched ball.
#'   \item SF: career sacrifice fly.
#'   \item OBP: career on-base percentage.
#'   \item ebWAR: career era-adjusted wins above replacement as computed by Baseball Reference.
#'   \item efWAR: career era-adjusted wins above replacement as computed by FanGraphs.
#' }
#' @references
#' Shen Yan, Adrian Burgos Jr., Christopher Kinson, and Daniel J. Eck (2024). "Comparing baseball players across eras via novel Full House Modeling." Available at: \url{https://arxiv.org/abs/2207.11332}
#'
#' Website: \url{https://eckeraadjustment.web.illinois.edu/}
"batters_career_adjusted_2024"
