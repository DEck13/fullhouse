#' MLB Talent Pool
#'
#' This dataset contains the talent pool for the American League (AL) and
#' National League (NL) from 1871 to 2024.
#'
#' @usage data(talentpool)
#'
#' @format A tibble with 154 observations of 3 variables:
#' \itemize{
#'   \item year: a numeric vector for year.
#'   \item NLpop: a numeric vector for the talent pool size for the NL.
#'   \item ALpop: a numeric vector for the talent pool size for the AL.
#' }
#'
#' @source See [this technical report](https://eckeraadjustment.web.illinois.edu/MLBeligiblepop.html)
#' for details on how these estimates of the talent pool were constructed from
#' 1871 to 2020. The talent pool sizes from from 2021-2024 are calculated by linear extrapolation.
"talentpool"
