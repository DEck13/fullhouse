#' Historical Figures Dataset (HPI2020)
#'
#' HPI2020 dataset contains information on notable historical figures
#' categorized into distinct peer groups based on different eras.
#' It includes variables such as name, gender, birth year, and group, allowing
#' for analysis across various historical periods.
#'
#' @usage data(hpi2020)
#'
#' @format A tibble with 68650 observations of 5 variables:
#' \itemize{
#'   \item name:  a character vector containing the name of the individual
#'   \item gender: a categorical vector indicating the gender identity of the
#'   individual, where 'M' signifies male and 'F' signifies female.
#'   \item birthyear: a numeric vector representing the birth year of the individual
#'   \item hpi: Historical Popularity Index
#'   \item centuries: a character vector categorizing individuals into distinct peer
#'   groups based on historical periods or eras. Each group represents a unique era,
#'   allowing for the differentiation between notable figures from various time periods.
#' }
#'
#' @source
"hpi2020"
