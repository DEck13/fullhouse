% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pitchers_adjusted.R
\docType{data}
\name{pitchers_adjusted}
\alias{pitchers_adjusted}
\title{Era-adjusted Pitchers Career Dataset}
\format{
A data frame with 31501 rows and 10 variables:
\itemize{
\item playerID: The unique ID of the player.
\item name:  a character vector containing the name of the player.
\item age: The age of the player during the season (e.g., integer).
\item year: The season year (e.g., integer).
\item IP: Innings pitched, the total number of innings the pitcher has thrown (e.g., numeric).
\item ER:Earned Runs, the number of runs that scored against a pitcher that were not a result of fielding errors.
\item ERA: Earned Run Average, calculated as (9 * ER)/IP (e.g., numeric).
\item K:strikeout, the number of times a pitcher struck out a batter.
\item ebWAR: career era-adjusted wins above replacement as computed by Baseball Reference.
\item efWAR: career era-adjusted wins above replacement as computed by FanGraphs.
}
}
\usage{
data(pitchers_adjusted)
}
\description{
A dataset containing era-adjusted season statistics for pitchers up to 2024.
}
\details{
These era-adjusted statistics are obtained from Full House Modeling.
This model computes era-adjusted statistics through a principled
balancing of how well a player performed "vs their peers" and the
size of the MLB talent pool. Under this model, great all-time
statistics requires that an MLB player is both better than their
peers and played during a time in which the talent pool is large.
In this way, the model constructs an even playing field that
extends across eras.
}
\references{
Shen Yan, Adrian Burgos Jr., Christopher Kinson, and Daniel J. Eck (2025). Comparing baseball players across eras via novel Full House Modeling. Annals of Applied Statistics, 19(2): 1778-1799. DOI: 10.1214/24-AOAS1992

Website: \url{https://eckeraadjustment.web.illinois.edu/}
}
\keyword{datasets}
