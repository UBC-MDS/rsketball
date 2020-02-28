#' Creating Boxplot for NBA Stats
#'
#'  Creates a boxplot of the categorical variable of interest on the y-axis and
#'  the stat of interest on the x-axis.
#'
#' @param dataset  dataframe
#' This dataframe is created after using the nba_scraper.R function or if
#' the csv has already been loaded, read the csv in and pass it as the parameter.
#' @param yaxis string
#' The parameter of interest examples: Points, 3_Pointers, Turnovers
#' @param xaxis string
#' The categorical variable of interest examples: Team, Position
#'
#' @return ggplot boxplot
#' @export
#'
#' @examples
#' NBA_reg_01_02 <- read.csv("NBA_reg_2001-2002.csv")
#' nba_boxplot(NBA_reg_01_02, Team, Points)

nba_boxplot <- function(dataset, yaxis, xaxis) {

  return()
}
