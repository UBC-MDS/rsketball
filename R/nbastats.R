#' Generate summary stats for NBA players
#'
#' @description
#' The function filters the dataset further using the arguments provided and
#' produces a tibble with summary statistics for a list of columns of a few players or teams.
#' The function can only use one of the two filters - playerNames and teamNames. Hence, If playerNames
#' are provided, teamNames are ignored.
#'
#' @param data tibble
#' @param columnNames character vector
#' @param playerNames character vector
#' @param teamNames character vector
#'
#' @return A tibble
#' @export
#'
#' @examples
#' nbastats(nba_data, columnNames = c('GP', '3PM', 'FT%'),
#'          playerNames = c('Stephen Curry', 'Paul George'))
#' nbastats(nba_data, columnNames = c('GP', '3PM', 'FT%'),
#'          teamNames = c('UTAH', 'PHX', 'DET'))
#'
#'
nbastats <- function(data, columnNames = c("all"),  playerNames = c("all"), teamNames = c("all")) {
  print("nbastats function triggered!")
}
