#' Generate summary stats for NBA players
#'
#' @description
#' The function filters the dataset further using the arguments provided and
#' produces a tibble with summary statistics for a list of columns of a few players or teams.
#' The function can only use one of the two filters - playerNames and teamNames. Hence, If playerNames
#' are provided, teamNames are ignored.
#'
#' @param nba_data tibble
#' @param columnNames character vector
#' @param playerNames character vector
#' @param teamNames character vector
#' @param positions character vector
#' @param all boolean
#'
#' @return A tibble
#' @export
#'
#' @importFrom readr read_csv
#' @import dplyr
#'
#' @examples
#' \donttest{
#' nba_data = read_csv(
#' "https://raw.githubusercontent.com/kfoofw/nba_espn/master/0.data/NBA_reg_2018-2019.csv"
#' )
#' nbastats(nba_data, columnNames = c('GP', '3PM', 'FT%'),
#'          playerNames = c('Stephen Curry', 'Paul George'))
#' nbastats(nba_data, columnNames = c('GP', '3PM', 'FT%'),
#'          teamNames = c('UTAH', 'PHX', 'DET'))
#' }
#'
nbastats <- function(nba_data, columnNames = c(),  playerNames = c(), teamNames = c(), positions = c(), all = FALSE) {

  # Test inputs
  if (!is.data.frame(nba_data)) {
    warning('Data is not in correct format. A dataframe was expected by the function')
    return
  }
  if (!is.logical(all)) {
    warning('Function expects argument all to be a boolean.')
  }

  # Filter data
  if (all == FALSE) {
    if (length(playerNames) != 0) {
      nba_data <- nba_data[nba_data$PLAYER %in% playerNames, ]
    }

    if (length(teamNames) != 0) {
      nba_data <- nba_data[nba_data$Team %in% teamNames, ]
    }

    if (length(positions) != 0) {
      nba_data <- nba_data[nba_data$POS %in% positions, ]
    }
  }

  # Select columns
  if (length(columnNames) != 0) {
    columnNames <- c('PLAYER', 'Team', 'POS',columnNames)
    nba_data <- nba_data[columnNames]
  }

  # Generate summary
  nba_summary <-
    nba_data %>% group_by(Team, POS) %>%
    summarise_if(.predicate = function(x) is.numeric(x),
                 list(~ mean(., na.rm = TRUE, trim = .2), ~ median(., na.rm = TRUE), ~ quantile(., probs = 0.25, na.rm = TRUE), ~ quantile(., probs = 0.75, na.rm = TRUE)))

  nba_summary

}
