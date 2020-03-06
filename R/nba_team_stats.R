#' Generate summary stats for NBA players
#'
#' @description
#' The function provides descriptive (mean, median, 25%, and 75% quantiles) team statistics of NBA data. Users can specify which
#' statistic of interest (3PA, 3PM, etc) along with teams of interest (GS, HOU, etc). If positions of interest (C, PG, etc)
#' are specified, the returned tibble depicts relevant descriptive statistics for the relevant positions in the relevant teams.
#'
#' @param nba_data tibble of scraped ESPN NBA data
#' @param stats_filter character vector
#' @param teams_filter character vector
#' @param positions_filter character vector
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
#' # Find descriptive stats for all teams without position
#' nba_team_stats(nba_data)
#'
#' # Find specific stats (3PM, 3PA) for specific teams (GS, HOU) for specific positions (PG, C)
#' nba_team_stats(nba_data, stats_filter = c("3PM","3PA"), teams_filter = c("GSW","HOU"), positions_filter = c("C","PG"))
#'
#' # Find specific stats (3PM, 3PA) for specific teams (GS, HOU) without positions_filter inputs
#' nba_team_stats(nba_data, stats_filter = c("3PM","3PA"), teams_filter = c("GS","HOU"))
#'
#' # Find specific stats (3PM, 3PA) for all teams (unspecified) for specific positions (PG, C)
#' nba_team_stats(nba_data, stats_filter = c("3PM","3PA"), positions_filter = c("C","PG"))
#' }
nba_team_stats <- function(nba_data, stats_filter = c(), teams_filter = c(), positions_filter = c()) {

  # Test inputs
  if (!is.data.frame(nba_data)) {
    stop('Data is not in correct format. A dataframe was expected by the function')
  }

  # Filter data on teams
  if (length(teams_filter) != 0) {
    nba_data <- nba_data[nba_data$Team %in% teams_filter, ]
  }
  # Filter data on positions
  if (length(positions_filter) != 0) {
    nba_data <- nba_data[nba_data$POS %in% positions_filter, ]
  }

  # Select stats to include
  if (length(stats_filter) != 0) {
    filtered_columns <- c('Team', 'POS', stats_filter)
    nba_data <- nba_data[filtered_columns]
  }
  # If all inputs (stats, teams, and positions) are NULL, show all
  if ((is.null(stats_filter)) & (is.null(teams_filter)) & (is.null(positions_filter))) {
    nba_data <- nba_data
  }
  # Generate summary
  # If position is null, only group_by Teams
  if (is.null(positions_filter)) {
    nba_summary <-
      nba_data %>% group_by(Team) %>% # Note Team grouping only
      summarise_if(.predicate = function(x) is.numeric(x),
                   list(~ mean(., na.rm = TRUE, trim = .2),
                        ~ median(., na.rm = TRUE),
                        ~ quantile(., probs = 0.25, na.rm = TRUE),
                        ~ quantile(., probs = 0.75, na.rm = TRUE)))
  }
  # If position is not null, group_by Teams and Specified positions
  else {
    nba_summary <-
      nba_data %>% group_by(Team, POS) %>% # Difference in grouping here
      summarise_if(.predicate = function(x) is.numeric(x),
                   list(~ mean(., na.rm = TRUE, trim = .2),
                        ~ median(., na.rm = TRUE),
                        ~ quantile(., probs = 0.25, na.rm = TRUE),
                        ~ quantile(., probs = 0.75, na.rm = TRUE)))
  }

  colnames(nba_summary) <- gsub("quantile..3", "quantile_25", colnames(nba_summary))
  colnames(nba_summary) <- gsub("quantile..4", "quantile_75", colnames(nba_summary))

  nba_summary
}
