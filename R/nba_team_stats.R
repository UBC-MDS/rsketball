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
#' @importFrom tibble tibble
#'
#' @examples
#' nba_data <- tibble::tibble(NAME = c("James", "Steph", "Bosh", "Klay", "Kobe"),
#'                           TEAM = c("MIA","MIA","MIA","GS","GS"),
#'                           POS = c("SF", "PG", "C", "C", "PG"),
#'                           PTS = c(5,4,3,2,10),
#'                           TO = c(1,2,3,4,3))
#'
#' # Find descriptive stats for all teams without position
#' nba_team_stats(nba_data)
#'
#' # Find specific stats (PTS, TO) for specific teams (GS, MIA) for specific positions (PG, C)
#' nba_team_stats(nba_data, stats_filter = c("PTS","TO"),
#'                teams_filter = c("GS","MIA"), positions_filter = c("C","PG"))
#'
#' # Find specific stats (PTS, TO) for specific teams (GS) without positions_filter inputs
#' nba_team_stats(nba_data, stats_filter = c("PTS","TO"), teams_filter = c("GS"))
#'
#' # Find specific stats (PTS, TO) for all individual teams (unspecified)
#' # for specific positions (PG, C)
#' nba_team_stats(nba_data, stats_filter = c("PTS","TO"), positions_filter = c("PG"))
#
nba_team_stats <- function(nba_data, stats_filter = c(), teams_filter = c(), positions_filter = c()) {

  # Test inputs
  if (!is.data.frame(nba_data)) {
    stop('Data is not in correct format. A dataframe/tibble was expected by the function')
  }

  # Filter data on teams
  if (length(teams_filter) != 0) {
    nba_data <- nba_data[nba_data$TEAM %in% teams_filter, ]
  }
  # Filter data on positions
  if (length(positions_filter) != 0) {
    nba_data <- nba_data[nba_data$POS %in% positions_filter, ]
  }

  # Select stats to include
  if (length(stats_filter) != 0) {
    filtered_columns <- c('TEAM', 'POS', stats_filter)
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
      nba_data %>% group_by(TEAM) %>% # Note Team grouping only
      summarise_if(.predicate = function(x) is.numeric(x),
                   list(~ mean(., na.rm = TRUE, trim = .2),
                        ~ median(., na.rm = TRUE),
                        ~ quantile(., probs = 0.25, na.rm = TRUE),
                        ~ quantile(., probs = 0.75, na.rm = TRUE)))
  }
  # If position is not null, group_by Teams and Specified positions
  else {
    nba_summary <-
      nba_data %>% group_by(TEAM, POS) %>% # Difference in grouping here
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
