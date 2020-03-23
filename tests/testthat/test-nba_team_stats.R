#' Test function for nba_team_stats
#'
#' @description
#' The function tests the nbastats function with various ranges of values to ensure the functionality is not broken
#'
#' @param data tibble
#' @export
#'
#' @importFrom readr read_csv
#' @import dplyr
#'
#' @examples
#' test_nba_team_stats(nba_data)
test_nba_team_stats <- function(nba_data) {
  # Sample test inputs
  sample_stats_filter = c('PTS', 'TO')
  sample_teams_filter = c('GS', 'MIA')
  sample_positions_filter = c('C', 'PG')

  test_that("Function does not throw an error when an improper dataframe is passed in data.", {
    expect_error(
      is.data.frame(nba_team_stats('random values', teams_filter = sample_teams_filter))
    )
  })

  test_that("Function does not return a dataframe when `teams_filter` and `positions_filter` are provided.", {
    expect_equal(
      is.data.frame(nba_team_stats(nba_data, teams_filter = sample_teams_filter , positions_filter = sample_positions_filter)),
      TRUE
    )
  })

  test_that("Function does not return rows of all teams when there is no input for `teams_filter`, `positions_filter` and `stats_filter`.", {
    expect_equal(
      nrow(nba_team_stats(nba_data)),
      length(unique(nba_data$TEAM))
    )
  })

  test_that("Function does not return rows of all teams when there is no input for `teams_filter` and `positions_filter`.", {
    expect_equal(
      nrow(nba_team_stats(nba_data, stats_filter = sample_stats_filter)),
      length(unique(nba_data$TEAM))
    )
  })
  test_that("Function does not return the correct combination of (team and position) rows when there is input for `teams_filter` and `positions_filter`", {
    expect_equal(
      nrow(nba_team_stats(nba_data, teams_filter = sample_teams_filter, positions_filter = sample_positions_filter)),
      length(sample_teams_filter)*length(sample_positions_filter)
    )
  })
  test_that("Function does not return correct number of descriptive stats columns when `stats_filter` is given without `positions_filter`", {
    expect_equal(
      ncol(nba_team_stats(nba_data, stats_filter = sample_stats_filter)),
      length(sample_stats_filter)*4 + 1
    )
  })
  test_that("Function does not return correct number of descriptive stats columns when `stats_filter` and `positions_filter` are given", {
    expect_equal(
      ncol(nba_team_stats(nba_data, stats_filter = sample_stats_filter, positions_filter = sample_positions_filter)),
      length(sample_stats_filter)*4 + 2
    )
  })



  test_that("Partially wrong inputs in teams_filter will raise warning", {
    expect_warning(nba_team_stats(nba_data,
                                  stats_filter = c("PTS","TOR"),
                                  teams_filter = c("GS","HELLO"),
                                  positions_filter = c("C","PG"))
    )
    expect_equal(nrow(nba_team_stats(nba_data,
                                     stats_filter = c("PTS","TOR"),
                                     teams_filter = c("GS","HELLO"),
                                     positions_filter = c("C","PG"))),
                 2
    )
  })

  test_that("Fully wrong inputs in teams_filter will raise error", {
    expect_error(nba_team_stats(nba_data,
                                  stats_filter = c("PTS"),
                                  teams_filter = c("HELLO"),
                                  positions_filter = c("C","PG"))
    )
  })

  test_that("Partial wrong inputs in positions_filter will raise warning", {
    expect_warning(nba_team_stats(nba_data,
                                  stats_filter = c("PTS","TOR"),
                                  teams_filter = c("GS","MIA"),
                                  positions_filter = c("C","PG","POW", "CLA"))
    )
    expect_equal(nrow(nba_team_stats(nba_data,
                                  stats_filter = c("PTS","TOR"),
                                  teams_filter = c("GS","MIA"),
                                  positions_filter = c("C","PG","POW", "CLA"))),
                 4
    )
  })

  test_that("Fully wrong inputs in positions_filter will raise error", {
    expect_error(nba_team_stats(nba_data,
                                  stats_filter = c("PTS","TOR"),
                                  teams_filter = c("GS","MIA"),
                                  positions_filter = c("POW", "CLA"))
    )
  })

  test_that("Partially wrong inputs in stats_filter will raise warning", {
    expect_warning(nba_team_stats(nba_data,
                                  stats_filter = c("PTS","Chicken"),
                                  teams_filter = c("GS","MIA"),
                                  positions_filter = c("C","PG","POW", "CLA"))
    )
    expect_equal(ncol(nba_team_stats(nba_data,
                                     stats_filter = c("PTS","Chicken"),
                                     teams_filter = c("GS","MIA"),
                                     positions_filter = c("C","PG","POW", "CLA"))),
                 6
    )
  })

  test_that("Fully wrong inputs in stats_filter will raise error", {
    expect_error(nba_team_stats(nba_data,
                                  stats_filter = c("Chicken"),
                                  teams_filter = c("GS","MIA"),
                                  positions_filter = c("C","PG","POW", "CLA"))
    )
  })

}

nba_data <- tibble::tibble(NAME = c("James", "Steph", "Bosh", "Klay", "Kobe"),
                           TEAM = c("MIA","MIA","MIA","GS","GS"),
                           POS = c("SF", "PG", "C", "C", "PG"),
                           PTS = c(5,4,3,2,10),
                           TO = c(1,2,3,4,3))
test_nba_team_stats(nba_data)

