#' Test function for nbastats
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
#' test_nbastats(nba_data)
test_nba_team_stats <- function(nba_data) {
  # Sample test inputs
  sample_stats_filter = c('GP', '3PM', 'FT%')
  sample_teams_filter = c('UTAH', 'DET')
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
      length(unique(nba_data$Team))
    )
  })

  test_that("Function does not return rows of all teams when there is no input for `teams_filter` and `positions_filter`.", {
    expect_equal(
      nrow(nba_team_stats(nba_data, stats_filter = sample_stats_filter)),
      length(unique(nba_data$Team))
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
}

nba_data = read_csv("https://raw.githubusercontent.com/kfoofw/nba_espn/master/0.data/NBA_reg_2018-2019.csv")
test_nba_team_stats(nba_data)
