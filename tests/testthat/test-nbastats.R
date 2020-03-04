#' Generate summary stats for NBA players
#'
#' @description
#' The function filters the dataset further using the arguments provided and
#' produces a tibble with summary statistics for a list of columns of a few players or teams.
#' The function can only use one of the two filters - playerNames and teamNames. Hence, If playerNames
#' are provided, teamNames are ignored.
#'
#' @param data tibble
#' @return A tibble
#' @export
#'
#' @importFrom readr read_csv
#' @import dplyr
#'
#' @examples
#' test_nbastats(nba_data)
#'
test_nbastats <- function(data) {

  test_that("nbastats returns a data frame when all is FALSE", {
    expect_equal(
      is.data.frame(nbastats(data, columnNames = c('GP', '3PM', 'FT%'),teamNames = c('UTAH', 'PHX', 'DET'), positions = c('C'), all = FALSE)),
      TRUE
    )
  })

  test_that("nbastats returns a data frame when all iS TRUE", {
    expect_equal(
      is.data.frame(nbastats(data, columnNames = c('GP', '3PM', 'FT%'),teamNames = c('UTAH', 'PHX', 'DET'), positions = c('C'), all = TRUE)),
      TRUE
    )
  })

  test_that("nbastats throws WARNING when all is not boolean", {
    expect_error(
      is.data.frame(nbastats(data, columnNames = c('GP', '3PM', 'FT%'),teamNames = c('UTAH', 'PHX', 'DET'), positions = c('C'), all = 5))
    )
  })

  test_that("nbastats returns data is not a dataframe", {
    expect_error(
      is.data.frame(nbastats('data', columnNames = c('GP', '3PM', 'FT%'),teamNames = c('UTAH', 'PHX', 'DET'), positions = c('C'), all = TRUE))
    )
  })

}


data = read_csv("https://raw.githubusercontent.com/kfoofw/nba_espn/master/0.data/NBA_reg_2018-2019.csv")
test_nbastats(data)
