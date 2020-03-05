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
#'


test_nbastats <- function(data) {

  test_that("Function does not return a dataframe when all is FALSE and playerNames and positions are provided.", {
    expect_equal(
      is.data.frame(nbastats(data, playerNames = c('Stephen Curry', 'Paul George'), positions = c('C'), all = FALSE)),
      TRUE
    )
  })

  test_that("Function does not return a dataframe when all is FALSE and teamNames and positions are provided.", {
    expect_equal(
      is.data.frame(nbastats(data, teamNames = c('UTAH', 'PHX', 'DET'), positions = c('C'), all = FALSE)),
      TRUE
    )
  })

  test_that("Function does not return a dataframe when all is TRUE and columnNames are provided.", {
    expect_equal(
      is.data.frame(nbastats(data, columnNames = c('GP', '3PM', 'FT%'), all = TRUE)),
      TRUE
    )
  })
  test_that("Function does not throw an error when a random value is passed to all.", {
    expect_error(
      is.data.frame(nbastats(data, columnNames = c('GP', '3PM', 'FT%'), all = 4))
    )
  })

  test_that("Function does not throw an error when a proper dataframe is passed in data.", {
    expect_error(
      is.data.frame(nbastats('random values', columnNames = c('GP', '3PM', 'FT%'), all = FALSE))
    )
  })
}

data = read_csv("https://raw.githubusercontent.com/kfoofw/nba_espn/master/0.data/NBA_reg_2018-2019.csv")
test_nbastats(data)
