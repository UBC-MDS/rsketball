#' Test function for rsketball::nba_scraper() function
#'
#' @description
#' Test inputs edge cases for nba_scraper.
#'
#' @export
#'
#' @importFrom readr read_csv
#' @import dplyr
#'
#' @examples
#' # Before executing, please setup the Docker container as per instructions
#' # in Readme.md located in "tests" subfolder.
#' test_nba_scraper()
test_nba_scraper <- function() {
  # Test season_year input that is wrong format
  test_that("season_year that is not an integer will throw an error", {
    expect_error(nba_scraper(season_year = 2003.5,
                             season_type = "postseason",
                             port=4445L,
                             csv_path = NULL))
  })

  # Test season_year input that is out of range
  test_that("season_year below value of 2001 will throw an error", {
    expect_error(nba_scraper(season_year = 1999,
                             season_type = "postseason",
                             port=4445L,
                             csv_path = NULL))
  })

  # Test season_type input
  test_that("season_type that is not 'regular' or 'postseason' will give an error", {
    expect_error(nba_scraper(season_year = 2018,
                             season_type = "hello",
                             port=4445L,
                             csv_path = NULL))
  })

  # Test port input
  test_that("port input without L suffix will give an error", {
    expect_error(nba_scraper(season_year = 2018, season_type = "regular",
                             port=4445,
                             csv_path = "nba_2017_playoffs.csv"))
  })

  #* Test port input
  test_that("port input of a negative number will give an error", {
    expect_error(nba_scraper(season_year = 2018, season_type = "regular",
                             port=-4445L,
                             csv_path = "nba_2017_playoffs.csv"))
  })

  # Test csv_path input
  test_that("csv_path string input that does not end in '.csv' will give an error", {
    expect_error(nba_scraper(season_year = 2018,
                             season_type = "regular",
                             port=4445L,
                             csv_path = "nba_2017_playoffs"))
  })

  # Run scraper without storing output csv file
  nba_2017 <- nba_scraper(season_year = 2017, season_type = "postseason",
                          port=4445L)

  # Test that completed scraping returns a dataframe
  test_that("Function does not return a data frame upon completion of scraping", {
    expect_true(is.data.frame(nba_2017) ==1)
  })

  # Run scraper and store output as nba_2018 tibble
  nba_2018 <- nba_scraper(season_year = 2018, season_type = "postseason",
                          port=4445L,
                          csv_path = "nba_2018_playoffs.csv")

  # Read in csv as tibble for testing
  nba_2018_read_csv <- as_tibble(read.csv("nba_2018_playoffs.csv"))

  # Test that completed scraping returns a dataframe
  test_that("Function does not return a data frame upon completion of scraping", {
    expect_true(sum(dim(nba_2018) == dim(nba_2018_read_csv)) ==2 )
  })
}

test_nba_scraper()
