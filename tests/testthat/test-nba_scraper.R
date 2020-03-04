#' Test function for rysketball::nba_scraper() function
#'
#' Test inputs edge cases for nba_scraper.
#'
test_nba_scraper <- function() {
  # Test season_year input
  test_that("season_year below value of 2001 will throw an error", {
    expect_error(nba_scraper(season_year = 1999,
                             season_type = "playoffs", "nba_2017_playoffs.csv",
                             port=4445L, sel_browser = "firefox",
                             nba_data_env_name = NULL))
  })

  # Test season_type input
  test_that("season_type that is not 'regular' or 'playoffs' will give an error", {
    expect_error(nba_scraper(season_year = 1999,
                             season_type = "hello",
                             "nba_2017_playoffs.csv",
                             port=4445L, sel_browser = "firefox",
                             nba_data_env_name = NULL))
  })

  # Test csv_path input
  test_that("csv_path string input that does not end in '.csv' will give an error", {
    expect_error(nba_scraper(season_year = 1999,
                             season_type = "hello",
                             csv_path = "nba_2017_playoffs",
                             port=4445L, sel_browser = "firefox",
                             nba_data_env_name = NULL))
  })

  # Test port input
  test_that("port input without L suffix will give an error", {
    expect_error(nba_scraper(season_year = 1999,season_type = "hello",
                             csv_path = "nba_2017_playoffs",
                             port=4445,
                             sel_browser = "firefox",
                             nba_data_env_name = NULL))
  })

  #* Test port input
  test_that("port input of a negative number will give an error", {
    expect_error(nba_scraper(season_year = 1999,season_type = "hello",
                             csv_path = "nba_2017_playoffs",
                             port=-4445L,
                             sel_browser = "firefox",
                             nba_data_env_name = NULL))
  })

  #* Test selenium browser choice input
  test_that("sel_browser input that is not 'chrome' or 'firefox' will give error", {
    expect_error(nba_scraper(season_year = 1999,season_type = "hello",
                             csv_path = "nba_2017_playoffs",
                             port=-4445L,
                             sel_browser = "firefox",
                             nba_data_env_name = NULL))
  })

  #* Test selenium browser choice input
  test_that("nba_data_env_name input must be a string input if not NULL", {
    expect_error(nba_scraper(season_year = 1999,season_type = "hello",
                             csv_path = "nba_2017_playoffs",
                             port=-4445L,
                             sel_browser = "firefox",
                             nba_data_env_name = 123))
  })
}
