#' Scrape NBA statistics data from ESPN into CSV file
#'
#' Scrape the tabular data from ESPN NBA website into a csv file.
#'
#' @param season_year string
#' @param season_type string
#' @param csv_path string
#' @param nba_data_env_name string
#'
#' @examples
#' # Regular season for 2018/2019 while assigning it into global env as an object called nba_2018
#' nba_scraper(season_year = "2018", season_type = "regular", csv_path = "~/nba_scraped_data/nba_2018_reg.csv",  nba_data_env_name = "nba_2018")
#'
#' # Playoff season for 2016/2017 without assigning it into global env as an object called nba_2018
#' nba_scraper(season_year = "2016", season_type = "playoff", csv_path = "~/nba_scraped_data/nba_2018_reg.csv",  nba_data_env_name = NULL)
nba_scraper <- function(season_year, season_type, csv_path, nba_data_env_name = NULL){

  # If nba_data_env_name is given, create a dataframe by reading it
  if (!is.null(nba_data_env_name)) {
    assign(nba_data_env_name, read.csv(csv_path), envir = .GlobalEnv)
  }
}
