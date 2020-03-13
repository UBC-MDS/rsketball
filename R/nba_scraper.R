#' Scrape NBA statistics data from ESPN into CSV file
#'
#' Scrape the tabular data from ESPN NBA website using RSelenium and returns a tibble
#' of the data. Users can specify the seaason year and season type. User should also
#' specify the port for Selenium driver. By default, the function will not write
#' to csv until a string input for "csv_path" is given.
#'
#' @param season_year int from 2001 to 2019 (upper limit based on latest year)
#' @param season_type string. Either "regular" or "postseason"
#' @param port int with L suffix. Must not be negative.
#' @param csv_path string for csv file. Defaults to NULL. If specified, must end with ".csv".
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom utils write.csv
#' @importFrom rvest html_nodes
#' @importFrom rvest html_text
#' @importFrom tibble as_tibble
#'
#' @return A tibble of scraped ESPN NBA data
#'
#' @examples
#' \donttest{
#' # Scrape regular season 2018/19 without saving to a csv file
#' nba_2018 <- nba_scraper(2018, season_type = "regular", port=4445L)
#'
#'
#' # Scrape playoffs season 2017/18 while saving to a local csv file.
#' nba_2017 <- nba_scraper(2017, season_type = "postseason",
#'                         port=4445L,
#'                         csv_path = "nba_2017_playoffs.csv")
#'}
nba_scraper <- function(season_year = 2018, season_type = "regular", port=4445L, csv_path = NULL) {

  # Check season_year is integer
  if ((season_year - round(season_year)) != 0){
    stop("'season_year' must be an integer and not a float")
  }

  # Check season_year is within range
  if ((season_year < 2001) | (season_year > 2019)){
    stop("'season_year' must be an integer between 2001 to 2019")
  }

  # Check season_type is not "regular" or "playoffs"
  if ((season_type != "postseason") & (season_type != "regular")){
    stop("'season_type' must be either 'regular' or 'postseason'")
  }

  # Check port must end with L suffix and must be positive integer
  if ((!is.integer(port)) | (port <0)){
    stop("'port' must be a positive integer ending with L suffix (eg 4445L)")
  }

  # Check csv_path does not end with csv
  if (!is.null(csv_path)) {
    if (substr(csv_path, nchar(csv_path)-3, nchar(csv_path)) != ".csv"){
      stop("Input 'csv_path' must be end with '.csv' if it is specified.")
    }
  }

  # Create url
  url_season_year <- season_year +1
  url_season_type <- ifelse(season_type == "regular", 2, 3)
  url <-paste0("https://www.espn.com/nba/stats/player/_/season/", url_season_year, "/seasontype/", url_season_type)

  # Set up parameters for remoteDriver
  remDr <- RSelenium::remoteDriver(
    remoteServerAddr = "localhost",
    port = port,
    browserName = "chrome"
  )

  # Print message
  print("Scraping commencing. Please wait!")

  # Initiate Driver
  remDr$open()

  # Use driver to browse the url
  remDr$navigate(url)

  # Dynamic loading on webpage to load all the data
  chk <- FALSE
  while (!chk) {
    res <- try({
      suppressMessages(showmore_button <- remDr$findElement(using = "xpath", "//*[@id='fittPageContainer']/div[3]/div[1]/div/section/div/div[3]/div/a"))
    }, silent = TRUE)
    if (class(res) == "try-error") {
      chk <- TRUE
    } else {
      showmore_button$clickElement()
      # Sleep for 2 seconds for the page to load
      Sys.sleep(5L)
    }
  }

  # Compilation List of plyr XML paths
  ply_trs <- remDr$getPageSource()[[1]] %>%
    xml2::read_html() %>%
    rvest::html_nodes(xpath="//*[@id='fittPageContainer']/div[3]/div[1]/div/section/div/div[3]/section/div[2]/table/tbody/tr")

  stats_trs <- remDr$getPageSource()[[1]] %>%
    xml2::read_html() %>%
    rvest::html_nodes(xpath="//*[@id='fittPageContainer']/div[3]/div[1]/div/section/div/div[3]/section/div[2]/div/div[2]/table/tbody/tr")

  # Close Selenium driver
  remDr$close()

  # Initiate compilation df
  compiled_df <- data.frame(matrix(, nrow=length(ply_trs), ncol=23))
  colnames(compiled_df) <- c("Name", "Team", "Position",
                             "GP", "MIN", "PTS", "FGM", "FGA", "FG%", "3PM", "3PA",
                             "3P%", "FTM", "FTA", "FT%", "REB", "AST", "STL", "BLK",
                             "TO", "DD2", "TD3", "PER")

  # Extracting information from scraped html
  for (i in seq(length(ply_trs))) {
    # From player
    player <- ply_trs[i]
    # player: Player name
    player_name <- player %>%
      html_nodes("[class='AnchorLink']") %>%
      html_text()
    # player: Team name
    player_team <- player %>%
      html_nodes("[class='pl2 n10 athleteCell__teamAbbrev']") %>%
      html_text()

    # From stats
    stats = stats_trs[i]
    # stats: list of all stats
    stats_list <- stats %>%
      html_nodes("td") %>%
      html_text()

    # Input into compiled df
    compiled_df[i,1] <- player_name
    compiled_df[i,2] <- player_team
    compiled_df[i,3:23] <- stats_list
  }

  # Convert scraped numeric data from character to numeric
  numeric_columns <- c("GP", "MIN", "PTS", "FGM", "FGA", "FG%", "3PM", "3PA",
                       "3P%", "FTM", "FTA", "FT%", "REB", "AST", "STL", "BLK",
                       "TO", "DD2", "TD3", "PER")

  compiled_df[, numeric_columns] <- lapply(compiled_df[, numeric_columns, drop = FALSE], function(x) as.numeric(as.character(x)))

  # If csv_path is given
  if (!is.null(csv_path)) {
    # Write to csv
    write.csv(compiled_df, csv_path, row.names = FALSE)
  }

  print(paste0("Data scraping of ", season_year," ", season_type," season completed."))

  return(as_tibble(compiled_df))
}
