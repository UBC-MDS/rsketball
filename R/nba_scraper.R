#' Scrape NBA statistics data from ESPN into CSV file
#'
#' Scrape the tabular data from ESPN NBA website into a csv file using RSelenium.
#'
#' @param season_year int from 2001 to 2019 (upper limit based on latest year)
#' @param season_type string
#' @param csv_path string
#' @param port int with L suffix
#' @param sel_browser string
#' @param nba_data_env_name string
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom utils write.csv
#' @importFrom rvest html_nodes
#' @importFrom rvest html_text
#'
#' @examples
#' # Scrape regular season 2018/19 using "chrome" driver and
#' # create "nba_2018_regular" tibble in Global Env
#' \dontrun{
#' #' nba_scraper(2018, season_type = "regular", "nba_2018.csv",
#'             port=4445L, sel_browser = "chrome",
#'             nba_data_env_name = "nba_2018_regular")
#' }
#'
#' # Scrape playoffs season 2017/18 using "firefox" driver without
#' # creating tibble in Global Env
#' \dontrun{
#' nba_scraper(2017, season_type = "playoffs", "nba_2017_playoffs.csv",
#'             port=4445L, sel_browser = "firefox",
#'             nba_data_env_name = NULL)
#' }
nba_scraper <- function(season_year = 2018, season_type = "regular", csv_path = "nba_2018.csv", port=4445L, sel_browser = "chrome", nba_data_env_name = NULL) {

  # Check season_year is not integer or not within range
  if ((round(season_year) != season_year) & (season_year < 2001) & (season_year > 2019)){
    stop("'season_year' must be an integer between 2001 to 2019")
  }

  # Check season_type is not "regular" or "playoffs"
  if ((season_type != "playoffs") & (season_type != "regular")){
    stop("'season_type' must be either 'regular' or 'playoffs'")
  }

  # Check csv_path does not end with csv
  if (substr(csv_path, nchar(csv_path)-3, nchar(csv_path)) != ".csv"){
    stop("'csv_path' must be end with '.csv'")
  }

  # Check port must end with L suffix and must be positive
  if ((!is.integer(port)) & (port <0)){
    stop("'port' must be a positive integer ending with L suffix (eg 4445L)")
  }

  # Check sel_browser must be either "chrome" or "firefox"
  if ((sel_browser != "chrome") & (sel_browser != "firefox")) {
    stop("'sel_browser' must be either 'chrome' or 'firefox'")
  }

  # Check nba_data_env_name is string
  if ((!is.null(nba_data_env_name)) & (!assertthat::is.string(nba_data_env_name))) {
    stop("'nba_data_env_name' must be a string if it is not a NULL input")
  }

  # Create url
  url_season_year <- season_year +1
  url_season_type <- ifelse(season_type == "regular", 2, 3)
  url <-paste0("https://www.espn.com/nba/stats/player/_/season/", url_season_year, "/seasontype/", url_season_type)

  # Set up parameters for remoteDriver
  remDr <- RSelenium::remoteDriver(
    remoteServerAddr = "localhost",
    port = port,
    browserName = sel_browser
  )

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
      Sys.sleep(2L)
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
  # Write to csv
  write.csv(compiled_df, csv_path)

  # If nba_data_env_name is given, create a tibble in Global Env
  if (!is.null(nba_data_env_name)) {
    assign(nba_data_env_name, tibble::tibble(compiled_df), envir = .GlobalEnv)
  }
  print("Data scraping completed")
}
