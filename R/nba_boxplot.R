#' Creating Boxplot for NBA Stats
#'
#'  Creates a boxplot of the categorical variable of interest on the y-axis and
#'  the stat of interest on the x-axis.
#'
#' @param nba_data  tibble
#' This dataframe is created after using the nba_scraper.R function or if
#' the csv has already been loaded, read the csv in and pass it as the parameter.
#' @param team_or_position string
#' To specify if you want a boxplot for either "team" or "position" grouping
#' @param grouping_list string
#' The grouping list for either "team" or "position".
#' For "team", grouping_list can be c("HOU","GS")
#' For "position, grouping_list can be c("PG","C").
#' If grouping_list is empty, function will return boxplot of all groups in
#' either "team" or "position".
#' @param stats_column non-string character input as per column name
#' The numeric variable of interest in the columns of the scraped NBA dataframe.
#' Examples:  PTS (Points), 3PM (3 Pointers Made), FT\% (Free Throw Percent).
#' Note that if a column name starts with a number (eg 3PA) or
#' has \% in it (eg 3P\%, FT\%), format it with backticks. See vignette for examples.
#'
#' @return ggplot boxplot
#' @export
#' @import ggplot2
#' @importFrom magrittr %>%
#' @importFrom rlang !!
#' @importFrom stats median
#' @importFrom forcats fct_reorder
#' @importFrom tibble tibble
#'
#' @examples
#' nba_data <- tibble::tibble(NAME = c("James", "Steph", "Bosh", "Klay", "Kobe"),
#'                            TEAM = c("MIA","GS","MIA","GS","LAL"),
#'                            POS = c("SF", "PG", "C", "SG", "SG"),
#'                            PTS = c(5,4,3,2,10),
#'                            TO = c(1,2,3,4,3))
#'
#' # Boxplot of all player positions based on PTS
#' nba_boxplot(nba_data, team_or_position = "position",
#'             grouping_list = NULL, stats_column = PTS)
#'
#' # Boxplot of specific player positions based on TO
#' nba_boxplot(nba_data, team_or_position = "position",
#'             grouping_list = c("C","PG","SG"), stats_column = TO)
#'
#' # Boxplot of all teams based on PTS
#' nba_boxplot(nba_data, team_or_position = "team",
#'             grouping_list = NULL, stats_column = PTS)
#'
#' # Boxplot of specific teams based on TO
#' nba_boxplot(nba_data, team_or_position = "team",
#'             grouping_list = c("MIA","GS"), stats_column = TO)
#'
nba_boxplot <- function(nba_data, team_or_position= "position", grouping_list = c(NULL), stats_column) {

  stats_quo <- enquo(stats_column)

  #Exception Handling
  #check if columns are in the dataframe
  if(!(rlang::as_name(stats_quo) %in% colnames(nba_data))){
    stop("Error: The column called in stats_column argument is not in the dataset.")
  }

  #columns are of correct type
  if(!is.numeric(nba_data %>%
                 select(!!stats_quo) %>%
                 pull())){
    stop("Error: stats_column must take in a numerical column")
  }

  # Check if team_or_position are specified correctly
  if((team_or_position != "position") &(team_or_position != "team")){
    stop("Error: team_or_position input must be either 'team' or 'position'")
  }

  # If "team" selected
  if(team_or_position == "team") {

    # If grouping list is not empty, filter dataset
    if (length(grouping_list) != 0) {
      nba_data <- nba_data %>%
        filter(TEAM %in% grouping_list)
    }

    # Reorder team for boxplot
    nba_data <- nba_data %>%
      select(TEAM, !!stats_quo) %>%
      mutate(TEAM = fct_reorder(TEAM, !!stats_quo, .fun = 'median'))

    # Boxplot
    boxplot <- nba_data %>%
      ggplot(aes(x= TEAM, y= !!stats_quo)) +
      geom_boxplot() +
      coord_flip() +
      ggtitle(paste("Plot for TEAMs and", rlang::as_name(stats_quo), "in Dataset", sep = " "))

    return(boxplot)
  }

  # If "position" selected
  if(team_or_position == "position") {

    # grouping_list is not empty
    if (length(grouping_list) != 0) {
      nba_data <- nba_data %>%
        filter(POS %in% grouping_list)
    }

    # Reorder pos for boxplot
    nba_data <- nba_data %>%
      select(POS, !!stats_quo) %>%
      mutate(POS = fct_reorder(POS, !!stats_quo, .fun = 'median'))

    # Boxplot
    boxplot <- nba_data %>%
      ggplot(aes(x= POS, y= !!stats_quo)) +
      geom_boxplot() +
      coord_flip() +
      ggtitle(paste("Plot for POS and", rlang::as_name(stats_quo), "in Dataset", sep = " "))

    return(boxplot)
  }
}
