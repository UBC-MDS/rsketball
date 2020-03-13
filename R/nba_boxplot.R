#' Creating Boxplot for NBA Stats
#'
#'  Creates a boxplot of the categorical variable of interest on the y-axis and
#'  the stat of interest on the x-axis.
#'
#' @param dataset  dataframe
#' This dataframe is created after using the nba_scraper.R function or if
#' the csv has already been loaded, read the csv in and pass it as the parameter.
#' @param position string
#' To specify as "POS" if you want to look at Position boxplot.
#' @param teams string
#' The names of teams you want to create boxplot for.
#' *Note you can only choose one of position or teams as a variable to vizualize
#' @param stats string
#' The numeric variable of interest examples:  Points, 3_Pointers, Turnovers
#'
#' @return ggplot boxplot
#' @export
#' @import ggplot2
#' @importFrom magrittr %>%
#' @importFrom stats median
#' @importFrom stats reorder
#'
#' @examples
#' nba_2018 <- data.frame(POS= c("C", "FOR", "PO","FOR", "C"), Team = c("ORL", "UTAH", "LAC", "MIN", "BOS"), GP = c(3, 5, 5, 2, 1))
#' nba_boxplot(nba_2018, position= "POS", teams= NULL, stats= "GP")
#'
nba_boxplot <- function(dataset, position= NULL, teams= c(NULL), stats) {


  #Exception Handling

  #check that position and teams are not used at the same time and both cannot be empty
  if(is.null(position) & is.null(teams)){
    stop("Error: Choose one of position/teams input to use for boxplot.")
  }
  if(!is.null(position) & !is.null(teams)){
    stop("Error: Cannot use both position and teams inputs, choose input one.")
  }

  #check if columns are in the dataframe
  if(!stats %in% colnames(dataset)){
    stop("Error: The column called in stats argument is not in the dataset.")
  }

  #columns are of correct type
  if(!is.numeric(dataset[[stats]])){
    stop("Error: stats must take in a numerical column")}

  #filter for Teams selected
  if(length(teams) != 0) {
    dataset <- dataset[dataset$Team %in% teams, ]
  }


  #Plotting for Teams and a stat

  if(is.null(position) & !is.null(teams)) {
    dataset[['Team']] <- reorder(dataset[['Team']], dataset[[stats]], FUN=median)

    team_boxplot <- dataset %>%
      ggplot(aes_string(x= 'Team', y= stats)) +
      geom_boxplot() +
      coord_flip() +
      ggtitle(paste("Plot for Teams and", stats, "in Dataset", sep = " "))

    return(team_boxplot)
  }


  #Plotting for position and a stat

  if(is.null(teams) & !is.null(position)) {
    dataset[[position]] <- reorder(dataset[[position]], dataset[[stats]], FUN=median)

    position_boxplot <- dataset %>%
      ggplot(aes_string(x=position, y= stats)) +
      geom_boxplot() +
      coord_flip() +
      ggtitle(paste("Plot for Positions and", stats, "in Dataset", sep = " "))

    return(position_boxplot)
  }
}
