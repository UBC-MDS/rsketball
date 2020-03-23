#' Generates a ranking and a visualization based on ESPN NBA data
#'
#' @description
#' This function generates creates a ranking of a variable `column` summarizing by a variable `by` using a function `FUN`. The result of this function is
#' a visualization with that information.
#'
#' For reference on the scraped data columns information, please refer to the
#' dataset description:
#' https://github.com/UBC-MDS/rsketball/blob/master/dataset_description.md
#'
#' For detailed use cases, please refer to the vignette:
#' https://ubc-mds.github.io/rsketball/articles/rsketball-vignette.html
#'
#' @param nba_data The tibble dataframe from the scraped nba data
#' @param column The categorical column from the dataset to rank. Should be either "NAME", "TEAM" or "POS"
#' @param by The column from the dataset to rank by. Should be the statistic numerical column of interest.
#' If the column starts with a number (eg 3PA) or has a \% character (eg FT\%), format it with backticks "`".
#' Refer to vignette for more examples on this.
#' @param top The number of elements in the ranking. Defaults to 5.
#' @param descending Boolean variable for the order of the ranking. TRUE if descending, FALSE otherwise.
#' Defaults to True.
#' @param FUN function to apply to the values. Defaults to the Mean function.
#'
#' @return ggplot visualization with the ranking
#'
#' @export
#'
#' @import dplyr
#' @import forcats
#' @import ggplot2
#' @import scales
#' @importFrom magrittr %>%
#' @importFrom rlang :=
#'
#' @examples
#' nba_data <- tibble::tibble(NAME = c("James", "Steph", "Bosh", "Klay", "Kobe"),
#'                            TEAM = c("MIA","GS","MIA","GS","LAL"),
#'                            POS = c("SF", "PG", "C", "SG", "SG"),
#'                            PTS = c(5,4,3,2,10),
#'                            TO = c(1,2,3,4,3))
#'
#' # Find top 3 players for points (PTS) where higher is better
#' nba_ranking(nba_data,
#'             column = NAME,
#'             by = PTS,
#'             top = 3,
#'             descending = TRUE,
#'             FUN = mean)
#'
#'#' # Find top 2 teams for turnover (TO) where lower is better
#' nba_ranking(nba_data,
#'             column = TEAM,
#'             by = TO,
#'             top = 2,
#'             descending = FALSE,
#'             FUN = mean)
nba_ranking <- function(nba_data, column, by, top = 5, descending = TRUE, FUN = mean){

  # Checks

  # Checking if the top variable has the desired type
  if(!typeof(top) == 'double'){
    stop("Argument top should be a numerical value")
  }

  # Checking if the data is a dataframe
  if(!typeof(nba_data) == 'list'){
    stop("Argument data should be a dataframe or a tibble")
  }

  ranked_type <- nba_data %>%
                  pull({{column}})

  # Checking the datatype of the ranked variable
  if(!(((ranked_type %>% typeof()) == "character" | is.factor(ranked_type)))){
    stop("The column argument should be a categorical variable")
  }


  by_type <- nba_data %>%
              pull({{by}}) %>%
              typeof()
  by_var <- nba_data %>%
              pull({{by}})
  # Checking the datatype of the ranking variable

  if(!(by_type == "double" |by_type == "numerical"| (by_type == "integer" & !is.factor(by_var)))){
    stop("The by argument should be a numerical variable")
  }
  # Warning

  #Warning to control the number of elements
  number_elements <- nba_data %>%
                      pull({{column}}) %>%
                      unique() %>%
                      length()

  if(top > number_elements){
    warning(paste0("The number of elements in the ranking is bigger than the number of categories in the variables \n Changing the top parameter to: ",number_elements))
    top <- number_elements
  }

  # Beginning of the function

  fun <- match.fun(FUN = FUN)

  data_filtered <- nba_data %>%
    dplyr::select({{column}}, {{by}}) %>%
    dplyr::group_by({{column}}) %>%
    dplyr::summarise({{by}} := fun({{by}})) %>%
    dplyr::mutate({{column}} := as.factor({{column}}),
                  {{column}} := fct_reorder({{column}}, {{by}}, .desc = descending),
                  number_factor = as.numeric({{column}})) %>%
    dplyr::filter(number_factor <= top) %>%
    dplyr::mutate({{column}} := fct_drop({{column}}),
                  text_value = as.factor(paste0(as.numeric({{column}}), ". ",{{column}}, " - ",round({{by}}, digits = 2))),
                  text_value = fct_reorder(text_value, {{by}}, .desc = descending)) %>%
    dplyr::arrange(number_factor)

  plot_title <- paste0("Top ", top," ", substitute(column)," based on ", substitute(by), " ranking")
  colour_scheme <- scales::seq_gradient_pal("#17408B", "#C9082A")(seq(0,1,length.out=top))
  text_size <- min(8, 75/top)

  plot <- data_filtered %>% ggplot2::ggplot(aes(as.integer({{column}}),{{by}},
                                            fill = {{column}},
                                            label = levels(text_value))) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::scale_x_reverse() +
    ggplot2::labs(title = plot_title, x = substitute(column)) +
    ggplot2::geom_text(hjust = 1.1, colour = "white")+
    ggplot2::theme(
      panel.background = element_blank(),
      legend.position = "none",
      title = element_text(size = 18),
      axis.title.y = element_blank(),
      axis.title.x = element_text(size = 16),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank()
    ) +
    ggplot2::scale_fill_manual(values = colour_scheme)

  return(plot)
}



