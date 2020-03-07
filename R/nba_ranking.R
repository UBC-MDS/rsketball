#' Generates a ranking and a visualization based on a column of a dataset
#'
#' @description
#' This function generates creates a ranking of a variable `column` summarizing by a variable `by` using a function `FUN`. The result of this function is
#' a visualization with that information.
#'
#' @param data The dataframe to make the ranking from
#' @param column The column from the dataset to rank
#' @param by The column from the dataset to rank by
#' @param top The number of elements in the ranking
#' @param descending Boolean variable for the order of the ranking. TRUE if descending, FALSE otherwise.
#' @param FUN function to apply to the values
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
#' nba_ranking(data.frame(ranked = c("1", "2", "3"), by = c(3, 2, 1)), ranked, by, 2, TRUE, mean)
nba_ranking <- function(data, column, by, top, descending = TRUE, FUN){

  # Checks

  # Checking if the top variable has the desired type
  if(!typeof(top) == 'double'){
    stop("Argument top should be a numerical value")
  }

  # Checking if the data is a dataframe
  if(!typeof(data) == 'list'){
    stop("Argument data should be a dataframe or a tibble")
  }

  ranked_type <- data %>%
                  pull({{column}})

  # Checking the datatype of the ranked variable
  if(!(((ranked_type %>% typeof()) == "character" | is.factor(ranked_type)))){
    stop("The column argument should be a categorical variable")
  }


  by_type <- data %>%
              pull({{by}}) %>%
              typeof()
  by_var <- data %>%
              pull({{by}})
  # Checking the datatype of the ranking variable

  if(!(by_type == "double" |by_type == "numerical"| (by_type == "integer" & !is.factor(by_var)))){
    stop("The by argument should be a numerical variable")
  }
  # Warning

  #Warning to control the number of elements
  number_elements <- data %>%
                      pull({{column}}) %>%
                      unique() %>%
                      length()

  if(top > number_elements){
    warning(paste0("The number of elements in the ranking is bigger than the number of categories in the variables \n Changing the top parameter to: ",number_elements))
    top <- number_elements
  }

  # Beginning of the function

  fun <- match.fun(FUN = FUN)

  data_filtered <- data %>%
    dplyr::select({{column}}, {{by}}) %>%
    dplyr::group_by({{column}}) %>%
    dplyr::summarise({{by}} := fun({{by}})) %>%
    dplyr::mutate({{column}} := as.factor({{column}}),
                  {{column}} := fct_reorder({{column}}, {{by}}, .desc = !descending),
                  number_factor = as.numeric({{column}})) %>%
    dplyr::filter(number_factor <= top) %>%
    dplyr::mutate({{column}} := fct_drop({{column}}),
                  text_value = as.factor(paste0(as.numeric({{column}}), ". ",{{column}}, " - ",round({{by}}, digits = 2))),
                  text_value = fct_reorder(text_value, {{by}}, .desc = !descending)) %>%
    dplyr::arrange(number_factor)

  plot_title <- paste0("Top ", top," ", substitute(column)," by ", substitute(by))
  colour_scheme <- scales::seq_gradient_pal("#17408B", "#C9082A")(seq(0,1,length.out=top))
  text_size <- min(8, 75/top)

  plot <- data_filtered %>% ggplot2::ggplot(aes(as.integer({{column}}),{{by}},
                                            fill = {{column}},
                                            label = levels(text_value))) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::scale_x_reverse() +
    ggplot2::labs(title = plot_title, x = substitute(column)) +
    ggplot2::geom_text(size = text_size, hjust = 1.1, colour = "white")+
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



