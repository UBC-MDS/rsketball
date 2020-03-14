#' Test the test_nba_boxplot.R Function
#'
#' @description
#' Test inputs edge cases for nba_nba_boxplot.
#'
#'
#' @return
#' @export
#' @examples
#' test_nba_boxplot ()
#'
test_nba_boxplot <- function() {

  test_that('Plot should use geom_boxplot and map stats to x-axis, and position/teams to y-axis.', {
    nba_2018 <- tibble::tibble(POS= c("C", "FOR", "PO","FOR", "C"), TEAM = c("ORL", "UTAH", "LAC", "MIN", "BOS"), GP = c(3, 5, 5, 2, 1))
    plot1 <- nba_boxplot(nba_2018, team_or_position= "position", grouping_list = NULL, stats_column= GP)
    expect_true("GeomBoxplot" %in% c(class(plot1$layers[[1]]$geom)))
    expect_true("POS"  == rlang::get_expr(plot1$mapping$x))
    expect_true("GP" == rlang::get_expr(plot1$mapping$y))
  })

  test_that('Plot should use geom_boxplot and map stats to x-axis, and position/teams to y-axis.', {
    nba_2018 <- tibble::tibble(POS= c("C", "FOR", "PO","FOR", "C"), TEAM = c("ORL", "UTAH", "LAC", "MIN", "BOS"), GP = c(3, 5, 5, 2, 1))
    plot2<- nba_boxplot(nba_2018, team_or_position = "team", grouping_list = c("ORL", "UTAH", "MIN", "BOS"), stats_column = GP)
    expect_true("GeomBoxplot" %in% c(class(plot2$layers[[1]]$geom)))
    expect_true("TEAM"  == rlang::get_expr(plot2$mapping$x))
    expect_true("GP" == rlang::get_expr(plot2$mapping$y))
  })

  test_that("position should be integer value",{
    nba_2018 <- tibble::tibble(POS= c("C", "FOR", "PO","FOR", "C"), TEAM = c("ORL", "UTAH", "LAC", "MIN", "BOS"), GP = c(3, 5, 5, 2, 1))
    p <- nba_boxplot(nba_2018, team_or_position= "position", grouping_list = c("C","FOR"), stats_column= GP)
    expect_type(p$data$POS, "integer")
  })

  test_that("stats should be numeric value",{
    nba_2018 <- tibble::tibble(POS= c("C", "FOR", "PO","FOR", "C"), TEAM = c("ORL", "UTAH", "LAC", "MIN", "BOS"), GP = c(3, 5, 5, 2, 1))
    p <- nba_boxplot(nba_2018, team_or_position= "team", grouping_list = c("ORL","UTAH"), stats= GP)
    expect_type(p$data$GP, "double")
  })

  test_that("Not inputting a position or teams input will result in default boxplot for positions", {
    nba_2018 <- tibble::tibble(POS= c("C", "FOR", "PO","FOR", "C"), TEAM = c("ORL", "UTAH", "LAC", "MIN", "BOS"), GP = c(3, 5, 5, 2, 1))
    plot1 <- nba_boxplot(nba_2018, stats_column= GP)
    expect_true("POS"  == rlang::get_expr(plot1$mapping$x))
  })

  test_that("This column use in stats input is not in the dataset.", {
    nba_2018 <- tibble::tibble(POS= c("C", "FOR", "PO","FOR", "C"), TEAM = c("ORL", "UTAH", "LAC", "MIN", "BOS"), GP = c(3, 5, 5, 2, 1))
    expect_error(nba_boxplot(nba_2018, team_or_position= "position", grouping_list = NULL, stats_column= GPR))
  })

  test_that("team_or_position should be 'team' or 'position'", {
    nba_2018 <- tibble::tibble(POS= c("C", "FOR", "PO","FOR", "C"), TEAM = c("ORL", "UTAH", "LAC", "MIN", "BOS"), GP = c(3, 5, 5, 2, 1))
    expect_error(nba_boxplot(nba_2018, team_or_position= "hello", grouping_list = NULL, stats_column= GP))
  })

  test_that("Error: stats must take in a numerical column", {
    nba_2018 <- tibble(POS= c("C", "FOR", "PO","FOR", "C"), TEAM = c("ORL", "UTAH", "LAC", "MIN", "BOS"), GP = c(3, 5, 5, 2, 1))
    expect_error(nba_boxplot(nba_2018, team_or_position= "position", grouping_list= NULL, stats_column= POS))
  })

}

test_nba_boxplot()
