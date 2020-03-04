#' Test the test_nba_boxplot.R Function
#'
#' @return
#' @export
#' @examples
#' test_nba_boxplot ()
#'
test_nba_boxplot <- function() {

  test_that('Plot should use geom_boxplot and map stats to x-axis, and position/teams to y-axis.', {
  plot1 <- nba_boxplot(nba_2018, position= "POS", teams= NULL, stats= "GP")
  expect_true("GeomBoxplot" %in% c(class(plot1$layers[[1]]$geom)))
  expect_true("POS"  == rlang::get_expr(plot1$mapping$x))
  expect_true("GP" == rlang::get_expr(plot1$mapping$y))
  })

  test_that("position should be integer value",{
  p <- nba_boxplot(nba_2018, position= "POS", teams= NULL, stats= "GP")
  expect_type(p$data$POS, "integer")
  })

  test_that("stats should be numeric value",{
    p <- nba_boxplot(nba_2018, position= "POS", teams= NULL, stats= "GP")
    expect_type(p$data$GP, "double")
  })

  test_that("Not inputting a position or teams input will result in a error, must input one", {
    expect_error(nba_boxplot(nba_2018, stats= "GP"))
  })

  test_that("Inputting both position or teams input will result in a error, must input one", {
    expect_error(nba_boxplot(nba_2018, position= "POS", teams= c("ORL", "UTAH", "LAC", "MIN", "BOS"), stats= "GP"))
  })
}


