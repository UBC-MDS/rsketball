#' Test the nba_ranking.R Function
#'
#' @return
#' @export
#'
#' @examples
#' test_ranking()
test_ranking <- function(){
  # Testing the data input
  test_that("The function is allowing other values as data input. Needs tibble or data frame",
    expect_error(nba_ranking("Ada Lovelace", column = a, by = b,top = 6, FALSE, mean)))

  # Testing the top input
  test_that("The function is allowing other values as data input. Needs tibble or data frame",
            expect_error(nba_ranking(data.frame(ranked = c(1, 2, 3), by = c(3, 2, 1)), ranked, by, "Ada Lovelace", TRUE, mean)))

  # Testing the warning
#  test_that("The function is allowing other values as data input. Needs tibble or data frame",
#            expect_message(nba_ranking(data.frame(ranked = c(1, 2, 3), by = c(3, 2, 1)), ranked, by, 15, TRUE, mean)))

  test_that('Plot should use a geom_col and a geom_text', {
    p <- nba_ranking(data.frame(ranked = c(1, 2, 3), by = c(3, 2, 1)), ranked, by, 2, TRUE, mean)
    expect_true("GeomCol" %in% c(class(p$layers[[1]]$geom)))
    expect_true("GeomText" %in% c(class(p$layers[[2]]$geom)))
  })
}

test_ranking()

