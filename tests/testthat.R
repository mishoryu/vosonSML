library(testthat)
library(vosonsml)

suppress_cat <- function(input = NULL) {
  invisible(capture.output(input, type = c("output")))
}

test_check("vosonsml")
