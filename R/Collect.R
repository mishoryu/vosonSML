#' @title Collect data from social media for generating networks
#'
#' @description This function collects data from social media and structures it into a dataframe that can be used for 
#' creating networks for further analysis. \code{collect} is the second step of the \code{\link{Authenticate}}, 
#' \code{collect}, and \code{\link{Create}} workflow.
#'
#' Refer to \code{\link{collect.twitter}}, \code{\link{collect.youtube}} and 
#' \code{\link{collect.reddit}} for parameters and usage.
#' 
#' @param credential A \code{credential} object generated from \code{Authenticate}.
#' @param ... Optional parameters to pass to functions providied by supporting R packages that are used for social media 
#' API collection.
#'
#' @export
collect <- function(credential, ...) {
  timer_pkg <- FALSE
  if (requireNamespace("tictoc", quietly = TRUE)) { timer_pkg <- TRUE }
  
  # set the environment encoding to UTF-8 for data collection
  saved_enc <- getOption("encoding")
  saved_ua <- getOption("HTTPUserAgent")
  on.exit({
    if (timer_pkg) { tictoc::toc(quiet = FALSE, func.toc = collectTocOutput) }
    options(encoding = saved_enc)
    options(HTTPUserAgent = saved_ua)
  }, add = TRUE)
  options(encoding = "UTF-8")
  options(HTTPUserAgent = paste0("vosonsml v.", getVosonSMLVer(), " (R Package)"))
  if (timer_pkg) { tictoc::tic(msg = "Elapsed time") }
  
  # searches the class list of credential for matching method
  UseMethod("collect", credential)
}

# default function
#' @export
collect.default <- function(credential, ...) {
  stop("Unknown social media type passed to collect.", call. = FALSE)
}
