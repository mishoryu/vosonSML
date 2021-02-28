#' @title Create networks from social media data
#'
#' @description This function creates networks from social media data as produced from \code{\link{Collect}}. 
#' \code{create_network} is the final step of the \code{\link{Authenticate}}, \code{\link{Collect}} and 
#' \code{create_network} workflow.
#' 
#' There are four types of networks that can be created from collected data: \code{activity}, \code{actor}, 
#' \code{twomode} or \code{semantic}.
#' 
#' For \code{activity} networks refer to \code{\link{create_network.activity.twitter}}, 
#' \code{\link{create_network.activity.youtube}} 
#' and \code{\link{create_network.activity.reddit}} for parameters and usage.
#' 
#' For \code{actor} networks refer to \code{\link{create_network.actor.twitter}}, 
#' \code{\link{create_network.actor.youtube}} and \code{\link{create_network.actor.reddit}}.
#' 
#' For \code{twomode} and \code{semantic} networks refer to \code{\link{create_network.twomode.twitter}} and 
#' \code{\link{create_network.semantic.twitter}} functions for parameters and usage respectively. 
#'
#' @param datasource Collected social media data of class \code{"datasource"} and \code{socialmedia}.
#' @param type Character string. Type of network to be created, can be \code{"activity"}, \code{"actor"},
#' \code{"twomode"} or \code{"semantic"}.
#' @param ... Optional parameters to pass to functions provided by supporting R packages that are used for social 
#' media network creation.
#'
#' @export
create_network <- function(datasource, type, ...) {
  UseMethod("create_network", type)
}

#' @export
create_network.default <- function(datasource, type, ...) {
  if (!is.data.frame(datasource)) { stop("Datasource is not a dataframe.", call. = FALSE) }
  if (nrow(datasource) < 1) { stop("Empty datasource passed to create.", call. = FALSE) }
  
  if (!is.character(type)) {
    stop("Create network type should be a character string.", call. = FALSE) 
  }
  
  func_name <- paste0("create_network", ".", type)
  if (!exists(func_name, where = asNamespace("vosonSML"), mode = "function")) {
    stop("Unknown network type passed to create.", call. = FALSE) 
  }
  
  class(type) <- append(class(type), type)
  create_network(datasource, type, ...)
}

#' @title Create activity networks from social media data
#' @noRd
#' @method create_network activity
#' @export
create_network.activity <- function(datasource, type, ...) {
  UseMethod("create_network.activity", datasource)
}

#' @noRd
#' @export
create_network.activity.default <- function(datasource, type, ...) {
  stop("Unknown datasource passed to create activity network.", call. = FALSE) 
}

#' @title Create actor network from social media data
#' @noRd
#' @method create_network actor
#' @export
create_network.actor <- function(datasource, type, ...) {
  UseMethod("create_network.actor", datasource)
}

#' @noRd
#' @export
create_network.actor.default <- function(datasource, type, ...) {
  stop("Unknown datasource passed to create actor network.", call. = FALSE) 
}

#' @title Create 2-mode networks from social media data
#' @noRd
#' @method create_network twomode
#' @export
create_network.twomode <- function(datasource, type, ...) {
  UseMethod("create_network.twomode", datasource)
}

#' @noRd
#' @export
create_network.twomode.default <- function(datasource, type, ...) {
  stop("Unknown datasource passed to create twomode network.", call. = FALSE) 
}

#' @title Creates a semantic network from social media data
#' @noRd 
#' @method create_network semantic
#' @export
create_network.semantic <- function(datasource, type, ...) {
  UseMethod("create_network.semantic", datasource)
}

#' @noRd
#' @export
create_network.semantic.default <- function(datasource, type, ...) {
  stop("Unknown datasource passed to create semantic network.", call. = FALSE) 
}
