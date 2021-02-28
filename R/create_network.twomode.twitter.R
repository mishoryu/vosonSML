#' @title Create twitter 2-mode network
#' 
#' @description Creates a 2-mode network from tweets returned from the twitter search query. In this network there are 
#' two types of nodes, twitter users who authored or were mentioned in collected tweets and hashtags found within
#' tweets. Network edges represent a users tweets that contain hashtags or mention users screen names.
#'
#' The creation of twitter 2-mode networks requires text processing and the tokenization of tweets. As such
#' this function requires the additional installation of the \pkg{tidytext} package to achieve this.
#' 
#' @param datasource Collected social media data with \code{"datasource"} and \code{"twitter"} class names.
#' @param type Character string. Type of network to be created, set to \code{"twomode"}.
#' @param rm_nodes Character vector. List of \code{@Users} and \code{#hashtags} to remove from the 2-mode network.
#' For example, this parameter could be used to remove the user or hashtag that was used to collect the data by
#' removing any nodes with matching name. Default is \code{NULL}.
#' @param weighted Logical. Add weights to network edges. If set to \code{FALSE} tweet \code{tweet_id} and 
#' \code{created_at} fields will be preserved for edges in the dataframe. Default is \code{TRUE}.
#' @param verbose Logical. Output additional information about the network creation. Default is \code{FALSE}.
#' @param ... Additional parameters passed to function. Not used in this method.
#' 
#' @return Network as a named list of two dataframes containing \code{$nodes} and \code{$edges}.
#' 
#' @examples
#' \dontrun{
#' # twitter 2-mode network creation additionally requires the tidytext package
#' # for working with text data
#' install.packages("tidytext")
#' 
#' # create a twitter 2-mode network graph removing the hashtag '#auspol'
#' 2mode_net <- twitter_data %>% 
#'   create_network("twomode", rm_nodes = c("#auspol"), verbose = TRUE)
#' 
#' # network nodes and edges
#' # 2mode_net$nodes
#' # 2mode_net$edges
#' }
#' 
#' @export
create_network.twomode.twitter <-
  function(datasource,
           type,
           rm_nodes = NULL,
           weighted = TRUE,
           verbose = FALSE,
           ...) {
    
    # renamed function params
    dots <- list(...)
    if (is.null(rm_nodes) && ("removeTermsOrHashtags" %in% names(dots))) { rm_nodes <- dots$removeTermsOrHashtags }
    
    if (!requireNamespace("tidytext", quietly = TRUE)) {
      stop(
        paste0(
          "Please install the tidytext package before calling create.twomode.twitter.",
          call. = FALSE
        )
      )
    }
    
    datasource <-
      datasource %>% dplyr::select(
        .data$status_id,
        .data$user_id,
        .data$screen_name,
        .data$text,
        .data$created_at,
        .data$is_retweet,
        .data$is_quote
      )
    datasource$text = textutils::HTMLdecode(datasource$text)
    
    capture.output(
      tokens_df <-
        datasource %>% tidytext::unnest_tokens(
          .data$word,
          .data$text,
          token = "tweets",
          to_lower = TRUE
        ),
      type = "output"
    )
    tokens_df <-
      tokens_df %>% dplyr::mutate(at_name = paste0("@", tolower(.data$screen_name)))
    
    if (!is.null(rm_nodes) &&
        length(rm_nodes) > 0) {
      rm_nodes <-
        unlist(lapply(rm_nodes, tolower))
      tokens_df <- tokens_df %>% dplyr::filter(
        !(.data$word %in% rm_nodes) &
          !(.data$user_id %in% rm_nodes) &
          !(tolower(.data$screen_name) %in% rm_nodes) &
          !(.data$at_name %in% rm_nodes)
      )
    }
    
    tokens_df <-
      tokens_df %>% dplyr::mutate(type = data.table::fcase(
        grepl("^#.*", .data$word),
        "hashtag",
        grepl("^@.*", .data$word),
        "user",
        default = "term"
      )) %>% dplyr::filter(.data$type %in% c("hashtag", "user") &
                             .data$at_name != .data$word)
    
    edges <-
      tokens_df %>% dplyr::select(
        from = .data$at_name,
        to = .data$word,
        tweet_id = .data$status_id,
        .data$created_at
      )
    
    if (weighted) {
      edges <-
        edges %>% dplyr::count(.data$from, .data$to, name = "weight")
    }
    
    nodes <-
      dplyr::distinct(tibble::tibble(name = c(edges$to, edges$from))) %>%
      dplyr::left_join(
        tokens_df %>% dplyr::select(.data$at_name, .data$user_id) %>% dplyr::distinct(),
        by = c("name" = "at_name")
      )
    
    network <- list("nodes" = nodes, "edges" = edges)
    class(network) <-
      append(c("network", "twomode", "twitter"), class(network))

    network
  }
