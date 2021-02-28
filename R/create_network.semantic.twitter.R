#' @title Create twitter semantic network
#' 
#' @description Creates a semantic network from tweets returned from the twitter search query. Semantic networks 
#' describe the semantic relationships between concepts. In this network the concepts are significant words and 
#' hashtags extracted from the tweet text. Network edges are weighted and represent occurrence of words and
#' hashtags in the same tweets.
#' 
#' The creation of twitter semantic networks requires text processing and the tokenization of tweets. As such
#' this function requires the additional installation of the \pkg{tidyr} and \pkg{tidytext} packages to achieve
#' this.
#' 
#' @note The words and hashtags passed to the function in the \code{rm_nodes} parameter are removed
#' before word frequencies are calculated and are therefore excluded from top percentage of most frequent words
#' completely rather than simply filtered out of the final network.
#' 
#' The top percentage of frequently occurring hashtags \code{top_hashtags} and words \code{top_words} are calculated to 
#' a minimum frequency and all words that have an equal or greater frequency than the minimum are included in the 
#' network as nodes. For example, of unique hashtags of varying frequencies in a dataset the top 50% of total
#' frequency or most common hashtags may calculate to being the first 20 hashtags. The frequency of the 20th hashtag is
#' then used as the minimum and all hashtags of equal or greater frequency are included as part of the top 50%
#' most frequently occurring hashtags. So the number of top hashtags may end up being greater than 20 if there is more
#' than one hashtag that has frequency matching the minimum. The exception to this is if the minimum frequency is 1
#' and the \code{top_hashtags} is set to less than 100, in this case only the first 20 hashtags will be included.
#' 
#' Hashtags and words in the top percentages are included in the network as isolates if there are no instances of
#' them occurring in tweet text with other top percentage frequency words.
#' 
#' @param datasource Collected social media data with \code{"datasource"} and \code{"twitter"} class names.
#' @param type Character string. Type of network to be created, set to \code{"semantic"}.
#' @param rm_nodes Character vector. Words or hashtags to remove from the semantic network. For example, 
#' this parameter could be used to remove the search term or hashtag that was used to collect the data by removing any
#' nodes with matching name. Effectively a custom stopwords list. Default is \code{NULL}.
#' @param stopwords Logical. Removes language stopwords from the tweet data. Default is \code{TRUE}.
#' @param stw_lang Character string. Language of stopwords to use. Refer to the \pkg{stopwords} package for
#' further information on supported languages. Default is \code{"en"} for english.
#' @param stw_src Character string. Source of stopwords list. Refer to the \pkg{stopwords} package for
#' further information on supported sources. Default is \code{"smart"}.
#' @param incl_numbers Logical. Include whole numerical tokens from the tweet text. For example, a year value
#' such as \code{2020} will be removed but not mixed values such as \code{G20}. Default is \code{TRUE}.
#' @param incl_urls Logical. Include twitter shortened URL tokens found in tweet text. Default is \code{FALSE}.
#' @param top_words Numeric integer. Specifies the percentage of most frequent words to include. For example,
#' \code{top_words = 20} means that the 20 percent most frequently occurring \code{words} will be included in the 
#' semantic network as nodes. A larger percentage will increase the number of nodes and therefore the size of graph. 
#' The default value is \code{5}, meaning the top 5 percent most frequent words are used.
#' @param top_hashtags Numeric integer. Specifies the percentage of most frequent \code{hashtags} to include. For 
#' example, \code{top_hashtags = 20} means that the 20 percent most frequently occurring hashtags will be included 
#' in the semantic network as nodes. The default value is \code{50}.
#' @param assoc Character string. Association of nodes. A value of \code{"limited"} includes only edges between
#' most frequently occurring hashtags and words. A value of \code{"full"} includes ties between most frequently
#' occurring hashtags and words, hashtags and hashtags, and also words and words. Default is \code{"limited"}.
#' @param verbose Logical. Output additional information about the network creation. Default is \code{FALSE}.
#' @param ... Additional parameters passed to function. Not used in this method.
#' 
#' @return Network as a named list of two dataframes containing \code{$nodes} and \code{$edges}.
#' 
#' @examples
#' \dontrun{
#' # twitter semantic network creation additionally requires the tidyr, tidytext and stopwords packages
#' # for working with text data
#' install.packages(c("tidyr", "tidytext", "stopwords"))
#' 
#' # create a twitter semantic network graph removing the hashtag '#auspol' and using the
#' # top 2% frequently occurring words and 10% most frequently occurring hashtags as nodes
#' semantic_net <- twitter_data %>% 
#'   create_network("semantic", rm_nodes = c("#auspol"), top_words = 2, top_hashtags = 10)
#' 
#' # network nodes and edges
#' # semantic_net$nodes
#' # semantic_net$edges
#' }
#' 
#' @export
create_network.semantic.twitter <-
  function(datasource,
           type,
           rm_nodes = NULL,
           stopwords = TRUE,
           stw_lang = "en",
           stw_src = "smart",
           incl_numbers = TRUE,
           incl_urls = FALSE,
           top_words = 5,
           top_hashtags = 20,
           assoc = "limited",
           verbose = FALSE,
           ...) {
    
    # renamed function params
    # removeTermsOrHashtags = NULL, stopwordsLang = "en", stopwordsSrc = "smart",
    # removeNumbers = TRUE, removeUrls = TRUE, termFreq = 5, hashtagFreq = 50,
    
    dots <- list(...)
    in_dots <- function(x) { x %in% names(dots) }
    if (is.null(rm_nodes) && in_dots("removeTermsOrHashtags")) { rm_nodes <- dots$removeTermsOrHashtags }
    if (in_dots("stopwordsLang")) { stw_lang <- dots$stopwordsLang }
    if (in_dots("stopwordsSrc")) { stw_src <- dots$stopwordsSrc }
    if (in_dots("removeNumbers")) { incl_numbers <- !dots$removeNumbers }
    if (in_dots("removeUrls")) { incl_urls <- !dots$removeUrls }
    if (in_dots("termFreq")) { top_words <- dots$termFreq }
    if (in_dots("hashtagFreq")) { top_hashtags <- dots$hashtagFreq }
    
    req_pkgs <-
      sapply(c("tidyr", "tidytext"), function(x) {
        requireNamespace(x, quietly = TRUE)
      })
    if (any(req_pkgs == FALSE)) {
      stop(
        paste0(
          "Please install ",
          paste0(names(which(
            req_pkgs == FALSE
          )), collapse = ', '),
          " package",
          ifelse(length(which(
            req_pkgs == FALSE
          )) > 1, "s", ""),
          " before calling create.semantic.twitter.",
          call. = FALSE
        )
      )
    }
    
    check_perc <- function(x, desc) {
      if (!is.numeric(x) || x > 100 || x < 1) {
        stop(paste0(desc, " must be a number between 1 and 100."), call. = FALSE)
      }
    }
    
    check_perc(top_words, "top_words")
    check_perc(top_hashtags, "top_hashtags")
    
    if (stopwords) {
      tryCatch({
        rm_stopwords <-
          tidytext::get_stopwords(language = stw_lang, source = stw_src)
      }, error = function(e) {
        stop(gsub("^Error:\\s", "", paste0(e)), call. = FALSE)
      })
    }
    
    datasource <-
      datasource %>% dplyr::select(.data$status_id, .data$text, .data$hashtags)
    datasource$text <- textutils::HTMLdecode(datasource$text)
    
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
    
    # removal of words and hashtags before frequencies are calculated
    # in future may want an option to do this at the end to simply filter words from result set
    if (!is.null(rm_nodes) &&
        length(rm_nodes) > 0) {
      rm_nodes <-
        unlist(lapply(rm_nodes, tolower))
      tokens_df <-
        tokens_df %>% dplyr::filter(!(.data$word %in% rm_nodes))
    }
    
    if (stopwords) {
      capture.output(tokens_df <- tokens_df %>% dplyr::anti_join(rm_stopwords), type = "output")
    }
    
    nodes_freq <- tokens_df %>% dplyr::count(.data$word, sort = TRUE)
    
    # clasify words as hashtags, users and words
    nodes_freq <- nodes_freq %>% dplyr::mutate(
      type = data.table::fcase(
        grepl("^#.*", .data$word),
        "hashtag",
        grepl("^@.*", .data$word),
        "user",
        grepl("^[[:digit:]]+$", .data$word) & incl_numbers,
        "number",
        grepl("^https?://t\\.co/", .data$word) & incl_urls,
        "url",
        default = "word"
      )
    )
    
    if (incl_numbers) {
      nodes_freq <- nodes_freq %>% dplyr::filter(.data$type != "number")
    }
    if (incl_urls) {
      nodes_freq <- nodes_freq %>% dplyr::filter(.data$type != "url")
    }
    
    # remove users
    nodes_freq <- nodes_freq %>% dplyr::filter(.data$type != "user")
    
    # tidytext unnest_tokens is changing hashtags starting with digits such as #9news to words i.e 9news
    # this causes a discrepancy between generated tokens and original hashtags data field count
    
    # remove words outside top percentage to keep
    rm_words <- function(rm_type, keep_perc) {
      type_df <-
        nodes_freq %>% dplyr::filter(.data$type == rm_type) %>% dplyr::arrange(dplyr::desc(n))
      
      if (nrow(type_df) > 0) {
        keep_count <- round(((nrow(type_df) / 100) * keep_perc), digits = 0)
        n_value <- type_df[keep_count, ]$n
        
        # keep top number of rows
        if (n_value <= 1 & keep_perc != 100) {
          rm_values <- type_df %>% dplyr::slice(keep_count + 1:n())
        } else {
          # keep tokens above n value cutoff
          rm_values <- type_df %>% dplyr::filter(.data$n < n_value)
        }
        
        nodes_freq %>% dplyr::filter(!(.data$word %in% rm_values$word))
      } else {
        nodes_freq
      }
    }
    
    # keep top percentage of hashtags and words
    nodes_freq <- rm_words("hashtag", top_hashtags)
    nodes_freq <- rm_words("word", top_words)
    nodes_freq <- nodes_freq %>% dplyr::arrange(dplyr::desc(n))
    
    if (tolower(assoc) == "full") {
      edges <-
        dplyr::inner_join((tokens_df %>% dplyr::select(-.data$hashtags)),
                          (nodes_freq %>% dplyr::select(-.data$n, -.data$type)),
                          by = "word")
      
      edges <-
        dplyr::inner_join(edges, (edges %>% dplyr::select(.data$status_id, .data$word)), by = "status_id") %>%
        dplyr::group_by(.data$status_id) %>%
        dplyr::filter(.data$word.x != .data$word.y) %>%
        dplyr::ungroup() %>%
        
        dplyr::select(-.data$status_id) %>%
        dplyr::mutate(from = .data$word.x, to = .data$word.y) %>%
        dplyr::group_by(.data$from, .data$to) %>%
        dplyr::summarise(weight = dplyr::n())
    } else {
      keep_hashtags <- nodes_freq %>% dplyr::filter(type == "hashtag")
      keep_words <- nodes_freq %>% dplyr::filter(type == "word")
      
      edges <-
        dplyr::inner_join(tokens_df, nodes_freq, by = "word") %>%
        tidyr::unnest(.data$hashtags) %>%
        dplyr::mutate(hashtags = data.table::fifelse(is.na(.data$hashtags), NA, paste0("#", tolower(.data$hashtags))))
      
      edges <- edges %>% dplyr::filter(
        .data$hashtags %in% unique(keep_hashtags$word) &
          .data$word %in% unique(keep_words$word)
      ) %>%
        dplyr::select(-.data$n, -.data$type)
      
      edges <-
        edges %>% dplyr::select(.data$hashtags, .data$word) %>%
        dplyr::mutate(from = .data$hashtags, to = .data$word) %>%
        dplyr::group_by(.data$from, .data$to) %>%
        dplyr::summarise(weight = dplyr::n())
    }
    
    network <- list("nodes" = nodes_freq, "edges" = edges)
    class(network) <-
      append(c("network", "semantic", "twitter"), class(network))
    
    network
  }
