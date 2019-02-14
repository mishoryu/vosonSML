#' @title Collection and network analysis of social media data
#'
#' @description The goal of the \pkg{vosonSML} package is to provide a suite of easy-to-use tools for collecting data 
#' from social media and generating different types of networks suited to Social Network Analysis (SNA) and text 
#' analytics. It offers tools to create unimodal, multimodal, and semantic networks. Excellent packages such as 
#' \pkg{rtweet}, \pkg{RedditExtractoR}, \pkg{magrittr}, \pkg{dplyr} and \pkg{igraph} were drawn on to provide an 
#' integrated work flow for creating different types of networks out of social media data. Creating networks from 
#' online social media is often non-trivial and time consuming. This package simplifies such tasks so users can focus 
#' on analysis.
#'
#' \pkg{vosonSML} uses a straightforward S3 class system. Data collected with this package produces \code{data.frame} 
#' inheritable objects that are assigned the class \code{"datasource"}. Additionally, \code{"datasource"} objects are 
#' attributed a class identifying the source of data, such as \code{"twitter"} or \code{"youtube"}. In this way 
#' \code{datasource} objects are fast, easy to work with, and can be used as input to easily construct different kinds 
#' of networks. For example, the function \code{Collect} can be used to collect twitter data, which is then passed to 
#' the \code{Create} function resulting in a twitter network (as igraph object) that is ready for analysis.
#'
#' @name vosonSML-package
#' @aliases vosonSML-package vosonSML
#' @docType package
#' @author Created by Timothy Graham and Robert Ackland, with major contributions by Chung-hong Chan and Bryan Gertzel.
#' @import tm
#' @import RCurl
#' @import bitops
#' @import rjson
#' @import data.table
#' @import httpuv
#' @import methods
#' @import httr
#' @importFrom Hmisc escapeRegex
#' @importFrom igraph delete.vertices simplify write.graph V 'V<-' vcount
#' graph_from_data_frame delete_vertex_attr set_graph_attr
#' @importFrom rtweet create_token rate_limit search_tweets users_data lookup_users
#' @importFrom stringr str_extract str_replace_all str_match_all
#' @importFrom utils "flush.console" head "install.packages" "read.table" "write.csv" "read.csv"
#' @importFrom RedditExtractoR reddit_content user_network
#' @importFrom magrittr '%>%' '%<>%'
#' @importFrom dplyr rename group_by summarise ungroup left_join select mutate filter coalesce row_number
#' distinct anti_join mutate_all mutate_at ends_with vars funs bind_rows
#' @importFrom rlang '.data'
NULL