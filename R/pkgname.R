#' R libraries to parse and write fisheries biotic and acoustic data format
#'
#' Tools to fetch and manipulate various data formats for fisheries (mainly geared towards biotic and acoustic data).
#'
#' The RstoxData package contains functions for reading, filtering and writing biotic, acoustic and landing data as XML files. Filtering can be done by R syntax such as longitude > 10, or by pre defined functions such as inside().
#' @docType package
#' @name RstoxData
#'
"_PACKAGE"
#' @useDynLib RstoxData, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL
