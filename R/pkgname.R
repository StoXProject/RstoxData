#' Utilities to Read Fisheries Biotic, Acoustic and Landing Data Formats
#'
#' Tools to fetch and manipulate various data formats for fisheries (mainly geared towards biotic and acoustic data).
#'
#' The RstoxData package contains functions for reading, filtering and writing biotic, acoustic and landing data as XML files. Filtering can be done by R syntax such as longitude > 10, or by pre defined functions such as inside(). On computers that return errors when trying to run the Rtools through RStudio (most institutional Windows machines), install the binary directly from https://github.com/StoXProject/RstoxData/releases. Download the newest RstoxData zip file, click the "Packages" tab -> "Install" -> "Install from:" "Package Archive File" -> "Install". If the installer does not complain, the package is installed correctly.
#' @docType package
#' @name RstoxData
#'
"_PACKAGE"

.onLoad <- function(libname, pkgname){
	# Initiate the RstoxData environment:
	initiateRstoxData()
} 

# Try to unload dynamic library
.onUnload <- function (libpath) {
	library.dynam.unload("RstoxData", libpath)
} 

#' @useDynLib RstoxData, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

