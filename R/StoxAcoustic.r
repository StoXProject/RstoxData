
#' Read fisheries XML data format file
#'
#' Read fisheries XML data format file. Currently supports IMR Biotic version 1 until 3, IMR Echosounder version 1, and IMR Landing version 2 formats at the moment.
#' Streaming XML pull parser can be used to avoid loading the whole XML into memory and it supports ZIP file reading. Please note that
#' the XML file inside the zip file should be using the same name as the zip file itself (e.g. test.xml inside test.zip). 
#'
#' @param xmlFilePath full path to the XML file to be read.
#' @param stream a streaming XML pull parser is used if this is set to TRUE. An XML DOM parser is used if this is set to FALSE. Default to FALSE.
#' @param useXsd Specify an xsd object to use. Default to NULL.
#'
#' @return List of data.table objects containing the "flattened" XML data.
#'
#' @examples
#' \dontrun{
#' # Reading test.xml using XML DOM parser
#' one <- readXmlFile("./test.xml")
#' # Reading test.xml using XML pull parser
#' two <- readXmlFile("./test.xml", stream = TRUE)
#' # Reading test.xml inside test.zip file
#' three <- readXmlFile("./test.zip", stream = TRUE)
#' }
#'
#' @useDynLib RstoxData
#' @importFrom Rcpp sourceCpp
#' @importFrom data.table as.data.table transpose
#'
stoxAcoustic <- function(
  
)
  
  
#' @export