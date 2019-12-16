#' @title xsdObjects
#' @description Pre-processed XSD file objects
#' @format A list with 4 elements
#' \describe{
#'   \item{\code{landingerv2.xsd}}{List Landing Format v2}
#'   \item{\code{nmdbioticv1.xsd}}{List NMD Biotic Format v1}
#'   \item{\code{nmdbioticv1.1.xsd}}{List NMD Biotic Format v1.1}
#'   \item{\code{nmdbioticv1.2.xsd}}{List NMD Biotic Format v1.2}
#'   \item{\code{nmdbioticv1.3.xsd}}{List NMD Biotic Format v1.3}
#'   \item{\code{nmdbioticv1.4.xsd}}{List NMD Biotic Format v1.4}
#'   \item{\code{nmdbioticv3.xsd}}{List NMD Biotic Format v3}
#'   \item{\code{nmdechosounderv1.xsd}}{List NMD Echosounder Format v1}
#' }
#' @source \url{https://www.imr.no/formats}
"xsdObjects"

#' @title stoxBioticObject
#' @description Pre-processed objects for raw XML data to StoXBiotic format
"stoxBioticObject"

##################################################
##################################################
#' StoX data type BioticData
#' 
#' Biotic data read from biotic xml files.
#' 
#' @details
#' This StoX data type is produced by \code{\link{ReadBiotic}}, and contains one list per input biotic file holding the tables read from each file, added a table named "metadata" holding the input file path and format. Currently supported are NMDBiotic1.4 and NMDBiotic3.0 (\url{https://www.imr.no/formats}), and ICESBiotic (\url{https://www.ices.dk/marine-data/data-portals/Pages/acoustic.aspx}, click on "Acoustic data format" to download the format description). 
#' 
#' @seealso \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxData}}
#' 
#' @name BioticData
#' 
NULL
