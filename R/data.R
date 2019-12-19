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
#' StoX data types of the RstoxData package
#' 
#' StoX data types are the data types used to transfer data and information between processes in a StoX estimation model.
#' 
#' @details
#' This RstoxData package produces the folliwing StoX data types:
#' \itemize{
#' \item{\code{\link{BioticData}}}
#' \item{\code{\link{StoxBioticData}}}
#' \item{\code{\link{AcousticData}}}
#' \item{\code{\link{StoxAcousticData}}}
#' \item{\code{\link{LandingData}}}
#' \item{\code{\link{StoxLandingData}}}
#' }
#' 
#' @seealso \code{\link[RstoxBase]{RstoxBase}} and \code{\link[RstoxFDA]{RstoxFDA}} for a list of all StoX data types produced by the other official StoX function packages.
#' 
#' @name DataTypes
#' 
NULL


##################################################
##################################################
#' StoX data type BioticData
#' 
#' Biotic data read from biotic xml files.
#' 
#' @details
#' This StoX data type is produced by \code{\link{ReadBiotic}}, and contains one list per input biotic file holding the tables read from each file, added a table named "metadata" holding the input file path and format. Currently supported are NMDBiotic1.4 (\url{https://www.imr.no/formats/nmdbiotic/v1.4/}), NMDBiotic3.0 (\url{https://www.imr.no/formats/nmdbiotic/v3/}), and ICESBiotic (\url{https://www.ices.dk/marine-data/data-portals/Pages/acoustic.aspx}, click on "Acoustic data format" to download the format description).
#' 
#' @seealso \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxData}}
#' 
#' @name BioticData
NULL


##################################################
##################################################
#' StoX data type StoxBioticData
#' 
#' Biotic data stored in the StoxBiotic format, which contains the variables needed for most estimation models used by StoX.
#' 
#' @details
#' This StoX data type is produced by \code{\link{StoxBiotic}}, and contains the tables Cruise, Station, Haul, SpeciesCategory, Sample and Individual in that hierachical order.
#' 
#' @seealso \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxData}}
#' 
#' @name StoxBioticData
#' 
NULL


##################################################
##################################################
#' StoX data type AcousticData
#' 
#' Biotic data read from biotic xml files.
#' 
#' @details
#' This StoX data type is produced by \code{\link{ReadAcoustic}}, and contains one list per input acoustic file holding the tables read from each file, added a table named "metadata" holding the input file path and format. Currently supported are NMDEchosounder1 (\url{https://www.imr.no/formats/nmdechosounder/v1/}), and ICESAcoustic (\url{https://www.ices.dk/marine-data/data-portals/Pages/acoustic.aspx}, click on "Acoustic data format" to download the format description). 
#' 
#' @seealso \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxData}}
#' 
#' @name AcousticData
#' 
NULL


##################################################
##################################################
#' StoX data type StoxAcousticData
#' 
#' Acoustic data stored in the StoxAcoustic format, which contains the variables needed for most estimation models used by StoX.
#' 
#' @details
#' This StoX data type is produced by \code{\link{StoxAcoustic}}, and contains the tables Cruise, Log, Beam, AcousticCategory, ChannelReference and NASC in that hierachical order.
#' 
#' @seealso \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxData}}
#' 
#' @name StoxAcousticData
#' 
NULL


#' LandingData
#' 
#' @section Data:
#' One entry 'Seddellinje' is one line of a sales-note or landing-note. 
#' These are issued as fish is landed, and a complete set of these for a period
#' can be considered a census of all first hand sale of fish sold from Norwegian vessels.
#' 
#' @section Format:
#' list() of \code{\link[data.table]{data.table}} 
#' representing the different complexTypes in namespace http://www.imr.no/formats/landinger/v2
#' For ease of merging: all top level attributes are repeated for all tables. And all line-identifying variables are included as top-level attributes.
#' 
#' @seealso \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxData}}
#' 
#' @name LandingData
#' 
NULL


#' StoxLandingData
#'
#' Table (\code{\link[data.table]{data.table}}) with aggregated landings data from sales notes.
#' Contains sales notes and landing notes.
#' These are issued as fish is landed, and can be considered a census of all first hand sale of fish.
#' Sales-notes should cover all landings from Norwegian vessels. Even those abroad.
#' In addition they cover landings by foreign vessels in Norwegian ports.
#'
#' @section Column definitions:
#'  \describe{
#'   \item{speciesFAOCommercial}{character() FAO code for species (ASFIS)}
#'   \item{speciesCategoryCommercial}{character() code for species category (several codes may code the same species or stock, and some species may be recorded only at higher taxonomic classifications)}
#'   \item{commonNameCommercial}{character() common name used for species category in trade documents}
#'   \item{year}{integer() Year of catch}
#'   \item{catchDate}{POSIXct() Date of catch (last catch on trip) in UTC}
#'   \item{gear}{character() Code for gear used for catch (dominant gear for trip)}
#'   \item{gearDescription}{character() Descriptive text for column 'gear'}
#'   \item{area}{character() Area code for the position of catch (dominant area for trip)}
#'   \item{location}{character() Location code (subdivision of 'Area') for the position of catch (dominant area for trip)}
#'   \item{icesAreaGroup}{character() Area code for the position of catch (dominant area for trip), based on different levels of the ICES spatial coding system}
#'   \item{coastal}{character() code indidcating whether catch was taken within coastal delimitation line (dominant side for trip)}
#'   \item{coastalDescription}{character() Descriptive text for column 'coastal'}
#'   \item{n62Code}{character() Code indidcating whether catch was taken north or south of 62 deg. Lat. (dominant side for trip)}
#'   \item{n62Description}{character() Descriptive text indidcating whether catch was taken north or south of 62 deg. Lat. (dominant side for trip)}
#'   \item{vesselLength}{numeric() Maximal length of vessel in meters}
#'   \item{countryVessel}{character() Country of the vessel that caugth the catch}
#'   \item{landingSite}{character() Code identifying landing site (buyer of catch)}
#'   \item{countryLanding}{character() Country where catch was landed}
#'   \item{usage}{character() Code for market usage of catch.}
#'   \item{usageDescription}{character() Descriptive text for column 'usage'}
#'   \item{weight}{numeric() Weight of round catch in kg. Round weight may be estimated from post-processing weights.}
#'  }
#'  
#' @section Correspondance to other formats:
#'  Correspondances indicate which field a value is derived from, not necessarily verbatim copied.
#' 
#'  Correspondance to LandingData (http://www.imr.no/formats/landinger/v2):
#'  \describe{
#'   \item{speciesFAOCommercial}{ArtFAO_kode}
#'   \item{speciesCategoryCommercial}{Art_kode}
#'   \item{commonNameCommercial}{Art_bokmål}
#'   \item{year}{Fangstår}
#'   \item{catchDate}{SisteFangstdato}
#'   \item{gear}{Redskap_kode}
#'   \item{gearDescription}{Redskap_bokmål}
#'   \item{area}{Hovedområde_kode}
#'   \item{location}{Lokasjon_kode}
#'   \item{icesAreaGroup}{Områdegruppering_bokmål}
#'   \item{coastal}{KystHav_kode}
#'   \item{coastalDescription}{KystHav_kode}
#'   \item{n62Code}{NordSørFor62GraderNord}
#'   \item{n62Description}{NordSørFor62GraderNord}
#'   \item{vesselLength}{StørsteLengde}
#'   \item{countryVessel}{Fartøynasjonalitet_kode}
#'   \item{landingSite}{Mottaksstasjon}
#'   \item{countryLanding}{Landingsnasjon_kode}
#'   \item{usage}{HovedgruppeAnvendelse_kode}
#'   \item{usageDescription}{HovedgruppeAnvendelse_bokmål}
#'   \item{weight}{Rundvekt}
#'  }
#'
#' @seealso \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxData}}
#' 
#' @name StoxLandingData
#'
NULL
