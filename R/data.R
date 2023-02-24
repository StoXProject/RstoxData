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
#'   \item{\code{nmdbioticv3.1.xsd}}{List NMD Biotic Format v3.1}
#'   \item{\code{nmdechosounderv1.xsd}}{List NMD Echosounder Format v1}
#' }
#' @source \url{https://www.imr.no/formats}
"xsdObjects"

#' @title stoxBioticObject
#' @description Pre-processed objects for raw XML data to StoXBiotic format
"stoxBioticObject"

##################################################
##################################################
#' General parameters of RstoxData.
#' 
#' All functions referring to a project, a model, a process or an output table use the same parameters, listed here.
#' 
#' @param processData The current data produced by a previous instance of the function.
#' @param UseProcessData Logical: If TRUE use the existing function output in the process. 
#' @param NumberOfCores The number of cores to use (defaulted to 1), truncated to the number of avaliable cores.
#' 
#' @name general_arguments
#' 
NULL


##################################################
##################################################
#' General sampling hierarchy of StoX
#' 
#' The general sampling hierarchy of StoX defines a common hierarchy of sampling levels for the StoxBiotic and StoxAcoustic data formats. 
#' 
#' @details The general sampling hierarchy of StoX is defined by 6 levels (tables) as shown alongside the levels of the StoxcBiotic and StoxAcoustic format in the following table:
#' 
#' \tabular{lll}{
#' General level \tab StoxBiotic level \tab StoxAcoustic level\cr
#' Cruise \tab Cruise \tab Cruise\cr
#' Station \tab Station \tab Log\cr
#' Equipment \tab Haul \tab Beam\cr
#' Species \tab SpeciesCategory \tab AcousticCategory\cr
#' Sample \tab Sample \tab ChannelReference\cr
#' Individual \tab Individual \tab NASC
#' }
#' 
#' The levels can be interpreted as follows: 
#' 
#' (1) The Cruise level is the entire trip or mission conducted by a platform, such as a research vessel. 
#' 
#' (2) The Station level is a geographical position at a specific point in time where sampling is conducted. 
#' 
#' (3) The Equipment level specifies the equipment used to sample, possibly several equipments at the same station, such as two different trawls or  different acoustic instruments or acoustic frequencies. 
#' 
#' (4) The Species level is the biological species or acoustic category (normally reflecting one or more biological species) sampled by the equipment. 
#' 
#' (5) The Sample level is the specific sample of the Species, such as herring or cod for StoxBiotic. For StoxAcoustic the Sample level denotes different coordinate systems in which the acoustic data are defined, with possible values "P" for pelagic channels defined by origin at the surface and z axis pointing vertically downwards, and "B" for bottom referenced channels with origin on the seabed and z axis pointing vertically upwards. 
#' 
#' (6) The Individual level contains for the StoxBiotic format the individuals selected for specific measurements of individual properties such as length, weight and gender, whereas for StoxAcoustic the indiivdual samples along an acouostic beam.
#' 
#' @name generalSamplingHierarhcy
#' 
NULL


##################################################
##################################################
#' StoxBiotic data format.
#' 
#' The StoxBiotic data format is defined by StoX as a common format to which data from different biotic sampling formats are converted, guaranteeing consistent interpretation and documentation of all its variables. 
#' 
#' @details The StoxBiotic format is defined according to the \code{\link[=generalSamplingHierarhcy]{general sampling hierarchy of StoX}} which is used as a basis for both the StoxcBiotic and StoxAcoustic format. The variables of the StoxBiotic format are given by the tables below:
#' 
#' \bold{Cruise level}:
#' \tabular{lllll}{
#' \bold{Variable} \tab \bold{Description} \tab \bold{Unit} \tab \bold{Data type} \tab \bold{Example} \cr
#' CruiseKey \tab Key of the Cruise table \tab None \tab Character \tab 2021105 \cr
#' Cruise \tab Unique Cruise identifier ("/" separated concatenation of cruise, missiontype, startyear, platform and missionnumber for NMDBiotic and LocalID for ICESBiotic) \tab None \tab Character \tab 2021105 \cr
#' Platform  \tab Data collection platform identifier \tab None \tab Character \tab 1019 \cr
#' }
#' 
#' 
#' \bold{Station level}:
#' \tabular{lllll}{
#' \bold{Variable} \tab \bold{Description} \tab \bold{Unit} \tab \bold{Data type} \tab \bold{Example} \cr
#' StationKey \tab Key of the Station level \tab None \tab 1 \cr
#' Station \tab Unique Station identifier \tab None \tab 2021105-1 \cr   
#' CatchPlatform \tab Platform performing the actual sampling (can be different from the data collection platform) \tab None \tab Character \tab 1019 \cr
#' DateTime \tab UTC time at start of the station \tab ISO 8601 format with milliseconds \tab Character \tab 2020-09-09T01:02:03.456Z \cr
#' Longitude \tab Longitude at start of the station \tab Decimal degrees \tab Numeric \tab 62.5 \cr
#' Latitude \tab Latitude at start of the station \tab Decimal degrees \tab Numeric \tab 5.1 \cr
#' BottomDepth \tab BottomDepth at the station, given directly in ICESBitoic and calculated as the average of bottom depth at start and end of the station in NMDBiotic \tab m \tab Numeric \tab 123.2 \cr
#' }
#' 
#' 
#' \bold{Haul level}:
#' \tabular{lllll}{
#' \bold{Variable} \tab \bold{Description} \tab \bold{Unit} \tab \bold{Data type} \tab \bold{Example} \cr
#' HaulKey \tab Key of the Haul level \tab None \tab Character \tab 2 \cr
#' Haul \tab Unique Haul identifier \tab None \tab Character \tab 2021105-1-2 \cr
#' Gear \tab Identifier of the gear \tab None \tab Character \tab 3270 \cr
#' TowDistance \tab Distance between start and end of the haul \tab Nautical miles \tab Numeric \tab 1.5 \cr
#' EffectiveTowDistance \tab Effective tow distance \tab Nautical miles \tab Numeric \tab 1.5 \cr
#' MinHaulDepth \tab Minimum depth of the haul (trawl headline) \tab m \tab Numeric \tab 65 \cr
#' MaxHaulDepth \tab Maximum depth of the haul (trawl headline) \tab m \tab Numeric \tab 35 \cr
#' VerticalNetOpening \tab Vertical span of the net \tab m \tab Numeric \tab 23 \cr
#' HorizontalNetOpening \tab Vertical span of the net \tab m \tab Numeric \tab 105 \cr
#' TrawlDoorSpread \tab Distance between the trawl doors \tab m \tab Numeric \tab 125 \cr
#' }
#' 
#' 
#' \bold{SpeciesCategory level}:
#' \tabular{lllll}{
#' \bold{Variable} \tab \bold{Description} \tab \bold{Unit} \tab \bold{Data type} \tab \bold{Example} \cr
#' SpeciesCategoryKey \tab Key of the SpeciesCategory level \tab None \tab Character \tab 126417 \cr
#' SpeciesCategory \tab The species category \tab None \tab Character \tab Herring \cr
#' }
#' 
#' 
#' \bold{Sample level}:
#' \tabular{lllll}{
#' \bold{Variable} \tab \bold{Description} \tab \bold{Unit} \tab \bold{Data type} \tab \bold{Example} \cr
#' SampleKey \tab Key of the Sample level \tab None \tab Character \tab  \cr          
#' Sample \tab Unique Sample identifier \tab None \tab Character \tab  \cr             
#' CatchFractionWeight \tab Total weight of the catch SpeciesCategory and sub category (fractions such as juveniles and adults) \tab Kg \tab Numeric \tab 49.9 \cr
#' CatchFractionNumber \tab Total number of individuals of the catch SpeciesCategory and sub category (fractions such as juveniles and adults) \tab None \tab Numeric \tab 295 \cr 
#' SampleWeight \tab Total weight of the sample for individual measurements \tab Kg \tab Numeric \tab 4.6 \cr       
#' SampleNumber \tab Size of the sample for individual measurements \tab None \tab Numeric \tab 100 \cr
#' }
#' 
#' 
#' \bold{Individual level}:
#' \tabular{lllll}{
#' \bold{Variable} \tab \bold{Description} \tab \bold{Unit} \tab \bold{Data type} \tab \bold{Example} \cr
#' IndividualKey \tab Key of the Individual level \tab None \tab Character \tab  \cr        
#' Individual \tab Unique Individual identifier \tab None \tab Character \tab  \cr           
#' IndividualRoundWeight \tab Round weight (the whole fish) fo the individual \tab g \tab Numeric \tab 123 \cr
#' IndividualTotalLength \tab Total length (from snout to end of fin), given as the lower end of the interval of width given by LengthResolution \tab cm \tab Numeric \tab 14.5 \cr
#' LengthResolution \tab Resolution of IndividualTotalLength \tab cm \tab Numeric \tab 0.5 \cr     
#' WeightMeasurement \tab \tab None \tab Character \tab  \cr    
#' IndividualAge \tab Age of an individual \tab year \tab Numeric \tab 3 \cr        
#' IndividualSex \tab sex of an individual \tab F is female, M is male \tab Character \tab F \cr
#' }
#' 
#' @name StoxBioticFormat
#' 
NULL

##################################################
##################################################
#' StoX data types of the RstoxData package
#' 
#' StoX data types are the data types used to transfer data and information between processes in a StoX estimation model. The data types are divided into two types, the \code{\link{ModelData}} and \code{\link{ProcessData}}.
#' 
#' @name DataTypes
#' 
NULL

##################################################
##################################################
#' Model data used by RstoxData
#' 
#' The model data of a StoX model are the data generated during the model run based on input data and user settings and resources given in the project description (project.json file). Model data are transient and only exists from a process has been run until the project is closed.
#' 
#' @param BioticData \code{\link{BioticData}}.
#' @param AcousticData \code{\link{AcousticData}}.
#' @param LandingData \code{\link{LandingData}}.
#' @param StoxBioticData \code{\link{StoxBioticData}}.
#' @param StoxAcousticData \code{\link{StoxAcousticData}}.
#' @param StoxLandingData \code{\link{StoxLandingData}}.
#' @param MergeStoxBioticData \code{\link{MergeStoxBioticData}}.
#' @param MergeStoxAcousticData \code{\link{MergeStoxAcousticData}}.
#' @param ICESBioticData \code{\link{ICESBioticData}}.
#' @param ICESAcousticData \code{\link{ICESAcousticData}}.
#' @param ICESDatrasData \code{\link{ICESDatrasData}}.
#' @param WriteICESBioticData \code{\link{WriteICESBioticData}}.
#' @param WriteICESAcousticData \code{\link{WriteICESAcousticData}}.
#' @param WriteICESDatrasData \code{\link{WriteICESDatrasData}}.
#'
#' @name ModelData
#' 
NULL


##################################################
##################################################
#' Process data used by RstoxData
#' 
#' The process data of a StoX model are data that are saved to the project description (project.json file), which in the case of RstoxData is only the \code{\link{Translation}} data type. 
#' 
#' @param Translation \code{\link{Translation}}.
#' 
#' @name ProcessData
#' 
NULL


##################################################
##################################################
#' StoX data type BioticData
#' 
#' Biotic data read from biotic xml files.
#' 
#' @details
#' This StoX data type is produced by \code{\link{ReadBiotic}}, and contains one list per input biotic file holding the tables read from each file, added a table named "metadata" holding the input file path and format. Currently supported are NMDBiotic1.4 (\url{https://www.imr.no/formats/nmdbiotic/v1.4/}), NMDBiotic3.0 (\url{https://www.imr.no/formats/nmdbiotic/v3/}), and ICESBiotic (\url{https://ices.dk/data/data-portals/Pages/acoustic.aspx}, click on "Acoustic data format" to download the format description).
#' 
#' @seealso \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxData}}
#' 
#' @name BioticData
NULL


##################################################
##################################################
#' StoX data type StoxBioticData
#' 
#' Biotic data stored in the \code{\link[=StoxBioticFormat]{StoxBiotic format}}, which contains the variables needed for most estimation models used by StoX.
#' 
#' @details
#' This StoX data type is produced by \code{\link{StoxBiotic}}, and contains the tables Cruise, Station, Haul, SpeciesCategory, Sample and Individual in that hierarchical order.
#' 
#' @seealso \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxData}}
#' 
#' @name StoxBioticData
#' 
NULL


##################################################
##################################################
#' StoX data type MergeStoxBioticData
#' 
#' Merged \code{\link{StoxBioticData}}.
#' 
#' @details
#' This StoX data type is produced by \code{\link{MergeStoxBiotic}}, and contains one merged table of \code{\link{StoxBioticData}}.
#' 
#' @seealso \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxData}}
#' 
#' @name MergeStoxBioticData
#' 
NULL


##################################################
##################################################
#' StoX data type AcousticData
#' 
#' Biotic data read from biotic xml files.
#' 
#' @details
#' This StoX data type is produced by \code{\link{ReadAcoustic}}, and contains one list per input acoustic file holding the tables read from each file, added a table named "metadata" holding the input file path and format. Currently supported are NMDEchosounder1 (\url{https://www.imr.no/formats/nmdechosounder/v1/}), and ICESAcoustic (\url{https://ices.dk/data/data-portals/Pages/acoustic.aspx}, click on "Acoustic data format" to download the format description). 
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
#' This StoX data type is produced by \code{\link{StoxAcoustic}}, and contains the tables Cruise, Log, Beam, AcousticCategory, ChannelReference and NASC in that hierarchical order.
#' 
#' @seealso \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxData}}
#' 
#' @name StoxAcousticData
#' 
NULL


##################################################
##################################################
#' StoX data type MergeStoxAcousticData
#' 
#' Merged \code{\link{StoxAcousticData}}.
#' 
#' @details
#' This StoX data type is produced by \code{\link{MergeStoxAcoustic}}, and contains one merged table of \code{\link{StoxAcousticData}}.
#' 
#' @seealso \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxData}}
#' 
#' @name MergeStoxAcousticData
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
#' list with one member for each sales-note set. 
#' Each member is a list of \code{\link[data.table]{data.table}} 
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
#' Contains a list with one element 'Landing', described below.
#'
#' 'Landing' is a \code{\link[data.table]{data.table}} with aggregated weight of landings from landing records.
#' Columns are specified in the section Column definitions Landing
#'
#' @section Column definitions Landing:
#'  \describe{
#'   \item{Species}{character() code for species category (species identified by market or regulation standards. Several codes may code the same species or stock, and some catch may be recorded only at higher taxonomic classifications)}
#'   \item{Year}{integer() Year of catch}
#'   \item{CatchDate}{POSIXct() Date of catch (last catch on trip) in UTC}
#'   \item{Gear}{character() Code for gear used for catch (dominant gear for trip)}
#'   \item{Area}{character() Area code for the position where the catch was caught (dominant area for trip)}
#'   \item{SubArea}{character() Subdivision of area code for the position where the catch was caught (dominant area for trip)}
#'   \item{Coastal}{character() Code indicating whether catch was taken within coastal delimitation line (dominant side for trip)}
#'   \item{N62Code}{character() Code indicating whether catch was taken north or south of 62 deg. Lat. (dominant side for trip)}
#'   \item{VesselLength}{character() Length of vessel in m}
#'   \item{CountryVessel}{character() Country of the vessel that caught the catch}
#'   \item{LandingSite}{character() Code identifying landing site (buyer of catch)}
#'   \item{CountryLanding}{character() Country where catch was landed}
#'   \item{Usage}{character() Code for market usage of catch.}
#'   \item{RoundWeight}{numeric() Weight of round catch in kg.}
#'  }
#'
#' @seealso \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxData}}
#' 
#' @name StoxLandingData
#'
NULL


##################################################
##################################################
#' StoX data type ICESAcousticData
#' 
#' Acoustic data stored in the ICESAcoustic (CSV) format.
#' 
#' @details
#' This StoX data type is produced by \code{\link{ICESAcoustic}}, and contains one list per input biotic file read to produec the input to \code{\link{ICESAcoustic}}, each holding the tables Instrument, Calibration, DataAcquisition, DataProcessing, Cruise and Data (here Data is a table merged from Log, Sample and Data of the ICESAocustic xml format). Each file read to produec the input to \code{\link{ICESAcoustic}} 
#' 
#' @seealso \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxData}}
#' 
#' @name ICESAcousticData
#' 
NULL

##################################################
##################################################
#' StoX data type ICESBioticData
#' 
#' Biotic data stored in the ICESBiotic (CSV) format.
#' 
#' @details
#' This StoX data type is produced by \code{\link{ICESBiotic}}, and contains one list per input biotic file read to produec the input to \code{\link{ICESBiotic}}, each holding the tables Cruise, Haul, Catch and Biology, in that hierarchical order.
#' 
#' @seealso \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxData}}
#' 
#' @name ICESBioticData
#' 
NULL

##################################################
##################################################
#' StoX data type ICESDatrasData
#' 
#' Biotic data stored in the ICESDatras (CSV) format.
#' 
#' @details
#' This StoX data type is produced by \code{\link{ICESDatras}}, and contains one list per input biotic file read to produec the input to \code{\link{ICESDatras}}, each holding the tables HH, HL and CA, in that hierarchical order.
#' 
#' @seealso \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxData}}
#' 
#' @name ICESDatrasData
#' 
NULL



##################################################
##################################################
#' Rbind \code{\link{ICESAcousticData}} to a string matrix. 
#' 
#' The output of this function is suited for submission to \url{https://acoustic.ices.dk/}.
#' 
#' @details
#' The ICESAcoustic CSV format is one string matrix containing all tables of \code{\link{ICESAcousticData}}, where column names are inclcuded as header rows.
#' 
#' @seealso \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxData}}
#' 
#' @name WriteICESAcousticData
#' 
NULL

##################################################
##################################################
#' Rbind \code{\link{ICESBioticData}} to a string matrix. 
#' 
#' The output of this function is suited for submission to \url{https://acoustic.ices.dk/}.
#' 
#' @details
#' The ICESBiotic CSV format is one string matrix containing all tables of \code{\link{ICESBioticData}}, where column names are inclcuded as header rows.
#' 
#' @seealso \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxData}}
#' 
#' @name WriteICESBioticData
#' 
NULL

##################################################
##################################################
#' Rbind \code{\link{ICESDatrasData}} to a string matrix. 
#' 
#' The output of this function is suited for submission to \url{https://www.ices.dk/data/data-portals/Pages/DATRAS.aspx}.
#' 
#' @details
#' The ICESDatras CSV format is one string matrix containing all tables of \code{\link{ICESDatrasData}}, where column names are inclcuded as header rows.
#' 
#' @seealso \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxData}}
#' 
#' @name WriteICESDatrasData
#' 
NULL


##################################################
##################################################
#' Translation definition (from file or from table).
#' 
#' @details
#' This StoX data type is produced by \code{\link{DefineTranslation}}, and contains the columns VariableName, Value and NewValue.
#' 
#' @seealso \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxData}}
#' 
#' @name Translation
#' 
NULL


