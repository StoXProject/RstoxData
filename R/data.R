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
#' @seealso The general sampling hierarchy is used in the format \code{\link{StoxBioticFormat}} and \code{\link{StoxAcousticFormat}}.
#' 
NULL


##################################################
##################################################
#' StoxBiotic data format.
#' 
#' The StoxBiotic data format is defined by StoX as a common format to which data from different biotic sampling formats are converted, guaranteeing consistent interpretation and documentation of all its variables. 
#' 
#' @details The StoxBiotic format is defined according to the \code{\link[=generalSamplingHierarhcy]{general sampling hierarchy of StoX}} which is used as a basis for both the StoxcBiotic and StoxAcoustic format. The variables of the StoxBiotic format are given by the tables below, where the columns NMDBiotic and ICESBiotic show the origin of variable in BioticData read from NMDBiotic \url{https://www.imr.no/formats/nmdbiotic/} and ICESBiotic \url{https://www.ices.dk/data/data-portals/Pages/acoustic.aspx} files, respectively (see \code{\link{BioticData}}). Variables with non-unique names across tables of the BioticData are written as variableName.tableName, and "-" and "/" denotes concatenation for character type variables with "-" and "/" as separation character, respectively. Units are given with shortname of the \code{\link{StoxUnits}}:
#' 
#' 
#' \bold{Cruise level}:
#' \tabular{lllllll}{
#' \bold{Variable} \tab \bold{Description} \tab \bold{Unit} \tab \bold{Data type} \tab \bold{Example} \tab \bold{NMDBiotic} \tab \bold{ICESBiotic} \cr
#' CruiseKey \tab Key of the Cruise table \tab None \tab Character \tab "2021105" \tab \code{cruise/missiontype/startyear/platform/missionnumber} \tab \code{LocalID} \cr
#' Cruise \tab Unique Cruise identifier \tab None \tab Character \tab "2021105" \tab Same as CruiseKey. Can be translated. \tab Same as CruiseKey. Can be translated. \cr
#' Platform  \tab Data collection platform identifier \tab None \tab Character \tab "1019" \tab \code{platform} \tab \code{Platform} \cr
#' }
#' 
#' 
#' \bold{Station level}:
#' \tabular{lllllll}{
#' \bold{Variable} \tab \bold{Description} \tab \bold{Unit} \tab \bold{Data type} \tab \bold{Example} \tab \bold{NMDBiotic} \tab \bold{ICESBiotic} \cr
#' StationKey \tab Key of the Station level \tab None \tab Character \tab "1" \tab \code{station} \tab \code{StationName} \cr
#' Station \tab Unique Station identifier \tab None \tab Character \tab "2021105-1" \tab \code{CruiseKey-StationKey} \tab \code{CruiseKey-StationKey} \cr   
#' CatchPlatform \tab Platform performing the actual sampling (can be different from the data collection platform) \tab None \tab Character \tab "1019" \tab \code{catchplatform} (\code{platform.fishstation} for NMDBiotic 1.1 and 1.4) \tab \code{Platform} \cr
#' DateTime \tab UTC time at start of the station, stored as \code{\link{POSIXct}} \tab ISO8601 \tab Character \tab 2020-09-09T01:02:03.456Z \tab \code{stationstartdate + stationstarttime} (\code{startdate.fishstation + starttime.fishstation} for NMDBiotic 1.1 and 1.4) \tab \code{StartTime} \cr
#' Longitude \tab Longitude at start of the station \tab degree east \tab Numeric \tab 62.5 \tab \code{longitudestart} \tab \code{StartLongitude} \cr
#' Latitude \tab Latitude at start of the station \tab degree north \tab Numeric \tab 5.1 \tab \code{latitudestart} \tab \code{StartLatitude} \cr
#' BottomDepth \tab BottomDepth at the station, given directly in ICESBitoic files and calculated as the average of bottom depth at start and end of the station in NMDBiotic files \tab m \tab Numeric \tab 123.2 \tab \code{(bottomdepthstart + bottomdepthstop) / 2} \tab \code{BottomDepth} \cr
#' }
#' 
#' 
#' \bold{Haul level}:
#' \tabular{lllllll}{
#' \bold{Variable} \tab \bold{Description} \tab \bold{Unit} \tab \bold{Data type} \tab \bold{Example} \tab \bold{NMDBiotic} \tab \bold{ICESBiotic} \cr
#' HaulKey \tab Key of the Haul level \tab None \tab Character \tab "2" \tab \code{serialnumber} (\code{serialno} for NMDBiotic 1.1 and 1.4) \tab \code{Number} \cr
#' Haul \tab Unique Haul identifier \tab None \tab Character \tab "2021105-1-2" \tab \code{CruiseKey-StationKey-HaulKey} \tab \code{CruiseKey-StationKey-HaulKey} \cr
#' Gear \tab Identifier of the gear \tab None \tab Character \tab "3270" \tab \code{gear} \tab \code{Gear} \cr
#' TowDistance \tab Distance between start and end of the haul \tab nmi \tab Numeric \tab 1.5 \tab \code{distance} \tab \code{Distance / 1852} \cr
#' EffectiveTowDistance \tab Effective tow distance \tab nmi \tab Numeric \tab 1.5 \tab Same as TowDistance. Can be translated. \tab Same as TowDistance. Can be translated. \cr
#' MinHaulDepth \tab Minimum depth of the haul (trawl headline) \tab m \tab Numeric \tab 65 \tab \code{fishingdepthmin} \tab \code{MinTrawlDepth} \cr
#' MaxHaulDepth \tab Maximum depth of the haul (trawl headline) \tab m \tab Numeric \tab 35 \tab \code{fishingdepthmax} \tab \code{MaxTrawlDepth} \cr
#' VerticalNetOpening \tab Vertical span of the net \tab m \tab Numeric \tab 23 \tab \code{verticaltrawlopening} (\code{trawlopening} for NMDBiotic 1.1 and 1.4) \tab \code{Netopening} \cr
#' HorizontalNetOpening \tab Vertical span of the net \tab m \tab Numeric \tab 105 \tab \code{wingspread} (missing (\code{NA}) for NMDBiotic 1.1 and 1.4) \tab \code{WingSpread} \cr
#' TrawlDoorSpread \tab Distance between the trawl doors \tab m \tab Numeric \tab 125 \tab \code{trawldoorspread} (\code{doorspread} for NMDBiotic 1.1 and 1.4) \tab \code{DoorSpread} \cr
#' }
#' 
#' 
#' \bold{SpeciesCategory level}:
#' \tabular{lllllll}{
#' \bold{Variable} \tab \bold{Description} \tab \bold{Unit} \tab \bold{Data type} \tab \bold{Example} \tab \bold{NMDBiotic} \tab \bold{ICESBiotic} \cr
#' SpeciesCategoryKey \tab Key of the SpeciesCategory level \tab None \tab Character \tab "sild'G03/161722.G03/126417/Clupea harengus" \tab \code{commonname/catchcategory/aphia/scientificname} (\code{noname/species/aphia/group} for NMDBiotic 1.1 and 1.4) \tab \code{SpeciesCode} \cr
#' SpeciesCategory \tab The species category \tab None \tab Character \tab "Herring" \tab Same as SpeciesCategoryKey. Can be translated. \tab Same as SpeciesCategoryKey. Can be translated. \cr
#' }
#' 
#' 
#' \bold{Sample level}:
#' \tabular{lllllll}{
#' \bold{Variable} \tab \bold{Description} \tab \bold{Unit} \tab \bold{Data type} \tab \bold{Example} \tab \bold{NMDBiotic} \tab \bold{ICESBiotic} \cr
#' SampleKey \tab Key of the Sample level \tab None \tab Character \tab "1" \tab \code{catchsampleid} (\code{samplenumber} for NMDBiotic 1.1 and 1.4) \tab \code{SpeciesCategory} \cr          
#' Sample \tab Unique Sample identifier \tab None \tab Character \tab "2021105-1-2-sild'G03/161722.G03/126417/Clupea harengus-1" \tab \code{CruiseKey-StationKey-HaulKey-SpeciesCategoryKey-SampleKey} \tab \code{CruiseKey-StationKey-HaulKey-SpeciesCategoryKey-SampleKey} \cr             
#' CatchFractionWeight \tab Total weight of the catch for the SpeciesCategory and sub category (e.g., fractions such as juveniles and adults) \tab kg \tab Numeric \tab 49.9 \tab \code{catchweight} (missing (\code{NA}) if \code{catchproducttype != 1}) (\code{weight} (missing (\code{NA}) if \code{producttype != 1}) for NMDBiotic 1.1 and 1.4) \tab \code{SpeciesCategoryWeight} (taking \code{WeightUnit} into account) \cr
#' CatchFractionNumber \tab Total number of individuals of the catch for the SpeciesCategory and sub category (e.g., fractions such as juveniles and adults) \tab individuals \tab Integer \tab 295 \tab \code{catchcount} (\code{count} for NMDBiotic 1.1 and 1.4) \tab \code{SpeciesCategoryNumber} \cr 
#' SampleWeight \tab Total weight of the sample for individual measurements \tab kg \tab Numeric \tab 4.6 \tab \code{lengthsampleweight} (missing (\code{NA}) if \code{sampleproducttype != 1}) \tab \code{SubsampleWeight} (taking \code{WeightUnit} into account) \cr       
#' SampleNumber \tab Size of the sample for individual measurements \tab individuals \tab Integer \tab 100 \tab \code{lengthsamplecount} \tab \code{SubsampledNumber} \cr
#' }
#' 
#' 
#' \bold{Individual level}:
#' \tabular{lllllll}{
#' \bold{Variable} \tab \bold{Description} \tab \bold{Unit} \tab \bold{Data type} \tab \bold{Example} \tab \bold{NMDBiotic} \tab \bold{ICESBiotic} \cr
#' IndividualKey \tab Key of the Individual level \tab None \tab Character \tab "2" \tab \code{specimenid} (\code{specimenno} for NMDBiotic 1.1 and 1.4) \tab \code{FishID} \cr        
#' Individual \tab Unique Individual identifier \tab None \tab Character \tab "2021105-1-2-sild'G03/161722.G03/126417/Clupea harengus-1-2" \tab \code{CruiseKey-StationKey-HaulKey-SpeciesCategoryKey-SampleKey-IndividualKey} \tab \code{CruiseKey-StationKey-HaulKey-SpeciesCategoryKey-SampleKey-IndividualKey} \cr           
#' IndividualRoundWeight \tab Round weight (the whole fish) fo the individual \tab g \tab Numeric \tab 123 \tab \code{individualweight * 1000} (missing (\code{NA}) if \code{individualproducttype != 1}) (\code{weight.individual} (missing (\code{NA}) if \code{producttype.individual != 1}) for NMDBiotic 1.1 and 1.4) \tab \code{IndividualWeight} (taking \code{WeightUnit.Biology} into account) \cr
#' IndividualTotalLength \tab Total length (from snout to end of fin), given as the lower end of the interval of width given by LengthResolution \tab cm \tab Numeric \tab 14.5 \tab \code{length * 100}  (missing (\code{NA}) if \code{lengthmeasurement != "E"}) \tab \code{LengthClass.Biology} (taking \code{LengthCode.Biology} into account) \cr
#' LengthResolution \tab Resolution of IndividualTotalLength \tab cm \tab Numeric \tab 0.5 \tab \code{lengthresolution} (\code{lengthunit} for NMDBiotic 1.1 and 1.4) converted to cm \tab \code{LengthCode.Biology} converted to cm \cr     
#' WeightMeasurement \tab Specification of how IndividualRoundWeight was measured; one of "IndividualWeight" and "AverageWeight". For data from NMDBiotic files this is always "IndividualWeight". \tab None \tab Character \tab "IndividualWeight" \tab "IndividualWeight" \tab IndividualWeight if \code{IndividualWeight} is given, "AverageWeight" if \code{WeightAtLength} and \code{NumberAtLength} are given \cr    
#' IndividualAge \tab Age of an individual \tab year \tab Numeric \tab 3 \tab \code{age} (located in the agedetermination level) for the \code{preferredagereading} \tab \code{IndividualAge} \cr      
#' IndividualSex \tab sex of an individual \tab F is female, M is male \tab Character \tab "F" \tab "F" if \code{sex = 1}, "M" if \code{sex = 2} \tab \code{IndividualSex} \cr
#' }
#' 
#' @name StoxBioticFormat
#' 
NULL

##################################################
##################################################
#' StoxAcoustic data format.
#' 
#' The StoxAcoustic data format is defined by StoX as a common format to which data from different acoustic sampling formats are converted, guaranteeing consistent interpretation and documentation of all its variables. 
#' 
#' @details The StoxAcoustic format is defined according to the \code{\link[=generalSamplingHierarhcy]{general sampling hierarchy of StoX}} which is used as a basis for both the StoxcBiotic and StoxAcoustic format. The variables of the StoxAcoustic format are given by the tables below, where the columns NMDEchosounder and ICESAcoustic show the origin of variable in AcousticData read from NMDEchosounder \url{https://www.imr.no/formats/nmdechosounder/} and ICESAcoustic \url{https://www.ices.dk/data/data-portals/Pages/acoustic.aspx} files, respectively (see \code{\link{AcousticData}}). Variables with non-unique names across tables of the BioticData are written as variableName.tableName, and "-" and "/" denotes concatenation for character type variables with "-" and "/" as separation character, respectively. Units are given with shortname of the \code{\link{StoxUnits}}:
#' 
#' \bold{Cruise level}:
#' \tabular{lllllll}{
#' \bold{Variable} \tab \bold{Description} \tab \bold{Unit} \tab \bold{Data type} \tab \bold{Example} \tab \bold{NMDBiotic} \tab \bold{ICESBiotic} \cr
#' CruiseKey \tab Key of the Cruise table \tab None \tab Character \tab "2021105" \tab \code{cruise} \tab \code{LocalID} \cr
#' Cruise \tab Unique Cruise identifier \tab None \tab Character \tab "2021105" \tab Same as CruiseKey. Can be translated. \tab Same as CruiseKey. Can be translated. \cr
#' Platform  \tab Data collection platform identifier \tab None \tab Character \tab "1019" \tab \code{platform} \tab \code{Platform} \cr
#' }
#' 
#' 
#' \bold{Log level}:
#' \tabular{lllllll}{
#' \bold{Variable} \tab \bold{Description} \tab \bold{Unit} \tab \bold{Data type} \tab \bold{Example} \tab \bold{NMDBiotic} \tab \bold{ICESBiotic} \cr
#' LogKey \tab Key of the Log level, given as ISO 8601 date time string \tab ISO8601 \tab Character \tab "2020-04-24T03:51:26.000Z" \tab ISO 8601 formatted \code{start_time} \tab ISO 8601 formatted \code{Time} \cr
#' Log \tab Sailed distance \tab nmi \tab Numeric \tab 123.1 \tab \code{log_start} \tab \code{Distance} \cr   
#' EDSU \tab Unique elementary distance sampling unit (EDSU/Log) identifier \tab None \tab Character \tab "2020821/2020-04-24T03:51:26.000Z" \tab \code{CruiseKey-LogKey} \tab \code{CruiseKey-LogKey} \cr   
#' DateTime \tab UTC time at start of the EDSU, stored as \code{\link{POSIXct}} \tab ISO8601 \tab Character \tab 2020-09-09T01:02:03.456Z \tab \code{start_time} \tab \code{Time} \cr
#' Longitude \tab Longitude at LogOrigin \tab degree east \tab Numeric \tab 62.5 \tab \code{lon_start} \tab \code{Longitude} \cr
#' Latitude \tab Latitude at LogOrigin \tab degree north \tab Numeric \tab 5.1 \tab \code{lat_start} \tab \code{Latitude} \cr
#' LogOrigin \tab Origin of the Longitude/Latitude, one of "start", "middle", "end" \tab None \tab Character \tab "start" \tab "start" \tab \code{LogOrigin} \cr
#' Longitude2 \tab Longitude at LogOrigin2 \tab degree east \tab Numeric \tab 62.6 \tab \code{lon_stop} \tab \code{Longitude2} \cr
#' Latitude2 \tab Latitude at LogOrigin2 \tab degree north \tab Numeric \tab 5.2 \tab \code{lat_stop} \tab \code{Latitude2} \cr
#' LogOrigin2 \tab Origin of the Longitude/Latitude, one of "start", "middle", "end" \tab None \tab Character \tab "end" \tab "end" \tab \code{LogOrigin2} \cr
#' LogDistance \tab The length of the EDSU \tab nmi \tab Numeric \tab 0.1 \tab \code{integrator_dist} \tab \code{PingAxisInterval} \cr
#' LogDuration \tab The duration of the EDSU \tab s \tab Numeric \tab 36 \tab \code{stop_time - start_time} \tab \code{NA} \cr
#' EffectiveLogDistance \tab The effective length of the EDSU \tab nmi \tab Numeric \tab 0.09 \tab Same as LogDistance. \tab Same as LogDistance \cr
#' BottomDepth \tab BottomDepth at the EDSU \tab m \tab Numeric \tab 123.2 \tab \code{NA} \tab \code{BottomDepth} \cr
#' }
#' 
#' 
#' \bold{Beam level}:
#' \tabular{lllllll}{
#' \bold{Variable} \tab \bold{Description} \tab \bold{Unit} \tab \bold{Data type} \tab \bold{Example} \tab \bold{NMDBiotic} \tab \bold{ICESBiotic} \cr
#' BeamKey \tab Key of the Beam level \tab None \tab Character \tab "38000/2" \tab \code{freq/transceiver} \tab \code{ID.Instrument} \cr
#' Beam \tab Unique Beam identifier \tab None \tab Character \tab "38000/2" \tab Same as BeamKey. Can be translated. \tab Same as BeamKey. Can be translated. \cr
#' Frequency \tab The acoustic frequency of the Beam \tab hertz \tab Numeric \tab 38000 \tab \code{freq} \tab \code{Frequency} \cr
#' }
#' 
#' 
#' \bold{AcousticCategory level}:
#' \tabular{lllllll}{
#' \bold{Variable} \tab \bold{Description} \tab \bold{Unit} \tab \bold{Data type} \tab \bold{Example} \tab \bold{NMDBiotic} \tab \bold{ICESBiotic} \cr
#' AcousticCategoryKey \tab Key of the AcousticCategory level \tab None \tab Character \tab "HER" \tab \code{acocat} \tab \code{SaCategory} \cr
#' AcousticCategory \tab The acousic category \tab None \tab Character \tab "Herring" \tab Same as AcousticCategoryKey Can be translated. \tab Same as AcousticCategoryKey Can be translated. \cr
#' }
#' 
#' 
#' \bold{ChannelReference level}:
#' \tabular{lllllll}{
#' \bold{Variable} \tab \bold{Description} \tab \bold{Unit} \tab \bold{Data type} \tab \bold{Example} \tab \bold{NMDBiotic} \tab \bold{ICESBiotic} \cr
#' ChannelReferenceKey \tab Key of the ChannelReference level \tab None \tab Character \tab "P" \tab \code{type} \tab "P" \cr
#' ChannelReferenceType \tab Unique ChannelReference identifier \tab None \tab Character \tab "P" \tab Same as ChannelReferenceKey \tab Same as ChannelReferenceKey \cr             
#' ChannelReferenceDepth \tab The depth of the ChannelReference origin. 0 for pelagic channels. Not yet given for bottom channels, as BottomDepth is not yet defined for NMDEchosounder data \tab m \tab Numeric \tab 0 \tab 0 if \code{ChannelReferenceType == "P"}, \code{NA} if \code{ChannelReferenceType == "B"} (see BottomDepth) \tab 0 \cr
#' ChannelReferenceTilt \tab The tilt angle of the beam, where 180 is vertically downwards and 0 is vertically upwards \tab degree \tab Numeric \tab 180 \tab 180 if \code{ChannelReferenceType == "P"}, 0 if \code{ChannelReferenceType == "B"} \tab Interpreted from \code{TransducerOrientation} \cr 
#' }
#' 
#' 
#' \bold{NASC level}:
#' \tabular{lllllll}{
#' \bold{Variable} \tab \bold{Description} \tab \bold{Unit} \tab \bold{Data type} \tab \bold{Example} \tab \bold{NMDBiotic} \tab \bold{ICESBiotic} \cr
#' NASCKey \tab Key of the NASC level \tab None \tab Character \tab "2" \tab \code{ch} \tab \code{ChannelDepthUpper/ChannelDepthLower} \cr
#' Channel \tab Unique NASC depth channel identifier \tab None \tab Character \tab "2" \tab Same as NASCKey Can be translated. \tab Same as NASCKey Can be translated. \cr           
#' MaxChannelRange \tab The maximum range of the channel. Translates to the lower channel depth for vertically downwards oriented echosounder. \tab m \tab Numeric \tab 40 \tab \code{pel_ch_thickness * ch} \tab \code{MaxChannelRange} \cr
#' MinChannelRange \tab The minimum range of the channel. Translates to the upper channel depth for vertically downwards oriented echosounder. \tab m \tab Numeric \tab 30 \tab \code{pel_ch_thickness * (ch - 1)} \tab \code{MinChannelRange} \cr
#' NASC \tab The nautical area scattering coefficient. \tab m^2/nmi^2 \tab Numeric \tab 59.24813 \tab \code{sa} \tab \code{Value} \cr
#' }
#' 
#' 
#' @name StoxAcousticFormat
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
#' @param ICESDatsuscData \code{\link{ICESDatsuscData}}.
#' @param WriteICESBioticData \code{\link{WriteICESBioticData}}.
#' @param WriteICESAcousticData \code{\link{WriteICESAcousticData}}.
#' @param WriteICESDatrasData \code{\link{WriteICESDatrasData}}.
#' @param WriteICESDatsuscData \code{\link{WriteICESDatsuscData}}.
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
#' StoX data type ICESDatsuscData
#' 
#' Biotic data stored in the ICESDatsusc (CSV) format.
#' 
#' @details
#' This StoX data type is produced by \code{\link{ICESDatsusc}}, and contains one list per input biotic file read to produec the input to \code{\link{ICESDatsusc}}, each holding the tables HH, HL and CA, in that hierarchical order.
#' 
#' @seealso \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxData}}
#' 
#' @name ICESDatsuscData
#' 
NULL



##################################################
##################################################
#' Rbind \code{\link{ICESAcousticData}} to a string matrix. 
#' 
#' The output of this function is suited for submission to \url{https://acoustic.ices.dk/}.
#' 
#' @details
#' The ICESAcoustic CSV format is one string matrix containing all tables of \code{\link{ICESAcousticData}}, where column names are included as header rows.
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
#' The ICESBiotic CSV format is one string matrix containing all tables of \code{\link{ICESBioticData}}, where column names are included as header rows.
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
#' The ICESDatras CSV format is one string matrix containing all tables of \code{\link{ICESDatrasData}}, where column names are included as header rows.
#' 
#' @seealso \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxData}}
#' 
#' @name WriteICESDatrasData
#' 
NULL


##################################################
##################################################
#' Rbind \code{\link{ICESDatsuscData}} to a string matrix. 
#' 
#' The output of this function is suited for submission to \url{https://www.ices.dk/data/data-portals/Pages/Stomach-content.aspx}.
#' 
#' @details
#' The ICESDatras CSV format is one string matrix containing all tables of \code{\link{ICESDatsuscData}}, where column names are included as header rows.
#' 
#' @seealso \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxData}}
#' 
#' @name WriteICESDatsuscData
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


