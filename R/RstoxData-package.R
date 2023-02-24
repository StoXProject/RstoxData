#' Tools to Read and Manipulate Fisheries Data
#'
#' Set of tools to read and manipulate various data formats for fisheries. Mainly catered towards scientific trawl survey sampling ('biotic') data, acoustic trawl data, and commercial fishing catch ('landings') data. Among the supported data formats are the data products from the Norwegian Institute Marine Research (IMR) and the International Council for the Exploration of the Sea (ICES).
#'
#' The RstoxData package contains functions for reading, filtering and writing biotic, acoustic and landing data as XML files. Filtering can be done by R syntax such as longitude > 10, or by pre defined functions such as inside(). On computers that return errors when trying to run the Rtools through RStudio (most institutional Windows machines), install the binary directly from https://github.com/StoXProject/RstoxData/releases. Download the newest RstoxData zip file, click the "Packages" tab -> "Install" -> "Install from:" "Package Archive File" -> "Install". If the installer does not complain, the package is installed correctly.
#' @docType package
#' @name RstoxData
#'
"_PACKAGE"

# Global variables
utils::globalVariables(c(
	 ".", "..Country", "..NAToInsert", "..Organisation", "..SurveyName", "..atResolution",
	 "..colList", "..columns", "..digits", "..groupingVariables", "..keep", "..parameterNames",
	 "..relevantColumns", "..replacement", "..signifDigits", "..sourceColumns",
	 "..targetAndSourceVariables", "..toKeep", "..valueVariablesInTranslation", "..varToExtract",
	 "..variableKeys", "..variablesInTable", "..x", "AcousticCategory", "Addition", "BeamKey",
	 "CANoAtLngt", "Channel", "ChannelDepthLower", "ChannelDepthUpper", "ChannelReferenceKey",
	 "ChannelReferenceType", "Constant", "Country", "Cruise", "CruiseKey", "DateTime", "DoorType",
	 "EDSU", "EchoType", "EffectiveTowDistance", "FishID", "Frequency", "Gear", "GearEx",
	 "HLNoAtLngt", "HaulNo", "ID", "LngtClass", "LngtCode", "LocalID", "LogKey", "N", "NewValue",
	 "NumberAtLength", "NumberOfIndividualsToGenerate", "NumberOfSampledIndividuals", "Quarter",
	 "ReplaceBy", "ResolutionCode", "SaCategory", "Scaling", "Ship", "SpecVal",
	 "SpeciesCategoryNumber", "SpeciesCategoryWeight", "SpeciesCode", "StatRec", "SubsampleWeight",
	 "SubsampledNumber", "Survey", "SweepLngt", "Time", "TransducerOrientation", "Value",
	 "VariableName", "WeightMeasurement", "acocat", "age", "agingstructure", "aphia",
	 "bottomdepthstart", "bottomdepthstop", "catCatchWgt", "catchcount", "catchpartnumber",
	 "catchproducttype", "catchweight", "ch", "cruise", "currentReportingUnit", "direction",
	 "distance", "fishingdepthcount", "fishingdepthmax", "fishingdepthmin", "freq", "gear",
	 "gearflow", "inapplicableFormats", "individualweight", "integrator_dist", "iskey", "lat_start",
	 "lat_stop", "latitudeend", "latitudestart", "lengthCode", "lengthmeasurement",
	 "lengthresolution", "lengthsamplecount", "lengthsampleweight", "level", "liverweight",
	 "lngtClass", "lngtCode", "log_start", "logstart", "lon_start", "lon_stop", "longitudeend",
	 "longitudestart", "lsCountTot", "maturity", "maxFishID", "max_bot_depth", "meanW",
	 "missionstartdate", "missionstopdate", "nInd", "nation", "newClass", "newLngtCode",
	 "newLngtCodeNumeric", "newReportingUnit", "noMeas", "parasite", "pel_ch_thickness", "platform",
	 "platformname", "preferredagereading", "readability", "reportingUnit", "res", "sa",
	 "sampleFac", "serialnumber", "sex", "shortname", "specialstage", "specimenid", "start_time",
	 "startyear", "station", "stationstartdate", "stationstarttime", "stationstopdate",
	 "stationstoptime", "stomach", "stoxBioticObject", "subFactor", "subWeight", "suffixes",
	 "sum_sa.x", "sum_sa.y", "sweeplength", "target", "threshold", "tissuesample", "totalNo",
	 "transceiver", "translationListOne", "trawldoorarea", "trawldoorspread", "trawldoortype",
	 "trawldoorweight", "variableToTranslate", "verticaltrawlopening", "vesselspeed",
	 "winddirection", "windspeed", "wingspread", "wiredensity", "wirediameter", "wirelength",
	 "xsdObjects"))

.onLoad <- function(libname, pkgname) {
	# Initiate the RstoxData environment:
	initiateRstoxData()
} 

# Try to unload dynamic library
.onUnload <- function (libpath) {
	library.dynam.unload("RstoxData", libpath)
} 

## usethis namespace: start
#' @useDynLib RstoxData, .registration = TRUE
## usethis namespace: end
NULL

## usethis namespace: start
#' @importFrom Rcpp sourceCpp
## usethis namespace: end
NULL

