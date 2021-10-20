#' Tools to Read and Manipulate Fisheries Data
#'
#' Set of tools to read and manipulate various data formats for fisheries. Mainly catered towards scientific trawl survey sampling ('biotic') data, acoustic trawl data, and commercial fishing catch ('landings') data. Among the supported data formats are the data products from the Norwegian Institute Marine Research ('IMR') and the International Council for the Exploration of the Sea (ICES).
#'
#' The RstoxData package contains functions for reading, filtering and writing biotic, acoustic and landing data as XML files. Filtering can be done by R syntax such as longitude > 10, or by pre defined functions such as inside(). On computers that return errors when trying to run the Rtools through RStudio (most institutional Windows machines), install the binary directly from https://github.com/StoXProject/RstoxData/releases. Download the newest RstoxData zip file, click the "Packages" tab -> "Install" -> "Install from:" "Package Archive File" -> "Install". If the installer does not complain, the package is installed correctly.
#' @docType package
#' @name RstoxData
#'
"_PACKAGE"

# Global variables
utils::globalVariables(c(
	 ".", "..Country", "..Organisation", "..SurveyName", "..allDuplicated", "..colList",
	 "..columns", "..digits", "..keep", "..key", "..parameterNames", "..signifDigits",
	 "..sourceColumns", "..targetAndSourceVariables", "..toKeep", "..valueVariablesInTranslation",
	 "..varToExtract", "..variableKeys", "..variablesInTable", "..x", "AcousticCategory",
	 "Addition", "BeamKey", "Channel", "ChannelDepthLower", "ChannelDepthUpper", "Constant",
	 "Country", "Cruise", "CruiseKey", "DateTime", "DoorType", "EDSU", "EchoType",
	 "EffectiveTowDistance", "FishID", "Gear", "GearEx", "HaulNo", "HaulVal", "LengthCode",
	 "LocalID", "LogKey", "N", "NumberAtLength", "NumberOfIndividualsToGenerate",
	 "NumberOfSampledIndividuals", "Quarter", "ReplaceBy", "SaCategory", "Scaling", "Ship",
	 "SpecVal", "SpeciesCategoryNumber", "SpeciesCategoryWeight", "SpeciesCode", "StatRec",
	 "SubsampleWeight", "SubsampledNumber", "Survey", "SweepLngt", "Time", "TransducerOrientation",
	 "VariableName", "WeightMeasurement", "age", "agingstructure", "ap", "aphia",
	 "bottomdepthstart", "bottomdepthstop", "catCatchWgt", "catchcount", "catchpartnumber",
	 "catchproducttype", "catchweight", "cc", "cruise", "cw", "direction", "distance",
	 "fishingdepthcount", "fishingdepthmax", "fishingdepthmin", "freq", "g", "gear",
	 "gearcondition", "gearflow", "hv", "inapplicableFormats", "individualweight", "isCrustacean",
	 "isHerringOrSprat", "isPelagic", "latitudeend", "latitudestart", "lenInterval", "lengthCode",
	 "lengthmeasurement", "lengthresolution", "lengthsamplecount", "lengthsampleweight", "level",
	 "lngtClass", "lngtCode", "logstart", "longitudeend", "longitudestart", "lsCountTot", "lsc",
	 "maturationstage", "maturity", "maxFishID", "meanW", "missionstartdate", "missionstopdate",
	 "ms", "nInd", "nWithWeight", "nation", "noMeas", "parasite", "platformname",
	 "preferredagereading", "readability", "reportInMM", "reportingUnit", "res", "rowIndex", "s",
	 "sampleFac", "samplequality", "sampletype", "serialnumber", "sex", "shortname", "sp",
	 "specialstage", "specimenid", "start_time", "startyear", "station", "stationstartdate",
	 "stationstarttime", "stationstopdate", "stationstoptime", "stomach", "stoxBioticObject",
	 "subFactor", "subWeight", "suffixes", "sweeplength", "target", "tissuesample", "totWeight",
	 "totalNo", "transceiver", "trawldoorarea", "trawldoorspread", "trawldoortype",
	 "trawldoorweight", "verticaltrawlopening", "winddirection", "windspeed", "wingspread",
	 "wiredensity", "wirediameter", "wirelength"))

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

