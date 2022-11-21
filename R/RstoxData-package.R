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
	 ".", "..Country", "..NAToInsert", "..Organisation", "..SurveyName", "..colList", "..columns",
	 "..digits", "..groupingVariables", "..keep", "..key", "..parameterNames", "..relevantColumns",
	 "..replacement", "..signifDigits", "..sourceColumns", "..targetAndSourceVariables", "..toKeep",
	 "..valueVariablesInTranslation", "..varToExtract", "..variableKeys", "..variablesInTable",
	 "..x", "AcousticCategory", "Addition", "BeamKey", "Channel", "ChannelDepthLower",
	 "ChannelDepthUpper", "ChannelReferenceKey", "ChannelReferenceType", "Constant", "Country",
	 "Cruise", "CruiseKey", "DateTime", "DoorType", "EDSU", "EchoType", "EffectiveTowDistance",
	 "FishID", "Gear", "GearEx", "HLNoAtLngt", "HaulNo", "LengthCode", "LengthResolution",
	 "LngtClass", "LocalID", "LogKey", "N", "NewValue", "NumberAtLength",
	 "NumberOfIndividualsToGenerate", "NumberOfSampledIndividuals", "Quarter", "ReplaceBy",
	 "SaCategory", "Scaling", "Ship", "SpecVal", "SpeciesCategoryNumber", "SpeciesCategoryWeight",
	 "SpeciesCode", "StatRec", "SubsampleWeight", "SubsampledNumber", "Survey", "SweepLngt", "Time",
	 "TransducerOrientation", "VariableName", "WeightMeasurement", "age", "agingstructure", "aphia",
	 "area", "as.formula", "bottomdepthstart", "bottomdepthstop", "catCatchWgt", "catchcount",
	 "catchpartnumber", "catchproducttype", "catchweight", "cruise", "direction", "distance",
	 "fishingdepthcount", "fishingdepthmax", "fishingdepthmin", "freq", "gear", "gearflow",
	 "inapplicableFormats", "individualweight", "iskey", "latitudeend", "latitudestart",
	 "lenInterval", "lengthCode", "lengthmeasurement", "lengthresolution", "lengthsamplecount",
	 "lengthsampleweight", "level", "liverweight", "lngtClass", "lngtCode", "location", "logstart",
	 "longitudeend", "longitudestart", "lsCountTot", "maturity", "maxFishID", "meanW",
	 "missionstartdate", "missionstopdate", "nInd", "nWithWeight", "nation", "newClass", "noMeas",
	 "parasite", "platformname", "preferredagereading", "readability", "reportInMM",
	 "reportingUnit", "res", "sa", "sampleFac", "serialnumber", "sex", "shortname", "specialstage",
	 "specimenid", "start_time", "startyear", "station", "stationstartdate", "stationstarttime",
	 "stationstopdate", "stationstoptime", "stomach", "stoxBioticObject", "subFactor", "subWeight",
	 "suffixes", "sum_sa.x", "sum_sa.y", "sweeplength", "target", "tissuesample", "totWeight",
	 "totalNo", "transceiver", "trawldoorarea", "trawldoorspread", "trawldoortype",
	 "trawldoorweight", "verticaltrawlopening", "vesselspeed", "winddirection", "windspeed",
	 "wingspread", "wiredensity", "wirediameter", "wirelength", "xsdObjects"))

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

