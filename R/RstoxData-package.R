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
	 ".", "..Country", "..Organisation", "..SurveyName", "..atResolution", "..attribsNames",
	 "..colList", "..columns", "..groupingVariables", "..keep", "..keys", "..parameterNames",
	 "..relevantColumns", "..replacement", "..simpletags", "..sourceColumns",
	 "..targetAndSourceVariables", "..toKeep", "..valueVariablesInTranslation", "..varToExtract",
	 "..variableKeys", "..variablesInTable", "..x", "AcousticCategory", "Addition",
	 "AphiaIDPredator", "AphiaIDPrey", "BeamKey", "Channel", "ChannelDepthLower",
	 "ChannelDepthUpper", "ChannelReferenceKey", "ChannelReferenceType", "Constant", "Count",
	 "Country", "Cruise", "CruiseKey", "DateTime", "Day", "DigestionStage", "DoorType", "EDSU",
	 "EchoType", "EffectiveTowDistance", "FishID", "Frequency", "Gear", "GearExceptions",
	 "GravMethod", "HaulNo", "HaulNumber", "ID", "IdentMet", "LengthClass", "LengthCode", "LocalID",
	 "LogKey", "Month", "N", "NewValue", "Number", "NumberAtLength",
	 "NumberOfIndividualsToGenerate", "NumberOfSampledIndividuals", "Platform", "PreySequence",
	 "Quarter", "ResolutionCode", "SaCategory", "Scaling", "Ship", "SpeciesCategoryNumber",
	 "SpeciesCategoryWeight", "SpeciesCode", "SpeciesValidity", "StationNumber",
	 "StatisticalRectangle", "StomachFullness", "SubFactor", "SubsampleWeight", "SubsampledNumber",
	 "Survey", "SweepLength", "Time", "TotalCount", "TransducerOrientation", "Value",
	 "VariableName", "WeightMeasurement", "Year", "acocat", "age", "agingstructure", "aphia",
	 "bottomdepthstart", "bottomdepthstop", "catCatchWgt", "catchcount", "catchpartnumber",
	 "catchproducttype", "catchweight", "ch", "cruise", "currentReportingUnit", "direction",
	 "distance", "fishingdepthcount", "fishingdepthmax", "fishingdepthmin", "freq", "gear",
	 "gearflow", "i.replace_number", "inapplicableFormats", "individualweight", "integrator_dist",
	 "interval", "iskey", "lat_start", "lat_stop", "latitudeend", "latitudestart", "lengthCode",
	 "lengthintervalcount", "lengthintervalstart", "lengthmeasurement", "lengthresolution",
	 "lengthsamplecount", "lengthsampleweight", "level", "liverweight", "lngtClass", "lngtCode",
	 "log_start", "logstart", "lon_start", "lon_stop", "longitudeend", "longitudestart",
	 "lsCountTot", "maturity", "maxFishID", "max_bot_depth", "meanW", "missionstartdate",
	 "missionstopdate", "nInd", "nation", "newClass", "newLngtCode", "newLngtCodeNumeric",
	 "newReportingUnit", "noMeas", "parasite", "pel_ch_thickness", "platform", "platformname",
	 "preferredagereading", "preycategory", "preydigestion", "preyforeignobject", "preysampleid",
	 "readability", "reportingUnit", "res", "sa", "sampleFac", "serialnumber", "sex", "shortname",
	 "specialstage", "specimenid", "start_time", "startyear", "station", "stationstartdate",
	 "stationstarttime", "stationstopdate", "stationstoptime", "stomach", "stomachfillfield",
	 "stomachweight", "stoxBioticObject", "subFactor", "subWeight", "suffixes", "sum_sa.x",
	 "sum_sa.y", "sweeplength", "tagid", "tagtype", "target", "threshold", "tissuesample",
	 "totalNo", "totalweight", "transceiver", "translationListOne", "trawldoorarea",
	 "trawldoorspread", "trawldoortype", "trawldoorweight", "variableToTranslate",
	 "verticaltrawlopening", "vesselspeed", "weightresolution", "winddirection", "windspeed",
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

