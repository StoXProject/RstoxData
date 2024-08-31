##################################################
##################################################
#' Definitions stored in the RstoxData environment
#' 
#' This function declares the RstoxData environment and writes vital definitions to it.
#' 
#' @return
#' A list of definitions.
#' 
#' @noRd
#' @seealso Use \code{\link{getRstoxDataDefinitions}} to get the definitions.
#' 
initiateRstoxData <- function(){
	
	if(!exists("xsdObjects")) {
		data(xsdObjects, package="RstoxData", envir = environment())
	}
	
	# Define the time zone used by StoX formats:
	StoxTimeZone <- "UTC"
	# The time format used in StoX:
	StoxDateTimeFormat <- "%Y-%m-%dT%H:%M:%OS"
	
	# Get the path to the extdata folder:
	fpath <- system.file("extdata", package = "RstoxData")
	
	# Define formats that contain non-unique variables, i.e., columns with the same name in different tables:
	nonUniqueFormats <- c(
		"nmdbioticv1", 
		"nmdbioticv1.1", 
		"nmdbioticv1.2", 
		"nmdbioticv1.3", 
		"nmdbioticv1.4"
	)
	
	# StoxBioticKeys: 
	StoxBioticKeys <- c(
		"CruiseKey", 
		"StationKey", 
		"HaulKey", 
		"SpeciesCategoryKey", 
		"SampleKey", 
		"IndividualKey", 
		"PreySpeciesCategoryKey", 
		"PreySampleKey", 
		"PreyIndividualKey"
	)
	# StoxBioticKeys: 
	StoxAcousticKeys <- c(
		"CruiseKey", 
		"LogKey", 
		"BeamKey", 
		"AcousticCategoryKey", 
		"ChannelReferenceKey", 
		"NASCKey"
	)
	
	#dataTypeDefinition <- list(
	#	# StoxAcousticDat: 
	#	StoxAcousticData = list(
	#		Cruise = c("CruiseKey", "Cruise", "Platform")
	#		Log = c("CruiseKey", "LogKey", "Log", "EDSU", "DateTime", "Longitude", "Latitude", "LogOrigin", "Longitude2", "Latitude2", "LogOrigin2", "LogDistance", "LogDuration", "EffectiveLogDistance","BottomDepth")
	#		Beam = c("CruiseKey","LogKey", "BeamKey", "Beam", "Frequency")
	#		AcousticCategory = c("CruiseKey", "LogKey", "BeamKey", "AcousticCategoryKey","AcousticCategory")
	#		ChannelReference = c("CruiseKey", "LogKey", "BeamKey", "AcousticCategoryKey", "ChannelReferenceKey", "ChannelReferenceType", "ChannelReferenceDepth","ChannelReferenceTilt")
	#		NASC = c("CruiseKey", "LogKey", "BeamKey", "AcousticCategoryKey","ChannelReferenceKey""NASCKey", "Channel", "MaxChannelRange", "MinChannelRange", "NASC")
	#	)
	#)
	
	
	#### Data type units: ####
	dataTypeUnits <- list(
		
		## StoxAcousticData
		
		# Cruise level:
		list(dataType = "StoxAcousticData", variableName = "CruiseKey", quantity = "NA", unit = "NA"), 
		list(dataType = "StoxAcousticData", variableName = "Cruise", quantity = "NA", unit = "NA"), 
		list(dataType = "StoxAcousticData", variableName = "Platform", quantity = "NA", unit = "NA"), 
		
		# Log level:
		list(dataType = "StoxAcousticData", variableName = "LogKey", quantity = "NA", unit = "NA"), 
		list(dataType = "StoxAcousticData", variableName = "Log", quantity = "NA", unit = "NA"), 
		list(dataType = "StoxAcousticData", variableName = "EDSU", quantity = "NA", unit = "NA"), 
		list(dataType = "StoxAcousticData", variableName = "DateTime", quantity = "datetime", unit = "ISO8601"), 
		list(dataType = "StoxAcousticData", variableName = "Longitude", quantity = "angle", unit = "degree east"), 
		list(dataType = "StoxAcousticData", variableName = "Latitude", quantity = "angle", unit = "degree north"), 
		list(dataType = "StoxAcousticData", variableName = "LogOrigin", quantity = "NA", unit = "NA"), 
		list(dataType = "StoxAcousticData", variableName = "Longitude2", quantity = "angle", unit = "degree east"), 
		list(dataType = "StoxAcousticData", variableName = "Latitude2", quantity = "angle", unit = "degree north"), 
		list(dataType = "StoxAcousticData", variableName = "LogOrigin2", quantity = "NA", unit = "NA"), 
		list(dataType = "StoxAcousticData", variableName = "LogDistance", quantity = "length", unit = "nmi"), 
		list(dataType = "StoxAcousticData", variableName = "LogDuration", quantity = "time", unit = "s"), 
		list(dataType = "StoxAcousticData", variableName = "EffectiveLogDistance", quantity = "length", unit = "nmi"), 
		list(dataType = "StoxAcousticData", variableName = "BottomDepth", quantity = "length", unit = "m"), 
		
		# Beam level:
		list(dataType = "StoxAcousticData", variableName = "BeamKey", quantity = "NA", unit = "NA"), 
		list(dataType = "StoxAcousticData", variableName = "Beam", quantity = "NA", unit = "NA"), 
		list(dataType = "StoxAcousticData", variableName = "Frequency", quantity = "frequency", unit = "hertz"), 
		
		# AcousticCategory level:
		list(dataType = "StoxAcousticData", variableName = "AcousticCategoryKey", quantity = "NA", unit = "NA"), 
		list(dataType = "StoxAcousticData", variableName = "AcousticCategory", quantity = "NA", unit = "NA"), 
		
		# ChannelReference level:
		list(dataType = "StoxAcousticData", variableName = "ChannelReferenceKey", quantity = "NA", unit = "NA"), 
		list(dataType = "StoxAcousticData", variableName = "ChannelReferenceType", quantity = "NA", unit = "NA"), 
		list(dataType = "StoxAcousticData", variableName = "ChannelReferenceDepth", quantity = "length", unit = "m"), 
		list(dataType = "StoxAcousticData", variableName = "ChannelReferenceTilt", quantity = "angle", unit = "degree"), 
		
		# NASC level:
		list(dataType = "StoxAcousticData", variableName = "NASCKey", quantity = "NA", unit = "NA"), 
		list(dataType = "StoxAcousticData", variableName = "Channel", quantity = "NA", unit = "NA"), 
		list(dataType = "StoxAcousticData", variableName = "MaxChannelRange", quantity = "length", unit = "m"), 
		list(dataType = "StoxAcousticData", variableName = "MinChannelRange", quantity = "length", unit = "m"), 
		list(dataType = "StoxAcousticData", variableName = "NASC", quantity = "NASC", unit = "m^2/nmi^2"),
		
		
		## StoxBioticData
		
		# Cruise level:
		list(dataType = "StoxBioticData", variableName = "CruiseKey", quantity = "NA", unit = "NA"), 
		list(dataType = "StoxBioticData", variableName = "Cruise", quantity = "NA", unit = "NA"), 
		list(dataType = "StoxBioticData", variableName = "Platform", quantity = "NA", unit = "NA"), 
		
		# Station level:
		list(dataType = "StoxBioticData", variableName = "StationKey", quantity = "NA", unit = "NA"), 
		list(dataType = "StoxBioticData", variableName = "Station", quantity = "NA", unit = "NA"), 
		list(dataType = "StoxBioticData", variableName = "CatchPlatform", quantity = "NA", unit = "NA"), 
		list(dataType = "StoxBioticData", variableName = "DateTime", quantity = "datetime", unit = "ISO8601"), 
		list(dataType = "StoxBioticData", variableName = "Longitude", quantity = "angle", unit = "degree east"), 
		list(dataType = "StoxBioticData", variableName = "Latitude", quantity = "angle", unit = "degree north"), 
		list(dataType = "StoxBioticData", variableName = "BottomDepth", quantity = "length", unit = "m"), 
		
		# Haul level:
		list(dataType = "StoxBioticData", variableName = "HaulKey", quantity = "NA", unit = "NA"), 
		list(dataType = "StoxBioticData", variableName = "Haul", quantity = "NA", unit = "NA"), 
		list(dataType = "StoxBioticData", variableName = "Gear", quantity = "NA", unit = "NA"), 
		list(dataType = "StoxBioticData", variableName = "TowDistance", quantity = "length", unit = "nmi"), 
		list(dataType = "StoxBioticData", variableName = "EffectiveTowDistance", quantity = "length", unit = "nmi"), 
		list(dataType = "StoxBioticData", variableName = "MinHaulDepth", quantity = "length", unit = "m"), 
		list(dataType = "StoxBioticData", variableName = "MaxHaulDepth", quantity = "length", unit = "m"), 
		list(dataType = "StoxBioticData", variableName = "VerticalNetOpening", quantity = "length", unit = "m"), 
		list(dataType = "StoxBioticData", variableName = "HorizontalNetOpening", quantity = "lengthlength", unit = "m"), 
		list(dataType = "StoxBioticData", variableName = "TrawlDoorSpread", quantity = "length", unit = "m"), 
		
		# SpeciesCategory level:
		list(dataType = "StoxBioticData", variableName = "SpeciesCategoryKey", quantity = "NA", unit = "NA"), 
		list(dataType = "StoxBioticData", variableName = "SpeciesCategory", quantity = "NA", unit = "NA"), 
		
		# Sample level:
		list(dataType = "StoxBioticData", variableName = "SampleKey", quantity = "NA", unit = "NA"), 
		list(dataType = "StoxBioticData", variableName = "Sample", quantity = "NA", unit = "NA"), 
		list(dataType = "StoxBioticData", variableName = "CatchFractionWeight", quantity = "mass", unit = "kg"), 
		list(dataType = "StoxBioticData", variableName = "CatchFractionNumber", quantity = "cardinality", unit = "individuals"), 
		list(dataType = "StoxBioticData", variableName = "SampleWeight", quantity = "mass", unit = "kg"), 
		list(dataType = "StoxBioticData", variableName = "SampleNumber", quantity = "cardinality", unit = "individuals"), 
		
		# Individual level:
		list(dataType = "StoxBioticData", variableName = "IndividualKey", quantity = "NA", unit = "NA"), 
		list(dataType = "StoxBioticData", variableName = "Individual", quantity = "NA", unit = "NA"), 
		list(dataType = "StoxBioticData", variableName = "IndividualRoundWeight", quantity = "mass", unit = "g"), 
		list(dataType = "StoxBioticData", variableName = "IndividualTotalLength", quantity = "length", unit = "cm"), 
		list(dataType = "StoxBioticData", variableName = "LengthResolution", quantity = "length", unit = "cm"), 
		list(dataType = "StoxBioticData", variableName = "WeightMeasurement", quantity = "NA", unit = "NA"), 
		list(dataType = "StoxBioticData", variableName = "IndividualAge", quantity = "age", unit = "year"), 
		list(dataType = "StoxBioticData", variableName = "IndividualSex", quantity = "NA", unit = "NA"), 
		
		# PreySpeciesCategory level:
		list(dataType = "StoxBioticData", variableName = "PreySpeciesCategoryKey", quantity = "NA", unit = "NA"), 
		list(dataType = "StoxBioticData", variableName = "PreySpeciesCategory", quantity = "NA", unit = "NA"), 
		
		# PreySample level:
		list(dataType = "StoxBioticData", variableName = "PreySampleKey", quantity = "NA", unit = "NA"), 
		list(dataType = "StoxBioticData", variableName = "PreySample", quantity = "NA", unit = "NA"), 
		list(dataType = "StoxBioticData", variableName = "PreySampleWeight", quantity = "mass", unit = "g"), 
		list(dataType = "StoxBioticData", variableName = "PreyWeightResolution", quantity = "mass", unit = "g"), 
		
		# PreyIndividual level:
		list(dataType = "StoxBioticData", variableName = "PreyIndividualKey", quantity = "NA", unit = "NA"), 
		list(dataType = "StoxBioticData", variableName = "PreyIndividual", quantity = "NA", unit = "NA"), 
		list(dataType = "StoxBioticData", variableName = "PreyIndividualTotalLength", quantity = "length", unit = "cm"), 
		list(dataType = "StoxBioticData", variableName = "PreyLengthResolution", quantity = "length", unit = "cm")
	)
	dataTypeUnits <- data.table::rbindlist(dataTypeUnits)
	
	
	targetAndSourceVariables <- list(
		target = "TargetVariable", 
		source = "SourceVariable"
	)
	
	# Define the columns required for VariableConversionTable:
	TranslationOldRequiredColumns <- c("VariableName", "Value", "NewValue")
	
	# Define the ICESBiotic keys (check with the package author whether this is already defined when reading the data):
	ICESAcousticKeys <- list(
		Cruise =   "LocalID", 
		Log =    c("LocalID", "Distance"), 
		Sample = c("LocalID", "Distance", "ChannelDepthUpper"), 
		Data =   c("LocalID", "Distance", "ChannelDepthUpper", "SaCategory")
	)
	ICESBioticKeys <- list(
		Cruise =    "LocalID", 
		Haul =    c("LocalID", "Gear", "Number"), 
		Catch =   c("LocalID", "Gear", "Number", "SpeciesCode", "SpeciesCategory"), 
		Biology = c("LocalID", "Gear", "Number", "SpeciesCode", "SpeciesCategory", "StockCode", "FishID")
	)
	
	# A function to extract the implemented xml formats for each of the types "biotic", "acoustic" and "landing":
	getImplementedXsd <- function(type = c("Biotic", "Acoustic", "Landing")) {
		type <- match_arg_informative(type)
		
		if(type == "Acoustic") {
			type <- c("acoustic", "echosounder")
		}
		
		# Grep the type in the xsd names:
		hasSelectedXsd <- do.call(pmax, lapply(type, grepl, names(xsdObjects), ignore.case = TRUE)) == 1
		useXsd <- tools::file_path_sans_ext(names(xsdObjects)[hasSelectedXsd]) 
		
		return(useXsd)
	}
	
	FilterStoxBioticWarningMessges <- list(
		SampleCount = "The FilterExpression contains the StoxBiotic variable \"SampleCount\", which was changed to \"SampleNumber\" in RstoxData v1.3.5. Please change the FilterExpression.",
		CatchFractionCount = "The FilterExpression contains the StoxBiotic variable \"CatchFractionCount\", which was changed to \"CatchFractionNumber\" in RstoxData v1.3.5. Please change the FilterExpression."
	)
	
	
	lengthResolutionTable <- data.table::data.table(
		lengthresolution = 1:12, 
		shortname = c(
			"1 mm", 
			"5 mm", 
			"1 cm", 
			"3 cm", 
			"5 cm", 
			"0.5 mm", 
			"0.1 mm", 
			"0.1 mm", 
			"2 mm", 
			"3 mm", 
			"2 cm", 
			"20 cm"
		)
	)
	
	lengthCode_unit_table <- data.table::data.table(
		shortnameNMDBiotic = c("1 mm", "5 mm", "1 cm"), 
		lengthCodeICESBiotic = c("mm", "halfcm", "cm"), 
		lengthCodeICESDatras = c(".", "0", "1"), 
		reportingUnit = c("mm", "mm", "cm"), 
		numericResolution = c(1, 5, 1)
	)
	lengthCode_unit_table[, rank := seq_len(.N)]
	
	
	#### Assign to RstoxDataEnv and return the definitions: ####
	definitionsNames <- ls()
	definitions <- lapply(definitionsNames, get, pos = environment())
	names(definitions) <- definitionsNames
	
	#### Create the RstoxDataEnv environment, holding definitions on folder structure and all the projects. This environment cna be accesses using RstoxData:::RstoxDataEnv: ####
	assign("RstoxDataEnv", new.env(), parent.env(environment()))
	assign("definitions", definitions, envir=get("RstoxDataEnv"))
	
	#### Return the definitions: ####
	definitions
}


##################################################
##################################################
#' Get RstoxData definitions
#' 
#' This function gets vital definitions from the RstoxData environment.
#' 
#' @param name  An optional string vector denoting which definitions to extract.
#' @param ...   values overriding the values of definitions.
#' 
#' @return
#' A list of definitions.
#' 
#' @examples
#' getRstoxDataDefinitions()
#' 
#' @export
#' 
getRstoxDataDefinitions <- function(name = NULL, ...) {
	
	# Save the optional inputs for overriding the output:
	l <- list(...)
	
	# Get all or a subset of the definitions:
	definitions <- get("RstoxDataEnv")$definitions
	if(length(name)){
		definitions <- definitions[[name]]
	}
	
	l <- l[names(l) %in% names(definitions)]
	if(length(l)){
		definitions <- utils::modifyList(definitions, l)
	}
	
	definitions
}
