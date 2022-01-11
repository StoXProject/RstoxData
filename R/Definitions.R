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
	
	# Define the number of digits (12) and the number of significant digits (6, used if values are very low) used by the Rstox packages:
	digits <- 12
	signifDigits <- 6
	
	# Define the time zone used by Stox formats:
	StoxTimeZone <- "UTC"
	
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
		"SubIndividualKey"
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
	
	targetAndSourceVariables <- list(
		target = "TargetVariable", 
		source = "SourceVariable"
	)
	
	# Define the columns required for VariableConversionTable:
	TranslationRequiredColumns <- c("VariableName", "Value", "NewValue")
	
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
	
	NA_classes <- list(
		numeric = NA_real_, 
		double = NA_real_, 
		integer = NA_integer_, 
		character = NA_character_
	)
	getNAByType <- function(type = c("numeric", "double", "integer", "character")) {
		type <- match.arg(type)
		NA_classes[[type]]
	}
	
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
