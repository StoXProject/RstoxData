
#' Convert AcousticData to ICESAcousticData.
#'
#' Note that this function only supports \code{\link{AcousticData}} object created from reading an ICES acoustic XML files.
#'
#' @param AcousticData A \code{\link{AcousticData}} object from an ICES acoustic XML format file.
#'
#' @return An \code{\link{ICESAcousticData}} object.
#'
#' @export
ICESAcoustic <- function(AcousticData){
	
	# Run for each file:
	ICESAcousticData <- lapply(
		AcousticData, 
		AcousticDataToICESAcousticCSVOne
	)
	
	# Rbind accross files:
	ICESAcousticData <- rbindlist_StoxFormat(ICESAcousticData)
	
	# Accept only one file:
	#if(length(AcousticData) != 1) {
	#	stop("Only AcousticData from exactly 1 file is accepted by ICESAcoustic")
	#}
	
	return(ICESAcousticData)
}



AcousticDataToICESAcousticCSVOne <- function(AcousticDataOne){
	
	if(AcousticDataOne$metadata$useXsd=='icesAcoustic') {
		ICESAcousticDataOne <- prepareICESAcousticCSV_icesAcoustic(AcousticDataOne)
	}
	else if(AcousticDataOne$metadata$useXsd=='nmdechosounderv1'){
		ICESAcousticDataOne <- prepareICESAcousticCSV_nmdechosounderv1(AcousticDataOne)
	}
	else {
		stop('StoX: Only ICESAcoustic version 1 and NMDEchosounder version 1 can be converted to the ICESAcoustic CSV format.')
	}
	
	
	# Run checks and create the ICESAcousticCSV format:
	ICESAcousticDataOne <- checkAndCreateICESAcousticCSV(ICESAcousticDataOne)
	
	return(ICESAcousticDataOne)
}

# From LUF25 to ICESAcoustic:
prepareICESAcousticCSV_icesAcoustic <- function(AcousticDataOne){
	
	# Make a copy as we are modifying by reference:
	ICESAcousticDataOne <- data.table::copy(AcousticDataOne)
	
	# Remove echo type, as this is not included in the CSV:
	ICESAcousticDataOne$Data$EchoType<-NULL
	
	
	# Add the Survey to the Cruise table, with a hack to get the last line (until it has been fixed so that only the code and not the value is present):
	ICESAcousticDataOne$Cruise$Survey <- utils::tail(ICESAcousticDataOne$Survey$Code, 1)
	
	# Translate according to the vocabulary:
	if(length(ICESAcousticDataOne$vocabulary)) {
		vocabulary <- findVariablesMathcinigVocabulary(
			vocabulary = ICESAcousticDataOne$vocabulary, 
			data = ICESAcousticDataOne
		)
		# Uniqueify since some columns (keys) are present in several tables:
		vocabulary <- unique(vocabulary)
		
		translateVariables(
			data = ICESAcousticDataOne, 
			TranslationDefinition = "FunctionInput",
			Translation = vocabulary, 
			translate.keys = TRUE, 
			warnMissingTranslation = FALSE
		)
	}
	
	return(ICESAcousticDataOne)
}




# From LUF20 to ICESAcoustic:
prepareICESAcousticCSV_nmdechosounderv1 <-  function(AcousticDataOne){
	
	#Check if multiple channel type
	if(length(unique(AcousticDataOne$ch_type$type)) > 1) {
		warning('Multiple channel type! you need to run filter acoustic to select the needed channel type')
	}
	
	
	#grab frequency
	freq <- AcousticDataOne$frequency
	freq <- unique(freq[,.(Frequency = freq, ID = as.character(transceiver))])
	
	
	#Prepare the Instrument table
	#Here we set the transducer ID to the transceiver number
	#The rest needs to be added using translation
	# NA_character_
	# NA_integer_
	# NA_real_
	Instrument <- freq[, .(
		Frequency = Frequency,
		TransducerLocation = NA_character_,
		TransducerManufacturer = NA_character_,
		TransducerModel = NA_character_, 
		TransducerSerial = NA_character_, 
		TransducerBeamType = NA_character_, 
		TransducerDepth = NA_real_,
		TransducerOrientation = NA_character_,
		TransducerPSI = NA_real_, 
		TransducerBeamAngleMajor = NA_real_,
		TransducerBeamAngleMinor = NA_real_,
		TransceiverManufacturer = NA_character_,
		TransceiverModel = NA_character_,
		TransceiverSerial = NA_character_,  
		TransceiverFirmware = NA_character_,
		Comments = NA_character_,     
		ID = ID
	)]
	
	
	#Prepare the calibration table
	#All values needs to be filled inn using translation
	Calibration <- data.table::data.table(
		Date = NA_character_,
		AcquisitionMethod = NA_character_,
		ProcessingMethod = NA_character_,
		AccuracyEstimate = NA_real_, 
		Report = NA_character_,
		Comments = NA_character_,
		ID = NA_character_
	)
	
	
	#Prepare the DataAcquisition table
	#All values needs to be filled in using translation
	DataAcquisition <- data.table::data.table(
		SoftwareName = NA_character_, 
		SoftwareVersion = NA_character_, 
		StoredDataFormat = NA_character_, 
		PingDutyCycle = NA_character_, 
		Comments = NA_character_, 
		ID = NA_character_
	)
	
	#Prepare the DataProcessing table
	#Perhaps the Frequency can be picked up from the LUF20 file
	#The rest of the fields needs to be filled using translation
	DataProcessing <- data.table::data.table(
		 SoftwareName = NA_character_, 
		 SoftwareVersion = NA_character_,
		 TriwaveCorrection = "NA",
		 Bandwidth = NA_real_, 
		 Frequency = NA_real_, 
		 TransceiverPower = NA_real_,   
		 TransmitPulseLength = NA_real_, 
		 OnAxisGain = NA_real_, 
		 OnAxisGainUnit = NA_character_,
		 SaCorrection = NA_real_, 
		 Absorption = NA_real_,
		 AbsorptionDescription = NA_character_, 
		 SoundSpeed = NA_real_,
		 SoundSpeedDescription = NA_character_, 
		 TransducerPSI = NA_real_,
		 Comments = NA_character_, 
		 ID = NA_character_,
		 ChannelID = NA_character_
	)
	
	
	#Prepare the cruise table,  
	#Some of the fields needs to be filled using translation
	Cruise <- AcousticDataOne$echosounder_dataset[, .(
		Country = as.character(nation),
		Platform = as.character(platform),
		StartDate = NA_character_,
		EndDate = NA_character_,
		Organisation = NA_character_,
		Survey = NA_character_,
		LocalID = as.character(cruise)
	)]
	
	
	#Prepare data for the Data table
	distanceFrequencySa <- mergeDataTables(
		AcousticDataOne, 
		tableNames = c("distance", "frequency", "sa"), 
		output.only.last = TRUE, 
		all = TRUE
	)
	
	# Create the Log, Sample and Data separately, then check definitions, and then merge:
	Log <- unique(distanceFrequencySa[, .(
		LocalID = Cruise$LocalID, 
		Distance = log_start,
		Time = start_time,
		Latitude = lat_start,
		Longitude = lon_start,
		Origin = 'start',
		Latitude2 = lat_stop, 
		Longitude2 = lon_stop, 
		Origin2 = 'end',
		Validity = 'V',
		BottomDepth = max_bot_depth                       #Have used max instead of min bottom depth
	)])
	
	Sample <- unique(distanceFrequencySa[, .(
		LocalID = Cruise$LocalID, 
		Distance = log_start,
		Instrument = as.character(transceiver), 
		ChannelDepthUpper = pel_ch_thickness * (ch - 1),     #Need to check
		ChannelDepthLower = pel_ch_thickness * (ch),      #Need to check if this is correct    
		PingAxisInterval = integrator_dist, 
		PingAxisIntervalType = 'distance',             #How can we see the unit, is it always distance (nmi)? 
		PingAxisIntervalUnit = 'nmi',
		SvThreshold = threshold,
		Calibration = NA_character_,                                  
		DataAcquisition = NA_character_, 
		DataProcessing = NA_character_, 
		PingAxisIntervalOrigin = 'start'              #Is this start? 
	)])
	
	Data <- distanceFrequencySa[, .(
		LocalID = Cruise$LocalID, 
		Distance = log_start,
		Instrument = as.character(transceiver), 
		ChannelDepthUpper = pel_ch_thickness * (ch - 1),     #Need to check
		SaCategory = as.character(acocat),                             #This may be a challenge in translation. we may need to merge to get UNK
		Type = 'C',                                      #Assumes sA values in LUF25
		Unit = "m2nmi-2",                              #Still an assumption
		Value = sa
	)]
	
	
	
	#Summarise sa value of same acocat in same channel
	#AJ can make this function more pretty 
	# Indeed:
	# Get the keys of the Data table:
	keysData <- c(
		getRstoxDataDefinitions("ICESAcousticKeys")$Data, 
		"Instrument" # This is introduced in the Sample table, and acts as a key in the Data table (before merging):
	)
	Data[, Value := sum(Value), by = keysData]
	Data <- unique(Data, by = keysData)
	
	
	#Prepare the output 
	ICESAcousticDataOne <- list(
		Instrument = Instrument, 
		Calibration = Calibration,
		DataAcquisition = DataAcquisition,
		DataProcessing = DataProcessing,
		Cruise = Cruise, 
		Log = Log,
		Sample = Sample, 
		Data = Data
		)
	
	return(ICESAcousticDataOne)
}


checkAndCreateICESAcousticCSV <- function(ICESAcousticDataOne) {
	
	# Check metadata against ices definitions
	checkICESAcousticDefinitions (ICESAcousticDataOne)
	
	# Set classes of the variables, especially taking care of NAs. The class of the variables is used later to format the output from WriteICESAcoustic, where NA double type is stored as empty sting to support these beingg empty fields in the written file:
	lapply(names(ICESAcousticDataOne), setClass_OneTable, ICESAcousticDataOne, RstoxData::xsdObjects$icesAcoustic.xsd)
	
	
	#### Rename columns to start with the table name:
	independentTables <- c("Instrument", "Calibration", "DataAcquisition", "DataProcessing")
	hierarchicalTables <- c("Cruise", "Log", "Sample", "Data")
	
	# Rename the independent tables:
	independentTablesColumnNames <- lapply(ICESAcousticDataOne[independentTables], names)
	independentTablesNewColumnNames <- mapply(paste0, independentTables, independentTablesColumnNames)
	mapply(
		data.table::setnames, 
		ICESAcousticDataOne[independentTables], 
		old = independentTablesColumnNames, 
		new = independentTablesNewColumnNames
	)
	
	# Rename the hierarchical tables and take care of the keys:
	renameToTableNameFirst(
		ICESAcousticDataOne, 
		tableNames = hierarchicalTables, 
		setToID = independentTables, 
		formatType = "Acoustic"
	)
	
	# Merge the Log, Sample and Data to make the merged Data table:
	hierarchicalTablesSansCruise <- setdiff(hierarchicalTables, "Cruise")
	LogSampleData <- mergeDataTables(ICESAcousticDataOne[hierarchicalTablesSansCruise], output.only.last = TRUE, all = TRUE)
	LogSampleData <- unique(LogSampleData)
	
	# Create the output:
	ICESAcousticDataOne <- c(
		ICESAcousticDataOne[c("Instrument", "Calibration", "DataAcquisition", "DataProcessing", "Cruise")],
		list(Data = LogSampleData)
	)
	
	# Move ID columns last:
	ICESAcousticDataOne <- lapply(ICESAcousticDataOne, moveIDsLast)
	
	return(ICESAcousticDataOne)
}


is_online <- function(site = "https://raw.githubusercontent.com/StoXProject/repo/master/README.md") {
	tryCatch(
		{
			readLines(site, n = 1)
			TRUE
		},
		warning = function(w) invokeRestart("muffleWarning"),
		error = function(e) FALSE
	)
}


testICESURL <- function(baseURL = "https://acoustic.ices.dk/Services/Schema/XML", testSchema = "AC_TransducerLocation") {
	testURL <- getICESURL(testSchema, baseURL = baseURL)
	if(!is_online()) {
		warning("Internet connecion does not work, or is too slow to read a small file within the timeout of ", options("timeout")$timeout, ".")
		return(FALSE)
	}
	else if(!is_online(testURL)) {
		warning("The URL ", testURL, " failed to download within the timeout of ", options("timeout")$timeout, ".")
		return(FALSE)
	}
	else {
		return(TRUE)
	}
}

# Get the URL to ICES schema:

getICESURL <- function(schema, baseURL) {
	paste(baseURL, paste(schema, "xml", sep = "."), sep = "/")
}



checkICESAcousticDefinitions <- function(
	ICESAcousticDataOne, 
	baseURL = "https://acoustic.ices.dk/Services/Schema/XML", 
	testSchema = "AC_TransducerLocation"
) {
	# Test the internet connection:
	if(testICESURL(baseURL = baseURL, testSchema = testSchema)) {
		compareICES("Instrument", "TransducerLocation", ICESAcousticDataOne, getICESURL("AC_TransducerLocation", baseURL = baseURL))
		compareICES("Instrument", "TransducerBeamType", ICESAcousticDataOne, getICESURL("AC_TransducerBeamType", baseURL = baseURL))
		compareICES("Calibration", "AcquisitionMethod", ICESAcousticDataOne, getICESURL("AC_AcquisitionMethod", baseURL = baseURL))
		compareICES("Calibration", "ProcessingMethod", ICESAcousticDataOne, getICESURL("AC_ProcessingMethod", baseURL = baseURL))
		compareICES("DataAcquisition", "SoftwareName", ICESAcousticDataOne, getICESURL("AC_DataAcquisitionSoftwareName", baseURL = baseURL))
		compareICES("DataAcquisition", "StoredDataFormat", ICESAcousticDataOne, getICESURL("AC_StoredDataFormat", baseURL = baseURL))
		compareICES("DataProcessing", "SoftwareName", ICESAcousticDataOne, getICESURL("AC_DataProcessingSoftwareName", baseURL = baseURL))
		compareICES("DataProcessing", "TriwaveCorrection", ICESAcousticDataOne, getICESURL("AC_TriwaveCorrection", baseURL = baseURL))
		compareICES("DataProcessing", "OnAxisGainUnit", ICESAcousticDataOne, getICESURL("AC_OnAxisGainUnit", baseURL = baseURL))
		compareICES("Cruise", "Country", ICESAcousticDataOne, getICESURL("ISO_3166", baseURL = baseURL))
		compareICES("Cruise", "Platform", ICESAcousticDataOne, getICESURL("SHIPC", baseURL = baseURL))
		compareICES("Cruise", "Organisation", ICESAcousticDataOne, getICESURL("EDMO", baseURL = baseURL))
		compareICES("Cruise", "Survey", ICESAcousticDataOne, getICESURL("AC_Survey", baseURL = baseURL))
		compareICES("Log", "Origin", ICESAcousticDataOne, getICESURL("AC_LogOrigin", baseURL = baseURL))
		compareICES("Log", "Validity", ICESAcousticDataOne, getICESURL("AC_LogValidity", baseURL = baseURL))
		compareICES("Sample", "PingAxisIntervalType", ICESAcousticDataOne, getICESURL("AC_PingAxisIntervalType", baseURL = baseURL))
		compareICES("Sample", "PingAxisIntervalUnit", ICESAcousticDataOne, getICESURL("AC_PingAxisIntervalUnit", baseURL = baseURL))
		compareICES("Sample", "PingAxisIntervalOrigin", ICESAcousticDataOne, getICESURL("AC_PingAxisIntervalOrigin", baseURL = baseURL))
		compareICES("Data", "SaCategory", ICESAcousticDataOne, getICESURL("AC_SaCategory", baseURL = baseURL))
		compareICES("Data", "Type", ICESAcousticDataOne, getICESURL("AC_AcousticDataType", baseURL = baseURL))
		compareICES("Data", "Unit", ICESAcousticDataOne, getICESURL("AC_DataUnit", baseURL = baseURL))
	}
	else {
		warning("Reference data for ICESAcoustic was not checked!!!")
	}
}






#' Write ICESAcoustic to CSV fille
#'
#' Writes \code{\link{ICESAcousticData}} to a csv file for each input acoustic file used to create the \code{\link{ICESAcousticData}}
#'
#' @param ICESAcousticData A \code{\link{ICESAcousticData}} object obtained from an ICES acoustic XML format file.
#'
#' @return List of string matrices in the ICES acoustic CSV format.
#' 
#' @return An object of StoX data type \code{\link{WriteICESAcousticData}}.
#'
#' @export
#' 
WriteICESAcoustic <- function(ICESAcousticData){
	
	#WriteICESAcousticData <- lapply(
	#	ICESAcousticData, 
	#	WriteICESAcousticOne
	#)
	
	WriteICESAcousticData <- WriteICESAcousticOne(ICESAcousticData)
	
	return(WriteICESAcousticData)
}


WriteICESAcousticOne <- function(ICESAcousticDataOne){
	
	# Convert all tables to string with header and reccord, and rbind:
	#ICESAcousticCSVDataOne <- convertToHeaderRecordMatrix(ICESAcousticDataOne, keepNA = "DataProcessingTriwaveCorrection")
	ICESAcousticCSVDataOne <- convertToHeaderRecordMatrix(ICESAcousticDataOne)
	ICESAcousticCSVDataOne <- expandWidth(ICESAcousticCSVDataOne, na = NA)
	
	# Stack all matrices:
	ICESAcousticCSVDataOne <- do.call(rbind, ICESAcousticCSVDataOne)
	
	return(ICESAcousticCSVDataOne)
}




# Function to convert a list of ICESAcoustic data to a list of tables with Header and Reccord, fitting the ICES CSV formats:
convertToHeaderRecordMatrix <- function(ICESData) {
	# Run through the table names and convert to Header, Record, and stringify:
	lapply(names(ICESData), createHeaderRecordMatrix, ICESData = ICESData)
}

createHeaderRecordMatrix <- function(ICESDataTableName, ICESData) {
	
	thisTable <- ICESData[[ICESDataTableName]]
	# # Move IDs last:
	# endsWithID <- endsWith(names(thisTable), "ID")
	# data.table::setorderv(thisTable, c(names(thisTable)[!endsWithID], names(thisTable)[!endsWithID]))
	
	header <- c(
		ICESDataTableName, 
		"Header", 
		#paste0(ICESDataTableName, names(thisTable))
		names(thisTable)
	)
	
	
	# Set to character with NAs as empty character:
	cols <- names(thisTable)
	thisTable[, (cols) := lapply(.SD, as.character), .SDcols = cols]
	thisTable <- as.matrix(thisTable)
	record <- cbind(
		ICESDataTableName, 
		"Record", 
		thisTable
	)
	
	unname(rbind(header, record))
}


# Function to convert a list of ICESAcoustic data to a list of tables with Header and Reccord, fitting the ICES CSV formats:
convertToRecordTypeMatrix <- function(ICESData) {
	# Run through the table names and convert to Header, Record, and stringify:
	lapply(names(ICESData), createRecordTypeMatrix, ICESData = ICESData)
}

createRecordTypeMatrix <- function(ICESDataTableName, ICESData) {
	
	thisTable <- ICESData[[ICESDataTableName]]
	
	header <- c(
		"RecordType", 
		names(thisTable)
	)
	
	record <- cbind(
		ICESDataTableName, 
		# Convert all columns to string, but use trim = FALSE to avoid left padding with spaces for integers:
		#as.matrix(thisTable, trim = TRUE)
		#as.matrix(format(thisTable, trim = TRUE))
		trimws(as.matrix(thisTable))
	)
	
	unname(rbind(header, record))
}


moveIDsLast <- function(x) {
	# Move IDs last:
	endsWithID <- endsWith(names(x), "ID")
	data.table::setcolorder(x, c(names(x)[!endsWithID], names(x)[endsWithID]))
}


expandWidth <- function(x, na = NA) {
	# Get the number of columns and rows:
	ncols <- sapply(x, ncol)
	nrows <- sapply(x, nrow)
	# Create matrices of NAs with dimension so that added to the original data we end up with identical number of rows:
	dims <- cbind(nrows, max(ncols) - ncols)
	dimsList <- split(dims, seq_along(x))
	NAArrays <- mapply(array, dim = dimsList, SIMPLIFY = FALSE, MoreArgs = list(data = na))
	mapply(cbind, x, NAArrays, SIMPLIFY = FALSE)
}









#' Convert BioticData to ICESBiotic format
#'
#' Given an \code{\link{BioticData}} object, this function converts to ICESBiotic format. Note that this function only supports
#' \code{BioticData} generated from NMDBiotic version > 3 XML files.
#'
#' @param BioticData a \code{BioticData} object from an XML file with NMD biotic version 3 format.
#' @param SurveyName A string naming the survey. Must be one of the names listed on \url{https://vocab.ices.dk/?ref=1453} or NONE.
#' @param Country The ISO_3166 code of the country running the cruise. See \url{http://vocab.ices.dk/?ref=337}.
#' @param Organisation An integer code representing the organization running the cruise. See \url{https://vocab.ices.dk/?ref=1398} for a list of possible codes (e.g., Institute of Marine Research: 612).
#' @param AllowRemoveSpecies ICES submission will not allow the resulting CSV file to be uploaded if the file contains species not listed in
#' https://acoustic.ices.dk/Services/Schema/XML/SpecWoRMS.xml. Setting this parameter to TRUE will remove the unlisted species records.
#'        
#' @return An \code{\link{ICESBioticData}} object.
#'
#' @export
ICESBiotic <- function(
	BioticData, 
	SurveyName = character(), 
	Country = character(), 
	Organisation = integer(), 
	AllowRemoveSpecies = TRUE
) {

	# Convert to ICESBiotic:
	ICESBioticData <- lapply(
		BioticData, 
		BioticDataToICESBioticOne, 
		SurveyName = SurveyName,
		Country = Country,
		Organisation = Organisation,
		AllowRemoveSpecies = AllowRemoveSpecies
	)
	
	# Rbind accross files:
	ICESBioticData <- rbindlist_StoxFormat(ICESBioticData)
	
	return(ICESBioticData)
} 






BioticDataToICESBioticOne <- function(
	BioticDataOne, 
	SurveyName = character(), 
	Country = character(), 
	Organisation = integer(),  
	AllowRemoveSpecies = TRUE
) {
	
	if(BioticDataOne$metadata$useXsd %in% "icesBiotic") {
		ICESBioticDataOne <- BioticData_ICESToICESBioticOne(BioticDataOne)
	}
	else if(BioticDataOne$metadata$useXsd %in% c("nmdbioticv3", "nmdbioticv3.1")) {
		ICESBioticDataOne <- BioticData_NMDToICESBioticOne(
			BioticDataOne, 
			SurveyName = SurveyName,
			Country = Country,
			Organisation = Organisation,
			AllowRemoveSpecies = AllowRemoveSpecies
		)
	}
	else {
		warning("StoX: Only NMD Biotic version 3 and 3.1 data can be converted to ICESBiotic (was ", BioticDataOne$metadata$useXsd, " for the file ", BioticDataOne$metadata$file, ". NA returned.")
		return(NULL)
	}
	
	
	# Order the tables:
	hierarchicalTables <- c("Cruise", "Haul", "Catch", "Biology")
	ICESBioticDataOne <- ICESBioticDataOne[hierarchicalTables]
	
	# Set classes of the variables, especially taking care of NAs. The class of the variables is used later to format the output from WriteICESBiotic, where NA double type is stored as empty sting to support these being empty fields in the written file:
	lapply(names(ICESBioticDataOne), setClass_OneTable, ICESBioticDataOne, RstoxData::xsdObjects$icesBiotic.xsd)
	
	
	# Create a table of the original and new column names, but remove keys:
	renameToTableNameFirst(
		ICESBioticDataOne, 
		tableNames = hierarchicalTables, 
		formatType = "Biotic"
	)
	
	# Move ID columns last:
	ICESBioticDataOne <- lapply(ICESBioticDataOne, moveIDsLast)
	
	
	
	return(ICESBioticDataOne)
}


BioticData_ICESToICESBioticOne <- function(BioticData_ICESOne) {
	
	# Make a copy for safety:
	BioticDataOne <- data.table::copy(BioticData_ICESOne)
	
	# Add the Survey to the Cruise table, with a hack to get the last line (until it has been fixed so that only the code and not the value is present):
	BioticDataOne$Cruise$Survey <- utils::tail(BioticDataOne$Survey$Code, 1)
	
	
	tablesToTranslate <- c("Cruise", "Haul", "Catch", "Biology")
	
	# Apply translations defined in the table 'vocabulary':
	if(length(BioticDataOne$vocabulary)) {
		vocabulary <- findVariablesMathcinigVocabulary(
			vocabulary = BioticDataOne$vocabulary, 
			data = BioticDataOne[tablesToTranslate]
		)
		# Uniqueify since some columns (keys) are present in several tables:
		vocabulary <- unique(vocabulary)
		
		translateVariables(
			data = BioticDataOne[tablesToTranslate], 
			TranslationDefinition = "FunctionInput",
			Translation = vocabulary, 
			translate.keys = TRUE, 
			warnMissingTranslation = FALSE
		)
	}
	
	
	return(BioticDataOne)
}

BioticData_NMDToICESBioticOne <- function(
	BioticData_NMDOne, 
	SurveyName = character(), 
	Country = character(), 
	Organisation = integer(), 
	AllowRemoveSpecies = TRUE
) {
	
	cruiseRaw <- BioticData_NMDOne$mission
	
	# Check that Survey, Country and Organisation are given:
	if(!all(length(SurveyName), length(Country), length(Organisation))) {
		stop("All of SurveyName, Country and Organisation must be given.")
	}
	
	Cruise <- cruiseRaw[, .(
		Survey = ..SurveyName,
		Country = ..Country,
		Organisation = ..Organisation,
		# Changed to platformname on 2022-05-12. The user should translate to the codes on https://vocab.ices.dk/services/pox/GetCodeList/SHIPC:
		#Platform = getICESShipCode(platformname),
		Platform = platformname,
		StartDate = gsub("Z", "", missionstartdate),
		EndDate = gsub("Z", "", missionstopdate),
		LocalID = cruise
	)]
	
	
	haulRaw <- merge(cruiseRaw, BioticData_NMDOne$fishstation)
	
	Haul <- haulRaw[, .(
		LocalID = cruise,
		Gear = gear, # Is gear assumed formatted correctly in the input file???
		Number = serialnumber,
		StationName = station,
		StartTime = ifelse(is.na(stationstartdate) | is.na(stationstarttime), NA_character_, gsub("Z", " ", paste0(stationstartdate, substr(stationstarttime, 1, 5)))),
		Duration = getTimeDiff(stationstartdate, stationstarttime, stationstopdate, stationstoptime),
		# Changed to NA on 2022-05-12, as StoX cannot decide which of gearcondition and samplequality should be used:
		#Validity = getHaulValiditySimple(gearcondition, samplequality),
		Validity = NA_character_,
		StartLatitude = latitudestart,
		StartLongitude = longitudestart,
		StopLatitude = latitudeend,
		StopLongitude = longitudeend,
		# Changed 2022-05-12 to pasting area and location:
		#StatisticalRectangle = getICESrect(latitudestart, longitudestart),
		# Changed 2023-01-14 to the new function getStatisticalRectangle() since getICESrect() has errors (includes "I" and A4, ..., A9) :
		#StatisticalRectangle = getICESrect(area, location),
		StatisticalRectangle = getStatisticalRectangle(latitudestart, longitudestart),
		MinTrawlDepth = ifelse(is.na(fishingdepthmin), fishingdepthmax, fishingdepthmin),
		MaxTrawlDepth = fishingdepthmax,
		BottomDepth = ifelse(bottomdepthstop > fishingdepthmax, bottomdepthstop, NA_real_),
		# Before, the function getDistanceMeter() was used, which was inherited from old Datras code. Instead we use the distance that is given by NMDBiotic:
		#Distance = getDistanceMeter(latitudestart, longitudestart, latitudeend, longitudeend),
		# Distance is in nautical miles in NMDBiotic and in meters in ICEBiotic:
		Distance = distance * 1852,
		Netopening = verticaltrawlopening,
		CodendMesh = NA_integer_,
		#SweepLength = getGOVSweepByEquipment(gear),
		SweepLength = sweeplength,
		GearExceptions = NA_character_, # Should this be set?
		DoorType = trawldoortype,
		# Warp length in integer meter: 
		WarpLength = round(wirelength),
		# Warp diameter in integer millimeter: 
		WarpDiameter = round(wirediameter),
		# Warp density in integer kg per linear meter of warp.: 
		WarpDensity = round(wiredensity),
		DoorSurface = trawldoorarea,
		# Door weight in integer kilograms.
		DoorWeight = round(trawldoorweight),
		DoorSpread = trawldoorspread,
		WingSpread = wingspread,
		Buoyancy = NA_integer_,
		KiteArea = NA_real_,
		GroundRopeWeight = NA_integer_,
		Rigging = NA_character_,
		Tickler = NA_integer_,
		HydrographicStationID = NA_character_,
		# Direction is integer degrees: 
		TowDirection = round(direction),
		# Added vessel speed on 2024-09-20 for StoX 4.1.0:
		#SpeedGround = NA_real_,
		SpeedGround = vesselspeed,
		SpeedWater = gearflow,
		# Wind direction is integer degrees: 
		WindDirection = winddirection,
		# Wind speed is meter per second: 
		WindSpeed = round(windspeed),
		SwellDirection = NA_integer_,
		SwellHeight = NA_real_,
		#LogDistance = NA,
		LogDistance = logstart,
		Stratum = NA_character_
	)]
	
	catchRaw <- merge(BioticData_NMDOne$catchsample, haulRaw, by = intersect(names(BioticData_NMDOne$catchsample), names(haulRaw)))
	
	# We must filter records with aphia == NA
	catchRaw <- catchRaw[!is.na(aphia)]
	
	
	
	Catch <- catchRaw[, .(
		LocalID = cruise,
		Gear = gear,
		Number = serialnumber,
		SpeciesCode = aphia,
		SpeciesCategory = catchpartnumber,
		DataType = "R", # Always raw for NMDBiotic, and on 2021-04-21 only R is implemented by ICES at all
		SpeciesValidity = ifelse(is.na(catchproducttype), 0, catchproducttype), # What is meant here. SpeciesValidity can only be 0 or 1, but catchproducttype has a range of values. Should it be ifelse(is.na(catchproducttype), 0, 1) ??
		SpeciesCategoryNumber = catchcount,
		WeightUnit = "kg", # Always kg in NMDBiotic (see http://www.imr.no/formats/nmdbiotic/)
		SpeciesCategoryWeight = catchweight,
		SpeciesSex = NA_character_,
		SubsampledNumber = lengthsamplecount,
		SubsamplingFactor = catchcount / lengthsamplecount,
		SubsampleWeight = lengthsampleweight,
		LengthCode = NA_character_, # NMDBiotic has no way of storing a length distribution.
		LengthClass = NA_integer_, # NMDBiotic has no way of storing a length distribution.
		#LengthType = "1", # Should not this be interpreted from the catchsample$lengthmeasurement ???
		LengthType = lengthmeasurement, 
		NumberAtLength = lengthsamplecount,
		WeightAtLength = NA_real_ # Not relevant for NMDBiotic. Used in the Baltic Acoustic Survey, as noted in the documentation of ICESBiotic.
	)]
	
	# Logic for missing important records
	Catch[is.na(SpeciesCategoryNumber) & is.na(SpeciesCategoryWeight) & !is.na(SubsampledNumber), SpeciesCategoryNumber := SubsampledNumber]
	Catch[is.na(SpeciesCategoryNumber) & is.na(SpeciesCategoryWeight) & !is.na(SubsampleWeight),  SpeciesCategoryWeight := SubsampleWeight]
	
	# NA means that nothing is subsampled
	Catch[!is.na(SpeciesCategoryWeight) & is.na(SubsampleWeight), SubsampleWeight := 0]
	
	
	# Combine required tables for the Biology level
	indRaw <- BioticData_NMDOne$individual
	indRaw[is.na(preferredagereading), preferredagereading := 1]
	
	baseAge <- intersect(names(indRaw), names(BioticData_NMDOne$agedetermination))
	indRaw <- merge(indRaw, BioticData_NMDOne$agedetermination, by.x=c(baseAge, "preferredagereading"), by.y= c(baseAge, "agedeterminationid"), all.x = TRUE)
	indRaw <- merge(catchRaw, indRaw, by = intersect(names(catchRaw), names(indRaw)))
	
	# Take special care of the agingstructure, which should only be given for individuals with age:
	indRaw[is.na(age), agingstructure := NA_character_]
	
	Biology <- indRaw[, .(
		LocalID = cruise,
		Gear = gear,
		Number = serialnumber,
		SpeciesCode = aphia,
		SpeciesCategory = catchpartnumber,
		StockCode = NA_character_,
		GeneticPopulationCode = NA_character_,
		FishID = specimenid,
		#LengthCode = "mm", 
		LengthCode = getLengthCodeICES(lengthresolution, format = "ICESBiotic"), 
		#LengthClass = length * 1000
		LengthClass = scaleLengthUsingLengthCode(length, getLengthCodeICES(lengthresolution, format = "ICESBiotic"), inputUnit = "m", format = "ICESBiotic"), 
		#WeightUnit = 'gr',
		WeightUnit = 'kg', # Always kg in NMDBiotic (see http://www.imr.no/formats/nmdbiotic/)
		#IndividualWeight = individualweight * 1000, # Always kg in NMDBiotic (see http://www.imr.no/formats/nmdbiotic/)
		IndividualWeight = individualweight,
		#IndividualSex = ifelse(is.na(sex), NA, ifelse(sex == "1", "F", "M")),
		IndividualSex = sex,
		#IndividualMaturity = getDATRASMaturity(getQuarter(stationstartdate), aphia, specialstage, maturationstage),
		# Here specialstage is assumed to be have been translated to ICES "M6" codes as per http://vocab.ices.dk/?ref=1480:
		IndividualMaturity = specialstage,
		#MaturityScale = "M6",
		MaturityScale = NA_character_,
		IndividualAge = age,
		IndividualGrowthRings = NA_integer_,
		AgePlusGroup = NA_character_,
		#AgeSource = "Otolith",
		# Do not interpret agingstructure, as this should be the responsibility of the user:
		AgeSource = agingstructure, 
		GeneticSamplingFlag = NA_character_,
		StomachSamplingFlag = NA_character_,
		ParasiteSamplingFlag = NA_character_,
		IndividualVertebraeCount = NA_integer_
	)]
	
	if(!testICESURL()) {
		if(AllowRemoveSpecies) {
			warning("Reference data for ICESAcoustic cannot be checked!!! This can lead to invalid species being inclcuded since AllowRemoveSpecies is set to TRUE.")
		}
		else {
			warning("Reference data for ICESAcoustic cannot be checked!!!")
		}
		
	}
	else {
		if(AllowRemoveSpecies) {
			# Check for valid aphias, mark other as invalid
			xmlRaw <- read_xml("https://acoustic.ices.dk/Services/Schema/XML/SpecWoRMS.xml")
			validCodes <- xml_text(xml_find_all(xmlRaw, "//Code//Key"))
			
			notPresentInCatch <- unique(setdiff(Catch$SpeciesCode, validCodes))
			notPresentInBiology <- unique(setdiff(Biology$SpeciesCode, validCodes))
			
			if(length(notPresentInCatch)) {
				warning("StoX: The following species are not listed in https://acoustic.ices.dk/Services/Schema/XML/SpecWoRMS.xml were automatically removed from table Catch (set AllowRemoveSpecies = FALSE to prevent this):\n", paste(notPresentInCatch, collapse = ", "))
				
			}
			if(length(notPresentInBiology)) {
				warning("StoX: The following species are not listed in https://acoustic.ices.dk/Services/Schema/XML/SpecWoRMS.xml were automatically removed from table Biology (set AllowRemoveSpecies = FALSE to prevent this):\n", paste(notPresentInBiology, collapse = ", "))
				
			}
			
			Catch <- Catch[SpeciesCode %in% validCodes, ]
			Biology <- Biology[SpeciesCode %in% validCodes, ]
		} else {
			message("AllowRemoveSpecies is set to FALSE. Will only give warning for records with species that is not accepted by the ICES system.")
			compareICES("https://acoustic.ices.dk/Services/Schema/XML/SpecWoRMS.xml", unique(Catch$SpeciesCode))
		}
	}
	
	
	ICESBioticCSV <- list(
		Cruise = Cruise, 
		Haul = Haul, 
		Catch = Catch, 
		Biology = Biology
	)
	
	return(ICESBioticCSV)
}











#' Write ICESBiotic to CSV fille
#'
#' Writes \code{\link{ICESBioticData}} to a csv file for each input acoustic file used to create the \code{\link{ICESBioticData}}
#'
#' @param ICESBioticData A \code{\link{ICESBioticData}} object obtained from an ICES acoustic XML format file.
#'
#' @return An object of StoX data type \code{\link{WriteICESBioticData}}.
#'
#' @export
WriteICESBiotic <- function(ICESBioticData){
	
	#WriteICESBioticData <- lapply(
	#	ICESBioticData, 
	#	WriteICESBioticOne
	#)
	
	WriteICESBioticData <- WriteICESBioticOne(ICESBioticData)
	
	return(WriteICESBioticData)
}


WriteICESBioticOne <- function(ICESBioticDataOne){
	
	# Convert all tables to string with header and reccord, and rbind:
	ICESBioticCSVDataOne <- convertToHeaderRecordMatrix(ICESBioticDataOne)
	ICESBioticCSVDataOne <- expandWidth(ICESBioticCSVDataOne, na = NA)
	
	# Stack all matrices:
	ICESBioticCSVDataOne <- do.call(rbind, ICESBioticCSVDataOne)
	
	return(ICESBioticCSVDataOne)
}






# Function to add the table name to the column names, but not to the keys.
renameToTableNameFirst <- function(data, tableNames, setToID = NULL, formatType = c("Biotic", "Acoustic")) {
	
	formatType <- match_arg_informative(formatType)
	
	# Create a table of the original and new column names, but remove keys:
	columnName <- lapply(data[tableNames], names)
	tableName = rep(tableNames, lengths(columnName))
	columnName <- unlist(columnName)
	
	#areTableNames <- columnName  %in% names(data)
	areTableNames <- columnName  %in% setToID
	
	newColumnName <- character(length(columnName))
	newColumnName[areTableNames] <- paste0(columnName[areTableNames], "ID")
	newColumnName[!areTableNames] <- paste0(tableName[!areTableNames], columnName[!areTableNames])
	
	conversionTable <- data.table::data.table(
		tableName = tableName,
		columnName = columnName,
		newColumnName = newColumnName
	)
	
	# Remove the keys:
	ICESKeys <- getRstoxDataDefinitions(paste0("ICES", formatType, "Keys"))
	# Do not remove the key of the last table:
	ICESKeys <- unique(unlist(ICESKeys[!names(ICESKeys) %in% tail(tableNames, 1)]))
	
	conversionTable <- conversionTable[!(duplicated(columnName) & columnName %in% ICESKeys), ]
	
	# Remove rows where tableName and columnNames are identical:
	conversionTable <- subset(conversionTable, tableName != columnName)
	
	# Split the conversionTable by tableName to allow for the duplicately named columns between tables:
	conversionTableList <- split(conversionTable, by = "tableName")
	
	# Add again the keys:
	keys <- conversionTable[columnName %in% ICESKeys, ]
	conversionTableList <- lapply(conversionTableList, rbind, keys)
	conversionTableList <- lapply(conversionTableList, unique)
	
	# Rename by reference:
	mapply(
		data.table::setnames, 
		data[tableNames], 
		old = lapply(conversionTableList, "[[", "columnName"), 
		new = lapply(conversionTableList, "[[", "newColumnName"), 
		skip_absent = TRUE
	)
}











#' Convert BioticData to ICESDatras format
#'
#' Given an \code{\link{BioticData}} object, this function converts to ICESDatras format. Note that this function only supports
#' \code{\link{BioticData}} NMDBiotic version > 3 XML files.
#'
#' @param BioticData a \code{BioticData} object from an XML file with NMD biotic version 3 format.
#'
#' @details
#' As of StoX 4.1.3 (RstoxData 2.1.4) the SpeciesSex is set to NA in the HL table of ICESDatrasData, and where TotalNumber and SubsampledNumber are sums over all sexes (a consequence of setting Sex to NA). The reason for this is that in Norwegian biotic data catch categories (sub-samples) are not separated by sex, which results in SubWgt and CatCatchWgt being sums over sexes. In other words, the resolution in the Norwegian biotic data is not by sex, hence the change to Sex set to NA in the HL table.
#' 
#'
#' @return An \code{\link{ICESDatrasData}} object.
#'
#' @export
#' 
ICESDatras <- function(
	BioticData#, 
	#Survey = character(), 
	#EDMO = character()
) {

	
	# @param Survey Character: The code of the survey. See the table "Survey" on https://vocab.ices.dk/.
	# @param EDMO Character: The code of the organization, e.g. 1351 for IMR. 
	
	
	# Run for each file:
	ICESDatrasData <- lapply(
  		BioticData, 
  		ICESDatrasOne#, 
  		#Survey = Survey, 
  		#EDMO = EDMO
  	)

	# Remove empty data (from invavlid files, non NMDBiotic >= 3)
	ICESDatrasData <- ICESDatrasData[lengths(ICESDatrasData) > 0]
	
	# Rbind accross files:
	ICESDatrasData <- rbindlist_StoxFormat(ICESDatrasData)
	
	return(ICESDatrasData)
}


ICESDatrasOne <- function(
	BioticDataOne#, 
	#Survey = character(), 
	#EDMO = character()
) {
	
	# Check input is a NMD Biotic v3 data
	if(!(BioticDataOne$metadata$useXsd %in% c("nmdbioticv3", "nmdbioticv3.1"))) {
		warning("StoX: Currently, only NMD Biotic version 3 and 3.1 data can be written by ICESDatras")
		return(matrix(1, 0, 0))
	}

	## 1. HH ##
	finalHH <- merge(BioticDataOne$mission, BioticDataOne$fishstation)

	# Make HH records
	finalHH[, `:=`(
		"Quarter" = getQuarter(stationstartdate),
		# Changed on 2022-05-12:
		#"Country" = getTSCountryByIOC(nation),
		"Country" = nation,
		# Changed to platformname on 2022-05-12. The user should translate to the codes on https://vocab.ices.dk/services/pox/GetCodeList/SHIPC:
		#"Ship" = getICESShipCode(platformname),
		"Platform" = platformname,
		"Gear" = gear, # Changed from "GOV" on 2022-01-27
		# Changed to NA, as the user should define the sweep length. It could generally be dependent on species, gear, depth, etc:
		#"SweepLngt" = getGOVSweepByEquipment(gear),
		"SweepLength" = NA_real_,
		# Removed this hard coding on 2022-05-12: 
		#"GearEx" = getGearEx(getGOVSweepByEquipment(gear), startyear, serialnumber, bottomdepthstart),
		"GearExceptions" = NA_character_,
		"DoorType" = trawldoortype, # Changed from "P" on 2022-01-27
		"StationName" = serialnumber,
		# This seems like a bug. The HaulNo should be a "Sequential numbering of hauls during cruise.", so for Norwegian data we use serialnumber also for this one:
		# "HaulNo" = station,
		# Changing back to "HaulNo" = station for consistency with historical data:
		#"HaulNo" = serialnumber,
		"HaulNumber" = station,
		"Year" = getYear(stationstartdate),
		"Month" = getMonth(stationstartdate),
		"Day" = getDay(stationstartdate),
		"StartTime" = getTimeShot(stationstarttime),
		"DepthStratum" = NA_character_,
		"HaulDuration" = as.numeric(getTimeDiff(stationstartdate, stationstarttime, stationstopdate, stationstoptime)),
		# Changed this to NA, since trawling should happen at day, and any check for this should be done prior to StoX:
		#"DayNight" = getDayNight(stationstartdate, stationstarttime, latitudestart, longitudestart),
		"DayNight" = NA_character_,
		"ShootLatitude" = round(latitudestart, digits = 4), 
		"ShootLongitude" = round(longitudestart, digits = 4), 
		"HaulLatitude" = round(latitudeend, digits = 4),
		"HaulLongitude" = round(longitudeend, digits = 4),
		# Changed 2022-05-12 to pasting area and location:
		#"StatRec" = getICESrect(latitudestart, longitudestart),
		#"StatRec" = getICESrect(area, location),
		# Changed 2023-01-14 to the new function getStatisticalRectangle() since getICESrect() has errors (includes "I" and A4, ..., A9) :
		"StatisticalRectangle" = getStatisticalRectangle(latitudestart, longitudestart),
		"BottomDepth" = round(bottomdepthstart),
		# Changed to NA on 2022-05-12, as StoX cannot decide which of gearcondition and samplequality should be used:
		#"HaulVal" = getHaulVal(gearcondition, samplequality),
		"HaulValidity" = NA_character_,
		"HydrographicStationID" = NA_character_,
		"StandardSpeciesCode" = "1", # We assume all possible species recorded. See http://vocab.ices.dk/?ref=88.
		"BycatchSpeciesCode" = "1", # We assume all possible species recorded. See http://vocab.ices.dk/?ref=89.
		"DataType" = "R", # "Data by haul", see http://vocab.ices.dk/?ref=9.
		"Netopening"= round(verticaltrawlopening, digits = 1),
		"Rigging" = NA_character_,
		"Tickler" = NA_integer_,
		# Before, the function getDistanceMeter() was used, which was inherited from old Datras code. Instead we use the distance that is given by NMDBiotic:
		#"Distance" = round(getDistanceMeter(latitudestart, longitudestart, latitudeend, longitudeend)),
		# Distance is in nautical miles in NMDBiotic and in meters in ICESDatras:
		"Distance" = round(distance * 1852),
		"WarpLength" = round(wirelength),
		"WarpDiameter" = NA_real_,
		"WarpDensity" = NA_real_,
		"DoorSurface" = trawldoorarea, # Changed from 4.5 on 2022-01-27. See https://kvalitet.hi.no/docs/pub/DOK04173.pdf.
		"DoorWeight" = trawldoorweight, # Changed from 1075 on 2022-01-27. See https://kvalitet.hi.no/docs/pub/DOK04173.pdf. 
		"DoorSpread" = ifelse(!is.na(trawldoorspread), round(trawldoorspread, digits = 1), NA_real_),
		"WingSpread" = NA_real_,
		"Buoyancy" = NA_real_,
		"KiteArea" = NA_real_, # Changed from 0.8 on 2022-01-27.
		"GroundRopeWeight" = NA_real_,
		"TowDirection" = ifelse(!is.na(direction), round(direction), NA_real_),
		# Changed to vesselspeed on 2022-05-12, which is in knots as required:
		#"GroundSpeed" = round(gearflow, digits = 1),
		"SpeedGround" = vesselspeed,
		"SpeedWater" = NA_real_,
		"SurfaceCurrentDirection" = NA_real_,
		"SurfaceCurrentSpeed" = NA_real_,
		"BottomCurrentDirection" = NA_real_,
		"BottomCurrentSpeed" = NA_real_,
		"WindDirection" = NA_real_,
		"WindSpeed" = NA_real_,
		"SwellDirection" = NA_real_,
		"SwellHeight" = NA_real_,
		"SurfaceTemperature" = NA_real_,
		"BottomTemperature" = NA_real_,
		"SurfaceSalinity" = NA_real_,
		"BottomSalinity" = NA_real_,
		"ThermoCline" = NA_character_,
		"ThermoClineDepth" = NA_real_,
		"CodendMesh" = NA_real_ ,
		"SecchiDepth" = NA_real_,
		"Turbidity" = NA_real_,
		"TidePhase" = NA_real_,
		"TideSpeed" = NA_real_,
		"PelagicSamplingType" = NA_character_,
		"MinTrawlDepth" = NA_real_,
		"MaxTrawlDepth" = NA_real_, 
		"SurveyIndexArea" = NA_character_,
		"Survey" = NA_character_,
		"EDMO" = NA_character_
	)]
	
	HHraw <- data.table::copy(finalHH[, c(
		"Quarter", "Country", "Platform", "Gear",
		"SweepLength", "GearExceptions", "DoorType", "StationName", "HaulNumber", "Year", "Month", "Day",
		"StartTime", "DepthStratum", "HaulDuration", "DayNight", "ShootLatitude", "ShootLongitude", "HaulLatitude", "HaulLongitude",
		"StatisticalRectangle", "BottomDepth", "HaulValidity", "HydrographicStationID", "StandardSpeciesCode", "BycatchSpeciesCode", "DataType", "Netopening",
		"Rigging", "Tickler", "Distance", "WarpLength", "WarpDiameter", "WarpDensity", "DoorSurface", "DoorWeight",
		"DoorSpread", "WingSpread", "Buoyancy", "KiteArea", "GroundRopeWeight", "TowDirection", "SpeedGround",
		"SpeedWater", "SurfaceCurrentDirection", "SurfaceCurrentSpeed", "BottomCurrentDirection", "BottomCurrentSpeed", "WindDirection", "WindSpeed",
		"SwellDirection", "SwellHeight", "SurfaceTemperature", "BottomTemperature", "SurfaceSalinity", "BottomSalinity", "ThermoCline", "ThermoClineDepth",
		"CodendMesh", "SecchiDepth", "Turbidity", "TidePhase", "TideSpeed", "PelagicSamplingType", "MinTrawlDepth", "MaxTrawlDepth", "SurveyIndexArea", "Survey", "EDMO")]
	)
	
	## 2. HL ##
	
	mergedHL <- merge(BioticDataOne$catchsample, finalHH, by=intersect(names(BioticDataOne$catchsample), names(finalHH)))
	
	groupCA <- c("missiontype", "startyear", "platform", "missionnumber", "serialnumber", "aphia", "sex")
	groupHL <- c(groupCA, "catchpartnumber")
	
	# Remove rows with empty aphia
	mergedHL <- mergedHL[!is.na(aphia)]
	
	#getSpecVal <- function(HaulVal, catchcount, lengthsamplecount, catchweight){
	#	temp <-  as.data.table(cbind(hv=HaulVal, cc=catchcount, lsc=lengthsamplecount, cw=catchweight))
	#	
	#	# Default is invalid
	#	temp[, res := "0"]
	#	
	#	temp[!is.na(cc) & !is.na(lsc) & !is.na(cw), res:="1"]
	#	temp[!is.na(cc) &  is.na(lsc) &  is.na(cw), res:="4"]
	#	temp[ is.na(cc) &  is.na(lsc) & !is.na(cw), res:="6"]
	#	temp[!is.na(cc) &  is.na(lsc) & !is.na(cw), res:="7"]
	#	temp[ is.na(cc) &  is.na(lsc) &  is.na(cw), res:="5"]
	#	temp[!is.na(cc) & !is.na(lsc) &  is.na(cw), res:="0"]
	#	
	#	temp[hv == "I", res:="0"]
	#	
	#	return(temp$res)
	#}
	
	# Changed on 2022-05-12 to NA:
	#mergedHL[, SpeciesValidity := getSpecVal(HaulVal, catchcount, lengthsamplecount, catchweight)]
	mergedHL[, SpeciesValidity := NA_character_]
	
	
	# catCatchWgt & subWeight
	# This simply uses the catchweight and lengthsampleweight from NMDBiotic, which in practice is grouped by station, species and subsample, where the subsample is not separated for sex. The format description for DATRAS is that sex should be an aggregation variable in these fields, and consequently we need to ignore sex in the calculation of TotalNumber and SubsampledNumber we do this by setting sex as NA:
	
	mergedHL[!is.na(catchweight), catCatchWgt := ceiling(catchweight * 1000)]
	mergedHL[!is.na(lengthsampleweight), subWeight := ceiling(lengthsampleweight * 1000)]
	
	# get sampleFac
	mergedHL[, sampleFac := catchweight / lengthsampleweight]
	
	# Merge with individual
	mergedHL <- merge(mergedHL, BioticDataOne$individual, by = intersect(names(mergedHL), names(BioticDataOne$individual)), all.x = TRUE)
	
	# # The followinng calculation of lngtCode interprets species, likely due to information about length reso# lution sitting in the individual table of NMDBiotic, and not the catchsample which is the table being trea# ted here. Otherwise the function getLengthCodeICES() would be a natural choise.
	# # Get herring or sprat
	# mergedHL[,`:=`( isHerringOrSprat = ifelse(aphia %in% c("126417", "126425"), TRUE, FALSE),
	# 				isCrustacean = ifelse(aphia %in% c("107275", "107276", "107369", "107253", "107703", "107704", "107350", "107254", "107205", "140712", "140687", "140658"), TRUE, FALSE))]
	# 
	# # Calculate lngtCode
	# mergedHL[,lngtCode := "1"]
	# mergedHL[is.na(sampletype), lngtCode := NA]
	# mergedHL[isCrustacean == TRUE, lngtCode := "."]
	# mergedHL[isHerringOrSprat == TRUE, lngtCode := "0"]
	
	mergedHL[, lngtCode := getLengthCodeICES(lengthresolution, format = "ICESDatras")]
	mergedHL[, lngtClass := scaleLengthUsingLengthCode(length, lngtCode, inputUnit = "m", format = "ICESDatras")]
	
	#LengthClass = scaleLengthUsingLengthCode(length, getLengthCodeICES(lengthresolution), inputUnit = "m"), 
	
	
	# lenInterval, and reportInMM
	### mergedHL[,`:=`(lenInterval = ifelse(lngtCode=="0", 5, 1), reportInMM = ifelse(lngtCode %ni% c("1", NA_real_), TRUE, FALSE))]
	### mergedHL[is.na(lenInterval), lenInterval := 1]
	
	# Get count
	mergedHL[, N := sum(!is.na(specimenid)), by = groupHL]
	# For the record with empty individual data
	mergedHL[N == 0, `:=`(lngtClass = NA_integer_, sex = NA_character_)]
	
	# Get Individual length
	### mergedHL[, length := length * 100]
	
	## 2022-05-12: Remove this in the future, when Convert-functionality is in place in StoX:
	## Some species have very small length in cm, use mm instead
	#mergedHL[length < 1, `:=`(lngtCode = ".", lenInterval = 1, reportInMM = TRUE)]
	## Process MM length
	#mergedHL[reportInMM == TRUE, length := length * 10]
	
	# 2022-05-12: Changed to use sex, and leave translate to the user:
	# Get sex
	#mergedHL[, sex := ifelse(is.na(sex), NA_character_, ifelse(sex == "1", "F", "M"))]
	#mergedHL[, sex := sex]
	
	### # Get lngtClass
	### for(interval in unique(mergedHL$lenInterval)) {
	### 	intVec <- seq(0, max(mergedHL$length, na.rm = T), by = interval)
	### 	mergedHL[lenInterval == interval, lngtClass := intVec[findInterval(length, intVec)]]
	### }
	
	
	
	# Count measured individual
	mergedHL[!is.na(length), lsCountTot := 1]
	
	
	# To fix the problem that the procedure at the IMR is to not split sex into different subsamples, we set the sex to NA here, so that aggregation to produce noMeas and totalNo sum over sexes. The reason for this is that catCatchWgt and subWeight are in practice summed over sexes, and we do not want to do estimation to split these by sex.
	finalHL <- data.table::copy(mergedHL)
	finalHL[, sex := NA]
	
	# Aggregate hlNoAtLngth and lsCountTot
	# We ignore the defined aggregation variable devstage, as this is always NA in norwegian data (hard coded below):
	finalHL <- finalHL[, lsCountTot := sum(lsCountTot), by = c(groupHL, "lngtClass")]
	finalHL <- unique(finalHL, by = c(groupHL, "lngtClass"))
	
	#finalHL <- finalHL[, .(N, lsCountTot = sum(lsCountTot)), by = c(
	#	groupHL,  
	#	"lngtClass", "Quarter", "Country", "Ship", "Gear", "SweepLngt", "GearEx", "DoorType", "HaulNo", "SpeciesValidity", "catCatchWgt", "sampleFac", "subWeight", "lngtCode", "stationtype", "lengthmeasurement"
	#	)
	#]
	
	# To fix the problem that the procedure at the IMR is to not split sex into different subsamples, we set the sex to NA here, so that aggregation to produce noMeas and totalNo sum over sexes. The reason for this is that catCatchWgt and subWeight are in practice summed over sexes, and we do not want to do estimation to split these by sex.
	#finalHL[, sex := NA]
	
	
	#finalHL <- finalHL[!duplicated(finalHL)]
	finalHL[,`:=`(noMeas = sum(lsCountTot)), by = groupHL]
	finalHL[,`:=`(totalNo = noMeas * sampleFac, subFactor = sampleFac)]
	
	HLraw <- data.table::copy(finalHL[, .(
		"Quarter" = Quarter,
		"Country" = Country,
		"Platform" = Platform,
		"Gear" = Gear,
		"SweepLength" = SweepLength,
		"GearExceptions" = GearExceptions,
		"DoorType" = DoorType,
		"StationName" = serialnumber,
		"HaulNumber" = HaulNumber,
		"Year" = as.character(startyear),
		"SpeciesCodeType" = "W", # "W" means that aphia is used for SpeciesCode
		"SpeciesCode" = aphia,
		"SpeciesValidity" = SpeciesValidity,
		"SpeciesSex" = sex,
		"TotalNumber" = round(totalNo, digits = 2),
		"SpeciesCategory" = catchpartnumber,
		"SubsampledNumber" = noMeas,
		"SubsamplingFactor" = round(subFactor, 4),
		"SubsampleWeight" = round(subWeight),
		"SpeciesCategoryWeight" = round(catCatchWgt),
		"LengthCode" = lngtCode,
		"LengthClass" = lngtClass,
		"NumberAtLength" = round(lsCountTot, 2),
		"DevelopmentStage" = NA_character_,
		# 2022-05-12: changed to lengthmeasurement, and the user should translate:
		#"LenMeasType" = convLenMeasType(lengthmeasurement)
		"LengthType" = lengthmeasurement,
		"Survey" = NA_character_
	)]
	)
	
	
	## 3. CA ##
	# Set preferredagereading to 1 if not given:
	mergedHL[is.na(preferredagereading), preferredagereading := 1]
	baseAge <- intersect(names(mergedHL), names(BioticDataOne$agedetermination))
	mergedCA <- merge(mergedHL, BioticDataOne$agedetermination, by.x=c(baseAge, "preferredagereading"), by.y= c(baseAge, "agedeterminationid"), all.x = TRUE)
	
	# Remove empty individual
	mergedCA <- mergedCA[!is.na(specimenid)]
	
	# 2022-0512: Changed to NA
	# Get maturity
	#mergedCA[, maturity:=getDATRASMaturity(Quarter, aphia, specialstage, maturationstage)]
	# Added speciealstage for Maturity on 2022-11-21, see below:
	mergedCA[, maturity := specialstage]
	
	# Aggregate count
	aggregateBy <- c(groupCA,  "lngtClass", "maturity", "age")
	#mergedCA[!is.na(individualweight), `:=`(nWithWeight = .N, totWeight = sum(individualweight)), by = aggregateBy]
	#mergedCA[, `:=`(nInd = .N), by = aggregateBy]
	#mergedCA <- unique(mergedCA, by = aggregateBy)
	#mergedCA[!is.na(nWithWeight),  meanW := totWeight / nWithWeight]
	# Get number of individuals and mean weight (to use for the individuals without weight):
	mergedCA[, nInd := .N, by = aggregateBy]
	mergedCA[, meanW := mean(individualweight, na.rm = TRUE), by = aggregateBy]
	mergedCA <- unique(mergedCA, by = aggregateBy)
	
	
	CAraw <- data.table::copy(mergedCA[,
		.(
			"Quarter" = Quarter,
			"Country" = Country,
			"Platform" = Platform,
			"Gear" = Gear,
			"SweepLength" = SweepLength,
			"GearExceptions" = GearExceptions,
			"DoorType" = DoorType,
			"StationName" = serialnumber,
			"HaulNumber" = HaulNumber,
			"Year" = as.character(startyear),
			"SpeciesCodeType" = "W", # "W" means that aphia is used for SpeciesCode (http://vocab.ices.dk/?ref=96).
			"SpeciesCode" = aphia,
			"AreaType" = "0", # "0" means that StatisticalRectangle is used for AreaCode (http://vocab.ices.dk/?ref=10).
			"AreaCode" = StatisticalRectangle,
			"LengthCode" = lngtCode,
			"LengthClass" = lngtClass,
			"IndividualSex" = sex,
			"IndividualMaturity" = maturity,
			"AgePlusGroup" = NA_character_,
			# Changed to age on 2022-05-12:
			#"AgeRings" = ifelse(!is.na(age), age, NA_real_),
			"IndividualAge" = age,
			"NumberAtLength" = nInd,
			"IndividualWeight" = ifelse(!is.na(meanW), round(meanW * 1000, 1), NA_real_),
			# 2022-05-12: The user should select maturity scale:
			#"MaturityScale" = "M6", # See getDATRASMaturity() which is made for MaturityScale M6. See also http://vocab.ices.dk/?ref=1481.
			"MaturityScale" = NA_character_,
			"FishID" = specimenid,
			"GeneticSamplingFlag" = ifelse(!is.na(tissuesample), "Y", "N"),
			"StomachSamplingFlag" = ifelse(!is.na(stomach), "Y", "N"),
			# 2022-05-12: Replaced by agingstructure, and the user should translate:
			#"AgeSource" = convAgeSource(agingstructure),
			"AgeSource" = agingstructure,
			"AgePreparationMethod" = NA_character_,
			# 2022-05-12: The user needs to do the translation here, as readability
			#"OtGrading" = ifelse(readability %in% as.character(c(1:4)), readability, NA_character_),  # From http://tomcat7.imr.no:8080/apis/nmdapi/reference/v2/dataset/otolithreadability?version=2.0 and http://vocab.ices.dk/?ref=1395
			"OtolithGrading" = ifelse(agingstructure %in% as.character(2), readability, NA_character_), 
			"ParasiteSamplingFlag" = ifelse(!is.na(parasite), "Y", "N"), 
			"LiverWeight" = liverweight,
			"Survey" = NA_character_
		)]
	)
	
	
	## Prepare for cleaning (mostly from the old Rstox. Will change as we improve) ##
	hh <- HHraw
	hl <- HLraw
	ca <- CAraw
	
	## WARN #0:
	# It's possible to have two same aphia (but different species, e.g. SILD05) catch sampes in a haul.
	# We need to combine them if we have two different TotalNumber and catcatchwgt.
	
	# Find duplicate species in a haul
	dupl <- stats::aggregate(catchcategory ~ aphia + serialnumber, BioticDataOne$catchsample, FUN = function(x) length(unique(x)))
	dupl <- dupl[dupl$catchcategory > 1, ]
	
	# Find the above in our DATRAS HL
	if(nrow(dupl)) {
		hlAphiaSerialnumber <- hl[(hl$SpeciesCode %in% dupl$aphia & hl$StationName %in% dupl$serialnumber),]
		
		# Build the formula to use in stats::aggregate(), containing only those columns that are not all NA:
		groupingVariables <- c("StationName", "SpeciesCode", "SpeciesSex", "SpeciesCategory")
		allNA <- sapply(groupingVariables, function(x) all(is.na(hlAphiaSerialnumber[[x]])))
		groupingVariables <- groupingVariables[!allNA]
		aggregateFormula <- stats::as.formula(paste0("SpeciesCategoryWeight ~ ", paste(groupingVariables, collapse = " + ")))
		
		found <- stats::aggregate(
			aggregateFormula, 
			hlAphiaSerialnumber, 
			FUN = function(x) length(unique(x))
		)
		found <- found[found$SpeciesCategoryWeight > 1, ]
		
		for(iz in seq_len(nrow(found))) {
			
			atMatch <- apply(mapply("==", hl[, ..groupingVariables], found[iz, groupingVariables]), 1, all)
			tmpHL <- hl[atMatch, ]
			# Fix SpeciesCategoryWeight
			hl[atMatch, "SpeciesCategoryWeight"] <- round(mean(tmpHL$SpeciesCategoryWeight))
			# Fix SpeciesCategoryWeight
			hl[atMatch, "SubsampleWeight"] <- round(mean(tmpHL$SubsampleWeight))
			# Fix totalNo
			hl[atMatch, "TotalNumber"] <- sum(unique(tmpHL$TotalNumber))
			# Fix noMeas
			hl[atMatch, "SubsampledNumber"] <- sum(tmpHL$NumberAtLength)
			# Finally, fix SubsamplingFactor
			hl[atMatch, "SubsamplingFactor"] <- sum(unique(tmpHL$TotalNumber))/sum(tmpHL$NumberAtLength)
		}
	}
	
	## WARN #2:
	## will now get errors in DATRAS upload for duplicate records
	hl <- hl[!duplicated(hl),]
	
	## hl and ca contain 0-tow info - must throw these out
	hl <- hl[hl$StationName %in% hh$StationName,]
	ca <- ca[ca$StationName %in% hh$StationName,]
	# throw out ca records for Invalid hauls
	ca <- ca[!ca$StationName %in% hh$StationName[hh$HaulValidity == 'I'],]
	
	#IU: Improved cleaning#
	# Use join to find missing value in HL
	if (nrow(ca) > 0) {
		#testca <- unique(data.frame(StNo=ca$StNo, SpecCode=ca$SpecCode, ca=TRUE))
		#testhl <- unique(data.frame(StNo=hl$StNo, SpecCode=hl$SpecCode, hl=TRUE))
		testca <- unique(data.table::data.table(StationName = ca$StationName, SpeciesCode = ca$SpeciesCode, ca = TRUE))
		testhl <- unique(data.table::data.table(StationName = hl$StationName, SpeciesCode = hl$SpeciesCode, hl = TRUE))
		tt <- merge(testca, testhl, by = c("StationName","SpeciesCode"), all = TRUE)
		missingHL <- tt[is.na(tt$hl),]
		
		# Populate missing value in HL
		for(idxHL in seq_len(nrow(missingHL))) {
			r <- missingHL[idxHL,]
			tmp <- hl[hl$StationName == r$StationName,][1,]
			tmp$SpeciesCode <- r$SpeciesCode
			tmp$SpeciesValidity <- 4
			tmp$TotalNumber <- c(hh$HaulDuration[hh$StationName == r$StationName])
			tmp$SpeciesCategoryWeight <- NA
			hl <- rbind(hl, tmp)
		}
	}
	## WARN #4:
	
	# Removed on 2022-05-12. The user should use TranslateICESDatras:
	# Use plus group for herring and mackerel individuals with age ring above 15
	#ca[ which((ca$SpecCode==127023 | ca$SpecCode==126417) & ca$AgeRings >= 15), c("PlusGr", "AgeRings")] <- list("+", 15)
	
	# Order HL
	hl <- hl[order(hl$StationName),]
	
	#
	ICESDatrasData <- list(HH = hh, HL = hl, CA = ca)
	
	return(ICESDatrasData)
}











#getTSCountryByIOC <- function(nation) {
#	cnvTbl <- c("58" = "NO")
#	
#	x <- cnvTbl[as.character(nation)]
#	x[is.null(x)] <- NA
#	return(x)
#}

getGearEx <- function(sweep, year, serialnumber, depth) {
	
	temp <-  as.data.table(cbind(sweep, year, serialnumber, depth))
	temp[, res:= "S"]
	
	temp[year == 2011 & serialnumber > 24362 & depth >= 70 | year == 2012
		 | year == 2011 & serialnumber >= 24135 & depth >= 70, res:="ST"]
	
	return (temp$res)
}

getYear <- function(stationstartdate) {
	format(as.Date(stationstartdate, format="%Y-%m-%dZ"), "%Y")
}

getMonth <- function(stationstartdate) {
	format(as.Date(stationstartdate, format="%Y-%m-%dZ"), "%m")
}

getDay <- function(stationstartdate) {
	format(as.Date(stationstartdate, format="%Y-%m-%dZ"), "%d")
}

getTimeShot <- function(stationstarttime) {
	
	timeshot <- function(y) {
		if(length(y) == 3) {
			return(paste0(y[1], y[2]))
		} else {
			return(NA)
		}
	}
	
	x <- strsplit(stationstarttime, ":")
	
	return(unlist(lapply(x, timeshot)))
}

getQuarter <- function(stationstartdate) {
	x <- format(as.Date(stationstartdate, format="%Y-%m-%dZ"), "%m")
	return(floor((as.numeric(x) - 1) / 3 + 1))
}

# # Adopted from: https://www.mathworks.com/matlabcentral/fileexchange/62180-sunriseset-lat-lng-utcoff-date-plot
# getDayNight <- function(stationstartdate, stationstarttime, latitudestart, longitudestart, UTCoff = 0) {
# 	
# 	deg2rad <- function(val) {
# 		return(val * (pi / 180))
# 	}
# 	
# 	rad2deg <- function(val) {
# 		return(val * (180 / pi))
# 	}
# 	
# 	datetime0 <- as.POSIXct("1990-12-30", tz = "UTC")
# 	
# 	uniqueDates <- unique(stationstartdate)
# 	
# 	nDaysA = as.numeric(difftime(uniqueDates, datetime0, units = "days")) # Number of days since 01/01
# 	
# 	nTimes = 24*3600                       # Number of seconds in the day
# 	tArray = seq(0, 1, length = nTimes)
# 	
# 	ssTab <- list()
# 	
# 	for(idx in seq_len(length(nDaysA))) {
# 		
# 		nDays <- nDaysA[idx]
# 		lat <- latitudestart[idx]
# 		lng <- longitudestart[idx]
# 		localdate <- as.POSIXct(uniqueDates[idx], tz = "UTC")
# 		
# 		# Compute
# 		# Letters correspond to colums in the NOAA Excel
# 		E = tArray
# 		F = nDays + 2415018.5 + E - UTCoff / 24
# 		G = (F - 2451545) / 36525
# 		I = (280.46646 + G * (36000.76983 + G * 0.0003032)) %% 360
# 		J = 357.52911 + G * (35999.05029 - 0.0001537 * G)
# 		K = 0.016708634 - G * (0.000042037 + 0.0000001267 * G)
# 		L = sin(deg2rad(J)) * (1.914602 - G * (0.004817 + 0.000014 * G))+sin(deg2rad(2 * J)) * (0.019993 - 0.000101*G) + sin(deg2rad(3 * J)) * 0.000289
# 		M = I + L
# 		P = M - 0.00569 - 0.00478 * sin(deg2rad(125.04 - 1934.136 * G))
# 		Q = 23 + (26 + ((21.448 - G * (46.815 + G * (0.00059 - G * 0.001813)))) / 60) / 60
# 		R = Q + 0.00256 * cos(deg2rad(125.04 - 1934.136 * G))
# 		T = rad2deg(asin(sin(deg2rad(R)) * sin(deg2rad(P))))
# 		U = tan(deg2rad(R/2)) * tan(deg2rad(R/2))
# 		V = 4 * rad2deg(U * sin(2 * deg2rad(I))-2 * K * sin(deg2rad(J)) + 4 * K * U * sin(deg2rad(J)) *  cos(2*deg2rad(I)) - 0.5 * U * U * sin(4 * deg2rad(I)) - 1.25 * K * K * sin(2 * # deg2rad(J)))
# 		AB = (E * 1440 + V + 4 *lng - 60 * UTCoff) %% 1440
# 		
# 		
# 		AC = ifelse (AB/4 < 0, AB/4 + 180, AB/4 - 180)
# 		
# 		AD = rad2deg(acos(sin(deg2rad(lat)) * sin(deg2rad(T)) + cos(deg2rad(lat)) * cos(deg2rad(T)) * cos(deg2rad(AC))))
# 		
# 		# Test whether we are in midnightsun: 
# 		WArg <- cos(deg2rad(90.833)) / (cos(deg2rad(lat)) * cos(deg2rad(T))) - tan(deg2rad(lat)) * tan(deg2rad(T))
# 		# Truncate WArg to [-1, 1], where below is midnightsun and above 1 is myrketid:
# 		if(any(WArg < -1)) {
# 			WArg[WArg < -1] <- -1
# 		}
# 		if(any(WArg > 1)) {
# 			WArg[WArg > 1] <- 1
# 		}
# 		
# 		W = rad2deg(acos(WArg))
# 		X = (720 - 4 * lng - V + UTCoff * 60) * 60
# 		
# 		sunrise = which.min(abs(X - round(W * 4 * 60) - nTimes * tArray))
# 		sunset = which.min(abs(X+round(W*4*60) - nTimes*tArray))
# 		
# 		sunrisetime = localdate + sunrise
# 		sunsettime = localdate + sunset
# 		
# 		ssTab[[uniqueDates[idx]]] <- list(sunrise = sunrisetime, sunset = sunsettime)
# 	}
# 	
# 	getDN <- function(x, ssTab) {
# 		
# 		y <- ssTab[[format(x, "%Y-%m-%dZ")]]
# 		
# 		if(x < y$sunrise || x >= y$sunset) {
# 			return("N")
# 		} else {
# 			return("D")
# 		}
# 	}
# 	
# 	datetime <- as.POSIXct(gsub("Z", " ", paste0(stationstartdate, stationstarttime)), tz = "UTC")
# 	
# 	return(unlist(lapply(datetime, getDN, ssTab)))
# }

# # Convert Length Measurement Type
# # http://tomcat7.imr.no:8080/apis/nmdapi/reference/v2/dataset/lengthmeasurement?version=2.0
# # http://vocab.ices.dk/?ref=1392
# convLenMeasType <- function(LenMeasType) {
# 	# Convert table
# 	ct <- c(
# 		"B" = 5,
# 		"C" = 6,
# 		"E" = 1,
# 		"F" = 8,
# 		"G" = 4,
# 		"H" = 3,
# 		"J" = 2,
# 		"L" = 7,
# 		"S" = 9
# 	)
# 	return(ct[LenMeasType])
# }

## Convert aging structure source
## http://tomcat7.imr.no:8080/apis/nmdapi/reference/v2/dataset/agingstructure?version=2.0
## http://vocab.ices.dk/?ref=1507
## This function seems to incorrect, as the AgeSource documentation on here only lists three values:
## http://vocab.ices.dk/?ref=1482
#convAgeSource <- function(AgeSource) {
#	# Convert table
#	if(!all(AgeSource %in% c(NA, "1", "2", "7"))) {
#		warning("StoX: The conversion from agingstructure to AgeSource may be wrong for other values than 1, 2 and 7. Please noify #the developers of StoX.")
#	}
#	
#	ct <- c("1" = "scale",
#			"2" = "otolith",
#			"4" = "df-spine",
#			"6" = "spine",
#			"7" = "vertebra",
#			"8" = "caudal-thorn")
#	return(ct[AgeSource])
#}


#translateAgeSource <- function(agingstructure) {
#	# Convert table:
#	ct <- c(
#		"1" = "Scale",
#		"2" = "Otolith",
#		"7" = "Vertebra"
#	)
#	return(ct[agingstructure])
#}

roundDrop0 <- function(x, digits = 0) {
	notNA <- !is.na(x)
	x[notNA] <- formatC(x[notNA], digits = digits, format = "f", drop0trailing = TRUE)
	return(x)
}



roundDownTo <- function(x, to, buffer = 1e-10) {
	floor(x / to + buffer) * to
}



#' Regroup LengthCode and LengthClass ICESDatrasData
#'
#' ICES Datras requires equal LengthCode and LengthClass per haul and species. 
#'
#' @param ICESDatrasData A \code{\link{ICESDatrasData}} object as returned from \code{\link{ICESDatras}}.
#' @param RegroupMethod Character: A string naming the method to use, one of "ResolutionTable", for specifying a table with the desired resolutions for combinations of the \code{ResolutionTableVariables}, or "HighestResolution" to set the highest possible resolution for each combination of the \code{GroupingVariables}. In the former case, the \code{ResolutionTableVariables} must be given when the function is used in the StoX GUI, but this is not required when applied directly in R.
#' @param ResolutionTableVariables A vector of names of the variables to use in the \code{ResolutionTable} in the GUI. Typically this could be "SpeciesCode".
#' @param ResolutionTable A table of a number of columns named by variables present in both the HL and the CA, and one column named "ResolutionCode" holding desired resolution for the combinations of values in the previous columns. The values in the column "ResolutionCode" must be "1 mm", "5 mm" or "1 cm" (see getRstoxDataDefinitions("lengthCode_unit_table")$shortnameNMDBiotic). An example is data.table::data.table(SpeciesCode = c("126417", "126441"), ResolutionCode = c("5 mm", "1 cm")).
#' @param GroupingVariables A vector of variable names giving the variables by which to reduce the resolution of LengthCode and LengthClass Defaulted to "HaulNumber" and "SpeciesCode". 
#' @param AggregateHLNoAtLngt Logical: If TRUE aggregate the variable NumberAtLength after regrouping lengths.
#' @param AggregationVariablesHL A vector of variables of the HL table for which to aggregate individuals. It is recommended to use c("HaulNumber", "SpeciesCode", "SpeciesCategory", "SpeciesSex", "LengthClass"), which is the default when creating a StoX process.
#' @param AggregateCANoAtLngt Logical: If TRUE aggregate the variable NumberAtLength after regrouping lengths.
#' @param AggregationVariablesCA A vector of variables of the HL table for which to aggregate individuals. It is recommended to use c("HaulNumber", "SpeciesCode", "LengthClass", "SpeciesSex", "IndividualMaturity", "IndividualAge"), which is the default when creating a StoX process.
#'
#' @return An \code{\link{ICESDatrasData}} object.
#'
#' @export
#' 
RegroupLengthICESDatras <- function(
	ICESDatrasData, 
	RegroupMethod = c("ResolutionTable", "HighestResolution"), 
	# For RegroupMethod = "ResolutionTable":
	ResolutionTableVariables = character(),
	ResolutionTable = data.table::data.table(), 
	# For RegroupMethod = "HighestResolution":
	GroupingVariables = character(),
	# Whether to aggregate numbers:
	AggregateHLNoAtLngt = TRUE, 
	AggregationVariablesHL = character(), 
	AggregateCANoAtLngt = TRUE, 
	AggregationVariablesCA = character()
) {
	
	RegroupMethod <- match_arg_informative(RegroupMethod)
	ICESDatrasData <- data.table::copy(ICESDatrasData)
	
	ICESDatrasData$HL <- RegroupLengthICESDatrasOneTable(
		ICESDatrasData$HL, 
		RegroupMethod = RegroupMethod, 
		ResolutionTable = ResolutionTable, 
		GroupingVariables = GroupingVariables
	)
	ICESDatrasData$CA <- RegroupLengthICESDatrasOneTable(
		ICESDatrasData$CA, 
		RegroupMethod = RegroupMethod, 
		ResolutionTable = ResolutionTable, 
		GroupingVariables = GroupingVariables
	)
	
	
	# Sum up individuals:
	if(AggregateHLNoAtLngt) {
		#sumBy <- c("StNo", "SpecCode", "SpeciesSex", "SpeciesCategory", "LengthClass")
		sumBy <- AggregationVariablesHL
		if(any(! sumBy %in% names(ICESDatrasData$HL))) {
			toRemove <- setdiff(sumBy, names(ICESDatrasData$HL))
			warning("Removing the following AggregationVariablesHL not present in the HL table: ", paste(toRemove, collapse = ", "))
			sumBy <- intersect(sumBy, names(ICESDatrasData$HL))
		}
		
		ICESDatrasData$HL[, NumberAtLength := sum(NumberAtLength), by = sumBy]
		ICESDatrasData$HL <- unique(ICESDatrasData$HL, by = sumBy)
	}
	
	# Sum up individuals:
	if(AggregateCANoAtLngt) {
		#sumBy <- c("StNo", "SpecCode", "SpeciesSex", "SpeciesCategory", "LengthClass")
		sumBy <- AggregationVariablesCA
		if(any(! sumBy %in% names(ICESDatrasData$CA))) {
			toRemove <- setdiff(sumBy, names(ICESDatrasData$CA))
			warning("Removing the following AggregationVariablesCA not present in the HL table: ", paste(toRemove, collapse = ", "))
			sumBy <- intersect(sumBy, names(ICESDatrasData$CA))
		}
		
		ICESDatrasData$CA[, NumberAtLength := sum(NumberAtLength), by = sumBy]
		ICESDatrasData$CA <- unique(ICESDatrasData$CA, by = sumBy)
	}
	
	return(ICESDatrasData)
}

RegroupLengthICESDatrasOneTable <- function(table, RegroupMethod, ResolutionTable, GroupingVariables) {
	
	# Get the Datras length resolution definitions:
	lengthCode_unit_table <- getRstoxDataDefinitions("lengthCode_unit_table")
	
	if(RegroupMethod == "ResolutionTable") {
		
		# Get the ResolutionTableVariables as the variables of the ResolutionTable except "ResolutionCode" (in StoX the columns of ResolutionTable will be ser by the ResolutionTableVariables, but from R this should not be required):
		ResolutionTableVariables <- setdiff(names(ResolutionTable), "ResolutionCode")
		
		# Add the columns "newLngtCode", "newLngtCodeNumeric" and "newReportingUnit" to the ResolutionTable, and remove the "ResolutionCode", in order to merge the resolution information into the table:
		atResolution <- ResolutionTable[, match(ResolutionCode, lengthCode_unit_table$shortnameNMDBiotic)]
		ResolutionTable[, newLngtCode := lengthCode_unit_table$lengthCodeICESDatras[..atResolution]]
		ResolutionTable[, newLngtCodeNumeric := lengthCode_unit_table$numericResolution[..atResolution]]
		ResolutionTable[, newReportingUnit := lengthCode_unit_table$reportingUnit[..atResolution]]
		ResolutionTable[, ResolutionCode := NULL]
		
		# Merge in the resolution information (preserve column order):
		originalColOrder <- names(table)
		table <- merge(table, ResolutionTable, by = ResolutionTableVariables, all.x = TRUE, sort = FALSE)
		data.table::setcolorder(table, originalColOrder)
	}
	else if(RegroupMethod == "HighestResolution") {
		# Get the highest resolution by the GroupingVariables:
		table[, c("newLngtCode", "newLngtCodeNumeric", "newReportingUnit") := getHighestResolution(.SD), by = GroupingVariables]
	}
	
	# Regroup only rows with non-missing newLngtCode:
	toRegroup <- table[, !is.na(newLngtCode)]
	
	# Get the unit temporarily:
	table[, currentReportingUnit := lengthCode_unit_table$reportingUnit[match(LengthCode, lengthCode_unit_table$lengthCodeICESDatras)]]
	
	# Change the units:
	table[toRegroup, LengthClass := scaleUsingUnit(LengthClass, inputUnit = currentReportingUnit, outputUnit = newReportingUnit)]
	
	# Round down:
	table[toRegroup, LengthClass := roundDownTo(LengthClass, to = newLngtCodeNumeric)]
	
	# Set the new LengthCode:
	table[toRegroup, LengthCode := newLngtCode]
	
	table[, currentReportingUnit := NULL]
	table[, newLngtCode := NULL]
	table[, newLngtCodeNumeric := NULL]
	table[, newReportingUnit := NULL]
	
	return(table)
}

getHighestResolution <- function(table) {
	
	# Get the Datras length resolution definitions:
	lengthCode_unit_table <- getRstoxDataDefinitions("lengthCode_unit_table")
	
	# Get the lowest resolution:
	maxRank <- table[, max(lengthCode_unit_table$rank[match(LengthCode, lengthCode_unit_table$lengthCodeICESDatras)])]
	atMaxRank <- which(lengthCode_unit_table$rank == maxRank)
	
	# Insert the LengthCode, LngtCodeNumeric and ReportingUnit:
	newLngtCode <- lengthCode_unit_table$lengthCodeICESDatras[atMaxRank]
	newLngtCodeNumeric <- lengthCode_unit_table$numericResolution[atMaxRank]
	newReportingUnit <- lengthCode_unit_table$reportingUnit[atMaxRank]
	
	list(
		newLngtCode = newLngtCode, 
		newLngtCodeNumeric = newLngtCodeNumeric, 
		newReportingUnit = newReportingUnit
	)
}




#' Write ICESDatras to CSV fille
#'
#' Writes \code{\link{ICESDatrasData}} to a csv file for each input acoustic file used to create the \code{\link{ICESDatras}}
#'
#' @inheritParams ModelData
#'
#' @return An object of StoX data type \code{\link{WriteICESDatrasData}}.
#'
#' @export
WriteICESDatras <- function(ICESDatrasData){
	
	#WriteICESDatrasData <- lapply(
	#	ICESDatrasData, 
	#	WriteICESDatrasOne, 
	#	na = "-9"
	#)
	
	WriteICESDatrasData <- WriteICESDatrasOne(ICESDatrasData, na = "-9")
	
	return(WriteICESDatrasData)
}


WriteICESDatrasOne <- function(ICESDatrasDataOne, na = "-9"){
	
	# Convert all tables to string matrix with header and record, and rbind:
	ICESDatrasCSVDataOne <- convertToRecordTypeMatrix(ICESDatrasDataOne)
	
	# Replace NAs:
	if(length(na)) {
		ICESDatrasCSVDataOne <- lapply(ICESDatrasCSVDataOne, function(x) {x[is.na(x)] <- na; x})
	}
	
	# IMPORTANT NOTE: The ICES DATRAS format assumes that the three tables HH, HL and CA are stacked in a comma separated file but without padding the tables to equal number of columns as is done for ICESAcoustic and ICESBiotic. For this reason we need to paste the data to a chatacter vector and write as lines. RstoxFramework detects that the output as a vector of characters and uses writeLines() to produce the file.
	
	# Convert each line of each table to comma separated:
	ICESDatrasCSVDataOne <- lapply(ICESDatrasCSVDataOne, apply, 1, paste, collapse = ",")
	
	# Join to one vector, to be written to one file:
	ICESDatrasCSVDataOne <- unlist(ICESDatrasCSVDataOne)
	
	return(ICESDatrasCSVDataOne)
}






#' Convert BioticData to ICESDatsuSC format
#'
#' Given an \code{\link{BioticData}} object, this function converts to ICESDatsuSC format. Note that this function only supports
#' \code{\link{BioticData}} NMDBiotic version > 3 XML files.
#'
#' @param BioticData a \code{BioticData} object from an XML file with NMD biotic version 3 format.
#'
#' @return An \code{\link{ICESDatsuscData}} object.
#'
#' @export
#' 
ICESDatsusc <- function(
    BioticData
) {
  
  # Run for each file:
  ICESDatsuscData <- lapply(
    BioticData, 
    ICESDatsuscOne
  )
  
  # Remove empty data (from invavlid files, non NMDBiotic >= 3)
  ICESDatsuscData <- ICESDatsuscData[lengths(ICESDatsuscData) > 0]
  
  # Rbind accross files:
  ICESDatsuscData <- rbindlist_StoxFormat(ICESDatsuscData)
  
  return(ICESDatsuscData)
}


ICESDatsuscOne <- function(
    BioticDataOne
) {
  
  # Check input is a NMD Biotic v3 data
  if(!(BioticDataOne$metadata$useXsd %in% c("nmdbioticv3.1"))) {
    warning("StoX: Currently, only NMD Biotic version 3.1 data can be written by ICESDatras")
    return(matrix(1, 0, 0))
  }
  
  '%ni%' <- Negate('%in%')
  
  ## 1. FI ##
  
  #TODO: Is this the best way of handling nation? what if we have multiple nation?
  finalFI <-data.table::data.table('Country'=unique(BioticDataOne$fishstation$nation),
                                   'Reporting_organisation'=NA_character_, 
                                   'CruiseID'= NA_character_)
  
  ## 2. HH ##
  finalHH <- merge(BioticDataOne$mission, BioticDataOne$fishstation, all = TRUE, sort = FALSE)
  
  # Make HH records
  finalHH[, `:=`(
    "Ship" = platformname,  #%Mandatory
    "Gear" = gear,   #%Mandatory
    "HaulNo" = station,  #%Mandatory
    "StationNumber" = serialnumber,  #%Mandatory
    "Year" = getYear(stationstartdate),  #%Mandatory
    "Month" = getMonth(stationstartdate),  #%Mandatory
    "Day" = getDay(stationstartdate),  #%Mandatory
    "Time" = getTimeShot(stationstarttime),  #%Mandatory
    "ShootLat" = round(latitudestart, digits = 4),   #%Mandatory
    "ShootLong" = round(longitudestart, digits = 4),   #%Mandatory
    "HaulLat" = round(latitudeend, digits = 4),  #%Optional
    "HaulLong" = round(longitudeend, digits = 4),  #%Optional
    "ICESrectangle" = NA_character_,  #TODO: do we need to fix this? 
    "Depth" = round(bottomdepthstart),  #%Optional
    "Survey" = NA_character_,          #%Optional
    "ICESDatabase" = NA_character_     #%Optional
  )]
  
  HHraw <- data.table::copy(finalHH[, c(
     "Ship", "Gear","HaulNo","StationNumber","Year", "Month", 
     "Day", "Time", "ShootLat", "ShootLong","HaulLat","HaulLong",
     "ICESrectangle","Depth","Survey","ICESDatabase")]
  )
  
  
  
  ## 3. PI ##
  finalPI <- merge(BioticDataOne$mission,BioticDataOne$fishstation, all= TRUE, sort = FALSE)
  finalPI <- merge(BioticDataOne$catchsample,finalPI, all = TRUE, sort = FALSE)
  finalPI <- merge(BioticDataOne$individual,finalPI, all = TRUE, sort = FALSE)
  finalPI <- merge(BioticDataOne$agedetermination,finalPI, all = TRUE, sort = FALSE)
  
  
  # Make PI records
  finalPI[, `:=`(
    "Ship" = platformname,
    "Gear" = gear,
    "HaulNo" = station,
    "StationNumber" = serialnumber,
    "Year" = getYear(stationstartdate),
    "Month" = getMonth(stationstartdate),
    "Day" = getDay(stationstartdate),
    "Time" = getTimeShot(stationstarttime),
    "FishID" = specimenid,        
    "AphiaIDPredator" = aphia, 
    "IndWgt" = individualweight, 
    "Number" = NA_character_, #Number of species taken for stomach analyses (pooled samples)
    "MeasurementIncrement" = lengthresolution, 
    "Length" = length, 
    "AgeSource" = agingstructure, 
    "Age" = age, 
    "Sex" = sex, 
    "MaturityScale" = NA_character_,           #Need to be set by user
    "MaturityStage" = specialstage, 
    "PreservationMethod" = stomach,            
    "Regurgitated" = NA_character_,           
    "StomachFullness" = stomachfillfield,           
    "FullStomWgt" = stomachweight,            
    "EmptyStomWgt" = -9,            
    "StomachEmpty" = NA_character_,            
    "GenSamp" = NA_character_,          
    "Notes" = NA_character_           
  )]
 
  
  ## 4. PP ##
  finalPP <-  merge(BioticDataOne$prey, BioticDataOne$preylengthfrequencytable, all= TRUE, sort = FALSE)
  finalPP <-  merge(BioticDataOne$individual,finalPP, all= TRUE, sort = FALSE)
  finalPP <-  merge(BioticDataOne$catchsample,finalPP, all= TRUE, sort = FALSE)
  finalPP <-  merge(BioticDataOne$fishstation,finalPP, all= TRUE, sort = FALSE)
  finalPP <-  merge(BioticDataOne$mission,finalPP, all= TRUE, sort = FALSE)
  # Make PP records
  finalPP[, `:=`(
    "Ship" = platformname,
    "Gear" = gear, 
    "HaulNo" = station,
    "StationNumber" = serialnumber,
    "Year" = getYear(stationstartdate),
    "Month" = getMonth(stationstartdate),
    "Day" = getDay(stationstartdate),
    "Time" = getTimeShot(stationstarttime),
    "FishID" = specimenid,    
    "AphiaIDPredator" = aphia, 
    "AphiaIDPrey" = preycategory,           
    "IdentMet" = NA_character_,            
    "DigestionStage" = preydigestion,            
    "GravMethod" = stomach,            
    "SubFactor" = NA_character_,           
    "PreySequence" = preysampleid,            
    "Count" = lengthintervalcount,            
    "UnitWgt" = weightresolution,            
    "Weight"= totalweight,     
    "UnitLngt"= interval,   
    "Length" = lengthintervalstart,            
    "OtherItems" = NA_character_,           
    "OtherCount" = NA_character_,            
    "OtherWgt" = NA_character_,           
    "AnalysingOrg" = NA_character_,           
    "Notes" = NA_character_,           
    "preyforeignobject" = preyforeignobject
  )]
  
  #___________________________________________________________________________#
  #Special handling
  
  
  
  #Handling the foreign object. 
  #TODO: needs revision
  finalPP[!is.na(finalPP$preyforeignobject),]$OtherItems<-finalPP[!is.na(finalPP$preyforeignobject),]$preyforeignobject
  finalPP[!is.na(finalPP$preyforeignobject),]$OtherCount<-finalPP[!is.na(finalPP$preyforeignobject),]$Count
  finalPP[!is.na(finalPP$preyforeignobject),]$OtherWgt<-finalPP[!is.na(finalPP$preyforeignobject),]$Weight
  finalPP[!is.na(finalPP$preyforeignobject),]$Count<-NA_character_
  finalPP[!is.na(finalPP$preyforeignobject),]$Weight<-NA_character_
  
  #Handling the weight when having multiple length frequencies
  finalPP[, TotalCount := sum(Count), 
       by = .(Ship, Gear,HaulNo,StationNumber,Year,Month,Day,Time,FishID,
              AphiaIDPredator,AphiaIDPrey,IdentMet,DigestionStage,GravMethod,
              SubFactor,PreySequence)]
  finalPP[!is.na(finalPP$TotalCount)]$Notes<-'Computed using: total weight * count in length group / count for all length group'
  finalPP$Weight=finalPP$Weight*finalPP$Count/finalPP$TotalCount
  
  #Handling number of unique species in pray sampled in each individual fish
  #TODO: Needs to be checked
  replace<-finalPP[!is.na(finalPP$AphiaIDPrey), .(replace_number = data.table::uniqueN(AphiaIDPredator)), 
              by = .(Ship,Gear,HaulNo,StationNumber,Year,Month,Day,Time)]#,FishID,
                     # AphiaIDPredator)]
  finalPI[replace, Number := i.replace_number, 
     on = .(Ship,Gear,HaulNo,StationNumber,Year,Month,Day,
            Time)]#,FishID,AphiaIDPredator)]
  
  
  # Move values from StomachFullness to Regurgitated where StomachFullness is 6
  # Move  values from StomachFullness to StomachEmpty where StomachFullness is 1
  finalPI[StomachFullness == '6', c("Regurgitated", "StomachFullness") := .(StomachFullness, NA)]
  finalPI[StomachFullness == '1', c("StomachEmpty", "StomachFullness") := .(StomachFullness, NA)]
  
  
  PIraw <- data.table::copy(finalPI[, c(
    "Ship", "Gear","HaulNo","StationNumber","Year", "Month", "Day", "Time", 
    "FishID","AphiaIDPredator","IndWgt","Number","MeasurementIncrement","Length",
    "AgeSource","Age","Sex","MaturityScale","MaturityStage","PreservationMethod",
    "Regurgitated","StomachFullness","FullStomWgt","EmptyStomWgt","StomachEmpty",
    "GenSamp","Notes"
  )]
  )
  
  
  PPraw <- data.table::copy(finalPP[, c("Ship", "Gear","HaulNo","StationNumber","Year", "Month", "Day", "Time", 
    "FishID","AphiaIDPredator","AphiaIDPrey","IdentMet","DigestionStage",
    "GravMethod","SubFactor","PreySequence","Count","UnitWgt","Weight","UnitLngt","Length",
    "OtherItems","OtherCount","OtherWgt","AnalysingOrg","Notes"
  )])
  
  
  ICESDatsuscData <- list(FI = finalFI, HH = HHraw, PI = PIraw, PP = PPraw)
  
  return(ICESDatsuscData)
}





#' Write ICESDatras to CSV fille
#'
#' Writes \code{\link{ICESDatrasData}} to a csv file for each input acoustic file used to create the \code{\link{ICESDatras}}
#'
#' @inheritParams ModelData
#'
#' @return An object of StoX data type \code{\link{WriteICESDatrasData}}.
#'
#' @export
WriteICESDatsusc <- function(ICESDatsuscData){
  
  #WriteICESDatrasData <- lapply(
  #	ICESDatrasData, 
  #	WriteICESDatrasOne, 
  #	na = "-9"
  #)
  
  WriteICESDatsuscData <- WriteICESDatrasOne(ICESDatsuscData, na = "-9")
  
  return(WriteICESDatsuscData)
}


WriteICESDatsuscOne <- function(ICESDatsuscData, na = "-9"){
	# Convert all tables to string matrix with header and record, and rbind:
	ICESDatsuscCSVDataOne<- convertToRecordTypeMatrix(ICESDatsuscData)
	# Replace NAs:
	if(length(na)) {
		ICESDatsuscCSVDataOne <- lapply(ICESDatsuscCSVDataOne, function(x) {x[is.na(x)] <- na; x})
	}
	
	# IMPORTANT NOTE: The ICES DATRAS format assumes that the three tables HH, HL and CA are stacked in a comma separated file but without padding the tables to equal number of columns as is done for ICESAcoustic and ICESBiotic. For this reason we need to paste the data to a chatacter vector and write as lines. RstoxFramework detects that the output as a vector of characters and uses writeLines() to produce the file.
	
	
	# Convert each line of each table to comma separated:
	ICESDatsuscCSVDataOne <- lapply(ICESDatsuscCSVDataOne, apply, 1, paste, collapse = ",")
	# Join to one vector, to be written to one file:
	ICESDatsuscCSVDataOne <- unlist(ICESDatsuscCSVDataOne)
  
  return(ICESDatsuscCSVDataOne)
}





