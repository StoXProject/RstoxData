
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
	
	ICESAcousticData <- lapply(
		AcousticData, 
		AcousticDataToICESAcousticOne
	)
	
	return(ICESAcousticData)
}



AcousticDataToICESAcousticOne <- function(AcousticDataOne){
	
	if(AcousticDataOne$metadata$useXsd=='icesAcoustic'){
		
		ICESAcousticDataOne <- AcousticData_ICESToICESAcousticOne(AcousticDataOne)
	}
	else{
		stop('StoX: only ices acoustic format is allowed')
	}
	
	return(ICESAcousticDataOne)
}


AcousticData_ICESToICESAcousticOne <- function(AcousticData_ICESOne){
	
	# Make a copy as we are modifying by reference:
	ICESAcousticDataOne <- data.table::copy(AcousticData_ICESOne)
	
	# Remove echo type, as this is not included in the CSV:
	ICESAcousticDataOne$Data$EchoType<-NULL
	
	
	independentTables <- c("Instrument", "Calibration", "DataAcquisition", "DataProcessing")
	hierarchicalTables <- c("Cruise", "Log", "Sample", "Data")
	tablesToKeep <- c(independentTables, hierarchicalTables)
	
	# Add the Survey to the Cruise table, with a hack to get the last line (until it has been fixed so that only the code and not the value is present):
	ICESAcousticDataOne$Cruise$Survey <- utils::tail(ICESAcousticDataOne$Survey$Code, 1)
	
	# Translate according to the vocabulary:
	if(length(ICESAcousticDataOne$vocabulary)) {
		vocabulary <- findVariablesMathcinigVocabulary(
			vocabulary = ICESAcousticDataOne$vocabulary, 
			data = ICESAcousticDataOne[tablesToKeep]
		)
		# Uniqueify since some columns (keys) are present in several tables:
		vocabulary <- unique(vocabulary)
		
		translateVariables(
			data = ICESAcousticDataOne[tablesToKeep], 
			Translation = vocabulary, 
			translate.keys = TRUE, 
			warnMissingTranslation = FALSE
		)
	}
	
	
	
	#Check metadata towards ices definitions
	compareICES('https://acoustic.ices.dk/Services/Schema/XML/AC_TransducerLocation.xml',unique(ICESAcousticDataOne$Instrument$TransducerLocation))
	compareICES('https://acoustic.ices.dk/Services/Schema/XML/AC_TransducerBeamType.xml',unique(ICESAcousticDataOne$Instrument$TransducerBeamType))
	compareICES('https://acoustic.ices.dk/Services/Schema/XML/AC_AcquisitionMethod.xml',unique(ICESAcousticDataOne$Calibration$AcquisitionMethod))
	compareICES('https://acoustic.ices.dk/Services/Schema/XML/AC_ProcessingMethod.xml',unique(ICESAcousticDataOne$Calibration$ProcessingMethod))
	compareICES('https://acoustic.ices.dk/Services/Schema/XML/AC_DataAcquisitionSoftwareName.xml',unique(ICESAcousticDataOne$DataAcquisition$SoftwareName))
	compareICES('https://acoustic.ices.dk/Services/Schema/XML/AC_StoredDataFormat.xml',unique(ICESAcousticDataOne$DataAcquisition$StoredDataFormat))
	compareICES('https://acoustic.ices.dk/Services/Schema/XML/AC_DataAcquisitionSoftwareName.xml',unique(ICESAcousticDataOne$DataAcquisition$SoftwareName))
	compareICES('https://acoustic.ices.dk/Services/Schema/XML/AC_DataProcessingSoftwareName.xml',unique(ICESAcousticDataOne$DataProcessing$SoftwareName))
	compareICES('https://acoustic.ices.dk/Services/Schema/XML/AC_TriwaveCorrection.xml',unique(ICESAcousticDataOne$DataProcessing$TriwaveCorrection))
	compareICES('https://acoustic.ices.dk/Services/Schema/XML/AC_OnAxisGainUnit.xml',unique(ICESAcousticDataOne$DataProcessing$OnAxisGainUnit))
	compareICES('https://acoustic.ices.dk/Services/Schema/XML/ISO_3166.xml',unique(ICESAcousticDataOne$Cruise$Country))
	compareICES('https://acoustic.ices.dk/Services/Schema/XML/SHIPC.xml',unique(ICESAcousticDataOne$Cruise$Platform))
	compareICES('https://acoustic.ices.dk/Services/Schema/XML/EDMO.xml',unique(ICESAcousticDataOne$Cruise$Organisation))
	compareICES('https://acoustic.ices.dk/Services/Schema/XML/AC_Survey.xml',unique(ICESAcousticDataOne$Survey$Code))
	compareICES('https://acoustic.ices.dk/Services/Schema/XML/AC_LogOrigin.xml',unique(ICESAcousticDataOne$Log$Origin))
	compareICES('https://acoustic.ices.dk/Services/Schema/XML/AC_LogValidity.xml',unique(ICESAcousticDataOne$Log$Validity))
	compareICES('https://acoustic.ices.dk/Services/Schema/XML/AC_PingAxisIntervalType.xml',unique(ICESAcousticDataOne$Sample$PingAxisIntervalType))
	compareICES('https://acoustic.ices.dk/Services/Schema/XML/AC_PingAxisIntervalUnit.xml',unique(ICESAcousticDataOne$Sample$PingAxisIntervalUnit))
	compareICES('https://acoustic.ices.dk/Services/Schema/XML/AC_PingAxisIntervalOrigin.xml',unique(ICESAcousticDataOne$Sample$PingAxisIntervalOrigin))
	compareICES('https://acoustic.ices.dk/Services/Schema/XML/AC_SaCategory.xml',unique(ICESAcousticDataOne$Data$SaCategory))
	compareICES('https://acoustic.ices.dk/Services/Schema/XML/AC_AcousticDataType.xml',unique(ICESAcousticDataOne$Data$Type))
	compareICES('https://acoustic.ices.dk/Services/Schema/XML/AC_DataUnit.xml',unique(ICESAcousticDataOne$Data$Unit))
	
	#### Rename columns to start with the table name:
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





#' Writes \code{\link{ICESAcousticData}} to a csv file for each input acoustic file used to create the \code{\link{ICESAcousticData}}
#'
#' @param ICESAcousticData A \code{\link{ICESAcousticData}} object obtained from an ICES acoustic XML format file.
#'
#' @return List of string matrices in the ICES acoustic CSV format.
#'
#' @export
#' 
WriteICESAcoustic <- function(ICESAcousticData){
	
	WriteICESAcousticData <- lapply(
		ICESAcousticData, 
		WriteICESAcousticOne
	)
	
	return(WriteICESAcousticData)
}


WriteICESAcousticOne <- function(ICESAcousticDataOne){
	
	# Convert all tables to string with header and reccord, and rbind:
	ICESAcousticCSVDataOne <- convertToHeaderRecordMatrix(ICESAcousticDataOne)
	ICESAcousticCSVDataOne <- expandWidth(ICESAcousticCSVDataOne)
	
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
	
	# Format the table to a character table before converting to matrix class (used by RstoxFramework to write as csv). Here,  format(thisTable) cannot be used as it converts NAs to "NA" (at least per defaul):
	cols <- names(thisTable)
	thisTable[, (cols) := lapply(.SD, as.character), .SDcols = cols]
	thisTable <- as.matrix(thisTable)
	record <- cbind(
		ICESDataTableName, 
		"Record", 
		# Convert all columns to string:
		#as.matrix(thisTable, trim = TRUE)
		#format(thisTable, justify = "none", trim = TRUE)
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
#' @param SurveyName A string naming the survey. Must be one of the names listed on \url{https://vocab.ices.dk/?ref=1453} or NONE (the default).
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
	
	
	# Copy the data to prepare for using data.tables by reference functions (set* and :=)
	hierarchicalTables <- c("Cruise", "Haul", "Catch", "Biology")
	ICESBioticDataOne <- ICESBioticDataOne[hierarchicalTables]
	
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
		Platform = getICESShipCode(platformname),
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
		StartTime = ifelse(is.na(stationstartdate) | is.na(stationstarttime), NA, gsub("Z", " ", paste0(stationstartdate, substr(stationstarttime, 1, 5)))),
		Duration = getTimeDiff(stationstartdate, stationstarttime, stationstopdate, stationstoptime),
		Validity = getHaulValiditySimple(gearcondition, samplequality),
		StartLatitude = latitudestart,
		StartLongitude = longitudestart,
		StopLatitude = latitudeend,
		StopLongitude = longitudeend,
		StatisticalRectangle = getICESrect(latitudestart, longitudestart),
		MinTrawlDepth = ifelse(is.na(fishingdepthmin), fishingdepthmax, fishingdepthmin),
		MaxTrawlDepth = fishingdepthmax,
		BottomDepth = ifelse(bottomdepthstop > fishingdepthmax, bottomdepthstop, NA),
		# Before, the function getDistanceMeter() was used, which was inherited from old Datras code. Instead we use the distance that is given by NMDBiotic:
		#Distance = getDistanceMeter(latitudestart, longitudestart, latitudeend, longitudeend),
		# Distance is in nautical miles in NMDBiotic and in meters in ICEBiotic:
		Distance = distance * 1852,
		Netopening = verticaltrawlopening,
		CodendMesh = NA,
		#SweepLength = getGOVSweepByEquipment(gear),
		SweepLength = sweeplength,
		GearExceptions = NA, # Should this be set?
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
		Buoyancy = NA,
		KiteArea = NA,
		GroundRopeWeight = NA,
		Rigging = NA,
		Tickler = NA,
		HydrographicStationID = NA,
		# Direction is integer degrees: 
		TowDirection = round(direction),
		SpeedGround = NA,
		SpeedWater = gearflow,
		# Wind direction is integer degrees: 
		WindDirection = winddirection,
		# Wind speed is meter per second: 
		WindSpeed = round(windspeed),
		SwellDirection = NA,
		SwellHeight = NA,
		#LogDistance = NA,
		LogDistance = logstart,
		Stratum = NA
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
		SpeciesSex = NA,
		SubsampledNumber = lengthsamplecount,
		SubsamplingFactor = catchcount / lengthsamplecount,
		SubsampleWeight = lengthsampleweight,
		LengthCode = NA, # NMDBiotic has no way of storing a length distribution.
		LengthClass = NA, # NMDBiotic has no way of storing a length distribution.
		#LengthType = "1", # Should not this be interpreted from the catchsample$lengthmeasurement ???
		LengthType = lengthmeasurement, 
		NumberAtLength = lengthsamplecount,
		WeightAtLength = NA # Not relevant for NMDBiotic. Used in the Baltic Acoustic Survey, as noted in the documentation of ICESBiotic.
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
		StockCode = NA,
		FishID = specimenid,
		#LengthCode = "mm", 
		LengthCode = getLengthCodeICES(lengthresolution), 
		#LengthClass = length * 1000
		LengthClass = scaleLengthUsingLengthCode(length, getLengthCodeICES(lengthresolution), inputUnit = "m"), 
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
		MaturityScale = NA,
		IndividualAge = age,
		AgePlusGroup = NA,
		#AgeSource = "Otolith",
		# Do not interpret agingstructure, as this should be the responsibility of the user:
		AgeSource = agingstructure, 
		GeneticSamplingFlag = NA,
		StomachSamplingFlag = NA,
		ParasiteSamplingFlag = NA,
		IndividualVertebraeCount = NA
	)]
	
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
	
	ICESBioticCSV <- list(
		Cruise = Cruise, 
		Haul = Haul, 
		Catch = Catch, 
		Biology = Biology
	)
	
	return(ICESBioticCSV)
}




#' Writes \code{\link{ICESBioticData}} to a csv file for each input acoustic file used to create the \code{\link{ICESBioticData}}
#'
#' @param ICESBioticData A \code{\link{ICESBioticData}} object obtained from an ICES acoustic XML format file.
#'
#' @return List of string matrices in the ICES acoustic CSV format.
#'
#' @export
WriteICESBiotic <- function(ICESBioticData){
	
	WriteICESBioticData <- lapply(
		ICESBioticData, 
		WriteICESBioticOne
	)
	
	return(WriteICESBioticData)
}


WriteICESBioticOne <- function(ICESBioticDataOne){
	
	# Convert all tables to string with header and reccord, and rbind:
	ICESBioticCSVDataOne <- convertToHeaderRecordMatrix(ICESBioticDataOne)
	ICESBioticCSVDataOne <- expandWidth(ICESBioticCSVDataOne)
	
	# Stack all matrices:
	ICESBioticCSVDataOne <- do.call(rbind, ICESBioticCSVDataOne)
	
	return(ICESBioticCSVDataOne)
}






# Function to add the table name to the column names, but not to the keys.
renameToTableNameFirst <- function(data, tableNames, setToID = NULL, formatType = c("Biotic", "Acoustic")) {
	
	formatType <- match.arg(formatType)
	
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
#' @return An \code{\link{ICESDatrasData}} object.
#'
#' @export
ICESDatras <- function(
	BioticData
) {

	ICESDatrasData <- lapply(
  		BioticData, 
  		ICESDatrasOne
  	)

	# Remove empty data (from invavlid files, non NMDBiotic >= 3)
	ICESDatrasData <- ICESDatrasData[lengths(ICESDatrasData) > 0]
	
	return(ICESDatrasData)
}


ICESDatrasOne <- function(
	BioticDataOne
) {
	
	# Check input is a NMD Biotic v3 data
	if(!(BioticDataOne$metadata$useXsd %in% c("nmdbioticv3", "nmdbioticv3.1"))) {
		warning("StoX: Currently, only NMD Biotic version 3 and 3.1 data can be written by ICESDatras")
		return(matrix(1, 0, 0))
	}

	## 1. HH ##
	'%ni%' <- Negate('%in%')

	finalHH <- merge(BioticDataOne$mission, BioticDataOne$fishstation)

	# Make HH records
	finalHH[, `:=`(
		"Quarter" = getQuarter(stationstartdate),
		"Country" = getTSCountryByIOC(nation),
		"Ship" = getICESShipCode(platformname),
		"Gear" = "GOV",
		"SweepLngt" = getGOVSweepByEquipment(gear),
		"GearEx" = getGearEx(getGOVSweepByEquipment(gear), startyear, serialnumber, bottomdepthstart),
		"DoorType" = "P",
		"StNo" = serialnumber,
		"HaulNo" = station,
		"Year" = getYear(stationstartdate),
		"Month" = getMonth(stationstartdate),
		"Day" = getDay(stationstartdate),
		"TimeShot" = getTimeShot(stationstarttime),
		"DepthStratum" = NA,
		"HaulDur" = as.numeric(getTimeDiff(stationstartdate, stationstarttime, stationstopdate, stationstoptime)),
		"DayNight" = getDayNight(stationstartdate, stationstarttime, latitudestart, longitudestart),
		"ShootLat" = roundDrop0(latitudestart, digits = 4), 
		"ShootLong" = roundDrop0(longitudestart, digits = 4), 
		"HaulLat" = roundDrop0(latitudeend, digits = 4),
		"HaulLong" = roundDrop0(longitudeend, digits = 4),
		"StatRec" = getICESrect(latitudestart, longitudestart),
		"Depth" = roundDrop0(bottomdepthstart),
		"HaulVal" = getHaulVal(gearcondition, samplequality),
		"HydroStNo" = NA,
		"StdSpecRecCode" = 1,
		"BycSpecRecCode" = 1,
		"DataType" = "R",
		"Netopening"= roundDrop0(verticaltrawlopening, digits = 1),
		"Rigging" = NA,
		"Tickler" = NA,
		"Distance" = roundDrop0(getDistanceMeter(latitudestart, longitudestart, latitudeend, longitudeend)),
		"Warplngt" = roundDrop0(wirelength),
		"Warpdia" = NA,
		"WarpDen" = NA,
		"DoorSurface" = 4.5,
		"DoorWgt" = 1075,
		"DoorSpread" = ifelse(!is.na(trawldoorspread), roundDrop0(trawldoorspread, digits = 1), NA),
		"WingSpread" = NA,
		"Buoyancy" = NA,
		"KiteDim" = 0.8,
		"WgtGroundRope" = NA,
		"TowDir" = ifelse(!is.na(direction), roundDrop0(direction), NA),
		"GroundSpeed" = roundDrop0(gearflow, digits = 1),
		"SpeedWater" = NA,
		"SurCurDir" = NA,
		"SurCurSpeed" = NA,
		"BotCurDir" = NA,
		"BotCurSpeed" = NA,
		"WindDir" = NA,
		"WindSpeed" = NA,
		"SwellDir" = NA,
		"SwellHeight" = NA,
		"SurTemp" = NA,
		"BotTemp" = NA,
		"SurSal" = NA,
		"BotSal" = NA,
		"ThermoCline" = NA,
		"ThClineDepth" = NA,
		"CodendMesh" = NA ,
		"SecchiDepth" = NA,
		"Turbidity" = NA,
		"TidePhase" = NA,
		"TideSpeed" = NA,
		"PelSampType" = NA,
		"MinTrawlDepth" = NA,
		"MaxTrawlDepth" = NA
	)]
	
	HHraw <- data.table::copy(finalHH[, c(
		"Quarter", "Country", "Ship", "Gear",
		"SweepLngt", "GearEx", "DoorType", "StNo", "HaulNo", "Year", "Month", "Day",
		"TimeShot", "DepthStratum", "HaulDur", "DayNight", "ShootLat", "ShootLong", "HaulLat", "HaulLong",
		"StatRec", "Depth", "HaulVal", "HydroStNo", "StdSpecRecCode", "BycSpecRecCode", "DataType", "Netopening",
		"Rigging", "Tickler", "Distance", "Warplngt", "Warpdia", "WarpDen", "DoorSurface", "DoorWgt",
		"DoorSpread", "WingSpread", "Buoyancy", "KiteDim", "WgtGroundRope", "TowDir", "GroundSpeed",
		"SpeedWater", "SurCurDir", "SurCurSpeed", "BotCurDir", "BotCurSpeed", "WindDir", "WindSpeed",
		"SwellDir", "SwellHeight", "SurTemp", "BotTemp", "SurSal", "BotSal", "ThermoCline", "ThClineDepth",
		"CodendMesh", "SecchiDepth", "Turbidity", "TidePhase", "TideSpeed", "PelSampType", "MinTrawlDepth", "MaxTrawlDepth")]
	)
	
	## 2. HL ##
	
	mergedHL <- merge(BioticDataOne$catchsample, finalHH, by=intersect(names(BioticDataOne$catchsample), names(finalHH)))
	
	groupCA <- c("missiontype", "startyear", "platform", "missionnumber", "serialnumber", "aphia", "sex")
	groupHL <- c(groupCA, "catchpartnumber")
	
	# Remove rows with empty aphia
	mergedHL <- mergedHL[!is.na(aphia)]
	
	getSpecVal <- function(HaulVal, catchcount, lengthsamplecount, catchweight){
		temp <-  as.data.table(cbind(hv=HaulVal, cc=catchcount, lsc=lengthsamplecount, cw=catchweight))
		
		# Default is invalid
		temp[, res := "0"]
		
		temp[!is.na(cc) & !is.na(lsc) & !is.na(cw), res:="1"]
		temp[!is.na(cc) &  is.na(lsc) &  is.na(cw), res:="4"]
		temp[ is.na(cc) &  is.na(lsc) & !is.na(cw), res:="6"]
		temp[!is.na(cc) &  is.na(lsc) & !is.na(cw), res:="7"]
		temp[ is.na(cc) &  is.na(lsc) &  is.na(cw), res:="5"]
		temp[!is.na(cc) & !is.na(lsc) &  is.na(cw), res:="0"]
		
		temp[hv == "I", res:="0"]
		
		return(temp$res)
	}
	
	
	mergedHL[, SpecVal := getSpecVal(HaulVal, catchcount, lengthsamplecount, catchweight)]
	
	# Get herring or sprat
	mergedHL[,`:=`( isHerringOrSprat = ifelse(aphia %in% c("126417", "126425"), TRUE, FALSE),
					isCrustacean = ifelse(aphia %in% c("107275", "107276", "107369", "107253", "107703", "107704", "107350", "107254", "107205", "140712", "140687", "140658"), TRUE, FALSE))]
	
	# Calculate lngtCode
	mergedHL[,lngtCode := "1"]
	mergedHL[is.na(sampletype), lngtCode := NA]
	mergedHL[isCrustacean == TRUE, lngtCode := "."]
	mergedHL[isHerringOrSprat == TRUE, lngtCode := "0"]
	
	# lenInterval, and reportInMM
	mergedHL[,`:=`(lenInterval = ifelse(lngtCode=="0", 5, 1), reportInMM = ifelse(lngtCode %ni% c("1", NA), TRUE, FALSE))]
	mergedHL[is.na(lenInterval), lenInterval := 1]
	
	# catCatchWgt & subWeight
	mergedHL[!is.na(catchweight), catCatchWgt := ceiling(catchweight * 1000)]
	mergedHL[!is.na(lengthsampleweight), subWeight := ceiling(lengthsampleweight * 1000)]
	
	# get sampleFac
	mergedHL[, sampleFac := catchweight / lengthsampleweight]
	
	# Merge with individual
	mergedHL <- merge(mergedHL, BioticDataOne$individual, by = intersect(names(mergedHL), names(BioticDataOne$individual)), all.x = TRUE)
	
	# Get count
	mergedHL[, N := sum(!is.na(specimenid)), by = groupHL]
	
	# For the record with empty individual data
	mergedHL[N == 0, `:=`(lngtClass = as.integer(NA), sex = as.character(NA))]
	
	# Get Individual length
	mergedHL[, length := length * 100]
	
	# Some species have very small length in cm, use mm instead
	mergedHL[length < 1, `:=`(lngtCode = ".", lenInterval = 1, reportInMM = TRUE)]
	
	# Process MM length
	mergedHL[reportInMM == TRUE, length := length * 10]
	
	# Get sex
	mergedHL[, sex := ifelse(is.na(sex), as.character(NA), ifelse(sex == "1", "F", "M"))]
	
	# Get lngtClass
	for(interval in unique(mergedHL$lenInterval)) {
		intVec <- seq(0, max(mergedHL$length, na.rm = T), by = interval)
		mergedHL[lenInterval == interval, lngtClass := intVec[findInterval(length, intVec)]]
	}
	
	# Count measured individual
	mergedHL[!is.na(length), lsCountTot := 1]
	
	# Aggregate hlNoAtLngth and lsCountTot
	finalHL <- mergedHL[, .(N, lsCountTot = sum(lsCountTot)), by = c(
		groupHL,  
		"lngtClass", "Quarter", "Country", "Ship", "Gear", "SweepLngt", "GearEx", "DoorType", "HaulNo", "SpecVal", "catCatchWgt", "sampleFac", "subWeight", "lngtCode", "stationtype", "lengthmeasurement"
		)
	]
	
	finalHL <- finalHL[!duplicated(finalHL)]
	finalHL[,`:=`(noMeas = sum(lsCountTot)), by = groupHL]
	finalHL[,`:=`(totalNo = noMeas * sampleFac, subFactor = sampleFac)]
	
	HLraw <- data.table::copy(finalHL[, .(
		"Quarter" = Quarter,
		"Country" = Country,
		"Ship" = Ship,
		"Gear" = Gear,
		"SweepLngt" = SweepLngt,
		"GearEx" = GearEx,
		"DoorType" = DoorType,
		"StNo" = serialnumber,
		"HaulNo" = HaulNo,
		"Year" = startyear,
		"SpecCodeType" = "W",
		"SpecCode" = aphia,
		"SpecVal" = SpecVal,
		"Sex" = sex,
		"TotalNo" = roundDrop0(totalNo, digits = 2),
		"CatIdentifier" = catchpartnumber,
		"NoMeas" = noMeas,
		"SubFactor" = roundDrop0(subFactor, 4),
		"SubWgt" = roundDrop0(subWeight),
		"CatCatchWgt" = roundDrop0(catCatchWgt),
		"LngtCode" = lngtCode,
		"LngtClass" = lngtClass,
		"HLNoAtLngt" = roundDrop0(lsCountTot, 2),
		"DevStage" = NA,
		"LenMeasType" = convLenMeasType(lengthmeasurement)
	)]
	)
	
	
	## 3. CA ##
	
	mergedHL[is.na(preferredagereading), preferredagereading := 1]
	baseAge <- intersect(names(mergedHL), names(BioticDataOne$agedetermination))
	mergedCA <- merge(mergedHL, BioticDataOne$agedetermination, by.x=c(baseAge, "preferredagereading"), by.y= c(baseAge, "agedeterminationid"), all.x = TRUE)
	
	# Remove empty individual
	mergedCA <- mergedCA[!is.na(specimenid)]
	
	# Get maturity
	mergedCA[, maturity:=getDATRASMaturity(Quarter, aphia, specialstage, maturationstage)]
	
	# Aggregate count
	mergedCA[!is.na(individualweight), `:=`(nWithWeight =.N, totWeight = sum(individualweight)), by = c(groupCA,  "lngtClass", "maturity", "age")]
	
	finalCA <- mergedCA[, .(nInd =.N), by = c(
		groupCA,  
		"lngtClass", "maturity", "age", "Quarter", "Country", "Ship", "Gear", "SweepLngt", "GearEx", "DoorType", "HaulNo", "SpecVal", "StatRec", "lngtCode", "stationtype", "nWithWeight", "totWeight", "specimenid", "tissuesample", "stomach", "agingstructure", "readability", "parasite")]
	finalCA[!is.na(nWithWeight),  meanW := totWeight / nWithWeight]
	
	CAraw <- data.table::copy(finalCA[,
		.(
			"Quarter" = Quarter,
			"Country" = Country,
			"Ship" = Ship,
			"Gear" = Gear,
			"SweepLngt" = SweepLngt,
			"GearEx" = GearEx,
			"DoorType" = DoorType,
			"StNo" = serialnumber,
			"HaulNo" = HaulNo,
			"Year" = startyear,
			"SpecCodeType" = "W",
			"SpecCode" = aphia,
			"AreaType" = "0",
			"AreaCode" = StatRec,
			"LngtCode" = lngtCode,
			"LngtClass" = lngtClass,
			"Sex" = sex,
			"Maturity" = maturity,
			"PlusGr" = as.character(NA),
			"AgeRings" = ifelse(!is.na(age), age, NA),
			"CANoAtLngt" = nInd,
			"IndWgt" = ifelse(!is.na(meanW), roundDrop0(meanW * 1000, 1), NA),
			"MaturityScale" = "M6",
			"FishID" = specimenid,
			"GenSamp" = ifelse(!is.na(tissuesample), "Y", "N"),
			"StomSamp" = ifelse(!is.na(stomach), "Y", "N"),
			"AgeSource" = convAgeSource(agingstructure),
			"AgePrepMet" = NA,
			"OtGrading" = ifelse(readability %in% as.character(c(1:4)), readability, NA),  # From http://tomcat7.imr.no:8080/apis/nmdapi/reference/v2/dataset/otolithreadability?version=2.0 and http://vocab.ices.dk/?ref=1395
			"ParSamp" = ifelse(!is.na(parasite), "Y", "N")
		)]
	)
	
	
	## Prepare for cleaning (mostly from the old Rstox. Will change as we improve) ##
	hh <- HHraw
	hl <- HLraw
	ca <- CAraw
	
	## WARN #0:
	# It's possible to have two same aphia (but different species, e.g. SILD05) catch sampes in a haul.
	# We need to combine them if we have two different TotalNo and catcatchwgt.
	
	# Find duplicate species in a haul
	dupl <- stats::aggregate(catchcategory ~ aphia + serialnumber, BioticDataOne$catchsample, FUN = function(x) length(unique(x)))
	dupl <- dupl[dupl$catchcategory > 1, ]
	
	# Find the above in our DATRAS HL
	if(nrow(dupl)) {
		found <- stats::aggregate(CatCatchWgt ~ StNo + SpecCode + Sex + CatIdentifier, hl[(hl$SpecCode %in% dupl$aphia & hl$StNo %in% dupl$serialnumber),], FUN = function(x) length(unique(x)))
		found <- found[found$CatCatchWgt > 1, ]
		for(iz in seq_len(nrow(found))) {
			tmpHL <- hl[hl$StNo==found[iz, "StNo"] & hl$SpecCode==found[iz, "SpecCode"] & hl$Sex==found[iz, "Sex"] & hl$CatIdentifier==found[iz, "CatIdentifier"], ]
			combinedCatCatchWgt <- tmpHL
			# Fix CatCatchWgt
			hl[hl$StNo==found[iz, "StNo"] & hl$SpecCode==found[iz, "SpecCode"] & hl$Sex==found[iz, "Sex"] & hl$CatIdentifier==found[iz, "CatIdentifier"], "CatCatchWgt"] <- roundDrop0(mean(tmpHL$CatCatchWgt))
			# Fix CatCatchWgt
			hl[hl$StNo==found[iz, "StNo"] & hl$SpecCode==found[iz, "SpecCode"] & hl$Sex==found[iz, "Sex"] & hl$CatIdentifier==found[iz, "CatIdentifier"], "SubWgt"] <- roundDrop0(mean(tmpHL$SubWgt))
			# Fix totalNo
			hl[hl$StNo==found[iz, "StNo"] & hl$SpecCode==found[iz, "SpecCode"] & hl$Sex==found[iz, "Sex"] & hl$CatIdentifier==found[iz, "CatIdentifier"], "TotalNo"] <- sum(unique(tmpHL$TotalNo))
			# Fix noMeas
			hl[hl$StNo==found[iz, "StNo"] & hl$SpecCode==found[iz, "SpecCode"] & hl$Sex==found[iz, "Sex"] & hl$CatIdentifier==found[iz, "CatIdentifier"], "NoMeas"] <- sum(tmpHL$HLNoAtLngt)
			# Finally, fix SubFactor
			hl[hl$StNo==found[iz, "StNo"] & hl$SpecCode==found[iz, "SpecCode"] & hl$Sex==found[iz, "Sex"] & hl$CatIdentifier==found[iz, "CatIdentifier"], "SubFactor"] <- sum(unique(tmpHL$TotalNo))/sum(tmpHL$HLNoAtLngt)
		}
	}
	
	## WARN #1:
	# Find species with different SpecVal, if any of them have SpecVal == 1, delete any other records with different SpecVal
	# otherwise, use the lowest SpecVal value for all
	
	tmp <- stats::aggregate(SpecVal ~ SpecCode + StNo, hl, FUN = function(x) length(unique(x)))
	tmp <- tmp[tmp$SpecVal>1, ]
	
	for( rownum in seq_len(nrow(tmp)) ) {
		tmpSpecs <- hl[(hl$StNo==tmp$StNo[rownum] & hl$SpecCode==tmp$SpecCode[rownum]),]$SpecVal
		if(any(tmpSpecs == 1))
			hl <- hl[!(hl$StNo==tmp$StNo[rownum] & hl$SpecCode==tmp$SpecCode[rownum] & hl$SpecVal!=1),]
		else
			hl[(hl$StNo==tmp$StNo[rownum] & hl$SpecCode==tmp$SpecCode[rownum]), c("SpecVal")] <- min(tmpSpecs)
	}
	
	## SpecVal Conditionals
	hl[hl$SpecVal==0, c("Sex", "TotalNo", "CatIdentifier", "NoMeas", "SubFactor", "SubWgt", "CatCatchWgt", "LngtCode", "LngtClass", "HLNoAtLngt")] <- NA
	
	hl[hl$SpecVal==4, c("NoMeas", "SubWgt", "CatCatchWgt", "LngtCode", "LngtClass", "HLNoAtLngt")] <- NA
	hl[hl$SpecVal==4, c("SubFactor")] <- 1
	
	hl[hl$SpecVal==5, c("TotalNo", "NoMeas", "SubWgt", "CatCatchWgt", "LngtCode", "LngtClass", "HLNoAtLngt")] <- NA
	hl[hl$SpecVal==5, c("SubFactor")] <- 1
	
	hl[hl$SpecVal==6, c("TotalNo", "NoMeas", "LngtCode", "LngtClass", "HLNoAtLngt")] <- NA
	
	hl[hl$SpecVal==7, c("NoMeas", "LngtCode", "LngtClass", "HLNoAtLngt")] <- NA
	
	hl[hl$SpecVal==10, c("CatCatchWgt")] <- NA
	
	## WARN #2:
	## will now get errors in DATRAS upload for duplicate records
	hl <- hl[!duplicated(hl),]
	
	## hl and ca contain 0-tow info - must throw these out
	hl <- hl[hl$StNo %in% hh$StNo,]
	ca <- ca[ca$StNo %in% hh$StNo,]
	# throw out ca records for Invalid hauls
	ca <- ca[!ca$StNo %in% hh$StNo[hh$HaulVal=='I'],]
	
	##########################################
	## Removing some benthos - this won't be needed in the future
	## keep 11725 138139 138482 138483 140600 140621 140624 140625 141443 141444 141449 153083 153131-- these are cephaolopods
	## required benthos: 107205
	hl <- hl[!hl$SpecCode %in% c(230,558,830,883,1302,1839,100635,100706,100930,103929,106048,106087,106204,106733,106791,
								 106854,106928,107044,107218,107230,107240,107273,107292,107318,107330,107346,107397,107398,107551,
								 107616,107643,111374,111597,111604,116986,117302,117809,117815,117890,123117,123867,123920,123970,
								 123987,124319,124418,124913,124929,124934,125128,125131,125134,129196,129229,130464,130867,132072,
								 132480,135144,135302,137704,137732,138223,138239,138760,138899,139004,139488,140299,140627,141753,
								 144129,150642,178639,181228,23986719494,21263,100817,100982,106738,107160,107232,107277,107322,
								 107323,107327,107387,107531,107552,107564,107649,107651,111367,123080,123083,123084,123776,123813,
								 124043,124154,124160,124287,124535,125166,125333,128517,129840,138802,138878,138920,140467,140717,
								 143755,145541,145546,145548,532031,589677,1762,123082,149),]
	
	ca <- ca[!ca$SpecCode %in% c(230,558,830,883,1302,1839,100635,100706,100930,103929,106048,106087,106204,106733,106791,
								 106854,106928,107044,107218,107230,107240,107273,107292,107318,107330,107346,107397,107398,107551,
								 107616,107643,111374,111597,111604,116986,117302,117809,117815,117890,123117,123867,123920,123970,
								 123987,124319,124418,124913,124929,124934,125128,125131,125134,129196,129229,130464,130867,132072,
								 132480,135144,135302,137704,137732,138223,138239,138760,138899,139004,139488,140299,140627,141753,
								 144129,150642,178639,181228,23986719494,21263,100817,100982,106738,107160,107232,107277,107322,
								 107323,107327,107387,107531,107552,107564,107649,107651,111367,123080,123083,123084,123776,123813,
								 124043,124154,124160,124287,124535,125166,125333,128517,129840,138802,138878,138920,140467,140717,
								 143755,145541,145546,145548,532031,589677,1762,123082,149),]
	
	#more benthods 10216 = skate egg case
	hl <- hl[!hl$SpecCode %in% c(443,938,1131,1292,1337,1360,19494,22988,100751,100757,100790,101054,103484,104062,
								 106122,106669,107011,107052,107148,107239,107388,107563,110690,110911,110956,111411,117136,
								 117258,123260,123276,123321,123335,123574,123593 ,123851,123922,123985,124085,125158,125269,
								 128506,130467,130987,131779,134591,137683,141872,146142 ,149864,445590,510534,105,175,927,1107,
								 1135,1267,100793),]
	hl <- hl[!hl$SpecCode %in% c(105,175,927,1107,1135,1267,100793,103443,103692,106057,106835,106903,107558,110908,111361,
								 117940,122348,123160,123426,124257,125027,125284,131495,135294,135301,135306,138992,140528,140687,
								 167882,178527,239867,291396,106763,137656,117225,100653,125125,100698,131774,134366,123386,117228,
								 117994,138923,123127,137701,123320,131629 ,152391,1363,214,103543,106994,103450,129400,140143,
								 146420,141905,22496,988,103717,107163,982,985,123622,102145,1082,10216,103483),]
	
	ca <- ca[!ca$SpecCode %in% c(443,938,1131,1292,1337,1360,19494,22988,100751,100757,100790,101054,103484,104062,
								 106122,106669,107011,107052,107148,107239,107388,107563,110690,110911,110956,111411,117136,
								 117258,123260,123276,123321,123335,123574,123593 ,123851,123922,123985,124085,125158,125269,
								 128506,130467,130987,131779,134591,137683,141872,146142 ,149864,445590,510534,105,175,927,1107,
								 1135,1267,100793),]
	ca <- ca[!ca$SpecCode %in% c(105,175,927,1107,1135,1267,100793,103443,103692,106057,106835,106903,107558,110908,111361,
								 117940,122348,123160,123426,124257,125027,125284,131495,135294,135301,135306,138992,140528,140687,
								 167882,178527,239867,291396,106763,137656,117225,100653,125125,100698,131774,134366,123386,117228,
								 117994,138923,123127,137701,123320,131629 ,152391,1363,214,103543,106994,103450,129400,140143,
								 146420,141905,22496,988,103717,107163,982,985,123622,102145,1082,10216,103483),]
	
	hl <- hl[!hl$SpecCode %in% c(NA, 101,106769,106782,107010,107726,122478,123506,12437,124951,128539,129402,196221,205077,124373, 123187, 124710),]
	ca <- ca[!ca$SpecCode %in% c(NA, 101,106769,106782,107010,107726,122478,123506,12437,124951,128539,129402,196221,205077,124373, 123187, 124710),]
	
	## IU: Filter out additional benthos:
	benthosSpecCodes <- c(104,956,966,1128,1296,1367,1608,11707,100782,100839,100854,103439,103732,104040,105865,106041,106673,106702,106789,106834,107152,
						  107205,107264,110749,110916,110993,111152,111355,111365,117093,117195,118445,122626,123204,123255,123613,124147,124151,124324,124670,
						  128490,128503,129563,130057,134691,136025,137710,138018,138068,138477,138631,138749,138938,140166,140173,140480,140625,141904,141929,
						  149854,152997,532035,816800)
	
	hl <- hl[!hl$SpecCode %in% benthosSpecCodes,]
	ca <- ca[!ca$SpecCode %in% benthosSpecCodes,]
	
	
	## WARN #3:
	## ca records with no HL records
	## these records are because there is no catch weight
	## DATRAS does not accept length info without catch weight
	## so create a line in the HL for each, but give SpecValue=4 and delete ca record
	
	#IU: Improved cleaning#
	# Use join to find missing value in HL
	if (nrow(ca) > 0) {
		#testca <- unique(data.frame(StNo=ca$StNo, SpecCode=ca$SpecCode, ca=TRUE))
		#testhl <- unique(data.frame(StNo=hl$StNo, SpecCode=hl$SpecCode, hl=TRUE))
		testca <- unique(data.table::data.table(StNo=ca$StNo, SpecCode=ca$SpecCode, ca=TRUE))
		testhl <- unique(data.table::data.table(StNo=hl$StNo, SpecCode=hl$SpecCode, hl=TRUE))
		tt <- merge(testca, testhl, by = c("StNo","SpecCode"), all=TRUE)
		missingHL <- tt[is.na(tt$hl),]
		
		# Populate missing value in HL
		for(idxHL in seq_len(nrow(missingHL))) {
			r <- missingHL[idxHL,]
			tmp <- hl[hl$StNo==r$StNo,][1,]
			tmp$SpecCode <- r$SpecCode
			tmp$SpecVal <- 4
			tmp$TotalNo <- c(hh$HaulDur[hh$StNo==r$StNo])
			tmp$CatCatchWgt <- NA
			hl <- rbind(hl,tmp)
		}
	}
	## WARN #4:
	
	# Use plus group for herring and mackerel individuals with age ring above 15
	ca[ which((ca$SpecCode==127023 | ca$SpecCode==126417) & ca$AgeRings >= 15), c("PlusGr", "AgeRings")] <- list("+", 15)
	
	# Order HL
	hl <- hl[order(hl$StNo),]
	
	#
	ICESDatrasData <- list(HH = hh, HL = hl, CA = ca)
	
	return(ICESDatrasData)
}



#' Writes \code{\link{ICESDatrasData}} to a csv file for each input acoustic file used to create the \code{\link{ICESDatras}}
#'
#' @param ICESDatrasData A \code{\link{ICESDatrasData}} object returned from \code{\link{ICESDatras}}.
#'
#' @return List of string matrices in the ICES Datras CSV format.
#'
#' @export
WriteICESDatras <- function(ICESDatrasData){
	
	WriteICESDatrasData <- lapply(
		ICESDatrasData, 
		WriteICESDatrasOne, 
		na = "-9"
	)
	
	return(WriteICESDatrasData)
}


WriteICESDatrasOne <- function(ICESDatrasDataOne, na = "-9"){
	
	# Convert all tables to string matrix with header and record, and rbind:
	ICESDatrasCSVDataOne <- convertToRecordTypeMatrix(ICESDatrasDataOne)
	
	# Replace NAs:
	if(length(na)) {
		ICESDatrasCSVDataOne <- lapply(ICESDatrasCSVDataOne, function(x) {x[is.na(x)] <- na; x})
	}
	
	#ICESDatrasCSVDataOne <- expandWidth(ICESDatrasCSVDataOne, na = na)
	
	# Stack all matrices:
	#ICESDatrasCSVDataOne <- do.call(rbind, ICESDatrasCSVDataOne)
	
	# Convert each line of each table to comma separated:
	ICESDatrasCSVDataOne <- lapply(ICESDatrasCSVDataOne, apply, 1, paste, collapse = ",")
	
	# Join to one vector, to be written to one file:
	ICESDatrasCSVDataOne <- unlist(ICESDatrasCSVDataOne)
	
	return(ICESDatrasCSVDataOne)
}










getTSCountryByIOC <- function(nation) {
	cnvTbl <- c("58" = "NO")
	
	x <- cnvTbl[as.character(nation)]
	x[is.null(x)] <- NA
	return(x)
}

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

# Adopted from: https://www.mathworks.com/matlabcentral/fileexchange/62180-sunriseset-lat-lng-utcoff-date-plot
getDayNight <- function(stationstartdate, stationstarttime, latitudestart, longitudestart, UTCoff = 0) {
	
	deg2rad <- function(val) {
		return(val * (pi / 180))
	}
	
	rad2deg <- function(val) {
		return(val * (180 / pi))
	}
	
	datetime0 <- as.POSIXct("1990-12-30", tz = "UTC")
	
	uniqueDates <- unique(stationstartdate)
	
	nDaysA = as.numeric(difftime(uniqueDates, datetime0, units = "days")) # Number of days since 01/01
	
	nTimes = 24*3600                       # Number of seconds in the day
	tArray = seq(0, 1, length = nTimes)
	
	ssTab <- list()
	
	for(idx in seq_len(length(nDaysA))) {
		
		nDays <- nDaysA[idx]
		lat <- latitudestart[idx]
		lng <- longitudestart[idx]
		localdate <- as.POSIXct(uniqueDates[idx], tz = "UTC")
		
		# Compute
		# Letters correspond to colums in the NOAA Excel
		E = tArray
		F = nDays + 2415018.5 + E - UTCoff / 24
		G = (F - 2451545) / 36525
		I = (280.46646 + G * (36000.76983 + G * 0.0003032)) %% 360
		J = 357.52911 + G * (35999.05029 - 0.0001537 * G)
		K = 0.016708634 - G * (0.000042037 + 0.0000001267 * G)
		L = sin(deg2rad(J)) * (1.914602 - G * (0.004817 + 0.000014 * G))+sin(deg2rad(2 * J)) * (0.019993 - 0.000101*G) + sin(deg2rad(3 * J)) * 0.000289
		M = I + L
		P = M - 0.00569 - 0.00478 * sin(deg2rad(125.04 - 1934.136 * G))
		Q = 23 + (26 + ((21.448 - G * (46.815 + G * (0.00059 - G * 0.001813)))) / 60) / 60
		R = Q + 0.00256 * cos(deg2rad(125.04 - 1934.136 * G))
		T = rad2deg(asin(sin(deg2rad(R)) * sin(deg2rad(P))))
		U = tan(deg2rad(R/2)) * tan(deg2rad(R/2))
		V = 4 * rad2deg(U * sin(2 * deg2rad(I))-2 * K * sin(deg2rad(J)) + 4 * K * U * sin(deg2rad(J)) *  cos(2*deg2rad(I)) - 0.5 * U * U * sin(4 * deg2rad(I)) - 1.25 * K * K * sin(2 * deg2rad(J)))
		AB = (E * 1440 + V + 4 *lng - 60 * UTCoff) %% 1440
		
		
		AC = ifelse (AB/4 < 0, AB/4 + 180, AB/4 - 180)
		
		AD = rad2deg(acos(sin(deg2rad(lat)) * sin(deg2rad(T)) + cos(deg2rad(lat)) * cos(deg2rad(T)) * cos(deg2rad(AC))))
		
		# Test whether we are in midnightsun: 
		WArg <- cos(deg2rad(90.833)) / (cos(deg2rad(lat)) * cos(deg2rad(T))) - tan(deg2rad(lat)) * tan(deg2rad(T))
		# Truncate WArg to [-1, 1], where below is midnightsun and above 1 is myrketid:
		if(any(WArg < -1)) {
			WArg[WArg < -1] <- -1
		}
		if(any(WArg > 1)) {
			WArg[WArg > 1] <- 1
		}
		
		W = rad2deg(acos(WArg))
		X = (720 - 4 * lng - V + UTCoff * 60) * 60
		
		sunrise = which.min(abs(X - round(W * 4 * 60) - nTimes * tArray))
		sunset = which.min(abs(X+round(W*4*60) - nTimes*tArray))
		
		sunrisetime = localdate + sunrise
		sunsettime = localdate + sunset
		
		ssTab[[uniqueDates[idx]]] <- list(sunrise = sunrisetime, sunset = sunsettime)
	}
	
	getDN <- function(x, ssTab) {
		
		y <- ssTab[[format(x, "%Y-%m-%dZ")]]
		
		if(x < y$sunrise || x >= y$sunset) {
			return("N")
		} else {
			return("D")
		}
	}
	
	datetime <- as.POSIXct(gsub("Z", " ", paste0(stationstartdate, stationstarttime)), tz = "UTC")
	
	return(unlist(lapply(datetime, getDN, ssTab)))
}

# Convert Length Measurement Type
# http://tomcat7.imr.no:8080/apis/nmdapi/reference/v2/dataset/lengthmeasurement?version=2.0
# http://vocab.ices.dk/?ref=1392
convLenMeasType <- function(LenMeasType) {
	# Convert table
	ct <- c(
		"B" = 5,
		"C" = 6,
		"E" = 1,
		"F" = 8,
		"G" = 4,
		"H" = 3,
		"J" = 2,
		"L" = 7,
		"S" = 9
	)
	return(ct[LenMeasType])
}

# Convert aging structure source
# http://tomcat7.imr.no:8080/apis/nmdapi/reference/v2/dataset/agingstructure?version=2.0
# http://vocab.ices.dk/?ref=1507
# This function seems to incorrect, as the AgeSource documentation on here only lists three values:
# http://vocab.ices.dk/?ref=1482
convAgeSource <- function(AgeSource) {
	# Convert table
	if(!all(AgeSource %in% c("1", "2", "7"))) {
		warning("The conversion from agingstructure to AgeSource may be wrong for other values than 1, 2 and 7. Please noify the developers of StoX.")
	}
	
	ct <- c("1" = "scale",
			"2" = "otolith",
			"4" = "df-spine",
			"6" = "spine",
			"7" = "vertebra",
			"8" = "caudal-thorn")
	return(ct[AgeSource])
}


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
