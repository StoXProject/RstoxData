##################################################
##################################################
#' Read biotic XML files
#' 
#' This function reads multiple biotic file to a list with a list of tables for each file.
#' 
#' @param FileNames The paths of the biotic files.
#' 
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @return
#' An object of StoX data type BioticData: A list of a list of data.tables of the different levels of the input biotic files.
#' 
#' @examples
#' exampleFile <- system.file("testresources","biotic3.1_example.xml", package="RstoxData")
#' bioticData <- ReadBiotic(exampleFile)
#' 
#' @seealso \code{\link[RstoxData]{readXmlFile}}.
#' 
#' @export
#' 
ReadBiotic <- function(FileNames) {
	
	# Read BioticData possibly on several cores:
	BioticData <- lapplyOnCores(
		FileNames, 
		FUN = RstoxData::readXmlFile, 
		NumberOfCores = 1L
	)
	
	# Add names as the file names:
	names(BioticData) <- basename(FileNames)
	
	return(BioticData)
}



##################################################
##################################################
#' Read acoustic XML files
#' 
#' This function reads multiple acoustic file to a list with a list of tables for each file.
#' 
#' @param FileNames The paths of the acoustic files.
#' 
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @return
#' An object of StoX data type AcousticData: A list of a list of data.tables of the different levels of the input acoustic files.
#' 
#' @examples
#' exampleFile <- system.file("testresources","libas_ListUserFile20__L40.0-2259.9_small.xml", package="RstoxData")
#' bioticData <- ReadBiotic(exampleFile)
#' 
#' @seealso \code{\link[RstoxData]{readXmlFile}}.
#' 
#' @export
#' 
ReadAcoustic <- function(FileNames) {
	
	# Read AcousticData possibly on several cores:
	AcousticData <- lapplyOnCores(
		FileNames, 
		FUN = RstoxData::readXmlFile, 
		NumberOfCores = 1L
	)
	
	# Add names as the file names:
	names(AcousticData) <- basename(FileNames)
	
	return(AcousticData)
}




##################################################
##################################################
#' Convert BioticData to ICESBiotic form.
#' 
#' This function converts BioticData read from files of format NMDBiotic v3 and v3.1 to ICESBiotic tables.
#' 
#' @inheritParams ModelData
#' @param SurveyName A string naming the survey. Must be one of the names listed on \url{https://vocab.ices.dk/?ref=1453} or NONE (the default).
#' @param Country The country code as listed on \url{http://vocab.ices.dk/?ref=337}.
#' @param Organisation An integer code representing the organization running the cruise. See \url{https://vocab.ices.dk/?ref=1398} for a list of possible codes (e.g., Institute of Marine Research: 612).
#' @param AllowRemoveSpecies Logical: ICES submission will not allow the resulting CSV file to be uploaded if the file contains species not listed on \url{https://acoustic.ices.dk/Services/Schema/XML/SpecWoRMS.xml}. Setting this parameter to TRUE will remove the unlisted species records.
#' 
#' @details
#' The ICESBiotic (name used by StoX) data format is defined on \url{https://ices.dk/data/data-portals/Pages/acoustic.aspx} (\href{https://ices.dk/data/Documents/Acoustic/ICES_Acoustic_data_format_description.zip}{Direct download link}).
#' 
#' @return
#' An object of StoX data type \code{\link{BioticData}}.
#' 
#' @seealso \code{\link{ReadBiotic}}.
#' 
#' @export
#' 
NMDBioticToICESBiotic <- function(
	BioticData, 
	SurveyName = "NONE", 
	Country = character(), 
	Organisation = integer(), 
	AllowRemoveSpecies = TRUE
) {
	
	ICESBioticData <- lapply(
		BioticData, 
		NMDBioticToICESBioticOne, 
		SurveyName = SurveyName, 
		Country = Country, 
		Organisation = Organisation, 
		AllowRemoveSpecies = AllowRemoveSpecies
	)
	
	return(ICESBioticData)
}




NMDBioticToICESBioticOne <- function(
	BioticDataOne, 
	SurveyName = "NONE", 
	Country = character(), 
	Organisation = integer(), 
	AllowRemoveSpecies = TRUE
) {
	
	if(!(BioticDataOne$metadata$useXsd %in% c("nmdbioticv3", "nmdbioticv3.1"))) {
		if(BioticDataOne$metadata$useXsd %in% "icesBiotic") {
			warning("StoX: BioticData from the ICES file ", BioticDataOne$metadata$file, " returned unchanged.")
			return(BioticDataOne)
		}
		else {
			warning("StoX: Only NMD Biotic version 3 and 3.1 data can be converted to ICESBiotic (was ", BioticDataOne$metadata$useXsd, " for the file ", BioticDataOne$metadata$file, ". NA returned.")
			return(NULL)
		}
		
	}
	
	cruiseRaw <- BioticDataOne$mission
	
	Cruise <- cruiseRaw[, .(
		Survey = ..SurveyName,
		Country = ..Country,
		Organisation = ..Organisation,
		Platform = getICESShipCode(platformname),
		StartDate = gsub("Z", "", missionstartdate),
		EndDate = gsub("Z", "", missionstopdate),
		LocalID = cruise
	)]
	
	
	haulRaw <- merge(cruiseRaw, BioticDataOne$fishstation)
	
	Haul <- haulRaw[, .(
		LocalID = cruise,
		Gear = gear,
		Number = serialnumber,
		StationName = station,
		StartTime = ifelse(is.na(stationstartdate) | is.na(stationstarttime), NA, gsub("Z", " ", paste0(stationstartdate, substr(stationstarttime, 1, 5)))),
		Duration = getTimeDiff(stationstartdate, stationstarttime, stationstopdate, stationstoptime),
		Validity = getHaulVal(gearcondition, samplequality),
		StartLatitude = latitudestart,
		StartLongitude = longitudestart,
		StopLatitude = latitudeend,
		StopLongitude = longitudeend,
		StatisticalRectangle = getICESrect(latitudestart, longitudestart),
		MinTrawlDepth = ifelse(is.na(fishingdepthmin), fishingdepthmax, fishingdepthmin),
		MaxTrawlDepth = fishingdepthmax,
		BottomDepth = ifelse(bottomdepthstop > fishingdepthmax, bottomdepthstop, NA),
		Distance = round(getDistanceMeter(latitudestart, longitudestart, latitudeend, longitudeend)),
		Netopening = verticaltrawlopening,
		CodendMesh = NA,
		SweepLength = getGOVSweepByEquipment(gear),
		GearExceptions = NA,
		DoorType = trawldoortype,
		WarpLength = wirelength,
		WarpDiameter = wirediameter,
		WarpDensity = wiredensity,
		DoorSurface = trawldoorarea,
		DoorWeight = trawldoorweight,
		DoorSpread = trawldoorspread,
		WingSpread = wingspread,
		Buoyancy = NA,
		KiteArea = NA,
		GroundRopeWeight = NA,
		Rigging = NA,
		Tickler = NA,
		HydrographicStationID = NA,
		TowDirection = direction,
		SpeedGround = NA,
		SpeedWater = gearflow,
		WindDirection = winddirection,
		WindSpeed = windspeed,
		SwellDirection = NA,
		SwellHeight = NA,
		LogDistance = NA,
		Stratum = NA
	)]
	
	catchRaw <- merge(BioticDataOne$catchsample, haulRaw, by = intersect(names(BioticDataOne$catchsample), names(haulRaw)))
	
	# We must filter records with aphia == NA
	catchRaw <- catchRaw[!is.na(aphia)]
	
	Catch <- catchRaw[, .(
		LocalID = cruise,
		Gear = gear,
		Number = serialnumber,
		SpeciesCode = aphia,
		SpeciesCategory = catchpartnumber,
		DataType = "R",
		SpeciesValidity = ifelse(is.na(catchproducttype), 0, catchproducttype),
		SpeciesCategoryNumber = catchcount,
		WeightUnit = "kg",
		SpeciesCategoryWeight = catchweight,
		SpeciesSex = NA,
		SubsampledNumber = lengthsamplecount,
		SubsamplingFactor = catchcount / lengthsamplecount,
		SubsampleWeight = lengthsampleweight,
		LengthCode = NA,
		LengthClass = NA,
		LengthType = "1",
		NumberAtLength = lengthsamplecount,
		WeightAtLength = NA
	)]
	
	# Logic for missing important records
	Catch[is.na(SpeciesCategoryNumber) & is.na(SpeciesCategoryWeight) & !is.na(SubsampledNumber), SpeciesCategoryNumber := SubsampledNumber]
	Catch[is.na(SpeciesCategoryNumber) & is.na(SpeciesCategoryWeight) & !is.na(SubsampleWeight),  SpeciesCategoryWeight := SubsampleWeight]
	
	# NA means that nothing is subsampled
	Catch[!is.na(SpeciesCategoryWeight) & is.na(SubsampleWeight), SubsampleWeight := 0]
	
	# Set Haul without any catch as invalid hauls
	'%ni%' <- Negate('%in%')
	Haul[Number %ni% unique(Catch$Number), Validity := "I"]
	
	# Combine required tables for the Biology level
	indRaw <- BioticDataOne$individual
	indRaw[is.na(preferredagereading), preferredagereading := 1]
	
	baseAge <- intersect(names(indRaw), names(BioticDataOne$agedetermination))
	indRaw <- merge(indRaw, BioticDataOne$agedetermination, by.x=c(baseAge, "preferredagereading"), by.y= c(baseAge, "agedeterminationid"), all.x = TRUE)
	indRaw <- merge(catchRaw, indRaw, by = intersect(names(catchRaw), names(indRaw)))
	
	Biology <- indRaw[, .(
		LocalID = cruise,
		Gear = gear,
		Number = serialnumber,
		SpeciesCode = aphia,
		SpeciesCategory = catchpartnumber,
		StockCode = NA,
		FishID = specimenid,
		LengthCode = "mm",
		LengthClass = length * 1000,
		WeightUnit = 'gr',
		IndividualWeight = individualweight * 1000,
		IndividualSex = ifelse(is.na(sex), NA, ifelse(sex == "1", "F", "M")),
		IndividualMaturity = getDATRASMaturity(getQuarter(stationstartdate), aphia, specialstage, maturationstage),
		MaturityScale = "M6",
		IndividualAge = age,
		AgePlusGroup = NA,
		AgeSource = "Otolith",
		GeneticSamplingFlag = NA,
		StomachSamplingFlag = NA,
		ParasiteSamplingFlag = NA,
		IndividualVertebraeCount = NA
	)]
	
	if(AllowRemoveSpecies) {
		message("All species that is not listed in https://acoustic.ices.dk/Services/Schema/XML/SpecWoRMS.xml are automatically removed. Set AllowRemoveSpecies = FALSE to prevent this.")
		# Check for valid aphias, mark other as invalid
		xmlRaw <- read_xml("https://acoustic.ices.dk/Services/Schema/XML/SpecWoRMS.xml")
		validCodes <- xml_text(xml_find_all(xmlRaw, "//Code//Key"))
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


