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
#' exampleFile <- system.file(
#'     "testresources","libas_ListUserFile20__L40.0-2259.9_small.xml", package="RstoxData")
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
	SurveyName = "NONE", 
	Country = character(), 
	Organisation = integer(), 
	AllowRemoveSpecies = TRUE
) {
	
	if(!(BioticDataOne$metadata$useXsd %in% c("nmdbioticv3", "nmdbioticv3.1"))) {
		if(BioticDataOne$metadata$useXsd %in% "icesBiotic") {
			#warning("StoX: BioticData from the ICES file ", BioticDataOne$metadata$file, " returned unchanged.")
			
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
				
				BioticDataOne[tablesToTranslate] <- translateVariables(
					data = BioticDataOne[tablesToTranslate], 
					Translation = vocabulary, 
					translate.keys = TRUE
				)
			}
			
			
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
		Gear = gear, # Is gear assumed formatted correctly in the input file???
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
		GearExceptions = NA, # Should this be set?
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
		DataType = "R", # Always raw for NMDBiotic
		SpeciesValidity = ifelse(is.na(catchproducttype), 0, catchproducttype), # What is meant here. SpeciesValidity can only be 0 or 1, but catchproducttype has a range of values. Should it be ifelse(is.na(catchproducttype), 0, 1) ??
		SpeciesCategoryNumber = catchcount,
		WeightUnit = "kg",
		SpeciesCategoryWeight = catchweight,
		SpeciesSex = NA,
		SubsampledNumber = lengthsamplecount,
		SubsamplingFactor = catchcount / lengthsamplecount,
		SubsampleWeight = lengthsampleweight,
		LengthCode = NA, # Not relevant??
		LengthClass = NA,
		LengthType = "1", # Should not this be interpreted from the catchsample$lengthmeasurement ???
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
		# Check for valid aphias, mark other as invalid
		xmlRaw <- read_xml("https://acoustic.ices.dk/Services/Schema/XML/SpecWoRMS.xml")
		validCodes <- xml_text(xml_find_all(xmlRaw, "//Code//Key"))
		
		notPresentInCatch <- unique(setdiff(Catch$SpeciesCode, validCodes))
		notPresentInBiology <- unique(setdiff(Biology$SpeciesCode, validCodes))
		
		if(length(notPresentInCatch)) {
			warning("StoX: The following species not listed in https://acoustic.ices.dk/Services/Schema/XML/SpecWoRMS.xml were automatically removed from table Catch (set AllowRemoveSpecies = FALSE to prevent this):\n", paste(notPresentInCatch, collapse = ", "))
			
		}
		if(length(notPresentInBiology)) {
			warning("StoX: The following species not listed in https://acoustic.ices.dk/Services/Schema/XML/SpecWoRMS.xml were automatically removed from table Biology (set AllowRemoveSpecies = FALSE to prevent this):\n", paste(notPresentInBiology, collapse = ", "))
			
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




# Function to compare with ices vocabulary of allowed values
compareICES <- function(url, field) {
	pg <- tryCatch(
		{
			read_xml(url)
		},
		error = function(e){
			warning(paste("StoX: Url", url, "is not exist or no internet connection available."))
			emptyXML <- xml2::as_xml_document(list(list()))
			return(emptyXML)
		}
	)
	recs <- xml_find_all(pg, "//Key")
	vals <- trimws(xml_text(recs))
	for(x in field){
		if(!x %in% vals){
			warning(paste0("StoX: ", x, " not defined in ", url))
		}
	}
}     

# Get quarter representation from a date
getQuarter <- function(stationstartdate) {
	x <- format(as.Date(stationstartdate, format="%Y-%m-%dZ"), "%m")
	return(floor((as.numeric(x) - 1) / 3 + 1))
}

# Get maturity indicator for a species
getDATRASMaturity <- function(quarter, aphia, specialstage, maturationstage) {
	
	temp <-  as.data.table(cbind(q=quarter, ap=aphia, sp=specialstage, ms=maturationstage, res=NA))
	
	temp[, `:=`(sp = as.numeric(sp), ms = as.numeric(ms), res = as.numeric(res) )]
	
	temp[, isHerringOrSpratOrMackerel := ifelse(ap %in% c("126417", "126425", "127023"), TRUE, FALSE)]
	
	temp[!is.na(sp) & isHerringOrSpratOrMackerel == TRUE,  res := ifelse(sp <= 2, 61, ifelse(sp <= 5, 62, 60 + sp - 3))]
	temp[!is.na(sp) & isHerringOrSpratOrMackerel == FALSE, res := 60 + sp]
	
	temp[is.na(sp) & !is.na(ms), res := ifelse(ms == 5 & q == "3", NA, 60 + ms)]
	
	return(temp$res)
}

# Convert gear number to sweep length
getGOVSweepByEquipment <- function(gear) {
	cnvTbl <- c("3120" = NA,
				"3190" = 60,
				"3191" = 60,
				"3192" = 60,
				"3193" = 110,
				"3194" = 110,
				"3195" = 110,
				"3196" = 60,
				"3197" = 110,
				"3198" = 60,
				"3199" = 60)
	
	x <- cnvTbl[as.character(gear)]
	x[is.null(x)] <- NA
	return(x)
}

# Get Haul validity
getHaulVal <- function(gearcondition, samplequality) {
	temp <-  as.data.table(cbind(g=gearcondition, s=samplequality))
	temp[, res:="I"]
	temp[(is.na(g) | g %in% c("1","2")) &
		 	(is.na(s) | s %in% c("0", "1")), res:="V"]
	
	return(temp$res)
}

# Generate ICES rectangle from a coordinate
# Stolen from: https://github.com/cran/mapplots/blob/master/R/ices.rect.R
getICESrect <- function(lat, lng){
	x <- floor(lng+60)+1000
	y <- floor(lat*2)-71+100
	num1<- substr(y,2,3)
	lett <- LETTERS[as.numeric(substr(x,2,3))]
	num2 <- substr(x,4,4)
	paste(num1,lett,num2,sep='')
}

# Get distance in meters between two coordinates
getDistanceMeter <- function(lat1, lon1, lat2, lon2) {
	x <-  acos( sin(lat1*pi/180)*sin(lat2*pi/180) + cos(lat1*pi/180)*cos(lat2*pi/180)*cos(lon2*pi/180-lon1*pi/180) ) * 6371000
	return(x)
}

# Calculate time diff
getTimeDiff <- function(stationstartdate, stationstarttime, stationstopdate, stationstoptime) {
	
	t0 <- ifelse(is.na(stationstartdate) | is.na(stationstarttime), NA, gsub("Z", " ", paste0(stationstartdate, stationstarttime)))
	t1 <- ifelse(is.na(stationstopdate) | is.na(stationstoptime), NA, gsub("Z", " ", paste0(stationstopdate, stationstoptime)))
	
	start <- as.POSIXct(t0)
	end <- as.POSIXct(t1)
	
	return(round(difftime(end, start, units = "mins")))
}

# Get ICES ship data
#' @importFrom xml2 xml_ns_strip xml_find_all xml_text
getICESShipCode <- function(platformname) {
	
	construct <- function(shipName) {
		# We have to remove "."," " and use uppercase
		shipName <- toupper(gsub("[[:space:][:punct:]]", "", shipName))

		# Replace the nordic character with AA
		shipName <- gsub("\u00C5", "AA", shipName)

		data <- tryCatch(
			{
				read_xml("https://vocab.ices.dk/services/pox/GetCodeList/SHIPC")
			},
			error = function(e){return(NA)}
		)

		# Can't download from ICES
		if (is.na(data))
			return(NA)

		xml_ns_strip(data)
		nodes <- xml_find_all(data, paste0("//Code[contains(translate(Description[normalize-space()],'abcdefghijklmnopqrstuvwxyz. ','ABCDEFGHIJKLMNOPQRSTUVWXYZ'), \"",
					shipName, "\") and contains(Deprecated, \"false\")]/*[self::Key or self::Modified]"))

		# Ship not found
		if (length(nodes) < 1) {
			return(NA)
		}

		# Get the latest matching ship code
		xx <- xml_text(nodes)
		yy <- as.data.frame(list(code = xx[seq(xx) %% 2 == 1], date = xx[seq(xx) %% 2 == 0]), stringsAsFactors = FALSE)
		shipCode <- head(yy[order(as.Date(yy$date), decreasing = TRUE), "code"], 1)

		return(shipCode)
	}
	
	nm <- unique(platformname)
	y <- unlist(lapply(nm, construct))
	names(y) <- nm
	
	x <- y[as.character(platformname)]
	x[is.null(x)] <- NA
	
	return(x)
}

