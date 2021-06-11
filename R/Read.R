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
#' Read landing XML files
#' 
#' This function reads multiple landing files (sales-notes) to a list with a list of tables for each file.
#' 
#' @details
#' This sales notes are expected to be XML-formatted with elements defined by the namespace: http://www.imr.no/formats/landinger/v2
#' 
#' @param FileNames The paths of the landing files.
#' 
#' @return
#' An object of StoX data type \code{\link[RstoxData]{LandingData}}).
#' 
#' @examples
#' exampleFile <- system.file(
#'     "testresources","landing.xml", package="RstoxData")
#' landingData <- ReadLanding(exampleFile)
#' 
#' @seealso \code{\link[RstoxData]{readXmlFile}}.
#' 
#' @export
#' 
ReadLanding <- function(FileNames) {
  
  # Read LandingData possibly on several cores:
  LandingData <- lapplyOnCores(
    FileNames, 
    FUN = RstoxData::readXmlFile, 
    NumberOfCores = 1L
  )
  
  # Add names as the file names:
  names(LandingData) <- basename(FileNames)
  
  return(LandingData)
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
	
	temp[, isPelagic := ifelse(ap %in% c("126417", "126421", "126425", "126426", "127023"), TRUE, FALSE)]
	
	temp[!is.na(sp) & isPelagic == TRUE,  res := ifelse(sp <= 2, 61, ifelse(sp <= 5, 62, 60 + sp - 3))]
	temp[!is.na(sp) & isPelagic == FALSE, res := 60 + sp]
	
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
	temp <-  data.table::data.table(g = gearcondition, s = samplequality)
	temp[, res := "I"]
	temp[(is.na(g) | g %in% c("1", "2")) &
		 	(is.na(s) | s %in% c("0", "1")), res := "V"]
	
	return(temp$res)
}

# Set gearcondition == 1 & samplequality == 1 to "V", and all other to NA:
getHaulValiditySimple <- function(gearcondition, samplequality) {
	Validity <- rep(NA_character_, length(gearcondition))
	valid <- gearcondition == 1 & samplequality == 1
	Validity[valid] <- "V"
	
	return(Validity)
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
	# Distance is integer in ICESBiotic:
	x <- round(x)
	return(x)
}

# Calculate time diff
getTimeDiff <- function(stationstartdate, stationstarttime, stationstopdate, stationstoptime) {
	
	t0 <- ifelse(is.na(stationstartdate) | is.na(stationstarttime), NA, gsub("Z", " ", paste0(stationstartdate, stationstarttime)))
	t1 <- ifelse(is.na(stationstopdate) | is.na(stationstoptime), NA, gsub("Z", " ", paste0(stationstopdate, stationstoptime)))
	
	start <- as.POSIXct(t0)
	end <- as.POSIXct(t1)
	
	# Diff in minutes:
	diffMins <- as.numeric(round(difftime(end, start, units = "mins")))
	
	return(diffMins)
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



# Function to interpret ICES LengthCode from NMCBiotic lengthresolution. This function uses getLengthResolutionTable() which defines the relationship between these.
getLengthCodeICES <- function(lengthresolution) {
	# Interpret/approximate the lengthresolution as mm, halfcm or cm, which also implies modifying the length if cm (see convertLengthGivenLengthCode()):
	lengthResolutionTable <- getLengthResolutionTable()
	LengthCode <- lengthResolutionTable$LengthCode[match(lengthresolution, lengthResolutionTable$lengthresolution)]
	nonValid <- !is.na(lengthresolution) & is.na(LengthCode)
	if(any(nonValid)) {
		warning("The following lengthresolution do not match any of the LengthCode defined by http://vocab.ices.dk/?ref=1486: ", paste(unique(lengthresolution[nonValid]), collapse = ", "), ". The following table shows the valid lengthresolution:\n",  paste(names(lengthResolutionTable), collapse = "\t"), "\n", paste(lengthResolutionTable[, do.call(paste, c(.SD, list(sep = "\t")))], collapse = "\n"))
	}
	return(LengthCode)
}

getLengthResolutionTable <- function() {
	# The length resolutions defined by NMDReference as per 2021-04-22:
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
	# Interpret length code:
	#value_unit <- strsplit(lengthResolutionTable$shortname, " ")
	#value <- as.numeric(sapply(value_unit, "[[", 1))
	#unit <- sapply(value_unit, "[[", 2)
	#mm <- value * 10^(match(unit, c("mm", "cm")) - 1)
	
	valid <- data.table::data.table(
		shortname = c("1 mm", "5 mm", "1 cm"), 
		LengthCode = c("mm", "halfcm", "cm")
	)
	
	lengthResolutionTable[, LengthCode := valid$LengthCode[match(shortname, valid$shortname)]]
	
	return(lengthResolutionTable)
}

scaleLengthUsingLengthCode <- function(length, LengthCode, inputUnit) {
	
	lengthCode_unit_table <- data.table::data.table(
		lengthCode = c("mm", "halfcm", "cm"), 
		reportingUnit = c("mm", "mm", "cm")
	)
	
	outputUnit <- lengthCode_unit_table[match(LengthCode, lengthCode), reportingUnit]
	
	outputLength <- length * scaleUnit(inputUnit, outputUnit)
	
	return(outputLength)
}

# Use the units package to scale 
scaleUnit <- function(inputLengthUnit, outputLengthUnit) {
	output <- double(max(length(inputLengthUnit), length(outputLengthUnit))) + 1
	
	# Use mixed units here as there may be fish with different units, and then update the units with set_units():
	output <- units::mixed_units(output, inputLengthUnit)
	output <- units::set_units(output, outputLengthUnit)
	
	output <- units::drop_units(output)
	return(output)
}



