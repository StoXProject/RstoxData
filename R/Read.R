#' @noRd
check_landing_duplicates_landingerv2 <- function(LandingData, warn=T, fix=F){
  for (f in names(LandingData)){
        xsdobj <- RstoxData::xsdObjects[["landingerv2.xsd"]]
        
        ids<-apply(LandingData[[f]]$Seddellinje[,1:xsdobj$prefixLens[["Seddellinje"]]],1,paste, collapse=".")
        duplicated <- ids[duplicated(ids)]
        
        if (length(duplicated)>0 && warn){
          warning(paste("Landings in", f, "contain duplicate key records. Consider the option ForceUnique or correct this in some other way."))
        }
        
        if (fix){
          highestLinjeNummer <- max(LandingData[[f]]$Seddellinje$Linjenummer)
          newLinjeNummerSeq <- (highestLinjeNummer+1):(highestLinjeNummer+sum(ids %in% duplicated))
          
          for (g in names(LandingData[[f]])){
            if (g != "Landingsdata" && g %in% names(xsdobj$prefixLens)){
              stopifnot(xsdobj$prefixLens[[g]]==xsdobj$prefixLens[["Seddellinje"]])
              LandingData[[f]][[g]]$Linjenummer[ids %in% duplicated] <- newLinjeNummerSeq
            }
          }
        }
      }
  return(LandingData)
}

#' @noRd
check_landing_duplicates <- function(LandingData, warn=T, fix=F){

  ok <- TRUE  
  for (f in names(LandingData)){
    if ("metadata" %in% names(LandingData[[f]]) && "useXsd" %in% names(LandingData[[1]][["metadata"]])){
      xsdobj <- paste(LandingData[[f]]$metadata$useXsd, "xsd", sep=".")
      if (!(xsdobj %in% names(RstoxData::xsdObjects)) || xsdobj != "landingerv2.xsd"){
        warning("Could not check for duplicates in Landings. Missing format defintion.")
        ok<-FALSE
      }
    }
  }
  
  if (ok){
    LandingData <- check_landing_duplicates_landingerv2(LandingData, warn, fix)
  }
  
  return(LandingData)
}

##################################################
##################################################
#' Read biotic XML files
#' 
#' This function reads multiple biotic file to a list with a list of tables for each file.
#' 
#' @param FileNames The paths of the biotic files.
#' 
#' @return
#' An object of StoX data type BioticData: A list of a list of data.tables of the different levels of the input biotic files.
#' 
#' @examples
#' exampleFile <- system.file("testresources","biotic3.1_example.xml", package="RstoxData")
#' bioticData <- ReadBiotic(exampleFile)
#' 
#' @seealso \code{\link{readXmlFile}} for reading xml files of the formats NMDAcoustic.
#' 
#' @export
#' 
ReadBiotic <- function(FileNames = character()) {
	
	if(!length(FileNames)) {
		stop("FileNames must be given.")
	}
	
	# Read BioticData possibly on several cores:
	BioticData <- lapplyOnCores(
		FileNames, 
		FUN = readXmlFile, 
		stream = TRUE, 
		#stream = FALSE, 
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
#' @return
#' An object of StoX data type AcousticData: A list of a list of data.tables of the different levels of the input acoustic files.
#' 
#' @examples
#' exampleFile <- system.file(
#'     "testresources","libas_ListUserFile20__L40.0-2259.9_small.xml", package="RstoxData")
#' acousticData <- ReadAcoustic(exampleFile)
#' 
#' @seealso \code{\link{readXmlFile}}.
#' 
#' @export
#' 
ReadAcoustic <- function(FileNames = character()) {
	
	if(!length(FileNames)) {
		stop("FileNames must be given.")
	}
	
	# Read AcousticData possibly on several cores:
	AcousticData <- lapplyOnCores(
		FileNames, 
		FUN = readXmlFile, 
		stream = TRUE, 
		#stream = FALSE, 
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
#' This sales notes are expected to be XML-formatted with elements defined by the namespace: http://www.imr.no/formats/landinger/v2.
#' 
#' Occasionally landing sets contain data that where rows are not uniquely identified by the key columns in that format.
#' In these cases a warning is issued, and it is important to handle those duplicates to avoid problems in later processing.
#' Uniqueness of keys are checked for in some typical downstream StoX processes, such as \code{\link[RstoxData]{StoxLanding}},
#' so the problem may potentially disappear after filtering. Otherways, the parameter 'ForceUnique' may be considered, if
#' one is confident these records does in fact represent separate landings.
#' 
#' @param FileNames The paths of the landing files.
#' @param ForceUnique Manipulate the field 'Linjenummer' with arbitrary changes to ensure that key columns uniquely identify rows.
#' 
#' 
#' @return
#' An object of StoX data type \code{\link{LandingData}}).
#' 
#' @examples
#' exampleFile <- system.file(
#'     "testresources","landing.xml", package="RstoxData")
#' landingData <- ReadLanding(exampleFile)
#' 
#' @seealso \code{\link{readXmlFile}}.
#' 
#' @export
#' 
ReadLanding <- function(FileNames = character(), ForceUnique=FALSE) {
  
	if(!length(FileNames)) {
		stop("FileNames must be given.")
	}
	
	# Read LandingData possibly on several cores:
  LandingData <- lapplyOnCores(
    FileNames, 
    FUN = readXmlFile, 
    stream = TRUE, 
    #stream = FALSE, 
    NumberOfCores = 1L
  )
  
  # Add names as the file names:
  names(LandingData) <- basename(FileNames)
  
  if (ForceUnique){
    LandingData <- check_landing_duplicates(LandingData, warn = F, fix = T)  
  }
  else{
    check_landing_duplicates(LandingData, warn=T, fix=F)  
  }
  
  
  return(LandingData)
}









# Function to compare with ices vocabulary of allowed values
compareICES <- function(tableName, fieldName, data, url) {
	pg <- tryCatch(
		{
			xml2::read_xml(url)
		},
		error = function(e){
			warning(paste("StoX: Url", url, "is not exist or no internet connection available."))
			emptyXML <- xml2::as_xml_document(list(list()))
			return(emptyXML)
		}
	)
	recs <- xml2::xml_find_all(pg, "//Key")
	vals <- trimws(xml2::xml_text(recs))
	for(x in unique(data[[tableName]][[fieldName]])){
		if(!x %in% vals){
			warning(paste0("StoX: ", tableName, fieldName, " = ", x, " is not defined in ", url))
		}
	}
}     

# Get quarter representation from a date
getQuarter <- function(stationstartdate) {
	x <- format(as.Date(stationstartdate, format="%Y-%m-%dZ"), "%m")
	return(floor((as.numeric(x) - 1) / 3 + 1))
}

## Get maturity indicator for a species
#getDATRASMaturity <- function(quarter, aphia, specialstage, maturationstage) {
#	
#	temp <-  as.data.table(cbind(q=quarter, ap=aphia, sp=specialstage, ms=maturationstage, res=NA))
#	
#	temp[, `:=`(sp = as.numeric(sp), ms = as.numeric(ms), res = as.numeric(res) )]
#	
#	temp[, isPelagic := ifelse(ap %in% c("126417", "126421", "126425", "126426", "127023"), TRUE, FALSE)]
#	
#	temp[!is.na(sp) & isPelagic == TRUE,  res := ifelse(sp <= 2, 61, ifelse(sp <= 5, 62, 60 + sp - 3))]
#	temp[!is.na(sp) & isPelagic == FALSE, res := 60 + sp]
#	
#	temp[is.na(sp) & !is.na(ms), res := ifelse(ms == 5 & q == "3", NA, 60 + ms)]
#	
#	return(temp$res)
#}

## Convert gear number to sweep length
#getGOVSweepByEquipment <- function(gear) {
#	cnvTbl <- c("3120" = NA,
#				"3190" = 60,
#				"3191" = 60,
#				"3192" = 60,
#				"3193" = 110,
#				"3194" = 110,
#				"3195" = 110,
#				"3196" = 60,
#				"3197" = 110,
#				"3198" = 60,
#				"3199" = 60)
#	
#	x <- cnvTbl[as.character(gear)]
#	x[is.null(x)] <- NA
#	return(x)
#}

# # Get Haul validity
# getHaulVal <- function(gearcondition, samplequality) {
# 	temp <-  data.table::data.table(g = gearcondition, s = samplequality)
# 	temp[, res := "I"]
# 	temp[(is.na(g) | g %in% c("1", "2")) &
# 		 	(is.na(s) | s %in% c("0", "1")), res := "V"]
# 	
# 	return(temp$res)
# }
# 
# Set gearcondition == 1 & samplequality == 1 to "V", and all other to NA:
getHaulValiditySimple <- function(gearcondition, samplequality) {
	Validity <- rep(NA_character_, length(gearcondition))
	valid <- gearcondition == 1 & samplequality == 1
	Validity[valid] <- "V"
	
	return(Validity)
}

# Generate ICES rectangle from a coordinate
# Stolen from: https://github.com/cran/mapplots/blob/master/R/ices.rect.R
getICESrect_old <- function(lat, lng){
	x <- floor(lng+60)+1000
	y <- floor(lat*2)-71+100
	num1<- substr(y,2,3)
	lett <- LETTERS[as.numeric(substr(x,2,3))]
	num2 <- substr(x,4,4)
	paste(num1,lett,num2,sep='')
}

getStatisticalRectangle <- function(lat, lon){
	# The ICES statistical rectangles (https://www.ices.dk/data/maps/Pages/ICES-statistical-rectangles.aspx) are defined on a grid with latitude in 0.5 degree steps from 36 to 85.5 degrees north, and with longitude from 44 degrees west to 68.5 degrees east:
	latGrid <- seq(36, 85.5, by = 0.5)
	lonGrid <- c(seq(-44, 68, by = 1), 68.5)
	# Locate the positions in the grid:
	latInd <- findInterval(lat, latGrid, rightmost.closed = TRUE)
	lonInd <- findInterval(lon, lonGrid, rightmost.closed = TRUE)
	# Store the indices of the positions that fall outside of the grid, and set these to NA in the output:
	atNA <- latInd %in% c(0L, length(latGrid)) | lonInd %in% c(0L, length(lonGrid))
	
	# Set the indices to NA to secure that the latCode and lonCode are not subset by using index 0:
	latInd[atNA] <- NA
	lonInd[atNA] <- NA
	
	# The names of the statistical rectangles is a concatenation of a latitude code and a longitude code. The latitude code is 01, 02, ..., 99:
	latCode <- formatC(latInd, width = 2, format = "d", flag = "0")
	# The longitude code is more complicated. It is A0, A1, A2, A3, B0, B1, ..., B9, C0, C1, ..., C9, ..., H0, H1, ..., H9, (no "I"), J0, J1, ..., J9, ..., M0, M1, ..., M8, where A0 is [-44, -43), ..., A3 is [-41, -40], and M8 is [68, 68.5]
	lonNames  <- c(
		paste0("A", 0:3), #  No  4, ..., 9
		c(t(outer(c("B", "C", "D", "E", "F", "G", "H"), 0:9, paste0))), 
		# No "I"!
		c(t(outer(c("J", "K", "L"), 0:9, paste0))), 
		paste0("M", 0:8)
	)
	lonCode <- lonNames[lonInd]
	
	# Get the final codes and insert NAs where approrpiate:
	latLonCode <- paste0(latCode, lonCode)
	latLonCode[atNA] <- NA
	
	return(latLonCode)
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

## Get ICES ship data
##' @importFrom xml2 xml_ns_strip xml_find_all xml_text
#getICESShipCode <- function(platformname) {
#	
#	construct <- function(shipName) {
#		# We have to remove "."," " and use uppercase
#		shipName <- toupper(gsub("[[:space:][:punct:]]", "", shipName))
#
#		# Replace the nordic character with AA
#		shipName <- gsub("\u00C5", "AA", shipName)
#
#		data <- tryCatch(
#			{
#				read_xml("https://vocab.ices.dk/services/pox/GetCodeList/SHIPC")
#			},
#			error = function(e){return(NA)}
#		)
#
#		# Can't download from ICES
#		if (is.na(data))
#			return(NA)
#
#		xml_ns_strip(data)
#		nodes <- xml_find_all(data, paste0("//Code[contains(translate(Description[normalize-space()],'abcdefghijklmnopqrstuvwxyz. ','ABCDEFGHIJKLMNOPQRSTUVWXYZ'), \"",
#					shipName, "\") and contains(Deprecated, \"false\")]/*[self::Key or self::Modified]"))
#
#		# Ship not found
#		if (length(nodes) < 1) {
#			return(NA)
#		}
#
#		# Get the latest matching ship code
#		xx <- xml_text(nodes)
#		yy <- as.data.frame(list(code = xx[seq(xx) %% 2 == 1], date = xx[seq(xx) %% 2 == 0]), stringsAsFactors = FALSE)
#		shipCode <- head(yy[order(as.Date(yy$date), decreasing = TRUE), "code"], 1)
#
#		return(shipCode)
#	}
#	
#	nm <- unique(platformname)
#	y <- unlist(lapply(nm, construct))
#	names(y) <- nm
#	
#	x <- y[as.character(platformname)]
#	x[is.null(x)] <- NA
#	
#	return(x)
#}



# Function to interpret ICES lengthCode from NMCBiotic lengthresolution. This function uses getLengthResolutionTable() which defines the relationship between these.
getLengthCodeICES <- function(lengthresolution, format = c("ICESBiotic", "ICESDatras")) {
	format <- match.arg(format)
	# Interpret/approximate the lengthresolution as mm, halfcm or cm, which also implies modifying the length if cm (see convertLengthGivenLengthCode()):
	lengthResolutionTable <- getLengthResolutionTable(format = format)
	lengthCode <- lengthResolutionTable$lengthCode[match(lengthresolution, lengthResolutionTable$lengthresolution)]
	nonValid <- !is.na(lengthresolution) & is.na(lengthCode)
	if(any(nonValid)) {
		warning("StoX: The following lengthresolution do not match any of the lengthCode defined by http://vocab.ices.dk/?ref=1486: ", paste(unique(lengthresolution[nonValid]), collapse = ", "), ". The following table shows the valid lengthresolution:\n",  paste(names(lengthResolutionTable), collapse = "\t"), "\n", paste(lengthResolutionTable[, do.call(paste, c(.SD, list(sep = "\t")))], collapse = "\n"))
	}
	return(lengthCode)
}

getLengthResolutionTable <- function(format = c("ICESBiotic", "ICESDatras")) {
	format <- match.arg(format)
	# The length resolutions defined by NMDReference as per 2021-04-22:
	lengthResolutionTable <- getRstoxDataDefinitions("lengthResolutionTable")
	lengthCode_unit_table <- getRstoxDataDefinitions("lengthCode_unit_table")

	lengthResolutionTable[, lengthCode := lengthCode_unit_table[[paste0("lengthCode", format)]][match(shortname, lengthCode_unit_table$shortname)]]
	
	return(lengthResolutionTable)
}

scaleLengthUsingLengthCode <- function(length, lengthCode, inputUnit, format = c("ICESBiotic", "ICESDatras")) {
	
	format <- match.arg(format)
	
	lengthCode_unit_table <- getRstoxDataDefinitions("lengthCode_unit_table")
	
	outputUnit <- lengthCode_unit_table[match(lengthCode, get(paste0("lengthCode", format))), reportingUnit]
	
	outputLength <- scaleUsingUnit(length, inputUnit = inputUnit, outputUnit = outputUnit)
	
	return(outputLength)
}

# Use the units package to scale 
scaleUsingUnit <- function(x, inputUnit, outputUnit) {
	len <- max(length(x), length(inputUnit), length(outputUnit))
	if(! length(x) %in% c(1, len) || ! length(inputUnit) %in% c(1, len) || ! length(outputUnit) %in% c(1, len)) {
		stop("The inputs must have the same length or length 1")
	}
	if(length(x) != len) {
		x <- rep(x, length.out = len)
	}
	if(length(inputUnit) != len) {
		inputUnit <- rep(inputUnit, length.out = len)
	}
	if(length(outputUnit) != len) {
		outputUnit <- rep(outputUnit, length.out = len)
	}
	
	# Scale only those with unit:
	atNAUnit <- is.na(inputUnit) | is.na(outputUnit)
	scaled <- x[!atNAUnit]
	
	# Use mixed units here as there may be fish with different units, and then update the units with set_units():
	scaled <- units::mixed_units(scaled, inputUnit[!atNAUnit])
	scaled <- units::set_units(scaled, outputUnit[!atNAUnit])
	scaled <- units::drop_units(scaled)
	
	# Insert the scaled:
	x[!atNAUnit] <- scaled
	
	return(x)
}



