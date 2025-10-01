#' Read fisheries XML data format file
#'
#' Read fisheries XML data format file. Currently supports IMR Biotic version 1 until 3, IMR Echosounder version 1, and IMR Landing version 2 formats at the moment.
#' Streaming XML pull parser can be used to avoid loading the whole XML into memory and it supports ZIP file reading. Please note that
#' the XML file inside the zip file should be using the same name as the zip file itself (e.g. test.xml inside test.zip). 
#'
#' @param xmlFilePath full path to the XML file to be read.
#' @param stream a streaming XML pull parser is used if this is set to TRUE. An XML DOM parser is used if this is set to FALSE. Default to TRUE.
#' @param useXsd Specify an xsd object to use. Default to NULL, in which case auto-detection is attempted. Valid options are the schemas listed in names(RstoxData::xsdObjects), but without the suffix ".xsd".
#' @param usePrefix Manually specify a namespace prefix. Default to NULL.
#' @param verbose Show verbose output. Default to FALSE.
#'
#' @return List of data.table objects containing the "flattened" XML data.
#'
#' @examples
#' \dontrun{
#' # Reading test.xml using XML pull parser
#' one <- readXmlFile("./test.xml")
#' # Reading test.xml using XML DOM parser
#' two <- readXmlFile("./test.xml", stream = FALSE)
#' # Reading test.xml inside test.zip file
#' three <- readXmlFile("./test.zip")
#' }
#'
#' @importFrom data.table as.data.table transpose data.table := .SD
#' @importFrom utils data
#' @importFrom xml2 read_xml
#'
#' @export
readXmlFile <- function(xmlFilePath, stream = TRUE, useXsd = NULL, usePrefix = NULL, verbose = FALSE) {

	# Are we reading a zip?
	zipped <- tolower(tools::file_ext(xmlFilePath)) == "zip"

	if (zipped & !stream){
		stop("Zip files can only be read in streaming mode")
	}
  
	# Accept only file extension "xml", "zip" (or "txt" for historic reasons):
	if(! tolower(tools::file_ext(xmlFilePath)) %in% c("xml", "zip", "txt")) {
		stop("File ", xmlFilePath, " does not have file extension xml or zip.")
	}
	
	# Expand path
	xmlFilePath <- path.expand(xmlFilePath)
	
	# Check file exists
	if(!file.exists(xmlFilePath) || isTRUE(file.info(xmlFilePath)$isdir)) {
		stop("File ", xmlFilePath, " does not exist.")
	}
	
	if(!exists("xsdObjects")) {
		data(xsdObjects, package="RstoxData", envir = environment())
	}
	
	
	if (!is.null(useXsd)){
	  supportedXsds <- gsub(".xsd", "", names(xsdObjects))
	  if (!(useXsd %in% supportedXsds)){
	    stop("useXsd=", useXsd, " is not supported. Supported values: ", paste(supportedXsds, collapse=","))
	  }
	}
	
	# Check that the zip contains a properly named file:
	checkFileNameInZip(xmlFilePath)
	
	# Try to do autodetect anyway
	found <- autodetectXml(xmlFilePath, xsdObjects, verbose)
	
	if(is.null(useXsd))
		useXsd <- found[["xsd"]]
        if(is.null(usePrefix))
                usePrefix <- found[["nsPrefix"]]
	
	if (!is.null(useXsd)){
	  if (useXsd != found["xsd"] & stream){
	    warning(paste("useXsd=", useXsd, " is different from schema for detected namespace (",found["xsd"],")."))
	  }
	  if (useXsd != found["xsd"] & !stream){
	    #Don't know exactly why this fails for DOM parsing.
	    stop(paste("useXsd=", useXsd, " is different from schema for detected namespace (",found["xsd"],"). If the namespaces are compatible: Use stream=T to read in data anyway."))
	  }
	}
	
	## See if we have a namespace prefix set
	#if(!is.null(usePrefix)) {
	#	warning(paste("File", basename(xmlFilePath), "contains namespace prefix(es). Will try to remove them before reading."))
	#	stripXslt <- system.file("extdata/stripns.xsl", package = "RstoxData")
	#	stripped <- xml2::xml_xslt(read_xml(xmlFilePath), read_xml(stripXslt))
	#	tempXml <- tempfile(fileext=".xml")
	#	fileConn <- file(tempXml)
	#	writeLines(as.character(stripped), fileConn)
	#	close(fileConn)
	#	xmlFilePath <- tempXml
	#}
	
	# Apply preprocess for ICES XSDs. This must happen on the xsdObjects, and not e.g. on a copy, as the zip reading fails in that case:
	if(!is.null(useXsd) && useXsd == "icesAcoustic") {
		xsdObjects$icesAcoustic.xsd <- icesAcousticPreprocess(xsdObjects$icesAcoustic.xsd)
	} else if(!is.null(useXsd) && useXsd == "icesBiotic") {
		xsdObjects$icesBiotic.xsd <- icesBioticPreprocess(xsdObjects$icesBiotic.xsd)
	}
	
	# Invoke C++ xml reading
	# convert path to native to ensure correct handling (typically on windows)
	#
	xmlFilePathNative <- enc2native(xmlFilePath)
	if(stream) {
		res <- readXmlCppStream(xmlFilePathNative, xsdObjects, useXsd, found[["encoding"]], verbose, nativeIsUTF8=l10n_info()[["UTF-8"]])
		
	} else {
		res <- readXmlCpp(xmlFilePathNative, xsdObjects, useXsd, found[["encoding"]], verbose, nativeIsUTF8=l10n_info()[["UTF-8"]])
	}
	

	result <- res[["result"]]
	xsdName <- res[["xsd"]]

	# Fix encoding on the result list names
	xx <- names(result)
	Encoding(xx) <- "UTF-8"
	names(result) <- xx

	# Convert to data.table and set the names of the columns:
	final <- lapply(names(result), setNames_OneTable, result, xsdObjects[[xsdName]])
	names(final) <- names(result)
	
	# Set the class of the columns by the format definition (xsd):
	lapply(names(final), setClass_OneTable, final, xsdObjects[[xsdName]])
	

	# Add metadata
	final[["metadata"]] <- data.table(useXsd = useXsd, file = xmlFilePath)

	# For ICES data, add their vocabulary
	if(useXsd == "icesAcoustic" || useXsd == "icesBiotic")
		final[["vocabulary"]] <- getIcesVocabulary(xmlFilePath)

	return(final)
}

# Ices Acoustic XSD needs several additional treatments
icesAcousticPreprocess <- function(xsdObject) {
	
	AC <- xsdObject
	
	# We only interested in these tables
	allData <- AC$tableOrder
	newAC <- lapply(AC, function(x) x[allData])
	
	# Set again the root
	newAC$root <- "Acoustic"
	
	# Re-build prefix data
	newAC$prefixLens[allData] <- 0
	
	allDatawithPrefix <- c("Instrument", "Calibration", "DataAcquisition", "DataProcessing", "Cruise", "Survey", "Log", "Sample", "Data")
	
	newAC$prefixLens[allDatawithPrefix] <- 1
	newAC$prefixLens["Log"] <- 2
	newAC$prefixLens["Sample"] <- 4
	newAC$prefixLens["Data"] <- 5
	
	newAC$tableHeaders$Log <- c("LocalID", newAC$tableHeaders$Log)
	newAC$tableTypes$Log <- c("xsd:string", newAC$tableTypes$Log)
	
	# We need here to add Instrument as the first header, since it must serve as a key:
	newAC$tableHeaders$Sample <- c("LocalID", "Distance", "Instrument", newAC$tableHeaders$Sample)
	newAC$tableTypes$Sample <- c("xsd:string", "xsd:float", "xsd:string", newAC$tableTypes$Sample)
	# Remove the duplicated Instrument:
	atDup <- duplicated(newAC$tableHeaders$Sample)
	newAC$tableHeaders$Sample <- newAC$tableHeaders$Sample[!atDup]
	newAC$tableTypes$Sample <- newAC$tableTypes$Sample[!atDup]
	
	newAC$tableHeaders$Data <- c("LocalID", "Distance", "Instrument", "ChannelDepthUpper", newAC$tableHeaders$Data)
	newAC$tableTypes$Data <- c("xsd:string", "xsd:float", "xsd:string", "xsd:float", newAC$tableTypes$Data)
	
	
	# Modify cruise structure to get LocalID as prefix (the types order are the same, as they are all type of string)
	newAC$tableHeaders$Cruise <- c("LocalID", "Country", "Platform", "StartDate", "EndDate", "Organisation")
	
	# Put back table order
	newAC$tableOrder <- allData
	
	return(newAC)
}

# Ices Biotic XSD needs several additional treatments
icesBioticPreprocess <- function(xsdObject) {
	
	AC <- xsdObject
	
	# We only interested in these tables
	allData <- AC$tableOrder
	newAC <- lapply(AC, function(x) x[allData])
	
	# Set again the root
	newAC$root <- "Biotic"
	
	# Re-build prefix data
	newAC$prefixLens[allData] <- 0
	
	allDatawithPrefix <- c("Cruise", "Survey", "Haul", "Catch", "Biology")
	
	newAC$prefixLens[allDatawithPrefix] <- 1
	newAC$prefixLens["Haul"] <- 3
	newAC$prefixLens["Catch"] <- 5
	newAC$prefixLens["Biology"] <- 6
	
	newAC$tableHeaders$Haul <- c("LocalID", newAC$tableHeaders$Haul)
	newAC$tableTypes$Haul <- c("xsd:string", newAC$tableTypes$Haul)
	
	newAC$tableHeaders$Catch <- c("LocalID", "Gear", "Number", "SpeciesCode", "SpeciesCategory", "DataType", "SpeciesValidity", tail(newAC$tableHeaders$Catch, length(newAC$tableHeaders$Catch) - 4))
	newAC$tableTypes$Catch <- c("xsd:string", "xsd:string", "xsd:int", "xsd:string", "xsd:int", "xsd:string", "xsd:string", tail(newAC$tableTypes$Catch, length(newAC$tableTypes$Catch) - 4))
	
	newAC$tableHeaders$Biology <- c("LocalID", "Gear", "Number", "SpeciesCode", "SpeciesCategory", newAC$tableHeaders$Biology)
	newAC$tableTypes$Biology <- c("xsd:string", "xsd:string", "xsd:int", "xsd:string", "xsd:int", newAC$tableTypes$Biology)
	
	# Modify cruise structure to get LocalID as prefix (the types order are the same, as they are all type of string)
	newAC$tableHeaders$Cruise <- c("LocalID", "Country", "Platform", "StartDate", "EndDate", "Organisation")
	
	# Put back table order
	newAC$tableOrder <- allData
	
	return(newAC)
}











# To UTf-8
toUTF8 <- function(srcvec) {
	Encoding(srcvec) <- "UTF-8"
	return(srcvec)
}

