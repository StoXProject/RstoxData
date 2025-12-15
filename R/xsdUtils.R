processXSD <- function(doc, path = NULL) {

	getNameType <- function(x) {
		y <- xml2::xml_attrs(x)
		return(c(y[["name"]], y[["type"]]))
	}

	getNameTypeExt <- function(x, base) {
		y <- xml2::xml_attrs(x)
		return(c(base, y[["base"]]))
	}

	getRecNameType <- function(x, rootName = "", recEnv) {
		# Extract name
		sName <- x[2]

		# Get rootname
		if(rootName == "")
			rootName <- x[1]

		# Clean sName
		sName <- gsub(paste0(rootName, ":"), "", sName)

		# Extract elements
		y <- xml2::xml_find_all(doc, paste0("//", defNS, "complexType[@name=\"", sName, "\"]//", defNS, "element"))

		# This is needed for echosounder v1 SA records
		extension <- xml2::xml_find_all(doc, paste0("//", defNS, "complexType[@name=\"", sName, "\"]//", defNS, "extension"))

		# If no children and "KeyType" (This might be specific to NMDBioticv3) or "*IDREFType*" (specific to ICES XSDs)
		if(length(y) > 0 && (!grepl("KeyType", sName) || !grepl("IDREFType", sName))) {

			if(grepl("IDREFType", sName)) message("IDREFType\n")

			z <- lapply(lapply(y, getNameType), getRecNameType, rootName, recEnv)

			# ICES data have extension and children
			if(length(extension) > 0) {
				ext <- lapply(lapply(extension, getNameTypeExt, x[1]), getRecNameType, rootName, recEnv)
				z <- c(z, ext[[1]]$members)
			}

			# Prepare flat
			recEnv$flat[[x[1]]] <- z
			recEnv$flatAttr[[x[1]]] <- sapply(xml2::xml_find_all(doc, paste0("//", defNS, "complexType[@name=\"", sName, "\"]//", defNS, "attribute/@name")), function(xx) xml2::xml_text(xx))

			# Remove nested elements
			recEnv$flat[[x[1]]] <- lapply(recEnv$flat[[x[1]]], function(xx){ if(is.list(xx)) return(xx[[1]][1]) else return(xx) })
			return(list(x, members=z))
		# Below is specific for Echosounder v1's SA records (with XSD extension) and NMDBioticv1.x ("StringDescriptionType")
		} else if (length(extension) > 0 && !grepl("StringDescriptionType", sName))  {
			z <- lapply(extension, getNameTypeExt, x[1])

			# Prepare flat
			recEnv$flat[[x[1]]] <- z
			recEnv$flatAttr[[x[1]]] <- sapply(xml2::xml_find_all(doc, paste0("//", defNS, "complexType[@name=\"", sName, "\"]//", defNS, "attribute/@name")), function(xx) xml2::xml_text(xx))

			# Remove nested elements
			recEnv$flat[[x[1]]] <- lapply(recEnv$flat[[x[1]]], function(xx){ if(is.list(xx)) return(xx[[1]][1]) else return(xx) })
			return(list(x, members=z))
		} else {
			return(x)
		}
	}

	# Get the default namespace
	defNS <- names(xml2::xml_ns(doc))[[grep("XMLSchema", as.list(xml2::xml_ns(doc)))]]
  
	if(length(defNS) > 0)
		defNS <- paste0(defNS[[1]], ":")
	else
		defNS <- ""

	# See if we need to include more file(s) (include schemaLocation)
	extraXSD <- xml2::xml_find_all(doc, paste0("//", defNS, "include"))
	if(length(extraXSD) > 0) {
		message("We have extra XSDs to be included!\n")
		exFiles <- xml2::xml_attr(extraXSD, "schemaLocation")
		exObj <- lapply(paste0(path, "/include/", exFiles), xml2::read_xml)
		lapply(exObj, function(x) lapply(xml2::xml_children(x), function(y) xml2::xml_add_child(doc, y)))
	}

	rootInfo <- getNameType(xml2::xml_find_all(doc, paste0("/", defNS, "schema/", defNS, "element"))[[1]])
  
	r_e <- new.env()
	r_e$flat <- list()
	r_e$flatAttr <- list()

	# start the recursive search
	getRecNameType(rootInfo, recEnv = r_e)
	
	return(list(flat = r_e$flat, flatAttr = r_e$flatAttr, rootInfo = rootInfo, doc = doc))

}

processMetadata <- function(flat, flatAttr, rootInfo, xsdFile, xsdDoc) {

	# Recursively try to find the column types
	traceType <- function(xx) {
		if(grepl("xs[:]|xsd[:]", xx))
			return(xx)
		else {
			# Try to search the root "type"
			findstring <- paste0('//*[@name="', xx, '"]')
			el <- xml2::xml_find_all(xsdDoc, findstring)
			elu <- unique(xml2::xml_attr(el, "type"))
			res <- unlist(lapply(elu, traceType))

			# Now try to search the root "base"
			if(is.null(res)) {
				findstring <- paste0('//*[@name="', xx, '"]//*[@base]')
				el <- xml2::xml_find_all(xsdDoc, findstring)
				elu <- unique(xml2::xml_attr(el, "base"))
				res <- unlist(lapply(elu, traceType))
			}
			return(res)
		}
	}

	# Process headers and dimensions
	getAttrTable <- function(root, headAttr = c(), xpath="", recEnv) {

		rootStart <- root[1]

		state <- flat[[rootStart]]
		attrs <- flatAttr[[rootStart]]

		# Combine with attributs
		tableElements <- c(headAttr, attrs)
		tableTypes <- rep("attr", length(tableElements))

		# List prefix for next children	
		prefix <- c(headAttr, attrs)

		# Get length of header
		headLen <- length(headAttr)

		# Get number of elements
		tempXpath <- paste(xpath, rootStart, sep='/')
		elemNumXpath <- paste0("count(", tempXpath, ")")
		xpath <- tempXpath

		for(s in 1:length(state)) {
			if(length(state[[s]]) > 1) {
				tableElements <- c(tableElements, state[[s]][1])
				tableTypes <- c(tableTypes, state[[s]][2])
			} else {
				getAttrTable(state[[s]], prefix, xpath, recEnv = recEnv)
			}
		}

		recEnv$tableHeaders[[rootStart]] <- unlist(tableElements)
		recEnv$tablePrefix[[rootStart]] <- unlist(prefix)
		recEnv$tableTypes[[rootStart]] <- unlist(tableTypes)
		recEnv$levelDims[[rootStart]] <- elemNumXpath
	}

	# Meta data before go to C++
	r_e <- new.env()
	r_e$tableHeaders <- list()
	r_e$tablePrefix <- list()
	r_e$levelDims <- list()

	# For element types
	r_e$tableTypes <- list()

	# Defining root
	root <- rootInfo[1]

	# Start recursive
	getAttrTable(rootInfo, recEnv = r_e)

	# Fill in missing values
	missingSets <- setdiff(names(flatAttr), names(r_e$tableHeaders))
	lapply(missingSets, function (x) {
			r_e$tablePrefix[[x]] <- character(0)
			r_e$tableHeaders[[x]] <- character(0)
	})
	r_e$tablePrefix[[root]] <- character(0)

	# Function to get information about children nodes
	getChildren <- function(root, flat) {

		x <- flat[[root]]

		children <- c()
		for(it in 1:length(x))
			if(length(x[[it]]) == 1)
				children <- c(children, x[[it]])
		return (children)
	} 

	# Get tree information
	treeStruct <- lapply(names(flat), getChildren, flat)
	names(treeStruct) <- names(flat)

	# Get prefix length information
	prefixLens <- lapply(r_e$tablePrefix, function(x) length(x))
	names(prefixLens) <- names(r_e$tablePrefix)

	# Unlist couple of metadata
	levelDims <- unlist(r_e$levelDims)
	prefixLens <- unlist(prefixLens)

	# Process types that are still unresolved
	for(nm in names(r_e$tableHeaders)) {
		hd <- r_e$tableHeaders[[nm]]
		tp <- r_e$tableTypes[[nm]]
		if(length(hd)) {
			for(it in 1:length(hd)){
				bla <- NULL
				if(!grepl("xs[:]|xsd[:]", tp[it])) {
					# Exclude obvious types
					if (tp[it] == "IDREF" || tp[it] == "IDREFType" || tp[it] == "ID") {
						bla <- "xs:string"
					} 
					# Position in float:
					else if(tp[it] %in% c("longitude", "latitude", "nonZeroFloat")) {
						bla <- "xsd:float"
					}
					else {
						# Try the recursive search
						bla <- traceType(hd[it])
					}
					
					if(is.null(bla[1]) || is.na(bla[1])) {
						stop("Error in determining types!")
					}
					r_e$tableTypes[[nm]][it] <- bla[1]
				}
			}
		}
	}

	xsdObject <- list() 
	xsdObject[["root"]] <- root
	xsdObject[["treeStruct"]] <- treeStruct;
	xsdObject[["tableTypes"]] <- r_e$tableTypes;
	xsdObject[["tableHeaders"]] <- r_e$tableHeaders;
	xsdObject[["prefixLens"]] <- prefixLens;
	xsdObject[["levelDims"]] <- levelDims;
	if ("targetNamespace" %in% names(xml2::xml_attrs(xsdDoc))){
	  xsdObject[["targetNamespace"]] <- xml2::xml_attrs(xsdDoc)[["targetNamespace"]]	  
	}
	else{
	  xsdObject[["targetNamespace"]] <- NA
	}

	
	return(xsdObject)
}

#' @importFrom xml2 xml_attrs read_xml xml_find_all xml_find_num xml_ns xml_text xml_add_child xml_attr xml_children
#' @importFrom utils tail
createXsdObject <- function(xsdFile) {
	
	# Check if XSD exists
	message(paste("Using:", xsdFile))

	# If not exists
	if(!file.exists(xsdFile)) {
		# Get path from local environment
		#fpath <- get("fpath", envir = localEnv)
		fpath <- getRstoxDataDefinitions("fpath")
		xsdFilePath <- paste0(fpath, "/", basename(xsdFile))
		if(!file.exists(xsdFilePath)) {
			message(paste("It seems that", xsdFile, "does not exist or the format is not supported."))
			return(NULL)
		}
	} else {
		xsdFilePath <- xsdFile
	}

	# Parse XSD
	xsdObj <- xml2::read_xml(xsdFilePath)

	# Get metadata based on XSD
	metaData <- processXSD(xsdObj, dirname(xsdFile))

	# Process XML file
	ret <- processMetadata(metaData$flat, metaData$flatAttr, metaData$rootInfo, xsdFile, metaData$doc)

	return(ret)
}


#' @importFrom xml2 xml_child read_html xml_find_all
autodetectXml <- function(xmlFile, xsdObjects, verbose) {

	# Read first 500 characters
	tmpText <- tryCatch(
		{
			suppressWarnings(readCharZip(xmlFile, 500))
		}, error=function(cond) {
			return(NULL)
		})
	
	if(is.na(tmpText) || is.null(tmpText)) {
			return(NULL)
        }

	bits <- read_html(tmpText)

	# Detect namespace prefix
	pfx <- unlist(regmatches(tmpText, regexec("xmlns:(\\w+)=", tmpText)))[2]
	
	if(is.na(pfx) || pfx %in% c("xsd", "xsi")) {
		pfx <- NULL
		prefix1 <- ""
		prefix2 <- ""
	} else {
		prefix1 <- paste0(":", pfx)
		prefix2 <- paste0(pfx, ":")
	}

	# Getting encoding
	tmpG1 <- regexpr('encoding="\\K[^"]*', tmpText, perl=T)
	if(tmpG1 > -1) {
		tmpG2 <- tmpG1 + attr(tmpG1, "match.length") - 1
		xmlEnc <- substr(tmpText, tmpG1, tmpG2)
	} else {
		xmlEnc <- NULL
	}

	# Getting XSD information
	# Peek xmlns
	if(verbose)
		message("Try to use XML namespace")

	tmpG1 <- regexpr(paste0('xmlns', prefix1, '="\\K[^"]*'), tmpText, perl = TRUE)
	if(tmpG1 > -1) {
		tmpG2 <- tmpG1 + attr(tmpG1, "match.length") - 1
		xmlXsd <- substr(tmpText, tmpG1, tmpG2)
		xmlXsd <- paste0(tail(unlist(strsplit(xmlXsd, "/")), 2), collapse = "")
	} else {
		xmlXsd <- NULL
	}

	if(paste0(xmlXsd, ".xsd") %in% names(xsdObjects))
		return(list(xsd = xmlXsd, encoding = xmlEnc, nsPrefix = pfx))

	if(verbose)
		message("Do manual detection")

	# Do manual detection
	# Later We need to distinguish Biotic v3&v3.1, Biotic v1.4&earlier
	if( length(xml2::xml_find_all(bits, paste0("//", prefix2, "mission[@startyear]"))) )
		xmlXsd <- "nmdbioticv3"
	else if( length(xml2::xml_find_all(bits, paste0("//", prefix2, "mission[@year]"))) )
		xmlXsd <- "nmdbioticv1.4"
	else if( length(xml2::xml_find_all(bits, paste0("//", prefix2, "biotic"))) )
		xmlXsd <- "icesBiotic"
	else if( length(xml2::xml_find_all(bits, paste0("//", prefix2, "echosounder_dataset"))) )
		xmlXsd <- "nmdechosounderv1"
	else if( length(xml2::xml_find_all(bits, paste0("//", prefix2, "acoustic"))) )
		xmlXsd <- "icesAcoustic"
	else if( length(xml2::xml_find_all(bits, paste0("//", prefix2, "Seddellinje"))) )
		xmlXsd <- "landingerv2"
	else
		xmlXsd <- NULL

	return(list(xsd = xmlXsd, encoding = xmlEnc, nsPrefix = pfx))
}

readCharZip <- function(x, ...) {
	
	if(tolower(tools::file_ext(x)) == "zip") {
		file <- utils::unzip(zipfile = x, list = TRUE)[, "Name"]
		file <- file[!grepl("__MACOSX", file)]
		if(length(file) > 1) {
			stop("Input data can be zipped, but then each file must be zipped individually, so that each zipfile contains only one file (and this file must have the same name as the zip, excluding file extension).")
		}
		output <- readChar(unz(x, file), ...)
	}
	else {
		output <- readChar(x, ...)
	}
	# Assume that the input file is UTF-8. We need to set the encoding here to make sure that substr repects characters not bytes (supporting norwegian characters):
	Encoding(output) <- "UTF-8"	
	
	# Make sure the output is a complete xml (ending with ">"):
	last <- utils::tail(gregexpr(">", output)[[1]], 1)
	if(is.finite(last)) {
		output <- substr(output, 1, last)
	}
	
	return(output)
}

checkFileNameInZip <- function(x) {
	if(tolower(tools::file_ext(x)) == "zip") {
		file <- utils::unzip(zipfile = x, list = TRUE)[, "Name"]
		# Check whether a file named by the basename of the zip exists (sans ext):
		if(!basename(tools::file_path_sans_ext(x)) %in% basename(tools::file_path_sans_ext(file))) {
			stop("Zipped input data must contain the a file with the same name as the zip, excluding file extension (was.", basename(tools::file_path_sans_ext(file)), ", should be ", basename(tools::file_path_sans_ext(x)), ").")
		}
	}
}


#' @importFrom data.table rbindlist setnames
#' @importFrom xml2 as_list read_xml xml_find_all
getIcesVocabulary <- function(xmlFile) {

	# Read only first 10e4 character
	tmpText <- readChar(xmlFile, 10e4)
	xmlObj <- read_html(tmpText)

	# Apply transformation to get the vocabulary translation table
	ret <- rbindlist(lapply(as_list(xml2::xml_find_all(xmlObj, "//vocabulary/*/code")), function(x) if(length(x)>0) return(list(attr(x, "id"), ifelse(length(x) > 0, unlist(x), NA), attr(x, "codetype")))))
	setnames(ret, c("id", "value", "codetype"))
	return(ret)
}


# Ices Acoustic XSD needs several additional treatments
icesAcousticPreprocess <- function(xsdObject) {
	
	# In elements which are lists holding one element per table, extract only the tables defined hard coded in processBioticData.R, which are found in the tableOrder element:
	tableOrder <- xsdObject$tableOrder
	xsdObject <- lapply(xsdObject, function(x) if(is.list(x) && all(tableOrder %in% names(x))) x[tableOrder] else x)
	
	# Set again the root
	xsdObject$root <- "Acoustic"
	
	# Re-build prefix data
	xsdObject$prefixLens[tableOrder] <- 0
	
	allDatawithPrefix <- c("Instrument", "Calibration", "DataAcquisition", "DataProcessing", "Cruise", "Survey", "Log", "Sample", "Data")
	
	xsdObject$prefixLens[allDatawithPrefix] <- 1
	xsdObject$prefixLens["Log"] <- 2
	xsdObject$prefixLens["Sample"] <- 4
	xsdObject$prefixLens["Data"] <- 5
	
	xsdObject$tableHeaders$Survey <- c("LocalID", xsdObject$tableHeaders$Survey)
	xsdObject$tableTypes$Survey <- c("xsd:string", xsdObject$tableTypes$Survey)
	
	xsdObject$tableHeaders$Log <- c("LocalID", xsdObject$tableHeaders$Log)
	xsdObject$tableTypes$Log <- c("xsd:string", xsdObject$tableTypes$Log)
	
	# We need here to add Instrument as the first header, since it must serve as a key:
	xsdObject$tableHeaders$Sample <- c("LocalID", "Distance", "Instrument", xsdObject$tableHeaders$Sample)
	xsdObject$tableTypes$Sample <- c("xsd:string", "xsd:float", "xsd:string", xsdObject$tableTypes$Sample)
	# Remove the duplicated Instrument:
	atDup <- duplicated(xsdObject$tableHeaders$Sample)
	xsdObject$tableHeaders$Sample <- xsdObject$tableHeaders$Sample[!atDup]
	xsdObject$tableTypes$Sample <- xsdObject$tableTypes$Sample[!atDup]
	
	xsdObject$tableHeaders$Data <- c("LocalID", "Distance", "Instrument", "ChannelDepthUpper", xsdObject$tableHeaders$Data)
	xsdObject$tableTypes$Data <- c("xsd:string", "xsd:float", "xsd:string", "xsd:float", xsdObject$tableTypes$Data)
	
	
	# Modify cruise structure to get LocalID as prefix (the types order are the same, as they are all type of string)
	xsdObject$tableHeaders$Cruise <- c("LocalID", "Country", "Platform", "StartDate", "EndDate", "Organisation")
	
	# Put back table order
	xsdObject$tableOrder <- tableOrder
	
	
	# Define Keys manually, since the keys do not necessarily stand first in the tables. These keys are excluding the InstrumentID, CalibrationID, DataAcquisitionID and DataProcessingID:
	xsdObject$keys <- list(
		Cruise = c("LocalID"), 
		Survey = c("LocalID", "Code"), 
		Log = c("LocalID", "Distance"), 
		Sample = c("LocalID", "Distance", "ChannelDepthUpper"), 
		Data = c("LocalID", "Distance", "ChannelDepthUpper", "SaCategory")
	)
	
	
	return(xsdObject)
}

# Ices Biotic XSD needs several additional treatments
icesBioticPreprocess <- function(xsdObject) {
	
	# In elements which are lists holding one element per table, extract only the tables defined hard coded in processBioticData.R, which are found in the tableOrder element:
	tableOrder <- xsdObject$tableOrder
	xsdObject <- lapply(xsdObject, function(x) if(is.list(x) && all(tableOrder %in% names(x))) x[tableOrder] else x)
	
	# Set again the root
	xsdObject$root <- "Biotic"
	
	# Re-build prefix data
	xsdObject$prefixLens[tableOrder] <- 0
	
	allDatawithPrefix <- c("Cruise", "Survey", "Haul", "Catch", "Biology")
	
	# These are hard coded number of keys:
	xsdObject$prefixLens["Cruise"] <- 1
	xsdObject$prefixLens["Survey"] <- 2
	xsdObject$prefixLens["Haul"] <- 3
	xsdObject$prefixLens["Catch"] <- 5
	xsdObject$prefixLens["Biology"] <- 6
	
	xsdObject$tableHeaders$Survey <- c("LocalID", xsdObject$tableHeaders$Survey)
	xsdObject$tableTypes$Survey <- c("xsd:string", xsdObject$tableTypes$Survey)
	
	xsdObject$tableHeaders$Haul <- c("LocalID", xsdObject$tableHeaders$Haul)
	xsdObject$tableTypes$Haul <- c("xsd:string", xsdObject$tableTypes$Haul)
	
	xsdObject$tableHeaders$Catch <- c("LocalID", "Gear", "Number", "SpeciesCode", "SpeciesCategory", "DataType", "SpeciesValidity", tail(xsdObject$tableHeaders$Catch, length(xsdObject$tableHeaders$Catch) - 4))
	xsdObject$tableTypes$Catch <- c("xsd:string", "xsd:string", "xsd:int", "xsd:string", "xsd:int", "xsd:string", "xsd:string", tail(xsdObject$tableTypes$Catch, length(xsdObject$tableTypes$Catch) - 4))
	
	xsdObject$tableHeaders$Biology <- c("LocalID", "Gear", "Number", "SpeciesCode", "SpeciesCategory", xsdObject$tableHeaders$Biology)
	xsdObject$tableTypes$Biology <- c("xsd:string", "xsd:string", "xsd:int", "xsd:string", "xsd:int", xsdObject$tableTypes$Biology)
	
	# Modify cruise structure to get LocalID as prefix (the types order are the same, as they are all type of string)
	xsdObject$tableHeaders$Cruise <- c("LocalID", "Country", "Platform", "StartDate", "EndDate", "Organisation")
	
	# Put back table order
	xsdObject$tableOrder <- tableOrder
	
	
	# Define Keys manually, since the keys do not necessarily stand first in the tables:
	xsdObject$keys <- list(
		Cruise = c("LocalID"), 
		Survey = c("LocalID", "Code"), 
		Haul = c("LocalID", "Gear", "Number"), 
		Catch = c("LocalID", "Gear", "Number", "SpeciesCode", "SpeciesCategory"), 
		Biology = c("LocalID", "Gear", "Number", "SpeciesCode", "SpeciesCategory", "FishID")
	)
	
	
	return(xsdObject)
}



getKeysFromXSD <- function(xsdObject) {
	
	if(missing(xsdObject) && !is.null(data[["metadata"]])) {
		datatype <- unlist(data[["metadata"]][1, "useXsd"])
		xsdObject <- RstoxData::xsdObjects[[paste0(datatype, ".xsd")]]
	}

	if(length(xsdObject)) {
		# Get the number of headers:
		plen <- xsdObject$prefixLens
		thead <- xsdObject$tableHeaders
		
		#keys <- mapply(function(x, p) names(x)[seq_len(p)], data, plen)
		keys <- lapply(names(plen), function(name) thead[[name]][seq_len(plen[[name]])])
		names(keys) <- names(plen)
	}
	else {
		
		keys <- vector("list", length(data))
		# Look for intersecting variables:
		for(ind in seq_len(length(data) - 1)) {
			keys[[ind]] <- intersect(names(data[[ind]]), names(data[[ind + 1]]))
		}
		
		names(keys) <- names(data)
	}
	
	return(keys)
}







