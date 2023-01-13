#' Merge list of data tables recursively
#'
#' @param data A list of data tables.
#' @param tableNames A character vector holding the names of the tables to merge.
#' @param output.only.last Only returns last merged table.
#' @param ... Extra parameters that will be passed into \code{\link[data.table]{merge}}.
#'
#' @return A merged data table.
#'
#' @export
#' 
mergeDataTables <- function(data, tableNames = NULL, output.only.last = FALSE, ...) {
	
	# Better use xsdObjects for getting header vars from XML data for merging
	## Get data type:
	plen <- NULL
	if(!is.null(data[["metadata"]])) {
		datatype <- unlist(data[["metadata"]][1, "useXsd"])
		plen <- RstoxData::xsdObjects[[paste0(datatype, ".xsd")]]$prefixLens
	}

	# Merge all tables by default:
	if(length(tableNames) == 0) {
		tableNames <- names(data)
	}
	# No merging if only one table given in 'tableNames':
	else if(length(tableNames) == 1)  {
		return(data)
	}
	
	# Make sure tableNames are ordered as in the data:
	dataNames <- names(data)
	tableNames <- dataNames[match(tableNames, dataNames)]
	tableNames <- tableNames[!is.na(tableNames)]
	
	# Merge
	for(ii in 2:length(tableNames)) {
		curr <- tableNames[ii]
		prev <- tableNames[(ii-1)]

		if(!is.null(plen) && !is.na(plen[prev]))
			vars <- names(data[[curr]])[1:plen[prev]]
		else
			vars <- intersect(names(data[[curr]]), names(data[[prev]]))

		# There can be duplicate names between two tables, see that we fix them by adding appropriate suffix before merging
		duplicates <- intersect(setdiff(names(data[[prev]]), vars), setdiff(names(data[[curr]]), vars))
		for(ddpl in duplicates) {
			#message(paste("Duplicate columns in merging", prev, "and", curr,  ": ", ddpl, "->", paste0(ddpl, ".", curr)))
			setnames(data[[curr]], ddpl, paste0(ddpl, ".", curr))
		}
		
		data[[curr]] <- merge(data[[prev]], data[[curr]], by=vars, suffixes = suffixes, ...)
	}

	# If tableNamestableNames == "last", return the last table:
	if(output.only.last) {
		data <- data[[utils::tail(tableNames, 1)]]
	}
	
	return(data)
}

#' Merge two data tables by the intersect of the names
#'
#' @param x,y Data tables of class \code{\link[data.table]{data.table}}.
#' @param ... Various overrides.
#' @param msg Verbose message switch, default to \code{FALSE}.
#'
#' @return A merged data table.
#'
#' @export
#' 
mergeByIntersect <- function(x, y, ..., msg = FALSE) {
	# Cascading merge if a list of tables is given:
	if(length(x) > 1  && 
	   is.list(x)  &&  
	   !data.table::is.data.table(x)  && 
	   data.table::is.data.table(x[[1]])) {
		for(ind in seq(2, length(x))) {
			x[[ind]] <- mergeByIntersect(x[[ind - 1]], x[[ind]], ..., msg = msg)
		}
		output <- x[[ind]]
	}
	else {
		by <- intersect(names(x), names(y))
		if(msg) {
			message("Merging by ", paste(by, collapse = ", "))
		}
		if(length(by)) {
			output <- merge(x, y, by = by, ...)
		}
		else {
			stop("No intersect between the names of ", deparse(substitute(x)), " and ", deparse(substitute(y)))
		}
	}
	
	return(output)
}


#' Merge two data tables by StoX keys
#'
#' @param x,y Data tables of class \code{\link[data.table]{data.table}}.
#' @param StoxDataType Input data type. Text string of \code{StoxBiotic}
#' 			or \code{StoxAcoustic}.
#' @param toMergeFromY Specify key columns from \code{y}. \code{NULL} means
#' 			all similarly named columns from \code{x} and \code{y} will be
#' 			merged. Default to \code{NULL}.
#' @param replace Whether to replace the variables in the target.
#' 			Default to \code{FALSE}.
#' @param unique Logical: If TRUE (default) make the tables unique after merging.
#' @param ... Extra parameters that will be passed into \code{\link[data.table]{merge}}.
#'
#' @return A merged data table.
#'
#' @export
#' 
mergeByStoxKeys <- function(x, y, StoxDataType, toMergeFromY = NULL, replace = FALSE, unique = TRUE, ...) {
	# Get the keys:
	#keys_x <- getKeys(x)
	#keys_y <- getKeys(y)
	#keys <- intersect(keys_x, keys_y)
    keys <- Reduce(intersect, 
        list(
            names(x), 
            names(y), 
            getStoxKeys(StoxDataType = StoxDataType)
        )
    )
	
	# Define the columns to merge:
	if(!length(toMergeFromY)) {
		toMergeFromY <- names(y)
	}
	# Make sure the toMergeFromY are present in y:
    toMergeFromY <- intersect(names(y), toMergeFromY)
    # Exclcude the keys:
	toMergeFromY <- setdiff(toMergeFromY, getStoxKeys(StoxDataType = StoxDataType))

	#  Replace the variable in the target:
	if(replace) {
		keep <- setdiff(names(x), toMergeFromY)
		x <- x[, ..keep]
	}
	
	# If there are any left, extract the keys and toMergeFromY:
	if(length(toMergeFromY)) {
		y <- y[, c(keys, toMergeFromY), with = FALSE]
		# Then merge:
		x <- merge(x, y, by = keys, ...)
	}
	else {
		x
	}
	
	if(unique) {
		x <- unique(x)
	}
	
	return(x)
}

#getKeys <- function(x, keystring = "Key", ignore.case = FALSE) {
#	namesx <- names(x)
#	namesx[endsWith(if(ignore.case) tolower(namesx) else namesx, if(ignore.case) tolower(keystring) else keystring#)]
#}

#' Get the keys of a StoX format
#' 
#' @param StoxDataType The name of the StoX format (only StoxBiotic implemented yet).
#' @param level The name of the level/table to get keys for.
#' @param keys.out Specification of what to return. One of "all", to return all keys of the level; "only.present", to return only the key of the \code{level}; and "all.but.present", to return all keys except the present key.
#'
#' @importFrom data.table key
#' @export
#' 
getStoxKeys <- function(StoxDataType = c("StoxBiotic", "StoxAcoustic"), level = NULL, keys.out = c("all", "only.present", "all.but.present")) {
	StoxDataType <- match_arg_informative(StoxDataType)
	if(StoxDataType == "StoxBiotic") {
		if(!exists("stoxBioticObject")) {
			data(stoxBioticObject, package="RstoxData", envir = environment())
		}
		keys <- stoxBioticObject$convertTable[iskey == "Y", c("variable", "level")]
		keys <- split(keys, by = "level")
		keys <- lapply(keys, "[[", "variable")
	}
	else if(StoxDataType == "StoxAcoustic") {
		stop("Not yet implemented")
	}
	
	if(length(level)) {
		keys <- keys[[level]]
	}
	else {
	    keys <- unique(unlist(keys))
	    return(keys)
	}
	
	keys.out <- match_arg_informative(keys.out)
	if(keys.out == "only.present") {
		keys <- utils::tail(keys, 1)
	}
	else if(keys.out == "all.but.present") {
		keys <- keys[-length(keys)]
	}
	
	return(keys)
}




# Detect OS
get_os <- function() {
	if (.Platform$OS.type == "windows") {
		"win"
	} else if (Sys.info()["sysname"] == "Darwin") {
		"mac"
	} else if (.Platform$OS.type == "unix") {
		"unix"
	} else {
		stop("Unknown OS")
	}
}

#' Pick a suitable number of cores
#'
#' @inheritParams lapplyOnCores
#' @param n Optional length of the data to apply parallel processing to.
#'
#' @return The number of cores to apply.
#'
#' @export
#' 
getNumberOfCores <- function(NumberOfCores = NULL, n = NULL) {
	# Detect number of cores if not given:
	if(!length(NumberOfCores)) {
		NumberOfCores <- as.integer(getOption("mc.cores"))
		if (!length(NumberOfCores) || is.na(NumberOfCores)) {
			NumberOfCores <- parallel::detectCores()
			if (is.na(NumberOfCores)) {
				return(1)
			}
		} 
	}
	
	# Do not use more cores than the number of elemens:
	if(length(n)) {
		NumberOfCores <- min(n, NumberOfCores)
	}
	
	return(NumberOfCores)
}

#' Run a function on all elements of x on one or more cores
#'
#' @param x An object to apply \code{FUN} to.
#' @param FUN The function to apply.
#' @inheritParams general_arguments
#' @param ... Additional arguments to \code{FUN}.
#'
#' @return A list of outputs from \code{FUN}.
#'
#' @export
#' 
lapplyOnCores <- function(x, FUN, NumberOfCores = 1L, ...) {
	# Get the number of cores to use:
	NumberOfCores <- getNumberOfCores(NumberOfCores, n = length(x))
	
	# Simple Lapply if onle one core:
	if(NumberOfCores == 1) {
		out <- lapply(x, FUN, ...)
	}
	# Run in parallel on Windows and other platforms:
	else if(NumberOfCores > 1){
		# On Windows run special args to speed up:
		if(get_os() == "win") {
			cl <- parallel::makeCluster(NumberOfCores, rscript_args = c("--no-init-file", "--no-site-file", "--no-environ"))
			parallel::clusterEvalQ(cl, {
				library(RstoxData)
			})
			out <- parallel::parLapply(cl, x, FUN, ...)
			parallel::stopCluster(cl)
		} 
		else {
			out <- parallel::mclapply(x, FUN, mc.cores = NumberOfCores, ...)
		}
	}
	else {
		out <- NULL
	}
	
	return(out)
}


#' Run a function on all elements of x on one or more cores
#'
#' @inheritParams lapplyOnCores
#' @param ...,MoreArgs,SIMPLIFY See \code{\link[base]{mapply}}.
#'
#' @return A list of outputs from \code{FUN}.
#'
#' @export
#' 
mapplyOnCores <- function(FUN, NumberOfCores = 1L, ..., MoreArgs = NULL, SIMPLIFY = FALSE) {
	# Get the number of cores to use:
	NumberOfCores <- getNumberOfCores(NumberOfCores, n = max(lengths(list(...))))
	
	# Simple mapply if only one core:
	if(NumberOfCores == 1) {
		out <- mapply(FUN, ..., MoreArgs = MoreArgs, SIMPLIFY = SIMPLIFY)
	}
	# Run in parallel on Windows and other platforms:
	else if(NumberOfCores > 1){
		# On Windows run special args to speed up:
		if(get_os() == "win") {
			cl <- parallel::makeCluster(NumberOfCores, rscript_args = c("--no-init-file", "--no-site-file", "--no-environ"))
			out <- parallel::clusterMap(cl, FUN, ..., MoreArgs = MoreArgs, SIMPLIFY = SIMPLIFY)
			parallel::stopCluster(cl)
		} 
		else {
			out <- parallel::mcmapply(FUN, mc.cores = NumberOfCores, ..., MoreArgs = MoreArgs, SIMPLIFY = SIMPLIFY)
		}
	}
	else {
		out <- NULL
	}
	
	return(out)
}


#' Round off to number of digits
#'
#' @param x A list of \code{data.table}s or a single \code{data.table} object.
#'
#' @return A transformed object.
#'
#' @export
#' 
setRstoxPrecisionLevel <- function(x) {
	# Get the defines number of digits:
	digits <- getRstoxDataDefinitions("digits")
	signifDigits <- getRstoxDataDefinitions("signifDigits")
	
	# If a data.table run roundSignifDT() directly:
	if(data.table::is.data.table(x)) {
		roundSignifDT(x, digits = digits, signifDigits = signifDigits)
	}
	else if(firstClass(x) == "SpatialPolygonsDataFrame") {
		
	}
	# If a list of data tables, loop through the list and set precision:
	else if(is.list(x)) {
		for(tableName in names(x)) {
			if(data.table::is.data.table(x[[tableName]])) {
				roundSignifDT(x[[tableName]], digits = digits, signifDigits = signifDigits)
			}
			else if(firstClass(x[[tableName]]) == "SpatialPolygonsDataFrame") {
				roundSignifSPDF(x[[tableName]], digits = digits, signifDigits = signifDigits)
			}
		}
	}
}
# Function setting the precision of one data table:
roundSignifDT <- function(DT, digits, signifDigits) {
	if(NROW(DT)) {
		# Detect numeric columns and round off to the specified number of digits:
		atNumeric <- sapply(DT, is.numeric)
		if(any(atNumeric)) {
			numericCols <- names(DT)[atNumeric]
			# DT[, (numericCols) := round(.SD, digits), .SDcols = numericCols]
			#DT[, (numericCols) := roundSignif(.SD, digits = ..digits, signifDigits = ..signifDigits), .SDcols = numericCols]
			for(numericCol in numericCols) {
				DT[, eval(numericCol) := roundSignif(get(numericCol), digits = ..digits, signifDigits = ..signifDigits)]
			}
		}
	}
}

#' Function setting the precision of one data table:
#' 
#' @param SPDF A SpatialPolygonsDataFrame object.
#' @param digits The number of digits to round off to. See also \code{signifDigits}.
#' @param signifDigits The number of significant digits, used when numbers are low to guarantee precision.
#'
#' @export
#' 
roundSignifSPDF <- function(SPDF, digits, signifDigits) {
	if(length(SPDF)) {
		# Detect numeric columns and round off to the specified number of digits:
		numberOfMultiPolygons <- length(SPDF)
		for(multiPolygonIndex in seq_len((numberOfMultiPolygons))) {
			numberOfPolygons <- length(SPDF@polygons[[multiPolygonIndex]]@Polygons)
			for(polygonIndex in seq_len((numberOfPolygons))) {
				SPDF@polygons[[multiPolygonIndex]]@Polygons[[polygonIndex]]@coords <- roundSignifMatrix(SPDF@polygons[[multiPolygonIndex]]@Polygons[[polygonIndex]]@coords, digits = digits, signifDigits = signifDigits)
			
			}
		}
	}
}

roundSignifMatrix <- function(x, digits = 12, signifDigits = NULL) {
	if(length(x)) {
		for(columnIndex in seq_len(ncol(x))) {
			x[, columnIndex] <- roundSignif(x[, columnIndex], digits = digits, signifDigits = signifDigits)
		}
	}
	
	return(x)
}


roundSignif <- function(x, digits = 12, signifDigits = NULL) {
	if(length(signifDigits)) {
		# Use this in the future when units are implemented for all variables in all data type (unitsless being those without unit). Units can be added and remoevd so that we end up with only the units we need, and can display these in the documentation:
		# digits <- pmax(signifDigits - floor(log10(abs(units::drop_units(x)))) - 1, digits)
		digits <- pmax(signifDigits - floor(log10(abs(x))) - 1, digits)
	}
	
	out <- round(x, digits)
	return(out)
}


#' Get the first element returned by class(), and translate "double" to "numeric". 
#'
#' @param x An R object.
#'
#' @export
firstClass <- function(x) {
	out <- class(x)[1]
	if(out == "double") {
		out <- "numeric"
	}
	return(out)
}


## Stolen from https://stackoverflow.com/questions/47190693/count-the-number-of-integer-digits:
#n_int_digits = function(x) {
#	result = floor(log10(abs(x)))
#	result[!is.finite(result)] = 0
#	result
#}


# Function to get the formats of StoX raw data:
getStoxRawDataFormat <- function(x, unlist = FALSE) {
	formats <- lapply(x, function(this) this$metadata$useXsd)
	names(x) <- names(x)
	if(unlist) {
		formats <- unlist(formats)
	}
	return(formats)
}
	
# Check that the formats are unique:
checkUniqueFormat <- function(x) {
	nonUniqueFormats <- getRstoxDataDefinitions("nonUniqueFormats")
	uniqueFormat <- !any(getStoxRawDataFormat(x, unlist = TRUE) %in% inapplicableFormats)
	return(uniqueFormat)
}


## Function to remove rows with duplicated keys in StoxBioticData:
#removeRowsOfDuplicatedKeysFromStoxBioticData <- function(StoxBioticData) {
#	StoxBioticKeys <- getRstoxDataDefinitions("StoxBioticKeys")
#	
#	# Run through the tables of the StoxBioticData and remove duplicate rows:
#	for(tableName in names(StoxBioticData)) {
#		# Get the names of the columns which are keys:
#		presentKeys <- intersect(names(StoxBioticData[[tableName]]), StoxBioticKeys)
#		# Find rows of duplicated keys:
#		duplicatedKeys <- duplicated(StoxBioticData[[tableName]][, ..presentKeys])
#		# Remove the rows with duplicated keys:
#		rowsToKeep <- !duplicatedKeys
#		if(any(duplicatedKeys)) {
#			warning("StoX: Removing ", sum(duplicatedKeys), " rows of duplicated keys.")
#			StoxBioticData[[tableName]] <- StoxBioticData[[tableName]][rowsToKeep, ]
#		}
#	}
#	
#	return(StoxBioticData)
#}


# Function to remove rows with duplicated keys in StoxBioticData:
#' @importFrom data.table .I
removeRowsOfDuplicatedKeys <- function(StoxData, stoxDataFormat = c("Biotic", "Acoustic")) {
	
	stoxDataFormat <- match_arg_informative(stoxDataFormat)
	StoxKeys <- getRstoxDataDefinitions(paste0("Stox", stoxDataFormat, "Keys"))
	
	# Run through the tables of the StoxData and remove duplicate rows:
	for(tableName in names(StoxData)) {
		# Get the names of the columns which are keys:
		presentKeys <- intersect(names(StoxData[[tableName]]), StoxKeys)
		# Find rows of duplicated keys:
		duplicatedKeys <- duplicated(StoxData[[tableName]], by = presentKeys)
		# Remove the rows with duplicated keys:
		if(any(duplicatedKeys)) {
			# Get the rows with equal keys, and indicate this in a copy of the data, and write to a tempfile:
			#allDuplicated <- duplicated(StoxData[[tableName]], by = presentKeys) | duplicated(StoxData[[tableName]], by = presentKeys, fromLast = TRUE)
			#dupData <- data.table::copy(StoxData[[tableName]])
			#dupData[, duplicated := ..allDuplicated]
			#dupData[, rowIndex := .I]
			#fileToWriteDupDataTo <- tempfile()
			#data.table::fwrite(dupData, fileToWriteDupDataTo)
			
			#warning("StoX: Removing ", sum(duplicatedKeys), " rows of duplicated keys from table ", tableName, ". This may be due to different files with the same keys, e.g. if different acoustic instruments are stored in different files. In such a case the order of the files is crucial, as only the information from the first file is kept. If not different files, then duplicated keys may be an error. To see the duplicated rows run the following in R: dat <- data.table::fread(\"", fileToWriteDupDataTo, "\")")
			#warning("StoX: Removing ", sum(duplicatedKeys), " rows of duplicated keys from table ", tableName, ". To see the duplicated rows run the following in R: dat <- data.table::fread(\"", fileToWriteDupDataTo, "\"), which contains the column \"duplicated\"")
			
			#rowsToKeep <- !duplicatedKeys
			StoxData[[tableName]] <- StoxData[[tableName]][!duplicatedKeys, ]
		}
	}
	
	return(StoxData)
}



AddToStoxData <- function(
	StoxData, 
	RawData, 
	VariableNames = character(), 
	#AddToLowestTable = FALSE, 
	SplitTableAllocation = c("Default", "Lowest", "Highest"), 
	NumberOfCores = 1L, 
	StoxDataFormat = c("Biotic", "Acoustic")
) {
	
	if(length(VariableNames) == 0) {
		warning("StoX: No variables specified to extract. Returning data unchcanged")
		return(StoxData)
	}
	
	# Check the the BioticData are all from the same source (ICES/NMD):
	checkDataSource(RawData)
	
	# If from NMDBiotic <= 1.4, skip all non-relevant tables (as e.g. the prey table does not contain the keys necessary to merge with individual, thus resulting in different columns than data from NMDBiotic >= 3):
	invalidTables1.4 <- c("missionlog", "prey", "preylength", "copepodedevstage", "tag")
	
	for(RawDataName in names(RawData)) {
		namesInInvalidTables <- unlist(lapply(RawData[[RawDataName]][invalidTables1.4], names))
		validTables1.4 <- setdiff(names(RawData[[RawDataName]]), invalidTables1.4)
		namesInValidTables <- unlist(lapply(RawData[[RawDataName]][validTables1.4], names))
		invalidRawNames <- setdiff(namesInInvalidTables, namesInValidTables)
		if(RawData[[RawDataName]]$metadata$useXsd %in% c("nmdbioticv1.1", "nmdbioticv1.4") && any(VariableNames %in% invalidRawNames)) {
			warning("StoX: Cannot add variables from the tables ", paste(invalidTables1.4, collapse = ", "), " from NMDBiotic 1.1 and 1.4 due to lack of keys for these tables.")
			RawData[[RawDataName]] <- subset(RawData[[RawDataName]], ! names(RawData[[RawDataName]]) %in% c("missionlog", "prey", "preylength", "copepodedevstage", "tag"))
		}
	}
	
	# Convert from BioticData to the general sampling hierarchy:
	StoxDataFormat <- match_arg_informative(StoxDataFormat)
	if(StoxDataFormat == "Biotic") {
		
		GeneralSamplingHierarchy <- BioticData2GeneralSamplingHierarchy(
			RawData, 
			NumberOfCores = NumberOfCores, 
			#AddToLowestTable = AddToLowestTable
			SplitTableAllocation = SplitTableAllocation
		)
		
		allNames <- unlist(lapply(GeneralSamplingHierarchy, names))
		if(any(VariableNames %in% allNames)) {
			warning("StoX: The following VariableNames are already present in the data, and will be overwritten: ", paste0(intersect(VariableNames, allNames)))
		}
		
		# Define a vector of the variables to extract:
		toExtract <- c(
			getRstoxDataDefinitions("StoxBioticKeys"), 
			VariableNames
		)
	}
	else if(StoxDataFormat == "Acoustic") {
		stop("Not yet implemented")
	}
	else {
		stop("Invalid StoxDataFormat")
	}
	
	# Extract the variables to add:
	toAdd <- lapply(GeneralSamplingHierarchy, function(x) lapply(x, extractVariables, var = toExtract))
	# Rbind for each StoxBiotic table:
	toAdd <- rbindlist_StoxFormat(toAdd)
	# Extract only those tables present in StoxBioticData:
	toAdd <- toAdd[names(StoxData)]
	# Keep only unique rows:
	toAdd <- lapply(toAdd, unique)
	
	
	# add a ccheck for exxisting variables, and remoev these with a warning
	
	# Merge with the present StoxBioticData:
	StoxData <- mapply(mergeAndOverwriteDataTable, StoxData, toAdd, all.x = TRUE, sort = FALSE)
	
	return(StoxData)
}

mergeAndOverwriteDataTable <- function(x, y, ...) {
	# Do not merge if y is empty:
	if(!length(y)) {
		return(x)
	}
	# Overwrite any non-keys already present in the StoxData
	toKeep <- !names(x) %in% names(y) | endsWith(names(x), "Key")
	mergeByIntersect(x[, ..toKeep], y, ...)
}


# Function to extracct variables from a table:
extractVariables <- function(x, var) {
	varToExtract <- intersect(names(x), var)
	if(length(varToExtract)) {
		x[, ..varToExtract]
	}
	else {
		#warning("None of the variables present")
		data.table::data.table()
	}
}

checkDataSource <- function(BioticData) {
	# Function to match the metadata against data source strings:
	matchSource <- function(x, BioticData) {
		matched <- startsWith(sapply(lapply(BioticData, "[[", "metadata"), "[[", "useXsd"), x)
		output <- rep(NA, length(matched))
		output[matched] <- x
		return(output)
	}
	
	# Detect the data source:
	possibleDataSources <- c("nmd", "ices")
	detectedDataSources <- sapply(possibleDataSources, matchSource, BioticData = BioticData, simplify = FALSE)
	numberOfFormats <- sum(sapply(detectedDataSources, function(x) any(!is.na(x))))
	#detectedDataSources <- apply(detectedDataSources, 1, min, na.rm = TRUE)
	# Accept only BioticData from a single source:
	if(numberOfFormats > 1) {
		stop("The function AddToStoxBiotic can only be applied to BioticData where all files read are of the same data source (NMD or ICES)")
	}
	
	return(detectedDataSources)
}







# Find the variables to translate, by looping through the rows of the vocabulary and locating the variables which has any values in the vocabulary$id: 
findVariablesMathcinigVocabulary <- function(vocabulary, data) {
	# Split into individual rows and find the variables that contain the values of each row, and add the variables as a column VariableName, as per the TranslationRequiredColumns:
	vocabularyList <- split(vocabulary, seq_len(nrow(vocabulary)))
	vocabularyList <- lapply(vocabularyList, findVariablesMathcinigVocabularyOne, data = data)
	# Rbind and rename the columns "id" and "value" to "Value" and "NewValue":
	vocabulary <- data.table::rbindlist(vocabularyList)
	data.table::setnames(
		vocabulary, 
		c("id", "value"), 
		c("Value", "NewValue")
	)
	# Clean the table of irrelevant columns:
	vocabulary <- vocabulary[, c("VariableName", "Value", "NewValue")]
	
	# Remove rows with VariableName not found in the data, which has VariableName = NA:
	vocabulary <- subset(vocabulary, !is.na(VariableName))
	
	return(vocabulary)
}

findVariablesMathcinigVocabularyOne <- function(vocabularyOne, data) {
	# Get the names of the columns which has values in vocabularyOne$id:
	#VariableName <- unlist(lapply(data, function(table) names(which(unlist(table[, lapply(.SD, function(x) any(x %in% vocabularyOne$id))])))))
	VariableName <- unlist(lapply(data, function(table) if(NROW(table)) names(which(unlist(table[, lapply(.SD, function(x) any(unique(x) %in% vocabularyOne$id))]))) else NULL))
	# Add the VariableName to the vocabularyOne
	if(!length(VariableName)) {
		# Add NA if no variable was recognized (this avoids warnings when cbind with character(0)
		VariableName <- NA
	}
	vocabularyOne <- cbind(
		VariableName = VariableName, 
		vocabularyOne
	)
	return(vocabularyOne)
}


#' Order a StoX format by keys
#'
#' @param data An object of type \code{\link{StoxAcousticData}} or \code{\link{StoxBioticData}}
#'
#' @export
#' 
orderRowsByKeys <- function(data) {
	lapply(data, setorderv_numeric, key = "Key")
}

#' Order a data.table (by reference) by interpreting characters as numeric if possible
#'
#' @param dataOne A data.table.
#' @param by Order by the given columns.
#' @param key If given and \code{by} is empty, order by the columns with names ending with \code{key}.
#' @param ... Passed on to \code{\link[data.table]{setorderv}}
#'
#' @export
#' 
setorderv_numeric <- function(dataOne, by = NULL, key = NULL, ...) {
	
	# Locate keys:
	if(!length(by)) {
		if(length(key)) {
			by <- names(dataOne)[endsWith(names(dataOne), key)]
		}
		else {
			by <- names(dataOne)
		}
	}
	
	if(length(by)) {
		orderKeys <- paste0(by, "OrderedAfterSplitting")
		
		# Create keys which are converted to ranks, splitting first and then treating individual elements as numbers if possible:
		dataOne[, (orderKeys) := lapply(.SD, createOrderKey), .SDcols = by]
		
		# Order the rows:
		data.table::setorderv(dataOne, orderKeys, ...)
		
		# Remove the orderKeys:
		dataOne[, (orderKeys) := NULL]
	}
	
	return(dataOne)
}


addNAs <- function(x, areNAs) {
	if(any(areNAs)) {
		NAs <- rep(NA, length(areNAs))
		NAs[!areNAs] <- x
		x <- NAs
	}
	return(x)
}

#' Convert a vector to an integer vector where individual string elelents at interpreted as numeric if possible.
#'
#' @param x A vector.
#' @param split A character to split strings by.
#'
#' @export
#' 
createOrderKey <- function(x, split = "/") {
	
	# Split the keys:
	if(!is.character(x)) {
		return(x)
	}
	firstNonNA <- x[1]
	if(is.na(firstNonNA)) {
		if(all(is.na(x))) {
			return(x)
		}
		firstNonNA <- x[min(which(!is.na(x)))]
	}
	# If the first element is coercable to numierc, try converting the entire vector to numeric, and check that no NAs were generated:
	if(!is.na(suppressWarnings(as.numeric(x[1])))) {
		numberOfNAs <- sum(is.na(x))
		xnumeric <- suppressWarnings(as.numeric(x))
		if(sum(is.na(xnumeric)) > numberOfNAs) {
			return(x)
		}
		else {
			return(xnumeric)
		}
	}
	
	# Split by the 'split' argument:
	if(!grepl(split, firstNonNA)) {
		return(x)
	}
	
	# Split the vector by the 'split' parameter:
	splitted <- strsplit(x, split, fixed = TRUE)
	
	# Check that all have the same number of elements, that is the same number of splits:
	if(!all(lengths(splitted) == length(splitted[[1]]))) {
		return(x)
	}
	
	# Create a data.table of the splitted elements and get the order of these:
	splittedDT <- data.table::rbindlist(lapply(splitted, as.list))
	suppressWarnings(splittedDT[, names(splittedDT) := lapply(.SD, as.numeric_IfPossible)])
	
	# Only accept if all elements can be converted to numeric:
	#if(any(is.na(splittedDT))) {
	
	# Acccpet if any of the values are not NA:
	if(all(is.na(splittedDT))) {
		return(x)
	}
	
	# Convert to integer ranks:
	#splittedDT[, names(splittedDT) := lapply(.SD, function(y) match(y, sort(unique(y))))]
	# Replicate data.table's soring which happend in C-locale (see ?data.table::setorderv):
	#splittedDT[, names(splittedDT) := lapply(.SD, function(y) match(y, stringi::stri_sort(unique(y), locale = "C")))]
	#splittedDT[, names(splittedDT) := lapply(.SD, function(y) match(y, stringr::str_sort(unique(y), locale = "C")))]
	
	# This sorting (en_US_POSIX) is the same that data.table uses in setorder/setorderv(), which uses the ICU C locale (https://icu.unicode.org/design/locale/root):
	splittedDT[, names(splittedDT) := lapply(.SD, function(y) match(y, stringi::stri_sort(unique(y), locale = "en_US_POSIX")))]
	
	# Count the maximum number of digits for each column, and multiply by the cummulative number of digits:
	numberOfDigits <- splittedDT[, lapply(.SD, max)]
	numberOfDigits <- nchar(numberOfDigits)
	exponent <- rev(c(0, cumsum(rev(numberOfDigits))[ -length(numberOfDigits)]))
	names(exponent) <- names(splittedDT)
	for(name in names(splittedDT)) {
		splittedDT[, (name) := get(name) * 10^(exponent[[name]])]
	}
	
	orderKey <- rowSums(splittedDT)
	
	
	#orderOfSplitted <- do.call(order, splittedDT)
	## Match with a sequence to create integers used as order key:
	#orderKey <- match(seq_along(x), orderOfSplitted)
	#
	return(orderKey)
}


createOrderKeyNewWithoutError <- function(x, split = "/") {
	
	# Remove NAs and add at the end:
	areNAs <- is.na(x)
	# Use c() here to rid off attributes:
	x <- c(stats::na.omit(x))
	
	# Split the keys:
	if(!is.character(x)) {
		return(addNAs(x, areNAs))
	}
	
	# Find the first non-NA:
	firstNonNA <- x[1]
	if(is.na(firstNonNA)) {
		firstNonNA <- x[min(which(!is.na(x)))]
	}
	
	
	# If the first non-NA element is coercable to numierc, try converting the entire vector to numeric, and check that no NAs were generated:
	if(!is.na(suppressWarnings(as.numeric(firstNonNA)))) {
		numberOfNAs <- sum(is.na(x))
		xnumeric <- suppressWarnings(as.numeric(x))
		# If there are NAs created, it signals that the vector is not coerable to numeric:
		if(sum(is.na(xnumeric)) > numberOfNAs) {
			return(addNAs(x, areNAs))
		}
		else {
			return(addNAs(xnumeric, areNAs))
		}
	}
	
	# Split by the 'split' argument:
	containsSplit <- sapply(split, grepl, firstNonNA)
	split <- split[containsSplit]
	# If the vector does not contain any of the characters to split by, return unchanegd:
	if(!any(containsSplit)) {
		return(addNAs(x, areNAs))
	}
	
	# Split the vector by the 'split' parameter:
	if(length(split) == 1) {
		splitted <- strsplit(x, split, fixed = TRUE)
	}
	else if(length(split) > 1) {
		splitted <- strsplit(x, split[1], fixed = TRUE)
		for(ind in seq(2, length(split))) {
			splitted <- lapply(splitted, function(x) unlist(strsplit(x, split[ind], fixed = TRUE)))
		}
	}
	else {
		stop("'split' must be given")
	}
	
	# Check that all have the same number of elements, that is the same number of splits:
	if(!all(lengths(splitted) == length(splitted[[1]]))) {
		return(addNAs(x, areNAs))
	}
	
	# Create a data.table of the splitted elements and get the order of these:
	splittedDT <- data.table::rbindlist(lapply(splitted, as.list))
	suppressWarnings(splittedDT[, names(splittedDT) := lapply(.SD, as.numeric_IfPossible)])
	
	# Only accept if all elements can be converted to numeric:
	#if(any(is.na(splittedDT))) {
	
	# Acccpet if any of the values are not NA:
	if(all(is.na(splittedDT))) {
		return(addNAs(x, areNAs))
	}
	
	# Convert to integer ranks:
	#splittedDT[, names(splittedDT) := lapply(.SD, function(y) match(y, sort(unique(y))))]
	# Replicate data.table's soring which happend in C-locale (see ?data.table::setorderv):
	#splittedDT[, names(splittedDT) := lapply(.SD, function(y) match(y, stringi::stri_sort(unique(y), locale = "C")))]
	#splittedDT[, names(splittedDT) := lapply(.SD, function(y) match(y, stringr::str_sort(unique(y), locale = "C")))]
	splittedDT[, names(splittedDT) := lapply(.SD, function(y) if(is.numeric(y)) rank(y) else match(y, stringi::stri_sort(unique(y), locale = "en_US_POSIX")))]
	
	# Count the maximum number of digits for each column, and multiply by the cummulative number of digits:
	numberOfDigits <- splittedDT[, lapply(.SD, max)]
	numberOfDigits <- nchar(numberOfDigits)
	exponent <- rev(c(0, cumsum(rev(numberOfDigits))[ -length(numberOfDigits)]))
	names(exponent) <- names(splittedDT)
	for(name in names(splittedDT)) {
		splittedDT[, (name) := get(name) * 10^(exponent[[name]])]
	}
	
	orderKey <- rowSums(splittedDT)
	
	#orderOfSplitted <- do.call(order, splittedDT)
	## Match with a sequence to create integers used as order key:
	#orderKey <- match(seq_along(x), orderOfSplitted)
	#
	
	
	return(addNAs(orderKey, areNAs))
}

as.numeric_IfPossible <- function(x) {
	num <- as.numeric(x)
	if(all(is.na(num))) {
		return(x)
	}
	else {
		return(num)
	}
}

createEmptyDataTable <- function(names, classes = NULL) {
	# Create the named empty data.table:
	DT <- stats::setNames(data.table::data.table(matrix(nrow = 0, ncol = length(names))), names)
	
	# Accept  classes given as a vector of the same length as the names:
	if(length(classes) == length(names) && !is.list(classes)) {
		classes <- structure(as.list(classes), names = names)
	}
	
	setColumnClasses(DT, classes = classes)
	
	return(DT)
}


#' Convert classes of a data.table:
#'
#' @param DT A data.table
#' @param classes A named list of classes. Columns in \code{DT} with name found in \code{classes} are modified if the existing class differs from the desired class.
#'
#' @export
#' 
setColumnClasses <- function(DT, classes = NULL) {
	
	# Create a table of the old and new class:
	classTable <- merge(
		data.table::data.table(
			columnName = names(DT), 
			oldClass = sapply(DT, firstClass)
		), 
		data.table::data.table(
			columnName = names(classes), 
			newClass = unlist(classes)
		), 
		by = "columnName", 
		all = FALSE
	)
	# Change only the columns that differ between old and new class:
	classTable <- subset(classTable, oldClass != newClass)
	
	# Accept  classes given as a vector of the same length as the names:
	for(col in classTable$columnName) {
		data.table::set(DT, j = col, value = do.call(paste("as", classes[[col]], sep = "."), list(DT[[col]])))
	}
	
	return(DT)
}




sanitizeExpression <- function(x) {
	# Detect one or more "system" followed by 0 or one "2" and 0 or more spaces and then one or more parenthesis start:
	usesSystem <- grepl("system+2? *\\(+", x)
	if(any(usesSystem)) {
		stop("The following expression applies a call to the operating system, and may contain harmful code (please do not try to hack using StoX): ", x)
	}
}




#' Informative match.arg
#'
#' @inheritParams base::match.arg
#' @param arg_name The name of the argument, interpreted by default.
#' @param ignore.case Logical: Should case be igngored.
#'
#' @export
#' 
match_arg_informative <- function (arg, choices, several.ok = FALSE, arg_name = substitute(arg), ignore.case = FALSE) {
	
	# Stolen from https://github.com/DarwinAwardWinner/rctutils/blob/master/R/fixes_for_builtins.R:
	
	if (missing(choices)) {
		formal.args <- formals(sys.function(sys.parent()))
		choices <- eval(formal.args[[as.character(substitute(arg))]])
	}
	if (is.name(arg_name)) {
		arg_name <- as.character(arg_name)
	}
	arg_name_string <- deparse_onestring(arg_name)
	if (is.null(arg))
		return(choices[1L])
	else if (!is.character(arg))
		stop(arg_name_string, " must be NULL or a character vector")
	if (!several.ok) {
		if (identical(arg, choices))
			return(arg[1L])
		if (length(arg) > 1L)
			stop(arg_name_string, " must be of length 1")
	}
	else if (length(arg) == 0L)
		stop(arg_name_string, " must be of length >= 1")
	fold_case <- identity
	if (ignore.case) {
		fold_case <- tolower
	}
	i <- pmatch(fold_case(arg), fold_case(choices), nomatch = 0L, duplicates.ok = TRUE)
	if (all(i == 0L))
		stop(gettextf("%s should be one of %s", arg_name_string, paste(dQuote(choices),
																	   collapse = ", ")), domain = NA)
	i <- i[i > 0L]
	
	if (!several.ok && length(i) > 1)
		stop("There is more than one match for ", arg_name_string, " in \"
			 match_arg_informative\"")
	choices[i]
}

deparse_onestring <- function(...) {
	paste(deparse(...), collapse = "\n")
}





#' Function to return the names of the arguments to show for a function:
#'
#' @param functionArgumentHierarchy The function argument hierarchy defined in the stoxFunctionAttributes.
#' @param functionArguments A list of the arguments to the function (both function inputs and function parameters).
#' @param return.only.names Logical: If TRUE return only the names of the arguments to show.
#'
#' @export
#' 
applyFunctionArgumentHierarchy <- function(functionArgumentHierarchy, functionArguments, return.only.names = TRUE) {
	
	# Loop through the arguments given by parent tags in the functionArgumentHierarchy, and set toShow to FALSE if not any of the criterias are fulfilled:
	toShow <- logical(length(functionArguments))
	names(toShow) <- names(functionArguments)
	for(argumentName in names(toShow)) {
		# Check whether the argument is given in the functionArgumentHierarchy. If not, it will be shown:
		atArgumentName <- which(argumentName == names(functionArgumentHierarchy))
		if(length(atArgumentName)) {
			# Loop through the occurrences of the argumentName in the functionArgumentHierarchy, applying &&:
			hitsOr <- logical(length(atArgumentName))
			for(ind in seq_along(atArgumentName)) {
				# Loop through the conditions and set hitsAnd to TRUE if at least one condition is fullfilled:
				conditionNames <- names(functionArgumentHierarchy[[atArgumentName[ind]]])
				hitsAnd <- logical(length(conditionNames))
				names(hitsAnd) <- conditionNames
				for(conditionName in conditionNames) {
					if(is.function(functionArgumentHierarchy[[atArgumentName[ind]]][[conditionName]])) {
						if(isTRUE(functionArgumentHierarchy[[atArgumentName[ind]]][[conditionName]](functionArguments))) {
							hitsAnd[conditionName] <- TRUE
						}
					}
					else {
						# Added requirement that functionArguments[[conditionName]] has positie length:
						if(length(functionArguments[[conditionName]]) && functionArguments[[conditionName]] %in% eval(functionArgumentHierarchy[[atArgumentName[ind]]][[conditionName]])) {
							hitsAnd[conditionName] <- TRUE
						}
					}
				}
				# Apply the AND condition, implying that hitsAnd is TRUE if all are TRUE:
				hitsOr[ind] <- all(hitsAnd)
			}
			toShow[[argumentName]] <- any(hitsOr)
		}
		else {
			toShow[[argumentName]] <- TRUE
		}
	}
	
	# Return only the names of the arguments to show:
	if(return.only.names) {
		toShow <- names(toShow)[toShow]
	}
	
	return(toShow)
}


#' Paste to a linespace + tab separeted string
#'
#' @param errorIDs A vector of strings.
#' @param collapse The separator.
#'
#' @export
#' 
printErrorIDs <- function(errorIDs, collapse = "\n\t") {
	paste0(collapse,  paste0(errorIDs, collapse = collapse))
}


getNAByType <- function(type = c("numeric", "double", "integer", "character")) {
	NA_classes <- list(
		numeric = NA_real_, 
		double = NA_real_, 
		integer = NA_integer_, 
		character = NA_character_
	)
	type <- match_arg_informative(type)
	NA_classes[[type]]
}


#' do.call which ignores non-relevant parameters
#'
#' @inheritParams base::do.call
#' @param keep.unnamed Logical: If TRUE keep all unnamed arguments.
#'
#' @export
#' 
do.call_robust <- function(what, args, quote = FALSE, envir = parent.frame(), keep.unnamed = FALSE) {
	
	# Get the function if given as a full address (with double colon):
	if(is.character(what) && grepl("::", what, fixed = TRUE)) {
		whatSplit <- as.list(strsplit(what, "::")[[1]])
		what <- do.call(what = `::`, args = whatSplit)
	}
	
	# Get the formals of the function:
	f <-  formals(what)
	# Keep only the relevant args:
	if(length(f) && is.list(args)) {
		keep <- which(names(args) %in% names(f))
		if(keep.unnamed) {
			keep  <- sort(c(keep, which(names(args) == "")))
		}
		args <- args[keep]
	}
	
	# Run the function:
	do.call(what, args, quote = quote, envir = envir)
}


