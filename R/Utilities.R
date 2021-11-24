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
		if(!exists("xsdObjects")) {
			xsdObjects <- RstoxData::xsdObjects
		}
		datatype <- unlist(data[["metadata"]][1, "useXsd"])
		plen <- xsdObjects[[paste0(datatype, ".xsd")]]$prefixLens
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
			message(paste("Duplicate columns in merging", prev, "and", curr,  ": ", ddpl, "->", paste0(ddpl, ".", curr)))
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
#' @param ... Extra parameters that will be passed into \code{\link[data.table]{merge}}.
#'
#' @return A merged data table.
#'
#' @export
#' 
mergeByStoxKeys <- function(x, y, StoxDataType, toMergeFromY = NULL, replace = FALSE, ...) {
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
		merge(x, y, by = keys, ...)
	}
	else {
		x
	}
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
	StoxDataType <- match.arg(StoxDataType)
	if(StoxDataType == "StoxBiotic") {
		if(!exists("stoxBioticObject")) {
			data(stoxBioticObject, package="RstoxData", envir = environment())
		}
		keys <- stoxBioticObject$convertTable[key == "Y", c("variable", "level")]
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
	
	keys.out <- match.arg(keys.out)
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
	
	# If a data.table run setPrecisionLevelOneDT() directly:
	if(data.table::is.data.table(x)) {
		setPrecisionLevelOneDT(x, digits = digits, signifDigits = signifDigits)
	}
	# If a list of data tables, loop through the list and set precision:
	else if(is.list(x)) {
		for(tableName in names(x)) {
			setPrecisionLevelOneDT(x[[tableName]], digits = digits, signifDigits = signifDigits)
		}
	}
}
# Function setting the precision of one data table:
setPrecisionLevelOneDT <- function(DT, digits, signifDigits) {
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


roundSignif <- function(x, digits = 12, signifDigits = NULL) {
	if(length(signifDigits)) {
		# Use this in the future when units are implemented for all variables in all data type (unitsless being those without unit). Units can be added and remoevd so that we end up with only the units we need, and can display these in the documentation:
		# digits <- pmax(signifDigits - floor(log10(abs(units::drop_units(x)))) - 1, digits)
		digits <- pmax(signifDigits - floor(log10(abs(x))) - 1, digits)
	}
	
	out <- round(x, digits)
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
	
	stoxDataFormat <- match.arg(stoxDataFormat)
	StoxKeys <- getRstoxDataDefinitions(paste0("Stox", stoxDataFormat, "Keys"))
	
	# Run through the tables of the StoxData and remove duplicate rows:
	for(tableName in names(StoxData)) {
		# Get the names of the columns which are keys:
		presentKeys <- intersect(names(StoxData[[tableName]]), StoxKeys)
		# Find rows of duplicated keys:
		duplicatedKeys <- duplicated(StoxData[[tableName]], by = presentKeys)
		# Remove the rows with duplicated keys:
		if(any(duplicatedKeys)) {
			# Get the rows with equall keys, and indicate this in a copy of the data, and write to a tempfile:
			allDuplicated <- duplicated(StoxData[[tableName]], by = presentKeys) | duplicated(StoxData[[tableName]], by = presentKeys, fromLast = TRUE)
			dupData <- data.table::copy(StoxData[[tableName]])
			dupData[, duplicated := ..allDuplicated]
			dupData[, rowIndex := .I]
			fileToWriteDupDataTo <- tempfile()
			data.table::fwrite(dupData, fileToWriteDupDataTo)
			
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
	NumberOfCores = 1L, 
	StoxDataFormat = c("Biotic", "Acoustic")
) {
	
	if(length(VariableNames) == 0) {
		warning("StoX: No variables specified to extract. Returning data unchcanged")
		return(StoxData)
	}
	
	# Check the the BioticData are all from the same source (ICES/NMD):
	checkDataSource(RawData)
	
	# Convert from BioticData to the general sampling hierarchy:
	StoxDataFormat <- match.arg(StoxDataFormat)
	if(StoxDataFormat == "Biotic") {
		GeneralSamplingHierarchy <- BioticData2GeneralSamplingHierarchy(RawData, NumberOfCores = NumberOfCores)
		
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
	
	# Merge with the present StoxBioticData:
	StoxData <- mapply(mergeAndOverwriteDataTable, StoxData, toAdd, all.x = TRUE, sort = FALSE)
	
	return(StoxData)
}

mergeAndOverwriteDataTable <- function(x, y, ...) {
	# Overwrite any non-keys already present in the StoxData
	toKeep <- !names(x) %in% names(y) | endsWith(names(x), "Key")
	merge(x[, ..toKeep], y, ...)
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
	
	return(vocabulary)
}

findVariablesMathcinigVocabularyOne <- function(vocabularyOne, data) {
	# Get the names of the columns which has values in vocabularyOne$id:
	#VariableName <- unlist(lapply(data, function(table) names(which(unlist(table[, lapply(.SD, function(x) any(x %in% vocabularyOne$id))])))))
	VariableName <- unlist(lapply(data, function(table) names(which(unlist(table[, lapply(.SD, function(x) any(unique(x) %in% vocabularyOne$id))])))))
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
		
		# Create keys which are converted to ranks, splitting first and then treatnig individual elemens as numbers if possible:
		dataOne[, (orderKeys) := lapply(.SD, createOrderKey), .SDcols = by]
		
		# Order the rows:
		data.table::setorderv(dataOne, orderKeys, ...)
		
		# Remove the orderKeys:
		dataOne[, (orderKeys) := NULL]
	}
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
	splitted <- strsplit(x, split)
	
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
		splitted <- strsplit(x, split)
	}
	else if(length(split) > 1) {
		splitted <- strsplit(x, split[1])
		for(ind in seq(2, length(split))) {
			splitted <- lapply(splitted, function(x) unlist(strsplit(x, split[ind])))
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
	for(col in names(classes)) {
		data.table::set(DT, j = col, value = do.call(paste("as", classes[[col]], sep = "."), list(DT[[col]])))
	}
	
	return(DT)
}



