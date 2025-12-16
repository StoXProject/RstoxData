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
	
	# Get the keys:
	keys <- getKeys(data)
	
	# Get the tree structure (list of children):
	treeStruct <- getTreestruct(data)
	
	# Merge all tables by default:
	if(length(tableNames) == 0) {
		tableNames <- names(data)
	}
	# No merging if only one table given in 'tableNames':
	else if(length(tableNames) == 1)  {
		return(data[[tableNames]])
	}
	
	
	
	## Make sure tableNames are ordered as in the data:
	#dataNames <- names(data)
	#tableNames <- dataNames[match(tableNames, dataNames)]
	#tableNames <- tableNames[!is.na(tableNames)]
	
	# Merge
	for(parent in head(tableNames, -1)) {
		child <- intersect(treeStruct[[parent]], tableNames)
		
		# Get the keys to merge by:
		intersectKeys <- intersect(keys[[child]], keys[[parent]])
		
		# There can be duplicate names between two tables, see that we fix them by adding appropriate suffix before merging
		duplicates <- intersect(setdiff(names(data[[parent]]), intersectKeys), setdiff(names(data[[child]]), intersectKeys))
		for(ddpl in duplicates) {
			#message(paste("Duplicate columns in merging", parent, "and", child,  ": ", ddpl, "->", paste0(ddpl, ".", child)))
			setnames(data[[child]], ddpl, paste0(ddpl, ".", child))
		}
		
		# Special case if the parentious table is empty. We then do nothing for the children table:
		if(length(data[[parent]])) {
			data[[child]] <- merge(data[[parent]], data[[child]], by = intersectKeys, ...)
		}
	}

	# If tableNamestableNames == "last", return the last table:
	if(output.only.last) {
		data <- data[[utils::tail(tableNames, 1)]]
	}
	
	return(data)
}


#' Get keys from the data
#' 
#' Look for the metadata list element specifying the XSD, or estimate the keys based on intersection between tables, in which case the keys of the last table is set to the keys of the parent plus the next field. For more control of keys, please use getRstoxDataDefinitions("keys") and feed this directly into translateVariables().
#' 
#' @param data An StoX data object
#' 
getKeys <- function(data) {
	
	if(!is.null(data[["metadata"]])) {
		datatype <- unlist(data[["metadata"]][1, "useXsd"])
		xsdObject <- RstoxData::xsdObjects[[paste0(datatype, ".xsd")]]
		keys <- xsdObject$keys
	}
	else {
		
		allVariableNames <- lapply(data, names)
		
		if(any(endsWith(unlist(allVariableNames), "Key"))) {
			keys <- allVariableNames
		}
		else {
			keys <- vector("list", length(data))
			# Look for intersecting variables:
			for(ind in seq_len(length(data) - 1)) {
				keys[[ind]] <- intersect(names(data[[ind]]), names(data[[ind + 1]]))
			}
			
			# Assume that the last table has the same keys as the parent:
			keys[[length(data)]] <- keys[[length(data) - 1]]
		}
		
		names(keys) <- names(data)
		
	}
	
	return(keys)
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
	
	# This all seems unnecessary, since the StoxBioticData and StoxAcousticData have keys that end with "Key". The risk of a non-key variable with name ending with "Key" entering those formats is small. This function is currently only used in mergeByStoxKeys() which is only used in StoxBiotic(). Instead we should consider a general funciton to get keys from any StoX datatype, reading from the XSD indicated in the metadata for those data that have this.
	
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
	availableNumberOfCores <- parallel::detectCores()
	if(!length(NumberOfCores)) {
		NumberOfCores <- as.integer(getOption("mc.cores"))
		if (!length(NumberOfCores) || is.na(NumberOfCores)) {
			NumberOfCores <- availableNumberOfCores
			if (is.na(NumberOfCores)) {
				return(1)
			}
		} 
	}
	
	# Do not use more cores than the number of elemens:
	if(length(n)) {
		NumberOfCores <- min(n, NumberOfCores, availableNumberOfCores)
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
		# The parallel processing was conducted differently on Windows and macOS/Linux before StoX 4.1.0 (for some historic reason), with sockets for the former and forks for the latter. This was a potential memory problem since forking copies the R workspace to all the cores. Instead, sockets are now used for all platforms:
		
		# On Windows run special args to speed up:
		#if(get_os() == "win") {
			
			# Removed the rscript_args, because it changes the envoronment compared to the partent environment:
			#cl <- parallel::makeCluster(NumberOfCores, rscript_args = c("--no-init-file", "--no-site-file", "--no-environ"))
			cl <- parallel::makeCluster(NumberOfCores)
			out <- parallel::clusterMap(cl, FUN, ..., MoreArgs = MoreArgs, SIMPLIFY = SIMPLIFY)
			parallel::stopCluster(cl)
		#} 
		#else {
		#	out <- parallel::mcmapply(FUN, mc.cores = NumberOfCores, ..., MoreArgs = MoreArgs, SIMPLIFY = SIMPLIFY)
		#}
	}
	else {
		out <- NULL
	}
	
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




# Function to remove rows with duplicated keys in StoxBioticData:
#' @importFrom data.table .I
removeRowsOfDuplicatedKeys <- function(StoxData, stoxDataFormat = c("Biotic", "Acoustic")) {
	
	stoxDataFormat <- match_arg_informative(stoxDataFormat)
	StoxKeys <- unlist(getRstoxDataDefinitions("keys")[[paste0("Stox", stoxDataFormat)]])
	
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



# Function to remove rows with duplicated keys in StoxBioticData:
#' @importFrom data.table .I
warningMissingKeys <- function(StoxData, stoxDataFormat = c("Biotic", "Acoustic")) {
	
	stoxDataFormat <- match_arg_informative(stoxDataFormat)
	StoxKeys <- unlist(getRstoxDataDefinitions("keys")[[paste0("Stox", stoxDataFormat)]])
	
	presentKeys <- lapply(StoxData, function(x) intersect(names(x), StoxKeys))
	
	atMaxNumberOfKeys <- which.max(lengths(presentKeys))
	hasmissingKeys <- StoxData[[atMaxNumberOfKeys]][, lapply(.SD, is.na), .SDcols = presentKeys[[atMaxNumberOfKeys]]]
	hasmissingKeys <- lapply(hasmissingKeys, any)
	
	# Warn if any keys have missing values:
	if(any(unlist(hasmissingKeys))) {
		warning("The Stox", stoxDataFormat, "Data has missing keys (", paste(names(StoxData)[unlist(hasmissingKeys)], names(hasmissingKeys)[unlist(hasmissingKeys)], sep = ": ", collapse = ", "), ")! Please translate fields in the Read", stoxDataFormat, "process to avoid this. ")
	}
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
			unlist(getRstoxDataDefinitions("keys")$StoxBiotic), 
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
	
	# Remove rows of duplicated keys. This is particularly relevant for NMDBiotic files where the station variable has bee used, in which case removeRowsOfDuplicatedKeys selects only the first row (keeping only the first station information):
	StoxBioticData <- removeRowsOfDuplicatedKeys(
		StoxData = StoxData, 
		stoxDataFormat = "Biotic"
	)
	
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
	# StoX keys are concatenated only by "/", whereas IDs are concatenated keys by "-":
	lapply(data, setorderv_numeric, key = "Key", split = "/")
}

#' Order a data.table (by reference) by interpreting characters as numeric if possible
#'
#' @param dataOne A data.table.
#' @param by Order by the given columns.
#' @param key If given and \code{by} is empty, order by the columns with names ending with \code{key}.
#' @param split Character: A vector of single character to split by. The default c("-", "/") splits between StoX keys and within StoX keys. 
#'
#' @export
#' 
setorderv_numeric <- function(dataOne, by = NULL, key = NULL, split = "/") {
#setorderv_numeric <- function(dataOne, by = NULL, key = NULL, ...) {
	
	# Locate keys:
	if(!length(by)) {
		if(length(key)) {
			by <- names(dataOne)[endsWith(names(dataOne), key)]
		}
		else {
			by <- names(dataOne)
		}
	}
	else {
		by <- intersect(by, names(dataOne))
	}
	
	if(length(by)) {
		orderKeys <- paste0(by, "OrderedAfterSplitting")
		
		# Create keys which are converted to ranks, splitting first and then treating individual elements as numbers if possible:
		dataOne[, (orderKeys) := lapply(.SD, createOrderKey, split = split), .SDcols = by]
		
		# Order the rows:
		# 2024-10-28: Change to sort out sorting:
		data.table::setorderv(dataOne, orderKeys, na.last = TRUE)
		
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

#' Convert a vector to an integer vector where individual string elements at interpreted as numeric if possible.
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
	# If the first element is coercible to numeric, try converting the entire vector to numeric, and check that no NAs were generated:
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
	if(!any(sapply(split, grepl, firstNonNA))) {
		return(x)
	}
	
	# Split the vector by the 'split' parameter:
	splitted <- x
	for(thisSplit in split) {
		splitted <- lapply(splitted, function(x) unlist(strsplit(x, thisSplit, fixed = TRUE)))
	}
	#splitted <- strsplit(x, split, fixed = TRUE)
	
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
	
	# Count the maximum number of digits for each column, and multiply by the cumulative number of digits:
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



as.numeric_IfPossible <- function(x) {
	# If any missing values are introduced, keep the original:
	num <- as.numeric(x)
	#if(all(is.na(num))) {
	# 2024-10-28: Change to sort out sorting:
	if(any(is.na(num) & !is.na(x))) {
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
#' @param classes A list of classes named by the columns for which the class should be set. Columns in \code{DT} with name found in the names of \code{classes} are modified if the existing class differs from the desired class.
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



#' Sanitize an expression given in a string
#' 
#' If the input contains "system" or "system2", an error is issued.
#'
#' @param x A string
#'
#' @export
#' 
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
		stop(gettextf("%s should be one of %s", arg_name_string, paste0(paste(dQuote(choices),
																	   collapse = ", "), " (was ", dQuote(arg), ")")), domain = NA)
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
#' @param ignore.condition Character: A vector of strings naming parameters to ignore in the hierarchy. Current use is "UseProcessData" to identify function inputs hidden by that parameter.
#'
#' @export
#' 
applyFunctionArgumentHierarchy <- function(
	functionArgumentHierarchy, 
	functionArguments, 
	return.only.names = TRUE, 
	#ignore = "UseProcessData"
	ignore.condition = NULL
) {
	
	# Support an expression at the top level:
	if(inherits(functionArgumentHierarchy, "expression")) {
		functionArgumentHierarchy <- eval(functionArgumentHierarchy)
	}
	
	# Loop through the arguments given by parent tags in the functionArgumentHierarchy, and set toShow to FALSE if not any of the criteria are fulfilled:
	toShow <- logical(length(functionArguments))
	names(toShow) <- names(functionArguments)
	for(argumentName in names(toShow)) {
		# Check whether the argument is given in the functionArgumentHierarchy. If not, it will be shown:
		atArgumentName <- which(argumentName == names(functionArgumentHierarchy))
		if(length(atArgumentName)) {
			# Loop through the occurrences of the argumentName in the functionArgumentHierarchy, applying &&:
			hitsOr <- logical(length(atArgumentName))
			for(ind in seq_along(atArgumentName)) {
				# Loop through the conditions (except the ones given by 'ignore.condition') and set hitsAnd to TRUE if at least one condition is fullfilled:
				conditionNames <- setdiff(names(functionArgumentHierarchy[[atArgumentName[ind]]]), ignore.condition)
				hitsAnd <- logical(length(conditionNames))
				names(hitsAnd) <- conditionNames
				for(conditionName in conditionNames) {
					if(is.function(functionArgumentHierarchy[[atArgumentName[ind]]][[conditionName]])) {
						if(isTRUE(functionArgumentHierarchy[[atArgumentName[ind]]][[conditionName]](functionArguments))) {
							hitsAnd[conditionName] <- TRUE
						}
					}
					else {
						# Added requirement that functionArguments[[conditionName]] has positive length:
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
		if("..." %in% names(f)) {
			keep <- seq_along(args)
		}
		else {
			keep <- which(names(args) %in% names(f))
			if(keep.unnamed) {
				keep  <- sort(c(keep, which(names(args) == "")))
			}
		}
		
		args <- args[keep]
	}
	
	# Run the function:
	do.call(what, args, quote = quote, envir = envir)
}



























# Process column names and types
setNames_OneTable <- function(tableName, data, xsd) {
	
	# For convenience extract the tableHeader of the current table:
	tableHeader <- xsd$tableHeaders[[tableName]]
	
	# There are duplicated column names in NMDBiotic 1, 1.1 and 1.4. we suffix the table name to those fields:
	if(anyDuplicated(tableHeader)) {
		dup <- duplicated(tableHeader)
		tableHeader[dup] <- paste(tableHeader[dup], tableName, sep = ".")
	}
	
	# Handle empty data. This is only relevant for NMDBiotic and NMDAcoustic, which both have levels with no data (i.e. "missions" and "distance_list", respectively):
	if(!length(data[[tableName]])) {
		data[[tableName]] <- matrix(data = "", nrow = 0, ncol = length(tableHeader))
	}
	
	# Convert to data.table
	output <- data.table(data[[tableName]])
	
	# Set column names
	setnames(output, tableHeader)
	
	return(output)
}





# Set types of the columns of the table named 'tableName' of 'data'. Note that this only considers the columns with names present in the xsd$tableHeader. For ICES formats the keys are not included in the tableHeader, but all keys are character
setClass_OneTable <- function(tableName, data, xsd) {
	
	# Known atomic data types
	conversionFunctionName <- getRstoxDataDefinitions("conversionFunctionName")
	
	# Set column types (only double and integer for now)
	tableHeader <- xsd$tableHeader[[tableName]]
	tableType <- xsd$tableTypes[[tableName]]
	
	# Subset by the present names in order to avoid mutually conditional fields, i.e. fields that cannot exist if the other field exists, specifically SaCategory and EchoType:
	present <- tableHeader %in% names(data[[tableName]])
	if(!all(present)) {
		tableHeader <- tableHeader[present]
		tableType <- tableType[present]
	}
	
	if(length(tableType) > 0) {
		for(i in seq_along(tableHeader)) {
			# Map the types
			doConv <- eval(
				parse(
					text = conversionFunctionName[[tableType[i]]]
				)
			)
			
			# Throw a proper warning when conversion fails:
			tryCatch(
				data[[tableName]][, tableHeader[i] := doConv(data [[tableName]] [[tableHeader[i]]] ) ], 
				error = function(e) {
					e
				}, 
				warning = function(w) {
					modifiedWarning <- paste0("The following variable could not converted to numeric as per the format definition and were set to NA: ", names(data[[tableName]])[i])
					warning(modifiedWarning)
				}
			)
		}
	}
	
	invisible(tableName)
}


asIntegerAfterRound <- function(x, prec = sqrt(.Machine$double.eps)) {
	# This operation requires that the input can be represented as numeric, so we test that first by observing whether the number of missing values increases:
	x_numeric <- as.numeric(x)
	x_integer <- as.integer(x)
	
	# Detect whether the input is not fully convertible to integer, which we assume is the case if there are mote missing values in the x_numeric, in which case we simply return the x_integer: 
	numberOfNAs <- sum(is.na(x))
	numberOfNAs_numeric <- sum(is.na(x_numeric))
	if(numberOfNAs_numeric > numberOfNAs) {
		warning("StoX: NAs introduced when trying to convert to integer.")
		return(x_integer)
	}
	
	# Convert to integer:
	x_integer <- as.integer(x)
	x_rounded <- round(x_numeric)
	# Find values which differ to the integer value by less than the input precision, and round these off before converting to integer to avoid occasional shifts in integer value due to floating point representation (e.g. as.integer(0.29 * 100) == 28):
	diff <- x_numeric - x_rounded
	atSmallDiff <- which(diff < 0 & -diff <= prec)
	
	# Convert to integer after rounding for values that differ to the integer value by less than the prec:
	x_integer[atSmallDiff] <- as.integer(x_rounded[atSmallDiff])
	
	return(x_integer)
}




expandICESKeysWithPrefix <- function(ICESKeys) {
	
	# Declare the output:
	ICESKeysOut <- ICESKeys
	
	# Loop through the tables in reversed order, and paste the table name to the keys, overwriting as we move to the higher tables:
	reversedTableOrder <- rev(names(ICESKeys))
	for(tableName1 in reversedTableOrder) {
		for(tableName2 in reversedTableOrder) {
			ICESKeysOut[[tableName1]][ICESKeys[[tableName1]] %in% ICESKeys[[tableName2]]] <- paste0(tableName2, ICESKeys[[tableName1]][ICESKeys[[tableName1]] %in% ICESKeys[[tableName2]]])
		}
	}
	
	return(ICESKeysOut)
}


