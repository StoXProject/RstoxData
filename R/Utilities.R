#' Merge data tables
#'
#' @param data A list of data tables.
#' @param tableNames A character vector holding the names of the tables to merge.
#' @param output.only.last Only returns last merged table.
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
		if(!exists("xsdObjects"))
			xsdObjects <- RstoxData::xsdObjects
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
			print(paste("Duplicate columns in merging", prev, "and", curr,  ": ", ddpl, "->", paste0(ddpl, ".", curr)))
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

# Pick a suitable number of cores
#' @importFrom parallel detectCores
getCores <- function() {
	cores <- as.integer(getOption("mc.cores"))
	if (length(cores) == 0 || is.na(cores)) {
		cores <- parallel::detectCores()
		if (is.na(cores)) {
			return(1)
		} else {
			# Don't use too many cores in autodetect
			if (cores > 4)
				return(4)
			else
				return(cores)
		}
	} else {
		return(cores)
	}
}

#' Run a function on all elements of x on one or more cores
#'
#' @param x An object to apply \code{FUN} to.
#' @param FUN The function to apply.
#' @param NumberOfCores The number of cores to use, defaulted to detect the avavilable number of cores, but never to run on more cores than the number of elements of \code{x}.
#' @param ... Additional arguments to \code{FUN}.
#'
#' @return A list of outputs from \code{FUN}.
#'
#' @export
#' 
runOnCores <- function(x, FUN, NumberOfCores = integer(), ...) {
	# Get the number of cores to use:
	if(length(NumberOfCores) == 0) {
		NumberOfCores <- getCores()
	}
	# Simple Lapply if onle one core:
	if(NumberOfCores == 1) {
		out <- lapply(x, FUN, ...)
	}
	# Run in parallel on Windows and other platforms:
	else {
		# Do not use more cores than the number of files:
		NumberOfCores <- min(length(x), NumberOfCores)
		
		# On Windows run special args to speed up:
		if(get_os() == "win") {
			cl <- parallel::makeCluster(NumberOfCores, rscript_args = c("--no-init-file", "--no-site-file", "--no-environ"))
			out <- parallel::parLapply(cl, x, FUN, ...)
			parallel::stopCluster(cl)
		} 
		else {
			out <- parallel::mclapply(x, FUN, mc.cores = NumberOfCores, ...)
		}
	}
	
	return(out)
}


#' Round off to number of digits
#'
#' @param data A list of data tables.
#' @param tableNames A character vector holding the names of the tables to merge.
#' @param output.only.last Only returns last merged table.
#'
#' @return A merged data table.
#'
#' @export
#' 
setRstoxPrecisionLevel <- function(x) {
	# Get the defines number of digits:
	digits <- getRstoxDataDefinitions("digits")
	
	# If a data.table run setPrecisionLevelOneDT() directly:
	if(data.table::is.data.table(x)) {
		setPrecisionLevelOneDT(x, digits = digits)
	}
	# If a list of data tables, loop through the list and set precision:
	else if(is.list(x)) {
		for(tableName in names(x)) {
			setPrecisionLevelOneDT(x[[tableName]], digits = digits)
		}
	}
}
# Functino setting the precision of one data table:
setPrecisionLevelOneDT <- function(DT, digits) {
	# Detect numeric columns and round off to the specified number of digits:
	atNumeric <- sapply(DT, is.numeric)
	if(any(atNumeric)) {
		numericCols <- names(DT)[atNumeric]
		DT[, (numericCols) := round(.SD, digits), .SDcols = numericCols]
	}
}


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

