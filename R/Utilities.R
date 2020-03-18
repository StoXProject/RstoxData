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
		data <- data[[length(data)]]
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
		cores <- detectCores()
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
