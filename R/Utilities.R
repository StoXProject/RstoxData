#' Merge data tables
#'
#' @param data A list of data tables.
#' @param tableNames A character vector holding the names of the tables to merge.
#'
#' @return A merged data table.
#'
#' @export
#' 
mergeDataTables <- function(data, tableNames = NULL, output.only.last = FALSE, ...) {
	
    # Merge all tables by default:
	if(length(tableNames) == 0) {
		tableNames <- names(data)
	}
	
	# Merge
	for(ii in 2:length(tableNames)) {
		curr <- tableNames[ii]
		prev <- tableNames[(ii-1)]
		#vars <- names(data[[curr]])[names(data[[curr]]) %in% names(data[[prev]])]
		vars <- intersect(names(data[[curr]]), names(data[[prev]]))
		data[[curr]] <- merge(data[[prev]], data[[curr]], by=vars, ...)
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
