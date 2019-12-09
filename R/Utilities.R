#' Merge data tables
#'
#' @param data A list of data tables.
#' @param tableNames A character vector holding the names of the tables to merge.
#'
#' @return A merged data table.
#'
#' @export
#' 
mergeDataTables <- function(data, tableNames = NULL) {
	# Merge all tables by default:
	if(length(tableNames) == 0) {
		tableNames <- names(data)
	}
	# Merge
	for(ii in 2:length(tableNames)) {
		curr <- tableNames[ii]
		prev <- tableNames[(ii-1)]
		vars <- names(data[[curr]])[names(data[[curr]]) %in% names(data[[prev]])]
		data[[curr]] <- merge(data[[prev]], data[[curr]], by=vars)
	}
	data[tableNames]
}
