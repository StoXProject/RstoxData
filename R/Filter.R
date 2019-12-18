#' Convert BioticData to StoxBioticData
#'
#' @param BioticData A list of biotic data (StoX data type \code{\link{BioticData}}), one element for each input biotic file.
#'
#' @return An object of StoX data type \code{\link{StoxBioticData}}.
#'
#' @import data.table
#' @export
#' 
filterData <- function(data, filterExpression, fropagateUpwards = "") {
	
	# Remove any filter specifying tables that are not present in the data, and order the expressions by the hierarchy of the StoxData:
	if(length(filterExpression) == 0) {
		return(data)
	}
	commonNames <- intersect(names(data), names(filterExpression))
	filterExpression <- filterExpression[commonNames]
	if(length(filterExpression) == 0) {
		return(data)
	}
	
	# Apply the filters recursively, starting from the highest table in the hierarchy:
	filterNames <- names(filterExpression)
	tableNames <- names(data)
	for(filterName in filterNames) {
		fromIndex <- which(tableNames == filterName) + 1
		toIndex <- length(tableNames)
		tableIndices <- seq()
		
		currentTableNames <- tableNames[seq(which(tableNames == filterName), length(tableNames))]
		for(tableName in currentTableNames) {
			
		}
	}
	
	
	
	
	
	filterDataOne <- function(filter, data) {
		
	}
	
	
}