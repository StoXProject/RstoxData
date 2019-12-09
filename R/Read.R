##################################################
##################################################
#' Read biotic XML files
#' 
#' This function reads multiple biotic file to a list with a list of tables for each file.
#' 
#' @param FileNames     The paths of the biotic files.
#' 
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @return
#' An object of StoX data type BioticData: A list of a list of data.tables of the different levels of the input biotic files.
#' 
#' @examples
#' x <- 1
#' 
#' @seealso \code{\link[RstoxData]{readXmlFile}}.
#' 
#' @export
#' 
ReadBiotic <- function(FileNames) {
	# NOTE: Fix the xsdobjects problem and remove the following line:
	out <- lapply(FileNames, RstoxData::readXmlFile)
	names(out) <- basename(FileNames)
	
	# A hack to convert the metadata of each file to data.table:
	message("RstoxData::readXmlFile should be changed to return metadata as a data.table instead of a list")
	for(name in names(out)) {
		out[[name]]$metadata <- data.table::as.data.table(out[[name]]$metadata)
	}
	
	out
}



##################################################
##################################################
#' Read acoustic XML files
#' 
#' This function reads multiple acoustic file to a list with a list of tables for each file.
#' 
#' @param FileNames     The paths of the acoustic files.
#' 
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @return
#' An object of StoX data type AcousticData: A list of a list of data.tables of the different levels of the input acoustic files.
#' 
#' @examples
#' x <- 1
#' 
#' @seealso \code{\link[RstoxData]{readXmlFile}}.
#' 
#' @export
#' 
ReadAcoustic <- function(FileNames) {
	# NOTE: Fix the xsdobjects problem and remove the following line:
	warning("The ReadAcoustic in RstoxBase only works with nmdechosounderv1 due to testing.")
	out <- lapply(FileNames, RstoxData::readXmlFile, stream = TRUE, useXsd = "nmdechosounderv1")
	names(out) <- basename(FileNames)
	
	# A hack to convert the metadata of each file to data.table:
	message("RstoxData::readXmlFile should be changed to return metadata as a data.table instead of a list")
	for(name in names(out)) {
		out[[name]]$metadata <- data.table::as.data.table(out[[name]]$metadata)
	}
	
	out
}



