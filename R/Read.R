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
#' @importFrom parallel makeCluster parLapply stopCluster mclapply
#' @export
#' 
ReadBiotic <- function(FileNames, cores = 1) {
	
	# Process Biotic data in parallel if specified:
	if(length(cores) == 0) {
		cores <- getCores()
	}
	if(cores == 1) {
		out <- lapply(FileNames, RstoxData::readXmlFile)
	}
	else {
		if(get_os() == "win") {
			cl <- makeCluster(cores)
			out <- parLapply(cl, FileNames, RstoxData::readXmlFile)
			stopCluster(cl)
		} else {
			out <- mclapply(FileNames, RstoxData::readXmlFile, mc.cores = cores)
		}
	}
	
	# Add names as the file names:
	names(out) <- basename(FileNames)
	
	return(out)
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
#' @importFrom parallel makeCluster parLapply stopCluster mclapply
#' @export
#' 
ReadAcoustic <- function(FileNames, cores = 1) {
	
	# Process Biotic data in parallel if specified:
	if(length(cores) == 0) {
		cores <- getCores()
	}
	if(cores == 1) {
		out <- lapply(FileNames, RstoxData::readXmlFile)
	}
	else {
		if(get_os() == "win") {
			cl <- makeCluster(cores)
			out <- parLapply(cl, FileNames, RstoxData::readXmlFile)
			stopCluster(cl)
		} else {
			out <- mclapply(FileNames, RstoxData::readXmlFile, mc.cores = cores)
		}
	}
	
	# Add names as the file names:
	names(out) <- basename(FileNames)
	
	return(out)
}

