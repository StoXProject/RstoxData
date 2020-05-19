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
ReadBiotic <- function(FileNames, Cores = NULL) {
	
	# Process Biotic data in parallel if specified:
	if(length(Cores) == 0) {
		Cores <- getCores()
	}

	Cores <- min(length(FileNames), Cores)

	if(Cores == 1) {
		out <- lapply(FileNames, RstoxData::readXmlFile)
	}
	else {
		if(get_os() == "win") {
			cl <- makeCluster(Cores, rscript_args = c("--no-init-file", "--no-site-file", "--no-environ"))
			out <- parLapply(cl, FileNames, RstoxData::readXmlFile)
			stopCluster(cl)
		} else {
			out <- mclapply(FileNames, RstoxData::readXmlFile, mc.cores = Cores)
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
ReadAcoustic <- function(FileNames, Cores = NULL) {
	
	# Process Biotic data in parallel if specified:
	if(length(Cores) == 0) {
		Cores <- getCores()
	}

	Cores <- min(length(FileNames), Cores)

	if(Cores == 1) {
		out <- lapply(FileNames, RstoxData::readXmlFile)
	}
	else {
		if(get_os() == "win") {
			cl <- makeCluster(Cores, rscript_args = c("--no-init-file", "--no-site-file", "--no-environ"))
			out <- parLapply(cl, FileNames, RstoxData::readXmlFile)
			stopCluster(cl)
		} else {
			out <- mclapply(FileNames, RstoxData::readXmlFile, mc.cores = Cores)
		}
	}
	
	# Add names as the file names:
	names(out) <- basename(FileNames)
	
	return(out)
}

