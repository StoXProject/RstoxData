##################################################
##################################################
#' Read biotic XML files
#' 
#' This function reads multiple biotic file to a list with a list of tables for each file.
#' 
#' @param FileNames     The paths of the biotic files.
#' @param NumberOfCores Overrides multi-core auto detection (default).
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
ReadBiotic <- function(FileNames, NumberOfCores = integer()) {
	
	# Read BioticData possibly on several cores:
	BioticData <- runOnCores(
		FileNames, 
		FUN = RstoxData::readXmlFile, 
		NumberOfCores = NumberOfCores
	)
	
	## Process Biotic data in parallel if specified:
	#if(length(NumberOfCores) == 0) {
	#	NumberOfCores <- getCores()
	#}
#
	## Do not use more cores than the number of files:
	#NumberOfCores <- min(length(FileNames), NumberOfCores)
#
	#if(NumberOfCores == 1) {
	#	out <- lapply(FileNames, RstoxData::readXmlFile)
	#}
	#else {
	#	if(get_os() == "win") {
	#		cl <- makeCluster(NumberOfCores, rscript_args = c("--no-init-file", "--no-site-file", "--no-environ"))
	#		out <- parLapply(cl, FileNames, RstoxData::readXmlFile)
	#		stopCluster(cl)
	#	} else {
	#		out <- mclapply(FileNames, RstoxData::readXmlFile, mc.cores = NumberOfCores)
	#	}
	#}
	
	# Add names as the file names:
	names(BioticData) <- basename(FileNames)
	
	return(BioticData)
}



##################################################
##################################################
#' Read acoustic XML files
#' 
#' This function reads multiple acoustic file to a list with a list of tables for each file.
#' 
#' @param FileNames     The paths of the acoustic files.
#' @param NumberOfCores Overrides multi-core auto detection (default).
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
ReadAcoustic <- function(FileNames, NumberOfCores = integer()) {
	
	# Read AcousticData possibly on several cores:
	AcousticData <- runOnCores(
		FileNames, 
		FUN = RstoxData::readXmlFile, 
		NumberOfCores = NumberOfCores
	)
	
	## Process Biotic data in parallel if specified:
	#if(length(NumberOfCores) == 0) {
	#	NumberOfCores <- getCores()
	#}
#
	## Do not use more cores than the number of files:
	#NumberOfCores <- min(length(FileNames), NumberOfCores)
#
	#if(NumberOfCores == 1) {
	#	out <- lapply(FileNames, RstoxData::readXmlFile)
	#}
	#else {
	#	if(get_os() == "win") {
	#		cl <- makeCluster(NumberOfCores, rscript_args = c("--no-init-file", "--no-site-file", "--no-environ"))
	#		out <- parLapply(cl, FileNames, RstoxData::readXmlFile)
	#		stopCluster(cl)
	#	} else {
	#		out <- mclapply(FileNames, RstoxData::readXmlFile, mc.cores = NumberOfCores)
	#	}
	#}
	
	# Add names as the file names:
	names(AcousticData) <- basename(FileNames)
	
	return(AcousticData)
}

