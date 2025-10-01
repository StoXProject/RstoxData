##################################################
##################################################
#' Parameters used in Copy* functions in RstoxData
#' 
#' @param FromVariable The name of the variable to copy.
#' @param ToVariable The name of the (possibly existing) variable insert the values in \code{FromVariable} to.
#' @param Overwrite Logical: If TRUE overwrite the existing variable, if \code{ToVariable} is the name of an existing variable in the same table as \code{FromVariable}.
#' @param PreserveClass Logical: Only used if \code{Overwrite} is TRUE. If \code{PreserveClass} is TRUE (the default) do not convert the class of the variable given by \code{ToVariable} to the class of the variable given by \code{FromVariable}.
#' 
#' @name copy_arguments
#' 
NULL

##################################################
#' Copy a variable of BioticData
#' 
#' This function copies a variable to another (possibly existing) variable of \code{\link{BioticData}} .
#' 
#' @inheritParams ModelData
#' @inheritParams copy_arguments
#' 
#' @return
#' A \code{\link{BioticData}} object.
#' 
#' @export
#' 
CopyBiotic <- function(
		BioticData, 
		FromVariable = character(), 
		ToVariable = character(), 
		Overwrite = FALSE, 
		PreserveClass = TRUE#, 
		#Conditional = FALSE, # If TRUE, ConditionalVariableNames and ConditionalTable are exposed in the GUI (see stoxFunctionAttributes).
		#ConditionalVariableNames = character(),
		#ConditionalTable = data.table::data.table()
) {
	# Make a copy, as we are copying by reference:
	BioticDataCopy <- data.table::copy(BioticData)
	
	copyData(
		data = BioticDataCopy, 
		fromVariable = FromVariable,
		toVariable = ToVariable,
		overwrite = Overwrite,
		preserveClass = PreserveClass
	)
	
	return(BioticDataCopy)
}


##################################################
#' Copy a variable of StoxBioticData
#' 
#' This function copies a variable to another (possibly existing) variable of \code{\link{StoxBioticData}} .
#' 
#' @inheritParams ModelData
#' @inheritParams copy_arguments
#' 
#' @return
#' A \code{\link{StoxBioticData}} object.
#' 
#' @export
#' 
CopyStoxBiotic <- function(
		StoxBioticData, 
		FromVariable = character(), 
		ToVariable = character(), 
		Overwrite = FALSE, 
		PreserveClass = TRUE
) {
	# Make a copy, as we are copying by reference:
	StoxBioticDataCopy <- data.table::copy(StoxBioticData)
	
	copyData(
		data = StoxBioticDataCopy, 
		fromVariable = FromVariable,
		toVariable = ToVariable,
		overwrite = Overwrite,
		preserveClass = PreserveClass
	)
	
	return(StoxBioticDataCopy)
}


##################################################
#' Copy a variable of ICESBioticData
#' 
#' This function copies a variable to another (possibly existing) variable of \code{\link{ICESBioticData}} .
#' 
#' @inheritParams ModelData
#' @inheritParams copy_arguments
#' 
#' @return
#' A \code{\link{ICESBioticData}} object.
#' 
#' @export
#' 
CopyICESBiotic <- function(
		ICESBioticData, 
		FromVariable = character(), 
		ToVariable = character(), 
		Overwrite = FALSE, 
		PreserveClass = TRUE
) {
	# Make a copy, as we are copying by reference:
	ICESBioticDataCopy <- data.table::copy(ICESBioticData)
	
	copyData(
		data = ICESBioticDataCopy, 
		fromVariable = FromVariable,
		toVariable = ToVariable,
		overwrite = Overwrite,
		preserveClass = PreserveClass
	)
	
	return(ICESBioticDataCopy)
}


##################################################
#' Copy a variable of ICESDatrasData
#' 
#' This function copies a variable to another (possibly existing) variable of \code{\link{ICESDatrasData}} .
#' 
#' @inheritParams ModelData
#' @inheritParams copy_arguments
#' 
#' @return
#' A \code{\link{ICESDatrasData}} object.
#' 
#' @export
#' 
CopyICESDatras <- function(
		ICESDatrasData, 
		FromVariable = character(), 
		ToVariable = character(), 
		Overwrite = FALSE, 
		PreserveClass = TRUE
) {
	# Make a copy, as we are copying by reference:
	ICESDatrasDataCopy <- data.table::copy(ICESDatrasData)
	
	copyData(
		data = ICESDatrasDataCopy, 
		fromVariable = FromVariable,
		toVariable = ToVariable,
		overwrite = Overwrite,
		preserveClass = PreserveClass
	)
	
	return(ICESDatrasDataCopy)
}

##################################################
#' Copy a variable of ICESDatrasData
#' 
#' This function copies a variable to another (possibly existing) variable of \code{\link{ICESDatrasData}} .
#' 
#' @inheritParams ModelData
#' @inheritParams copy_arguments
#' 
#' @return
#' A \code{\link{ICESDatrasData}} object.
#' 
#' @export
#' 
CopyICESDatsusc <- function(
    ICESDatsuscData, 
    FromVariable = character(), 
    ToVariable = character(), 
    Overwrite = FALSE, 
    PreserveClass = TRUE
) {
  # Make a copy, as we are copying by reference:
  ICESDatsuscDataCopy <- data.table::copy(ICESDatsuscData)
  
  copyData(
    data = ICESDatsuscDataCopy, 
    fromVariable = FromVariable,
    toVariable = ToVariable,
    overwrite = Overwrite,
    preserveClass = PreserveClass
  )
  
  return(ICESDatsuscDataCopy)
}

##################################################
#' Copy a variable of AcousticData
#' 
#' This function copies a variable to another (possibly existing) variable of \code{\link{AcousticData}} .
#' 
#' @inheritParams ModelData
#' @inheritParams copy_arguments
#' 
#' @return
#' A \code{\link{AcousticData}} object.
#' 
#' @export
#' 
CopyAcoustic <- function(
		AcousticData, 
		FromVariable = character(), 
		ToVariable = character(), 
		Overwrite = FALSE, 
		PreserveClass = TRUE
) {
	# Make a copy, as we are copying by reference:
	AcousticDataCopy <- data.table::copy(AcousticData)
	
	copyData(
		data = AcousticDataCopy, 
		fromVariable = FromVariable,
		toVariable = ToVariable,
		overwrite = Overwrite,
		preserveClass = PreserveClass
	)
	
	return(AcousticDataCopy)
}


##################################################
#' Copy a variable of StoxAcousticData
#' 
#' This function copies a variable to another (possibly existing) variable of \code{\link{StoxAcousticData}} .
#' 
#' @inheritParams ModelData
#' @inheritParams copy_arguments
#' 
#' @return
#' A \code{\link{StoxAcousticData}} object.
#' 
#' @export
#' 
CopyStoxAcoustic <- function(
		StoxAcousticData, 
		FromVariable = character(), 
		ToVariable = character(), 
		Overwrite = FALSE, 
		PreserveClass = TRUE
) {
	# Make a copy, as we are copying by reference:
	StoxAcousticDataCopy <- data.table::copy(StoxAcousticData)
	
	copyData(
		data = StoxAcousticDataCopy, 
		fromVariable = FromVariable,
		toVariable = ToVariable,
		overwrite = Overwrite,
		preserveClass = PreserveClass
	)
	
	return(StoxAcousticDataCopy)
}


##################################################
#' Copy a variable of ICESAcousticData
#' 
#' This function copies a variable to another (possibly existing) variable of \code{\link{ICESAcousticData}} .
#' 
#' @inheritParams ModelData
#' @inheritParams copy_arguments
#' 
#' @return
#' A \code{\link{ICESAcousticData}} object.
#' 
#' @export
#' 
CopyICESAcoustic <- function(
		ICESAcousticData, 
		FromVariable = character(), 
		ToVariable = character(), 
		Overwrite = FALSE, 
		PreserveClass = TRUE
) {
	# Make a copy, as we are copying by reference:
	ICESAcousticDataCopy <- data.table::copy(ICESAcousticData)
	
	copyData(
		data = ICESAcousticDataCopy, 
		fromVariable = FromVariable,
		toVariable = ToVariable,
		overwrite = Overwrite,
		preserveClass = PreserveClass
	)
	
	return(ICESAcousticDataCopy)
}


##################################################
#' Copy a variable of LandingData
#' 
#' This function copies a variable to another (possibly existing) variable of \code{\link{LandingData}} .
#' 
#' @inheritParams ModelData
#' @inheritParams copy_arguments
#' 
#' @return
#' A \code{\link{LandingData}} object.
#' 
#' @export
#' 
CopyLanding <- function(
		LandingData, 
		FromVariable = character(), 
		ToVariable = character(), 
		Overwrite = FALSE, 
		PreserveClass = TRUE
) {
	# Make a copy, as we are copying by reference:
	LandingDataCopy <- data.table::copy(LandingData)
	
	copyData(
		data = LandingDataCopy, 
		fromVariable = FromVariable,
		toVariable = ToVariable,
		overwrite = Overwrite,
		preserveClass = PreserveClass
	)
	
	return(LandingDataCopy)
}


##################################################
#' Copy a variable of StoxLandingData
#' 
#' This function copies a variable to another (possibly existing) variable of \code{\link{StoxLandingData}} .
#' 
#' @inheritParams ModelData
#' @inheritParams copy_arguments
#' 
#' @return
#' A \code{\link{StoxLandingData}} object.
#' 
#' @export
#' 
CopyStoxLanding <- function(
		StoxLandingData, 
		FromVariable = character(), 
		ToVariable = character(), 
		Overwrite = FALSE, 
		PreserveClass = TRUE
) {
	# Make a copy, as we are copying by reference:
	StoxLandingDataCopy <- data.table::copy(StoxLandingData)
	
	copyData(
		data = StoxLandingDataCopy, 
		fromVariable = FromVariable,
		toVariable = ToVariable,
		overwrite = Overwrite,
		preserveClass = PreserveClass
	)
	
	return(StoxLandingDataCopy)
}




copyData <- function(
	data, 
	fromVariable = character(), 
	toVariable = character(), 
	overwrite = FALSE, 
	preserveClass = TRUE, 
	conditionalTable = data.table::data.table()
) {
	# Copy for each table of the data:
	lapplyToStoxData(
		data, 
		copyDataOneTable, 
		fromVariable = fromVariable,
		toVariable = toVariable,
		overwrite = overwrite,
		preserveClass = preserveClass, 
		conditionalTable = conditionalTable
	)
}




copyDataOneTable <- function(
	table, 
	fromVariable = character(), 
	toVariable = character(), 
	overwrite = FALSE, 
	preserveClass = TRUE, 
	conditionalTable = data.table::data.table()
) {
	# Find the fromVariable:
	if(fromVariable %in% names(table)) {
		# Find the toVariable:
		if(toVariable %in% names(table)) {
			if(!overwrite) {
				stop("ToVariable already exists but Overwrite is FALSE.")
			}
			else {
				if(!preserveClass) {
					setColumnClasses(table, structure(list(class(translationListOne$NewValue)), names = variableToTranslate))
				}
			}
		}
		
		
		
		if(NROW(conditionalTable)) {
			table[conditionalTable, eval(toVariable) := get(fromVariable), on = names(conditionalTable)]
		}
		else {
			table[, eval(toVariable) := get(fromVariable)]
		}
	}
}



