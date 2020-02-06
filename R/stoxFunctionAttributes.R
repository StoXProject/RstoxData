#' Function specification for inclusion in StoX projects
#' @export
stoxFunctionAttributes <- list(
	#DefineGear = list(
	#  functionType = "modelData",
	#  functionCategory = "baseline",
	#  functionOutputDataType = "StoxLandingData",
	#  functionParameterType = list(StoxLandingData = "character"),
	#  functionParameterFormat = list(),
	#  functionArgumentHierarchy = list(),
	#  functionAlias = list(),
	#  functionParameterAlias = list(),
	#  functionParameterValueAilas = list()
	#), 
	
	# Read input biotic data:
	ReadBiotic = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "BioticData", 
		#functionParameterType = list(FileNames = "character"), 
		functionParameterFormat = list(FileNames = "filePaths"), 
		functionArgumentHierarchy = list()
	), 
	
	# Read input biotic data:
	ReadAcoustic = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "AcousticData", 
		#functionParameterType = list(FileNames = "character"), 
		functionParameterFormat = list(FileNames = "filePaths"), 
		functionArgumentHierarchy = list()
	), 
	
	# Convert AcousticData to StoxAcousticData:
	StoxAcoustic = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "StoxAcousticData", 
		functionParameterHierarchy = list()
	),
	
	# Convert BioticData to StoxBioticData:
	StoxBiotic = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "StoxBioticData", 
		functionParameterHierarchy = list()
	),
	
	# Convert LandingData to StoxLandingData:
	StoxLanding = list(
	 functionType = "modelData",
	 functionCategory = "baseline",
	 functionOutputDataType = "StoxLandingData",
	 functionParameterType = list(StoxLanding = "character",
	                              appendColumns = "character",
	                              appendColumnsNames = "character"),
	 functionArgumentHierarchy = list()
	), 
	
	# Convert AcousticData to StoxAcousticData:
	MergeStoxAcoustic = list(
	    functionType = "modelData", 
	    functionCategory = "baseline", 
	    functionOutputDataType = "MergedStoxAcousticData", 
	    functionParameterHierarchy = list()
	),
	
	# Convert BioticData to StoxBioticData:
	MergeStoxBiotic = list(
	    functionType = "modelData", 
	    functionCategory = "baseline", 
	    functionOutputDataType = "MergedStoxBioticData", 
	    functionParameterHierarchy = list()
	)
)