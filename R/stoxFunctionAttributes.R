#' Function specification for inclusion in StoX projects
#' @export
stoxFunctionAttributes <- list(
	#DefineGear = list(
	#  functionType = "modelData",
	#  functionCategory = "Baseline",
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
		functionCategory = "Baseline", 
		functionOutputDataType = "BioticData", 
		#functionParameterType = list(FileNames = "character"), 
		functionParameterFormat = list(FileNames = "filePaths"), 
		functionArgumentHierarchy = list()
	), 
	
	# Read input biotic data:
	ReadAcoustic = list(
		functionType = "modelData", 
		functionCategory = "Baseline", 
		functionOutputDataType = "AcousticData", 
		#functionParameterType = list(FileNames = "character"), 
		functionParameterFormat = list(FileNames = "filePaths"), 
		functionArgumentHierarchy = list()
	), 
	
	# Convert AcousticData to StoxAcousticData:
	StoxAcoustic = list(
		functionType = "modelData", 
		functionCategory = "Baseline", 
		functionOutputDataType = "StoxAcousticData", 
		functionParameterHierarchy = list()
	),
	
	# Convert BioticData to StoxBioticData:
	StoxBiotic = list(
		functionType = "modelData", 
		functionCategory = "Baseline", 
		functionOutputDataType = "StoxBioticData", 
		functionParameterHierarchy = list()
	)
)