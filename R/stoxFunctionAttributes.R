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
		functionArgumentHierarchy = list()
	),
	
	# Convert BioticData to StoxBioticData:
	StoxBiotic = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "StoxBioticData", 
		functionArgumentHierarchy = list()
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
	
	# Convert BioticData to StoxBioticData:
	FilterBiotic = list(
	    functionType = "modelData", 
	    functionCategory = "baseline", 
	    functionOutputDataType = "BioticData", 
	    functionParameterFormat = list(FilterExpression = "filterExpressionList")
	),
	
	# Convert BioticData to StoxBioticData:
	FilterAcoustic = list(
	    functionType = "modelData", 
	    functionCategory = "baseline", 
	    functionOutputDataType = "AcousticData", 
	    functionParameterFormat = list(FilterExpression = "filterExpressionList")
	),
	
	# Convert BioticData to StoxBioticData:
	FilterStoxBiotic = list(
	    functionType = "modelData", 
	    functionCategory = "baseline", 
	    functionOutputDataType = "StoxBioticData", 
	    functionParameterFormat = list(FilterExpression = "filterExpressionList")
	),
	
	# Convert BioticData to StoxBioticData:
	FilterStoxAcoustic = list(
	    functionType = "modelData", 
	    functionCategory = "baseline", 
	    functionOutputDataType = "StoxAcousticData", 
	    functionParameterFormat = list(FilterExpression = "filterExpressionList")
	),
	
	# Convert AcousticData to StoxAcousticData:
	MergeStoxAcoustic = list(
	    functionType = "modelData", 
	    functionCategory = "baseline", 
	    functionOutputDataType = "MergedStoxAcousticData", 
	    functionArgumentHierarchy = list()
	),
	
	# Convert BioticData to StoxBioticData:
	MergeStoxBiotic = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "MergedStoxBioticData", 
		functionArgumentHierarchy = list()
	),
	
	##### Define and Convert variables: #####
	
	# Biotic:
	DefineBioticVariableConversion = list(
		functionType = "processData", 
		functionCategory = "baseline", 
		functionOutputDataType = "BioticVariableConversion"
	),
	
	ConvertBioticVariables = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "BioticData", 
		functionParameterFormat = list(
			VariableConversionTable = "variableConversionTable"
		), 
		functionArgumentHierarchy = list(
			VariableConversionTable = list(
				ConversionMethod = "Table"
			), 
			FileName = list(
				ConversionMethod = "PreDefined"
			)
		)
	),
	
	# StoxBiotic:
	DefineStoxBioticVariableConversion = list(
		functionType = "processData", 
		functionCategory = "baseline", 
		functionOutputDataType = "StoxBioticVariableConversion"
	),
	
	ConvertStoxBioticVariables = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "StoxBioticData", 
		functionParameterFormat = list(
			VariableConversionTable = "variableConversionTable"
		), 
		functionArgumentHierarchy = list(
			VariableConversionTable = list(
				ConversionMethod = "Table"
			), 
			FileName = list(
				ConversionMethod = "PreDefined"
			)
		)
	),
	
	# Acoustic:
	DefineAcousticVariableConversion = list(
		functionType = "processData", 
		functionCategory = "baseline", 
		functionOutputDataType = "AcousticVariableConversion"
	),
	
	ConvertAcousticVariables = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "AcousticData", 
		functionParameterFormat = list(
			VariableConversionTable = "variableConversionTable"
		), 
		functionArgumentHierarchy = list(
			VariableConversionTable = list(
				ConversionMethod = "Table"
			), 
			FileName = list(
				ConversionMethod = "PreDefined"
			)
		)
	),
	
	# StoxAcoustic:
	DefineStoxAcousticVariableConversion = list(
		functionType = "processData", 
		functionCategory = "baseline", 
		functionOutputDataType = "StoxAcousticVariableConversion"
	),
	
	ConvertStoxAcousticVariables = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "StoxAcousticData", 
		functionParameterFormat = list(
			VariableConversionTable = "variableConversionTable"
		), 
		functionArgumentHierarchy = list(
			VariableConversionTable = list(
				ConversionMethod = "Table"
			), 
			FileName = list(
				ConversionMethod = "PreDefined"
			)
		)
	)
)

# Define the process property formats:
#' 
#' @export
#' 
processPropertyFormats <- list(
	filePaths = list(
		title = "The path to one or more files", 
		type = "vector"
		
	), 
	filterExpressionList = list(
		title = "A list of filter expressions, one for each table to filter on", 
		type = "list"
	), 
	variableConversionTable = list(
		title = "Define new values for spcific variables", 
		type = "table", 
		info = data.table::data.table(
			name = c(
				"TableName", 
				"VariableName", 
				"Value", 
				"NewValue"
			), 
			type = c(
				"character",
				"character",
				"character",
				"character"
			)
		)
	)
)

