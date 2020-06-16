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
		functionOutputDataType = "BioticVariableConversion", 
		functionParameterFormat = list(
			FileName = "filePath"
		)
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
			BioticVariableConversion = list(
				ConversionMethod = "PreDefined"
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
		functionOutputDataType = "StoxBioticVariableConversion", 
		functionParameterFormat = list(
			FileName = "filePath"
		)
	),
	
	ConvertStoxBioticVariables = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "StoxBioticData", 
		functionParameterFormat = list(
			VariableConversionTable = "variableConversionTable",
			VariableReplacementTable = "variableReplacementTable"
		), 
		functionArgumentHierarchy = list(
			ConversionMethod = list(
				ConversionType = "Mapping"
			), 
			BioticData = list(
				ConversionType = "Replacement"
			), 
			VariableReplacementTable = list(
				ConversionType = "Replacement"
			), 
			VariableConversionTable = list(
				ConversionMethod = "Table", 
				ConversionType = "Mapping"
			), 
			StoxBioticVariableConversion = list(
				ConversionMethod = "PreDefined", 
				ConversionType = "Mapping"
			), 
			FileName = list(
				ConversionMethod = "PreDefined", 
				ConversionType = "Mapping"
			)
		)
	),
	
	# Acoustic:
	DefineAcousticVariableConversion = list(
		functionType = "processData", 
		functionCategory = "baseline", 
		functionOutputDataType = "AcousticVariableConversion", 
		functionParameterFormat = list(
			FileName = "filePath"
		)
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
			AcousticVariableConversion = list(
				ConversionMethod = "PreDefined"
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
		functionOutputDataType = "StoxAcousticVariableConversion", 
		functionParameterFormat = list(
			FileName = "filePath"
		)
	),
	
	ConvertStoxAcousticVariables = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "StoxAcousticData", 
		functionParameterFormat = list(
			VariableConversionTable = "variableConversionTable",
			VariableReplacementTable = "variableReplacementTable"
		), 
		functionArgumentHierarchy = list(
			VariableConversionTable = list(
				ConversionMethod = "Table"
			), 
			StoxAcousticVariableConversion = list(
				ConversionMethod = "PreDefined"
			), 
			FileName = list(
				ConversionMethod = "PreDefined"
			)
		)
	), 
	
	AddStoxBioticVariables = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "StoxBioticData"#, 
		#functionParameterFormat = list(
		#	VariableNames = "variableNames"
		#)
	)
)

# Define the process property formats:
#' 
#' @export
#' 
processPropertyFormats <- list(
	filePath = list(
		title = "The path to a single file", 
		type = "single"
		
	), 
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
				"VariableName", 
				"Value", 
				"NewValue"
			), 
			type = c(
				"character",
				"character",
				"character"
			)
		)
	), 
	variableReplacementTable = list(
		title = "Repla columns in raw data to replace for spcific variables by", 
		type = "table", 
		info = data.table::data.table(
			name = c(
				"VariableName", 
				"Replacement"
			), 
			type = c(
				"character",
				"character"
			)
		)
	)#, 
	#variableNames = list(
	#	title = "One or more variables to add to the StoxBioticData from BioticData", 
	#	type = "vector"
	#)
)

