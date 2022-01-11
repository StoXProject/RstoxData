#' Function specification for inclusion in StoX projects
#' @export
stoxFunctionAttributes <- list(
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
	
	# Read input biotic data:
	ReadLanding = list(
	  functionType = "modelData", 
	  functionCategory = "baseline", 
	  functionOutputDataType = "LandingData", 
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
	
	# Filter StoxLandingData:
	FilterStoxLanding = list(
	  functionType = "modelData", 
	  functionCategory = "baseline", 
	  functionOutputDataType = "StoxLandingData", 
	  functionParameterFormat = list(FilterExpression = "filterExpressionList")
	),
	
	# Filter LandingData:
	FilterLanding = list(
	  functionType = "modelData", 
	  functionCategory = "baseline", 
	  functionOutputDataType = "LandingData", 
	  functionParameterFormat = list(FilterExpression = "filterExpressionList")
	),
	
	# Convert AcousticData to StoxAcousticData:
	MergeStoxAcoustic = list(
	    functionType = "modelData", 
	    functionCategory = "baseline", 
	    functionOutputDataType = "MergeStoxAcousticData", 
	    functionArgumentHierarchy = list()
	),
	
	# Convert BioticData to StoxBioticData:
	MergeStoxBiotic = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "MergeStoxBioticData", 
		functionArgumentHierarchy = list()
	),
	
	
	##### Define and Convert variables: #####
	# StoxBiotic:
	RedefineStoxBiotic = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "StoxBioticData", 
		functionParameterFormat = list(
			Redefinition = "redefinitionTable"
		)
	),
	
	DefineTranslation = list(
		functionType = "processData", 
		functionCategory = "baseline", 
		functionOutputDataType = "Translation", 
		functionParameterFormat = list(
			TranslationTable = "translationTable", 
			FileName = "filePath"
		), 
		functionArgumentHierarchy = list(
			DefinitionMethod = list(
				UseProcessData = FALSE
			), 
			# These two are joined with AND, and must both be fulfilled:
			TranslationTable = list(
				DefinitionMethod = "Table", 
				UseProcessData = FALSE
			), 
			# These two are joined with AND, and must both be fulfilled:
			FileName = list(
				DefinitionMethod = "ResourceFile", 
				UseProcessData = FALSE
			), 
			# These two are joined with AND, and must both be fulfilled:
			Conditional = list(
				#DefinitionMethod = "TranslationTable", 
				UseProcessData = FALSE
			), 
			# These two are joined with AND, and must both be fulfilled:
			VariableName = list(
				DefinitionMethod = "Table", 
				UseProcessData = FALSE
			), 
			# These two are joined with AND, and must both be fulfilled:
			ValueColumn = list(
				DefinitionMethod = "ResourceFile", 
				UseProcessData = FALSE
			), 
			# These two are joined with AND, and must both be fulfilled:
			NewValueColumn = list(
				DefinitionMethod = "ResourceFile", 
				UseProcessData = FALSE
			), 
			# These two are joined with AND, and must both be fulfilled:
			ConditionalVariableName = list(
				DefinitionMethod = "Table", 
				Conditional = TRUE, 
				UseProcessData = FALSE
			), 
			# These two are joined with AND, and must both be fulfilled:
			ConditionalValueColumn = list(
				DefinitionMethod = "ResourceFile", 
				Conditional = TRUE, 
				UseProcessData = FALSE
			)
		)
	),
	
	TranslateStoxBiotic = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "StoxBioticData"
	),
	
	#ConvertStoxBiotic = list(
	#	functionType = "modelData", 
	#	functionCategory = "baseline", 
	#	functionOutputDataType = "StoxBioticData",
	#	functionParameterFormat = list(
	#		#TargetVariable = "variable_StoxBioticData", 
	#		GruopingVariables = "variables_StoxBioticData", 
	#		Conversion = "conversionTable_StoxBioticData"
	#	)
	#),
	
	AddToStoxBiotic = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "StoxBioticData", 
		functionParameterFormat = list(
			VariableNames = "variableNames_AddToStoxBiotic"
		)
	), 
	
	CompensateEffectiveTowDistanceForFishingDepthCount = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "StoxBioticData"
	),
	
	TranslateStoxAcoustic = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "StoxAcousticData"
	),
	
	#ConvertStoxAcoustic = list(
	#	functionType = "modelData", 
	#	functionCategory = "baseline", 
	#	functionOutputDataType = "StoxAcousticData",
	#	functionParameterFormat = list(
	#		#TargetVariable = "variable_StoxAcousticData", 
	#		GruopingVariables = "variables_StoxAcousticData", 
	#		Conversion = "conversionTable_StoxAcousticData"
	#	)
	#),
	
	
	
	TranslateBiotic = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "BioticData"
	),
	
	#ConvertBiotic = list(
	#	functionType = "modelData", 
	#	functionCategory = "baseline", 
	#	functionOutputDataType = "BioticData",
	#	functionParameterFormat = list(
	#		#TargetVariable = "variable_BioticData", 
	#		GruopingVariables = "variables_BioticData", 
	#		Conversion = "conversionTable_BioticData"
	#	)
	#),
	
	
	TranslateAcoustic = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "AcousticData"
	),
	
	#ConvertAcoustic = list(
	#	functionType = "modelData", 
	#	functionCategory = "baseline", 
	#	functionOutputDataType = "AcousticData",
	#	functionParameterFormat = list(
	#		#TargetVariable = "variable_AcousticData", 
	#		GruopingVariables = "variables_AcousticData", 
	#		Conversion = "conversionTable_AcousticData"
	#	)
	#),
	
	TranslateStoxLanding = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "StoxLandingData"
	),
	
	TranslateLanding = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "LandingData"
	),
	
	
	
	ICESAcoustic = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "ICESAcousticData"
	), 
	ICESBiotic = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "ICESBioticData"
	), 
	ICESDatras = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "ICESDatrasData"
	),
	
	WriteICESAcoustic = list(
		functionType = "modelData", 
		functionCategory = "report", 
		functionOutputDataType = "WriteICESAcousticData"
	), 
	WriteICESBiotic = list(
		functionType = "modelData", 
		functionCategory = "report", 
		functionOutputDataType = "WriteICESBioticData"
	), 
	WriteICESDatras = list(
		functionType = "modelData", 
		functionCategory = "report", 
		functionOutputDataType = "WriteICESDatrasData"
	), 
	
	
	TranslateICESAcoustic = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "ICESAcousticData"
	),
	
	TranslateICESBiotic = list(
		functionType = "modelData", 
		functionCategory = "baseline", 
		functionOutputDataType = "ICESBioticData"
	)
	
)


#getConversionTableFormat <- function(type = c("StoxBiotic", "StoxAcoustic", "Biotic", "Acoustic")) {
#	
#	type <- match.arg(type)
#	
#	conversionTable = list(
#		class = "table", 
#		title = function(ConversionFunction = c("Constant", "Addition", "Scaling", "AdditionAndScaling")) {
#			ConversionFunction <- match.arg(ConversionFunction)
#			
#			if(identical(ConversionFunction, "Constant")) {
#				title <- "Replace variables of StoX data by a constant \"Constant\""
#			}
#			else if(identical(ConversionFunction, "Addition")) {
#				title <- "Add the value \"Addition\" to variables of StoX data"
#			}
#			else if(identical(ConversionFunction, "Scaling")) {
#				title <- "Multiply variables of StoX data by the value \"Scaling\""
#			}
#			else if(identical(ConversionFunction, "AdditionAndScaling")) {
#				title <- "Multiply variables of StoX data by the value \"Scaling\" and add the value \"Addition\""
#			}
#			else {
#				stop("Wrong ConversionFunction.")
#			}
#			
#			return(title)
#		}, 
#		columnNames = function(ConversionFunction = c("Constant", "Addition", "Scaling", "AdditionAndScaling"), GruopingVariables = NULL) {
#			ConversionFunction <- match.arg(ConversionFunction)
#			
#			if(identical(ConversionFunction, "Constant")) {
#				parameters <- "Constant"
#			}
#			else if(identical(ConversionFunction, "Addition")) {
#				parameters <- "Addition"
#			}
#			else if(identical(ConversionFunction, "Scaling")) {
#				parameters <- "Scaling"
#			}
#			else if(identical(ConversionFunction, "AdditionAndScaling")) {
#				parameters <- c("Addition", "Scaling")
#			}
#			else {
#				stop("Wrong ConversionFunction.")
#			}
#			
#			columnNames <- c(
#				GruopingVariables, 
#				#c("TargetVariable", "SourceVariable"), 
#				"SourceVariable", 
#				parameters, 
#				"RoundOffTo"
#			)
#			
#			return(columnNames)
#		}, 
#		variableTypes = function(ConversionFunction = c("Constant", "Addition", "Scaling", "AdditionAndScaling"), GruopingVariables = NULL) #{
#			ConversionFunction <- match.arg(ConversionFunction)
#			
#			if(identical(ConversionFunction, "Constant")) {
#				types <- "double"
#			}
#			else if(identical(ConversionFunction, "Addition")) {
#				types <- "double"
#			}
#			else if(identical(ConversionFunction, "Scaling")) {
#				types <- "double"
#			}
#			else if(identical(ConversionFunction, "AdditionAndScaling")) {
#				types <- c("double", "double")
#			}
#			else {
#				stop("Wrong ConversionFunction.")
#			}
#			
#			variableTypes <- c(
#				rep("character", length(GruopingVariables)), 
#				#c("character", "character"), 
#				"character", 
#				types, 
#				"double"
#			)
#			
#			return(variableTypes)
#		}, 
#		possibleValues = switch(
#			type, 
#			StoxBiotic = function(
#				StoxBioticData, 
#				ConversionFunction = c("Constant", "Addition", "Scaling", "AdditionAndScaling"), 
#				GruopingVariables = NULL
#			) {
#				conversionTableFormatPossibleValuesFunction(
#					data = StoxBioticData, 
#					ConversionFunction = ConversionFunction, 
#					GruopingVariables = GruopingVariables
#				)
#			}, 
#			StoxAcoustic = function(
#				StoxAcousticData, 
#				ConversionFunction = c("Constant", "Addition", "Scaling", "AdditionAndScaling"), 
#				GruopingVariables = NULL
#			) {
#				conversionTableFormatPossibleValuesFunction(
#					data = StoxAcousticData, 
#					ConversionFunction = ConversionFunction, 
#					GruopingVariables = GruopingVariables
#				)
#			}, 
#			Biotic = function(
#				BioticData, 
#				ConversionFunction = c("Constant", "Addition", "Scaling", "AdditionAndScaling"), 
#				GruopingVariables = NULL
#			) {
#				conversionTableFormatPossibleValuesFunction(
#					data = BioticData, 
#					ConversionFunction = ConversionFunction, 
#					GruopingVariables = GruopingVariables
#				)
#			}, 
#			Acoustic = function(
#				AcousticData, 
#				ConversionFunction = c("Constant", "Addition", "Scaling", "AdditionAndScaling"), 
#				GruopingVariables = NULL
#			) {
#				conversionTableFormatPossibleValuesFunction(
#					data = AcousticData, 
#					ConversionFunction = ConversionFunction, 
#					GruopingVariables = GruopingVariables
#				)
#			}
#		)
#	)
#	
#	return(conversionTable)
#}



conversionTableFormatPossibleValuesFunction = function(data, ConversionFunction = c("Constant", "Addition", "Scaling", "AdditionAndScaling"), GruopingVariables = NULL) {
	ConversionFunction <- match.arg(ConversionFunction)
	
	if(identical(ConversionFunction, "Constant")) {
		numTypes <- 1
	}
	else if(identical(ConversionFunction, "Addition")) {
		numTypes <- 1
	}
	else if(identical(ConversionFunction, "Scaling")) {
		numTypes <- 1
	}
	else if(identical(ConversionFunction, "AdditionAndScaling")) {
		numTypes <- 2
	}
	else {
		stop("Wrong ConversionFunction.")
	}
	
	dataFlatList <- unlist(lapply(unname(data), as.list), recursive = FALSE)
	variableNames <- sort(names(dataFlatList))
	possibleValues <- c(
		lapply(GruopingVariables, function(variableMame) sort(unique(dataFlatList[[variableMame]]))), 
		#rep(list(variableNames), 2), 
		list(variableNames), 
		rep(list(list()), numTypes + 1)
	)
	
	return(possibleValues)
}


#' Define the process property formats:
#' 
#' @export
#' 
processPropertyFormats <- list(
	filePath = list(
		class = "single", 
		title = "The path to a single file"
	), 
	filePaths = list(
		class = "vector", 
		title = "The path to one or more files", 
		variableTypes = "character"
	), 
	filterExpressionList = list(
		class = "list", 
		title = "A list of filter expressions, one for each table to filter on"
	), 
	
	redefinitionTable = list(
		class = "table", 
		title = "Define columns of StoX data by columns of the raw data", 
		columnNames = c(
			"VariableName", 
			"ReplaceBy"
		), 
		variableTypes = c(
			"character",
			"character"
		), 
		possibleValues = function(StoxBioticData, BioticData) {
			list(
				sort(unique(unlist(lapply(StoxBioticData, names)))), 
				sort(unique(unlist(lapply(BioticData, function(x) lapply(x, names)))))
			)
		}
	), 
	translationTable = list(
		class = "table", 
		title = "Translate columns of StoX data", 
		columnNames = function(Conditional, VariableName, ConditionalVariableName) {
			columnNames <- c(
				# If VariableName is not given as a parameter, add it to the format:
				if(!length(VariableName) || !nchar(VariableName)) "VariableName", 
				"Value", 
				"NewValue"
			)
			# Add a conditional variable:
			if(Conditional) {
				columnNames <- c(
					# If ConditionalVariableName is not given as a parameter, add it to the format:
					if(!length(ConditionalVariableName) || !nchar(ConditionalVariableName)) "ConditionalVariableName", 
					"ConditionalValue", 
					columnNames
				)
			}
			
			return(columnNames)
		}, 
		variableTypes = function(Conditional) {
			rep("character", 3 + 2 * as.numeric(Conditional))
		}
		#columnNames = c(
		#	"VariableName", 
		#	"Value", 
		#	"NewValue"
		#), 
		#variableTypes = c(
		#	"character",
		#	"character",
		#	"character"
		#)
	), 
	
	
	variable_StoxBioticData = list(
		class = "single", 
		title = "Select a StoxBioticData variable.", 
		variableTypes = "character", 
		possibleValues = function(StoxBioticData) {
			sort(unique(unlist(lapply(StoxBioticData, names))))
		}
	), 
	variable_StoxAcousticData = list(
		class = "single", 
		title = "Select a StoxAcousticData variable.", 
		variableTypes = "character", 
		possibleValues = function(StoxAcousticData) {
			sort(unique(unlist(lapply(StoxAcousticData, names))))
		}
	), 
	variable_BioticData = list(
		class = "single", 
		title = "Select a BioticData variable.", 
		variableTypes = "character", 
		possibleValues = function(BioticData) {
			sort(unique(unlist(lapply(BioticData, names))))
		}
	), 
	variable_AcousticData = list(
		class = "single", 
		title = "Select an AcousticData variable.", 
		variableTypes = "character", 
		possibleValues = function(AcousticData) {
			sort(unique(unlist(lapply(AcousticData, names))))
		}
	), 
	
	variables_StoxBioticData = list(
		class = "vector", 
		title = "Select StoxBioticData variables.", 
		variableTypes = "character", 
		possibleValues = function(StoxBioticData) {
			sort(unique(unlist(lapply(StoxBioticData, names))))
		}
	), 
	variables_StoxAcousticData = list(
		class = "vector", 
		title = "Select StoxAcousticData variables.", 
		variableTypes = "character", 
		possibleValues = function(StoxAcousticData) {
			sort(unique(unlist(lapply(StoxAcousticData, names))))
		}
	), 
	variables_BioticData = list(
		class = "vector", 
		title = "Select BioticData variables.", 
		variableTypes = "character", 
		possibleValues = function(BioticData) {
			sort(unique(unlist(lapply(BioticData, names))))
		}
	), 
	variables_AcousticData = list(
		class = "vector", 
		title = "Select AcousticData variables.", 
		variableTypes = "character", 
		possibleValues = function(AcousticData) {
			sort(unique(unlist(lapply(AcousticData, names))))
		}
	), 
	
	#conversionTable_StoxBioticData = getConversionTableFormat("StoxBiotic"),
	#
	#conversionTable_StoxAcousticData = getConversionTableFormat("StoxAcoustic"),
	#
	#conversionTable_BioticData = getConversionTableFormat("Biotic"),
	#
	#conversionTable_AcousticData = getConversionTableFormat("Acoustic"), 
	
	variableNames_AddToStoxBiotic = list(
		class = "vector", 
		title = "One or more variables to add to the StoxBioticData from BioticData", 
		possibleValues = function(BioticData) {
			sort(unique(unlist(lapply(BioticData, function(x) lapply(x, names)))))
		}, 
		variableTypes = "character"
	)
)





