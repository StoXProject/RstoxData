#' Function to convert from the VarialbeName-Value-NewValue translation form to the form where variable names are column names.
#' 
#' @param translationList A list of lists of each row of the Translation table, as read in using RstoxFramework:::readProjectDescription(). 
#' 
#' @export
#' 
oldToNewTranslationList <- function(translationList) {
	
	# Reshape only old Translation:
	if(length(translationList) && all(c("VariableName", "Value", "NewValue") %in% names(translationList[[1]]))) {
		
		values <- unique(unlist(lapply(translationList, "[[", "VariableName")))
		
		if(length(values) > 1 ) {
			warning("StoX: The existing Translation process data containis multiple unique values in the Value column. This cannot be converted to the new form of the Translation process data, where only one variable can be defined for translation (but multiple conditional variables), and where the variable names in the data are used as column names in the Translation process data. StoX should still be able to apply the translation, but making changes may break the process or loose the translation infomation.")
		}
		else {
			conditional <- "ConditionalVariableName" %in% names(translationList[[1]])
			if(conditional) {
				translationList <- lapply(
					translationList, 
					function(x) {
						# We only treat one row at the time, so we do not need to extract the first element of x$VariableName:
						out <- list(
							# Use the Value as the first column and ConditionalValue as the last, and add the VariableName and ConditionalVariableName afterwards: 
							x$Value, 
							NewValue = x$NewValue, 
							x$ConditionalValue
						)
						names(out) <- c(x$VariableName, "NewValue", x$ConditionalVariableName)
						
						return(out)
					}
				)
			}
			else {
				translationList <- lapply(
					translationList, 
					function(x) {
						# We only treat one row at the time, so we do not need to extract the first element of x$VariableName:
						out <- list(
							# Use the Value as the first column, and add the VariableName afterwards: 
							x$Value, 
							NewValue = x$NewValue
						)
						names(out) <- c(x$VariableName, "NewValue")
						
						return(out)
					}
				)
			}
		}
	}
	
	return(translationList)	
}

#' Function to translate from old to new Datras field name:
#' 
#' @param x Character: A vector of strings.
#' 
#' @export
#' 
translateDatrasField <- function(x) {
	# These translations are derived from information from ICES in 2025:
	translationDatras <- data.table::as.data.table(
		matrix(
			c("AgePlusGroup", "PlusGr",
			  "AgePreparationMethod", "AgePrepMet",
			  "BottomCurrentDirection", "BotCurDir",
			  "BottomCurrentSpeed", "BotCurSpeed",
			  "BottomDepth", "Depth",
			  "BottomSalinity", "BotSal",
			  "BottomTemperature", "BotTemp",
			  "BycatchSpeciesCode", "BySpecRecCode",
			  "DevelopmentStage", "DevStage",
			  "DoorWeight", "DoorWgt",
			  "GearExceptions", "GearEx",
			  "GeneticSamplingFlag", "GenSamp",
			  "GroundRopeWeight", "WgtGroundRope",
			  "HaulDuration", "HaulDur",
			  "HaulLatitude", "HaulLat",
			  "HaulLongitude", "HaulLong",
			  "HaulNumber", "HaulNo",
			  "HaulValidity", "HaulVal",
			  "HydrographicStationID", "HydroStNo",
			  "IndividualAge", "AgeRings",
			  "IndividualMaturity", "Maturity",
			  "IndividualSex", "Sex",
			  "IndividualWeight", "IndWgt",
			  "KiteArea", "KiteDim",
			  "LengthClass", "LngtClass",
			  "LengthCode", "LngtCode",
			  "LengthType", "LenMeasType",
			  "NumberAtLength", "HLNoAtLngt",
			  "OtolithGrading", "OtGrading",
			  "ParasiteSamplingFlag", "ParSamp",
			  "PelagicSamplingType", "PelSampType",
			  "Platform", "Ship",
			  "ShootLatitude", "ShootLat",
			  "ShootLongitude", "ShootLong",
			  "SpeciesCategory", "CatIdentifier",
			  "SpeciesCategoryWeight", "CatCatchWgt",
			  "SpeciesCode", "SpecCode",
			  "SpeciesCodeType", "SpecCodeType",
			  "SpeciesSex", "Sex",
			  "SpeciesValidity", "SpecVal",
			  "SpeedGround", "GroundSpeed",
			  "StandardSpeciesCode", "StdSpecRecCode",
			  "StartTime", "TimeShot",
			  "StationName", "StNo",
			  "StatisticalRectangle", "StatRec",
			  "StomachSamplingFlag", "StomSamp",
			  "SubsampledNumber", "NoMeas",
			  "SubsampleWeight", "SubWgt",
			  "SubsamplingFactor", "SubFactor",
			  "SurfaceCurrentDirection", "SurCurDir",
			  "SurfaceCurrentSpeed", "SurCurSpeed",
			  "SurfaceSalinity", "SurSal",
			  "SurfaceTemperature", "SurTemp",
			  "SweepLength", "SweepLngt",
			  "SwellDirection", "SwellDir",
			  "ThermoClineDepth", "ThClineDepth",
			  "TotalNumber", "TotalNo",
			  "TowDirection", "TowDir",
			  "WarpDensity", "WarpDen",
			  "WarpDiameter", "Warpdia",
			  "WarpLength", "Warplngt",
			  "WindDirection", "WindDir"), 
			ncol = 2, 
			byrow = TRUE
		)
	)
	names(translationDatras) <- c("FieldName", "FieldNameOld")
	
	out <- x
	
	# In addition translate whole words, such as in filter expressions. We use double brackets here to support lists, which is the case for filters, where the different tables are represented as named list elements:
	# Loop through the strings to translate:
	for(ind1 in seq_along(x)) {
		# Loop through the rows of the translation table:
		for(ind2 in seq_along(translationDatras$FieldNameOld)) {
			# Find whole words:
			wholeWord <- paste0("\\b", translationDatras$FieldNameOld[[ind2]], "\\b")
			keyWord <- translationDatras$FieldNameOld[[ind2]]
			
			
			# If we find the whole word in the output, translate in the output:
			if(grepl(wholeWord, out[[ind1]])) {
				out[[ind1]] <- gsub(wholeWord, translationDatras$FieldName[[ind2]], out[[ind1]])
			}
		}
	}
	
	
	return(out)
}


#' Backward compabitibility actions:
#' @export
backwardCompatibility_RstoxData <- list(
	
	addParameter = list(
		list(
			changeVersion = "1.1.5", 
			functionName = "DefineTranslation", 
			modelName = "baseline", 
			parameterName = "Conditional", 
			parameterValue = FALSE
		), 
		list(
			changeVersion = "1.2.11", 
			functionName = "DefineTranslation", 
			modelName = "baseline", 
			parameterName = "ValueColumn", 
			parameterValue = "Value"
		), 
		list(
			changeVersion = "1.2.11", 
			functionName = "DefineTranslation", 
			modelName = "baseline", 
			parameterName = "NewValueColumn", 
			parameterValue = "NewValue"
		), 
		list(
			changeVersion = "1.2.17", 
			functionName = "DefineTranslation", 
			modelName = "baseline", 
			parameterName = "VariableName", 
			parameterValue = "VariableName"
		), 
		list(
			changeVersion = "1.2.17", 
			functionName = "DefineTranslation", 
			modelName = "baseline", 
			parameterName = "ConditionalValueColumn", 
			parameterValue = "ConditionalValue"
		), 
		list(
			changeVersion = "1.2.17", 
			functionName = "DefineTranslation", 
			modelName = "baseline", 
			parameterName = "ConditionalVariableName", 
			parameterValue = "ConditionalVariableName"
		), 
		list(
			changeVersion = "1.5.9", 
			functionName = "TranslateBiotic", 
			modelName = "baseline", 
			parameterName = "TranslationDefinition", 
			parameterValue = "FunctionInput"
		), 
		list(
			changeVersion = "1.5.9", 
			functionName = "TranslateAcoustic", 
			modelName = "baseline", 
			parameterName = "TranslationDefinition", 
			parameterValue = "FunctionInput"
		), 
		list(
			changeVersion = "1.5.9", 
			functionName = "TranslateLanding", 
			modelName = "baseline", 
			parameterName = "TranslationDefinition", 
			parameterValue = "FunctionInput"
		), 
		list(
			changeVersion = "1.5.9", 
			functionName = "TranslateStoxBiotic", 
			modelName = "baseline", 
			parameterName = "TranslationDefinition", 
			parameterValue = "FunctionInput"
		), 
		list(
			changeVersion = "1.5.9", 
			functionName = "TranslateStoxAcoustic", 
			modelName = "baseline", 
			parameterName = "TranslationDefinition", 
			parameterValue = "FunctionInput"
		), 
		list(
			changeVersion = "1.5.9", 
			functionName = "TranslateStoxLanding", 
			modelName = "baseline", 
			parameterName = "TranslationDefinition", 
			parameterValue = "FunctionInput"
		), 
		list(
			changeVersion = "1.5.9", 
			functionName = "TranslateICESAcoustic", 
			modelName = "baseline", 
			parameterName = "TranslationDefinition", 
			parameterValue = "FunctionInput"
		), 
		list(
			changeVersion = "1.5.9", 
			functionName = "TranslateICESBiotic", 
			modelName = "baseline", 
			parameterName = "TranslationDefinition", 
			parameterValue = "FunctionInput"
		), 
		list(
			changeVersion = "1.5.9", 
			functionName = "TranslateICESDatras", 
			modelName = "baseline", 
			parameterName = "TranslationDefinition", 
			parameterValue = "FunctionInput"
		), 
		list(
			changeVersion = "1.5.17", 
			functionName = "AddToStoxBiotic", 
			modelName = "baseline", 
			parameterName = "AddToLowestTable", 
			parameterValue = FALSE
		), 
		list(
			changeVersion = "1.6.1", 
			functionName = "AddToStoxBiotic", 
			modelName = "baseline", 
			parameterName = "SplitTableAllocation", 
			parameterValue = "Default"
		), 
		list(
			changeVersion = "1.9.0-9001", 
			functionName = "RegroupLengthICESDatras", 
			modelName = "baseline", 
			parameterName = "RegroupMethod", 
			parameterValue = "HighestResolution"
		), 
		list(
			changeVersion = "2.2.0-9001", 
			functionName = "RedefineStoxBiotic", 
			modelName = "baseline", 
			parameterName = "SplitTableAllocation", 
			parameterValue = "Default"
		)
	), 
	
	renameFunction = list(
		list(
			changeVersion = "1.0.23", 
			functionName = "ICESAcousticCSV", 
			modelName = "baseline", 
			newFunctionName = "RstoxData::ICESAcoustic"
		), 
		list(
			changeVersion = "1.0.23", 
			functionName = "ICESBioticCSV", 
			modelName = "baseline", 
			newFunctionName = "RstoxData::ICESBiotic"
		), 
		list(
			changeVersion = "1.0.24", 
			functionName = "DefineStoxBioticTranslation", 
			modelName = "baseline", 
			newFunctionName = "RstoxData::DefineTranslation"
		), 
		# Renamed from Report to Write for the ICES export functions:
		list(
			changeVersion = "1.1.6", 
			functionName = "ReportICESAcoustic", 
			modelName = "report", 
			newFunctionName = "RstoxData::WriteICESAcoustic"
		), 
		list(
			changeVersion = "1.1.6", 
			functionName = "ReportICESBiotic", 
			modelName = "report", 
			newFunctionName = "RstoxData::WriteICESBiotic"
		), 
		list(
			changeVersion = "1.1.6", 
			functionName = "ReportICESDatras", 
			modelName = "report", 
			newFunctionName = "RstoxData::WriteICESDatras"
		)
	), 
	
	removeParameter = list(
		list(
			changeVersion = "1.0.18", 
			functionName = "ReadBiotic", 
			modelName = "baseline", 
			parameterName = "NumberOfCores"
		), 
		list(
			changeVersion = "1.0.18", 
			functionName = "ReadAcoustic", 
			modelName = "baseline", 
			parameterName = "NumberOfCores"
		), 
		list(
			changeVersion = "1.0.18", 
			functionName = "StoxBiotic", 
			modelName = "baseline", 
			parameterName = "NumberOfCores"
		), 
		list(
			changeVersion = "1.0.18", 
			functionName = "StoxAcoustic", 
			modelName = "baseline", 
			parameterName = "NumberOfCores"
		), 
		list(
			changeVersion = "1.0.18", 
			functionName = "AddToStoxBiotic", 
			modelName = "baseline", 
			parameterName = "NumberOfCores"
		), 
		list(
			changeVersion = "1.0.20", 
			functionName = "ICESDatras", 
			modelName = "baseline", 
			parameterName = "SurveyName"
		), 
		list(
			changeVersion = "1.0.23", 
			functionName = "ICESDatras", 
			modelName = "baseline", 
			parameterName = "AddStationType"
		), 
		list(
			changeVersion = "1.0.24", 
			functionName = "TranslateStoxBiotic", 
			modelName = "baseline", 
			parameterName = "Translation"
		), 
		list(
			changeVersion = "1.0.24", 
			functionName = "TranslateStoxBiotic", 
			modelName = "baseline", 
			parameterName = "TranslationDefinition"
		), 
		list(
			changeVersion = "1.6.1", 
			functionName = "AddToStoxBiotic", 
			modelName = "baseline", 
			parameterName = "AddToLowestTable"
		), 
		list(
			changeVersion = "2.2.0-9005", 
			functionName = "ICESBiotic", 
			modelName = "baseline", 
			parameterName = "AllowRemoveSpecies"
		)
	),  
	
	renameParameter = list(
		list(
			changeVersion = "1.0.24", 
			functionName = "TranslateStoxBiotic", 
			modelName = "baseline", 
			parameterName = "StoxBioticTranslation",
			newParameterName = "Translation"
		), 
		list(
			changeVersion = "1.0.24", 
			functionName = "DefineTranslation", 
			modelName = "baseline", 
			parameterName = "Translation",
			newParameterName = "TranslationTable"
		), 
		list(
			changeVersion = "1.5.7", 
			functionName = "DefineTranslation", 
			modelName = "baseline", 
			parameterName = "ConditionalVariableName",
			newParameterName = "ConditionalVariableNames"
		), 
		list(
			changeVersion = "1.5.7", 
			functionName = "DefineTranslation", 
			modelName = "baseline", 
			parameterName = "ConditionalValueColumn",
			newParameterName = "ConditionalValueColumns"
		), 
		list(
			changeVersion = "1.9.0-9001", 
			functionName = "RegroupLengthICESDatras", 
			modelName = "baseline", 
			parameterName = "AggregationVariables",
			newParameterName = "AggregationVariablesHL"
		)
	),  
	
	translateParameter = list(
		list(
			changeVersion = "1.0.24", 
			functionName = "DefineTranslation", 
			modelName = "baseline", 
			parameterName = "DefinitionMethod", 
			value = "Table", 
			newValue = "TranslationTable"
		), 
		list(
			changeVersion = "1.3.1", 
			functionName = "DefineTranslation", 
			modelName = "baseline", 
			parameterName = "DefinitionMethod", 
			value = "TranslationTable", 
			newValue = "Table"
		), 
		list(
			changeVersion = "1.5.7", 
			functionName = "DefineTranslation", 
			modelName = "baseline", 
			parameterName = "VariableName",
			value = list(
				list(), 
				"VariableName"
			), # Empty value was allowed in 3.3.0, implying that VariableName was given in the file to read, but will no longer be allowed.
			newValue = function(projectDescription, modelName, processIndex) {
				projectDescription[[modelName]][[processIndex]]$processData$Translation[[1]]$VariableName
			}
		), 
		list(
			changeVersion = "1.5.7", 
			functionName = "DefineTranslation", 
			modelName = "baseline", 
			parameterName = "ConditionalVariableNames",
			# Multiple values must be given in a list!!! Also if only :
			value = list(
				list(), 
				"ConditionalVariableName"
			), # Empty value was allowed in 3.3.0, implying that ConditionalVariableNames was given in the file to read, but will no longer be allowed.
			newValue = function(projectDescription, modelName, processIndex) {
				if(isTRUE(projectDescription[[modelName]][[processIndex]]$functionParameters$Conditional)) {
					# Has not yet been renamed from ConditionalVariableName to ConditionalVariableNames as per the order defiend in RstoxFramework::getRstoxFrameworkDefinitions("backwardCompatibilityActionNames"):
					projectDescription[[modelName]][[processIndex]]$processData$Translation[[1]]$ConditionalVariableName
				}
				else {
					list()
				}
			}
		)
	), 
	
	reshapeParameter = list(
		list(
			changeVersion = "1.5.7", 
			functionName = "DefineTranslation", 
			modelName = "baseline", 
			parameterName = "TranslationTable",
			newValue = function(projectDescription, modelName, processIndex) {
				# Return the reshaped parameter:
				RstoxData::oldToNewTranslationList(projectDescription[[modelName]][[processIndex]]$functionParameters$TranslationTable)
			}
		), 
		########## Changes in the Datras format in 2025: ##########
		# The following functions are affected by the ICESDatras format:
		# CopyICESDatras
		# FilterICESDatras
		# ICESDatras (has no arguments that can contain names of ICESDatras variables, but has been fundamentaly changed to support the new format)
		# RegroupLengthICESDatras
		# TranslateICESDatras
		# WriteICESDatras (has no arguments that can contain names of ICESDatras variables)
		# 
		# CopyICESDatras
		list(
			changeVersion = "2.2.0-9001", 
			functionName = "CopyICESDatras", 
			modelName = "baseline", 
			parameterName = "FromVariable",
			newValue = function(projectDescription, modelName, processIndex) {
				# Translate the fields:
				translateDatrasField(projectDescription[[modelName]][[processIndex]]$functionParameters$FromVariable)
			}
		), 
		list(
			changeVersion = "2.2.0-9001", 
			functionName = "CopyICESDatras", 
			modelName = "baseline", 
			parameterName = "ToVariable",
			newValue = function(projectDescription, modelName, processIndex) {
				# Translate the fields:
				translateDatrasField(projectDescription[[modelName]][[processIndex]]$functionParameters$ToVariable)
			}
		),
		# FilterICESDatras
		list(
			changeVersion = "2.2.0-9001", 
			functionName = "FilterICESDatras", 
			modelName = "baseline", 
			parameterName = "FilterExpression",
			newValue = function(projectDescription, modelName, processIndex) {
				# Translate the fields:
				translateDatrasField(projectDescription[[modelName]][[processIndex]]$functionParameters$FilterExpression)
			}
		), 
		# RegroupLengthICESDatras
		list(
			changeVersion = "2.2.0-9001", 
			functionName = "RegroupLengthICESDatras", 
			modelName = "baseline", 
			parameterName = "ResolutionTableVariables",
			newValue = function(projectDescription, modelName, processIndex) {
				# Translate the fields:
				translateDatrasField(projectDescription[[modelName]][[processIndex]]$functionParameters$ResolutionTableVariables)
			}
		), 
		list(
			changeVersion = "2.2.0-9001", 
			functionName = "RegroupLengthICESDatras", 
			modelName = "baseline", 
			parameterName = "ResolutionTable",
			newValue = function(projectDescription, modelName, processIndex) {
				# The ResolutionTable is a list of lists at the time that the backwards compatibility acitons are applied, so we convert to data.table, which will be ok when formatting the parameter afterwards:
				table <- data.table::rbindlist(projectDescription[[modelName]][[processIndex]]$functionParameters$ResolutionTable)
				
				# Translate the column names:
				newColNames <- translateDatrasField(names(table))
				data.table::setnames(table, names(table), newColNames)
				
				return(table)
			}
		), 
		list(
			changeVersion = "2.2.0-9001", 
			functionName = "RegroupLengthICESDatras", 
			modelName = "baseline", 
			parameterName = "AggregationVariablesHL",
			newValue = function(projectDescription, modelName, processIndex) {
				# Translate the fields:
				translateDatrasField(projectDescription[[modelName]][[processIndex]]$functionParameters$AggregationVariablesHL)
			}
		), 
		list(
			changeVersion = "2.2.0-9001", 
			functionName = "RegroupLengthICESDatras", 
			modelName = "baseline", 
			parameterName = "AggregationVariablesCA",
			newValue = function(projectDescription, modelName, processIndex) {
				# Translate the fields:
				translateDatrasField(projectDescription[[modelName]][[processIndex]]$functionParameters$AggregationVariablesCA)
			}
		), 
		# TranslateICESDatras
		list(
			changeVersion = "2.2.0-9001", 
			functionName = "TranslateICESDatras", 
			modelName = "baseline", 
			parameterName = "VariableName",
			newValue = function(projectDescription, modelName, processIndex) {
				# Translate the fields:
				translateDatrasField(projectDescription[[modelName]][[processIndex]]$functionParameters$VariableName)
			}
		), 
		list(
			changeVersion = "2.2.0-9001", 
			functionName = "TranslateICESDatras", 
			modelName = "baseline", 
			parameterName = "ConditionalVariableNames",
			newValue = function(projectDescription, modelName, processIndex) {
				# Translate the fields:
				translateDatrasField(projectDescription[[modelName]][[processIndex]]$functionParameters$ConditionalVariableNames)
			}
		), 
		list(
			changeVersion = "2.2.0-9001", 
			functionName = "TranslateICESDatras", 
			modelName = "baseline", 
			parameterName = "TranslationTable",
			newValue = function(projectDescription, modelName, processIndex) {
				# The TranslationTable is a list of lists at the time that the backwards compatibility acitons are applied, so we convert to data.table, which will be ok when formatting the parameter afterwards:
				table <- data.table::rbindlist(projectDescription[[modelName]][[processIndex]]$functionParameters$TranslationTable)
				
				# Translate the column names:
				newColNames <- translateDatrasField(names(table))
				data.table::setnames(table, names(table), newColNames)
				
				# Translate also the values:
				for(ind in seq_along(table)) {
					table[[ind]] <-  translateDatrasField(table[[ind]])
				}
				
				return(table)
			}
		)
	), 
	
	renameProcessData = list(
		list(
			changeVersion = "1.0.24", 
			functionName = "DefineTranslation", 
			modelName = "baseline", 
			processDataName = "StoxBioticTranslation",
			newProcessDataName = "Translation"
		)
	), 
	
	reshapeProcessData = list(
		list(
			changeVersion = "1.5.7", 
			functionName = "DefineTranslation", 
			modelName = "baseline", 
			processDataName = "Translation",
			newProcessData = function(projectDescription, modelName, processIndex) {
				RstoxData::oldToNewTranslationList(projectDescription[[modelName]][[processIndex]]$processData$Translation)
			}
		)
	), 
	
	splitProcess = list(
		# Split ICESBiotic process into one ICESBiotic and one FilterICESBiotic process:
		list(
			changeVersion = "2.2.0-9005", 
			functionName = "ICESBiotic", 
			modelName = "baseline", 
			newProcesses = function(projectDescription, modelName, processIndex) {
				
				# Get the process name of the filter process, hopefully not used elsewhere in the project:	
				processNameOfSecondProcess <- "FilterICESBiotic_KeepOnlyICESSpecWoRMS"
				
				allProcessNames <- unlist(lapply(projectDescription, function(model) lapply(model, "[[", "processName")))
				if(any(processNameOfSecondProcess %in% allProcessNames)) {
					stop("Cannot split process...")
				}
				
				# Create the filter expression:
				# Read the valid species codes from ICES:
				xmlRaw <- xml2::read_xml("https://acoustic.ices.dk/Services/Schema/XML/SpecWoRMS.xml")
				validCodes <- xml2::xml_text(xml2::xml_find_all(xmlRaw, "//Code//Key"))
				
				# Create a filter expression where only the species listed by ICES are kept (at the Catch level and subsequently at the Biology level). This expression should be pasted into the FilterExpression parameter of a FilterICESBiotic process:
				CatchFilterexpression <- paste(
					"CatchSpeciesCode", 
					"%in%", 
					paste(deparse(validCodes, 200), collapse = "")
				)
				
				# Create a list of the two processes:
				output <- list(
					# The first process unchanged:
					projectDescription[[modelName]][[processIndex]], 
					# The new filter process:
					list(
						processName = processNameOfSecondProcess, 
						functionName = "FilterICESBiotic", 
						functionInputs = list(
							ICESBioticData = projectDescription[[modelName]][[processIndex]]$processName
						), 
						functionParameters = list(
							FilterExpression = list(
								Catch = CatchFilterexpression
							)
						), 
						processParameters = list(
							enabled = TRUE, 
							showInMap = TRUE,
							fileOutput = TRUE
						),
						processData = list()
					)
				)
				
				names(output) <- c("ICESBiotic", processNameOfSecondProcess)
				
				return(output)
			}
		)
	)
	
)
