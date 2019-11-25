# A list of the attributes of the exported StoX functions:
#' 
#' @export
#' 
stoxFunctionAttributes <- list(
    
    # The format describes the actual content, such as catchabilityTable, filePath, filter, etc. These are used by StoX to choose action on these parameters.
    # The primitive type (one of integer, double, logical, character) will be interpreted in the process property functions from the type of the function input or parameter.
    
    # Read input biotic data:
    StoxAcoustic = list(
        functionType = "modelData", 
        functionCategory = "Baseline", 
        functionOutputDataType = "StoxAcousticData", 
        functionParameterFormat = list(FileNames = "filePaths")
    )
)
