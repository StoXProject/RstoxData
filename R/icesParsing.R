#' @noRd
initIC <- function(){
  output <- list()
  output$HI <- data.table::data.table(Country=character(), Year=character(), SeasonType=character(), Season=integer(), Fleet=character(), AreaType=character(), FishingArea=character(), DepthRange=character(), 
                                      UnitEffort=character(), Effort=integer(), AreaQualifier=character())
  output$SI <- data.table::data.table(Country=character(), Year=character(), SeasonType=character(), Season=integer(), Fleet=character(), AreaType=character(), FishingArea=character(), DepthRange=character(), 
                                      Species=character(), Stock=character(), CatchCategory=character(), ReportingCategory=character(), DataToFrom=character(), Usage=character(), SamplesOrigin=character(), QualityFlag=character(), UnitCATON=character(), CATON=numeric(), OffLandings=integer(), varCATON=numeric(), InfoFleet=character(), InfoStockCoordinator=character(), InfoGeneral=character())
  output$SD <- data.table::data.table(Country=character(), Year=character(), SeasonType=character(), Season=integer(), Fleet=character(), AreaType=character(), FishingArea=character(), DepthRange=character(), 
                                      Species=character(), Stock=character(), CatchCategory=character(), ReportingCategory=character(), 
                                      Sex=character(), CANUMtype=character(), AgeLength=integer(), PlusGroup=integer(), SampledCatch=integer(), NumSamplesLngt=integer(), NumLngtMeas=integer(), NumSamplesAge=integer(), NumAgeMeas=integer(), unitMeanWeight=character(), unitCANUM=character(), UnitAgeOrLength=character(), UnitMeanLength=character(), Maturity=character(), NumberCaught=numeric(), MeanWeight=numeric(), MeanLength=numeric(), varNumLanded=numeric(), varWgtLanded=numeric(), varLgtLanded=numeric())
  return(output)
}

#' Missing value for Int may be encoded as -9
#' @noRd
parseInt <- function(chr){
  if (is.na(chr) | chr=="-9"){
    return(as.integer(NA))
  }
  else{
    return(as.integer(chr))
  }
}

#' Missing value for Int may be encoded as -9
#' The intercatch manual is a bit ambigious on how missing values for decimal data types should be encoded
#' It contains examples that specify them as -9, and practical tests indicate that -9 is required for some of them.
#' @noRd
parseNumeric <- function(chr){
  if (is.na(chr) | chr=="-9"){
    return(as.numeric(NA))
  }
  else{
    return(as.numeric(chr))
  }
}

#' Adds any missing non-mandatory charachter fields to intercatch tables
#' @param row data.table with parsed headerless rows
#' @param lastNeededPosition last position of the table that is either mandatory or non-character
#' @param headers for the parsed table
#' @noRd
appendFields <- function(row, lastNeededPosition, headers){
  if (ncol(row) != length(headers)){
    if (ncol(row)>length(headers)){
      stop("Malformed InterCatch 1.0 file.")
    }
    if (ncol(row) < lastNeededPosition){
      stop("Malformed InterCatch 1.0 file.")
    }
    row <- cbind(row, rep(NA, length(headers)-ncol(row)))
  }
  return(row)
}

#' @noRd
processHI <- function(vec, output){
  stopifnot(vec[1]=="HI")
  row <- data.table::data.table(t(vec[2:length(vec)]))
  
  row <- appendFields(row, 11, names(output$HI))

  names(row) <- names(output$HI)
  
  #characters may be encoded as NA for missing values
  row[row=="NA"] <- as.character(NA)
  
  #integers may be encoded as -9 for missing values
  row$Season <- parseInt(row$Season)
  row$Effort <- parseInt(row$Effort)

  output$HI <- rbind(output$HI, row)
  return(output)
}

#' @noRd
processSI <- function(vec, output){
  stopifnot(vec[1]=="SI")
  row <- data.table::data.table(t(vec[2:length(vec)]))
  
  row <- appendFields(row, 21, names(output$SI))
  
  names(row) <- names(output$SI)
  
  #characters may be encoded as NA for missing values
  row[row=="NA"] <- NA
  
  #integers may be encoded as -9 for missing values
  row$Season <- parseInt(row$Season)
  row$OffLandings <- parseInt(row$OffLandings)

  #numeric variabes
  row$CATON <- parseNumeric(row$CATON)
  row$varCATON <- parseNumeric(row$varCATON)
  
  output$SI <- rbind(output$SI, row)
  return(output)
}

#' @noRd
processSD <- function(vec, output){
  stopifnot(vec[1]=="SD")
  row <- data.table::data.table(t(vec[2:length(vec)]))
  
  row <- appendFields(row, 33, names(output$SD))
  
  names(row) <- names(output$SD)
  
  #characters may be encoded as NA for missing values
  row[row=="NA"] <- NA
  
  #integers may be encoded as -9 for missing values
  row$Season <- parseInt(row$Season)
  row$AgeLength <- parseInt(row$AgeLength)
  row$PlusGroup <- parseInt(row$PlusGroup)
  row$SampledCatch <- parseInt(row$SampledCatch)
  row$NumSamplesLngt <- parseInt(row$NumSamplesLngt)
  row$NumLngtMeas <- parseInt(row$NumLngtMeas)
  row$NumSamplesAge <- parseInt(row$NumSamplesAge)
  row$NumAgeMeas <- parseInt(row$NumAgeMeas)

  #numeric variables
  row$NumberCaught <- parseNumeric(row$NumberCaught)
  row$MeanWeight <- parseNumeric(row$MeanWeight)
  row$MeanLength <- parseNumeric(row$MeanLength)
  row$varNumLanded <- parseNumeric(row$varNumLanded)
  row$varWgtLanded <- parseNumeric(row$varWgtLanded)
  row$varLgtLanded <- parseNumeric(row$varLgtLanded)
  
  output$SD <- rbind(output$SD, row)
  return(output)
}

#' Parses InterCatch
#' @description 
#'  Parses the InterCatch exchange format v 1.0 for Commercial Catch and Sample Data.
#'  Parses HI,SI and SD recrods
#' @details 
#'  The InterCatch exchange format is a jagged comma-separated format, 
#'  where the number of fields on a line is determined by a record-type identifier in position 1.
#'  Three record types are defined, "HI" (header information), "SI" (species information), and "SD" (species data).
#'  The format it specified on https://ices.dk/data/Documents/Intercatch/IC-ExchangeFormat1-0.pdf.
#'  
#' @param file path to file containing intercatch formatted data
#' @param encoding encoding of 'file'
#' @return named list with three members:
#' \describe{
#'  \item{HI}{\code{\link[data.table]{data.table}} with HI records}
#'  \item{SI}{\code{\link[data.table]{data.table}} with SI records}
#'  \item{SD}{\code{\link[data.table]{data.table}} with SD records}
#' }
#' @importFrom data.table as.data.table
#' @export
parseInterCatch <- function(file, encoding="UTF-8"){
  
  output <- initIC()
  
  stream <- file(file, encoding = encoding)
  lines <- readLines(stream)
  close(stream)
  
  for (l in lines){
    #add and remove trailing field to ensure consistent splitting
    vec <- strsplit(paste(l,"T",sep=","), ",")[[1]]
    vec <- vec[1:length(vec)-1]
    
    if (vec[1] == "HI"){
      output <- processHI(vec, output)
    }
    else if (vec[1] == "SI"){
      output <- processSI(vec, output)
    }
    else if (vec[1] == "SD"){
      output <- processSD(vec, output)
    }
    else if (vec == ""){
      #pass empty lines
    }
    else{
      stop(paste("Record type", vec[1], "not recognized."))
    }
  }
  
  return(output)
}