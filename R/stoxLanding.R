#' Check if argument is LandingData
#' @description 
#'  Checks if argument conforms to specification for \code{\link{LandingData}}
#' @param LandingData argument to be checked for data conformity
#' @return logical, TRUE if argument conformed to specification for \code{\link{LandingData}}
#' @name is.LandingData
#' @export
is.LandingData <- function(LandingData){

  if (!is.list(LandingData)){
    return(FALSE)
  }
  for (l in LandingData){
    if (!is.list(l)){
      return(FALSE)
    }
    if (!("Seddellinje" %in% names(l))){
      return(FALSE)
    }
    if (!data.table::is.data.table(l$Seddellinje)){
      return(FALSE)
    }
    if (!all(c("Dokumentnummer", 
               "Linjenummer", 
               "Art_kode", 
               "Registreringsmerke_seddel", 
               "SisteFangstdato", 
               "Redskap_kode")
             %in% names(l$Seddellinje))){
      return(FALSE)
    } 
  }
  
  return(TRUE)
}

#' Extracts aggregated Landings from NMD - landings (namespace: http://www.imr.no/formats/landinger/v2)
#' @noRd
extractNMDlandingsV2 <- function(LandingData, appendColumns=character(), appendColumnsNames=appendColumns){
  
  flatLandings <- LandingData$Seddellinje
  
  for (part in names(LandingData)[!(names(LandingData) %in% c("Landingsdata", "Seddellinje", "metadata"))]){
    keys <- names(LandingData[[part]])[names(LandingData[[part]]) %in% names(flatLandings)]
    flatLandings <- merge(LandingData[[part]], flatLandings, by=keys)
  }
  
  #
  # Note: if non-character columns are added to aggColumns. Handle accoridngly in NA-aggregation below
  #
  sourceColumns <- c("Art_kode", 
                     "Fangst\u00E5r", 
                     "SisteFangstdato", 
                     "Redskap_kode", 
                     "Hovedomr\u00E5de_kode",
                     "Lokasjon_kode",
                     "KystHav_kode", 
                     "NordS\u00F8rFor62GraderNord", 
                     "St\u00F8rsteLengde", 
                     "Fart\u00F8ynasjonalitet_kode",
                     "Mottaksstasjon",
                     "Mottakernasjonalitet_kode",
                     "HovedgruppeAnvendelse_kode")
  sourceColumns <- c(sourceColumns, appendColumns)
  
  outputColumns <- c( "Species",
                      "Year",
                      "CatchDate",
                      "Gear",
                      "Area",
                      "Location",
                      "Coastal",
                      "N62Code",
                      "VesselLength",
                      "CountryVessel",
                      "LandingSite",
                      "CountryLanding",
                      "Usage")
  outputColumns <- c(outputColumns, appendColumnsNames)
  
  # add NAs for missing columns
  # this is done because the underlaying format is supposed to be generalized to hetergenous formats like StoxBiotic.
  for (col in appendColumns[!(appendColumns %in% names(flatLandings))]){
    flatLandings[[col]] <- NA
  }
  
  flatLandings <- flatLandings[,c(sourceColumns, "Rundvekt"), with=F]
  
  aggList <- list()
  for (i in 1:length(sourceColumns)){
    sourcename <- sourceColumns[i]
    outputname <- outputColumns[i]
    if (outputname == "VesselLength"){
      flatLandings[[outputname]] <- flatLandings[[sourcename]]
      flatLandings[[outputname]][!is.na(flatLandings[[sourcename]])] <- as.character(flatLandings[[sourcename]][!is.na(flatLandings[[sourcename]])])
      if (any(is.na(flatLandings[[outputname]]))){
        flatLandings[[outputname]][is.na(flatLandings[[outputname]])] <- "<NA>" #set NAs to text-string for aggregation  
      }
    }
    
    else if (is.character(flatLandings[[sourcename]]) | is.integer(flatLandings[[sourcename]])){
      flatLandings[[outputname]] <- flatLandings[[sourcename]]  
      flatLandings[[outputname]][is.na(flatLandings[[sourcename]])] <- "<NA>" #set NAs to text-string for aggregation
    }
    else{
      stop(paste("Type of ", sourcename, "must be handled."))
    }
    aggList[[outputname]] <- flatLandings[[outputname]]
  }
  
  names(aggList) <- outputColumns
  aggLandings <- stats::aggregate(list(Rundvekt=flatLandings$Rundvekt), by=aggList, FUN=function(x){sum(x, na.rm=T)})
  aggLandings <- aggLandings[,c(outputColumns, "Rundvekt")]
  
  #reset NAs
  for (aggC in outputColumns){
    if (aggC == "VesselLength"){
      aggLandings[[aggC]][aggLandings[[aggC]] != "<NA>"] <- as.numeric(aggLandings[[aggC]][aggLandings[[aggC]] != "<NA>"])
      aggLandings[[aggC]][aggLandings[[aggC]] == "<NA>"] <- as.numeric(NA)
    }
    else{
      aggLandings[[aggC]][aggLandings[[aggC]] == "<NA>"] <- NA  
    }
    
  }
  aggLandings$RoundWeight <- aggLandings$Rundvekt
  outputColumns <- c(outputColumns, "RoundWeight")
  
  # format conversions
  aggLandings$CatchDate <- as.POSIXct(aggLandings$CatchDate, format="%d.%m.%Y", tzone="UTC")
  
  aggLandings$Year <- as.integer(aggLandings$Year)
  
  return(data.table::as.data.table(aggLandings[,outputColumns]))
}

#' Convert landing data
#' @description
#'  Convert landing data to the aggregated format \code{\link{StoxLandingData}}
#' @details 
#'  All columns that are not the ones aggregated (weight), will be used as aggregation variables.
#' 
#'  Correspondences indicate which field a value is derived from, not necessarily verbatim copied.
#' 
#'  Correspondence to LandingData (http://www.imr.no/formats/landinger/v2):
#'  \describe{
#'   \item{Species}{Art_kode}
#'   \item{Year}{Fangstår}
#'   \item{CatchDate}{SisteFangstdato}
#'   \item{Gear}{Redskap_kode}
#'   \item{Area}{Hovedområde_kode}
#'   \item{Location}{Lokasjon_kode}
#'   \item{Coastal}{KystHav_kode}
#'   \item{N62Code}{NordSørFor62GraderNord}
#'   \item{VesselLength}{StørsteLengde}
#'   \item{CountryVessel}{Fartøynasjonalitet_kode}
#'   \item{LandingSite}{Mottaksstasjon}
#'   \item{CountryLanding}{Landingsnasjon_kode}
#'   \item{Usage}{HovedgruppeAnvendelse_kode}
#'   \item{RoundWeight}{Rundvekt}
#'  }
#'  
#' @param LandingData Sales-notes data. See \code{\link{LandingData}}
#' @return \code{\link{StoxLandingData}}, aggregated landings data.
#' @name StoxLanding
#' @export
StoxLanding <- function(LandingData){
  
  #concatenate LandingData
  LdCat <- LandingData[[1]]
  if (length(LandingData) > 1){
    for (ld in LandingData[2:length(LandingData)]){
      for (n in names(LdCat)){
        LdCat[[n]] <- rbind(LdCat[[n]], ld[[n]])
      }
    }
  }
  
  if (any(duplicated(LdCat$Seddellinje[,c("Registreringsmerke_seddel", "Dokumentnummer", "Linjenummer", "Fangst\u00E5r")]))){
    stop("Duplicates detected in landings")
  }
  
  output <- list()
  landings <- extractNMDlandingsV2(LdCat)
  output$Landing <- landings
  
  return(output)
  
}

#' Check if argument is StoxLandingData
#' @description 
#'  Checks if argument conforms to specification for \code{\link{StoxLandingData}}
#' @param StoxLandingData argument to be checked for data conformity
#' @return logical, TRUE if argument conformed to specification for \code{\link{StoxLandingData}}
#' @name is.StoxLandingData
#' @export
is.StoxLandingData <- function(StoxLandingData){
  
  if (!is.list(StoxLandingData)){
    return(FALSE)
  }
  if (!("Landing") %in% names(StoxLandingData)){
    return(FALSE)
  }
  if (length(StoxLandingData) != 1){
    return(FALSE)
  }

  expected_colums <- c("Species",
                       "Year",
                       "CatchDate",
                       "Gear",
                       "Area",
                       "Location",
                       "Coastal",
                       "N62Code",
                       "VesselLength",
                       "CountryVessel",
                       "LandingSite",
                       "CountryLanding",
                       "Usage",
                       "RoundWeight"
  )
  
  if (!data.table::is.data.table(StoxLandingData$Landing)){
    return(FALSE)
  }
  
  if (!all(expected_colums %in% names(StoxLandingData$Landing))){
    return(FALSE)
  }
  
  return(TRUE)
}
