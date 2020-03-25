#' Convert AcousticData to StoxAcousticData
#'
#' @param AcousticData A list of acoustic data (StoX data type \code{\link{AcousticData}}), one element for each input acoustic file.
#' @param cores Overrides multi-core auto detection. Default to NULL.
#'
#' @return An object of StoX data type \code{\link{StoxAcousticData}}.
#'
#' @export
#' 
StoxAcoustic <- function(AcousticData, cores = NULL){
    
	
	## For flexibility accept a list of the input data, named by the data type:
	#if(is.list(AcousticData) && "AcousticData" %in% names(AcousticData)) {
	#	AcousticData <- AcousticData$AcousticData
	#}
    StoxAcousticOne <- function(data_list) {

    useXsd  <- data_list$metadata$useXsd
    if(useXsd == 'nmdechosounderv1'){
      ices_format <- FALSE
    } else if(useXsd == 'icesAcoustic') {
      ices_format <- TRUE
    } else {
      print("Unsupported input for StoxAcoustic")
      return(NULL)
    }

    if(ices_format==FALSE){
      #################################################################
      # Description: protocol to convert NMDacoustic to StoxAcoustic  #
      #################################################################
      
    	
      
      
      
      
      
      #################################################################
      #                       RENAME general level                    #
      #################################################################
      names(data_list)[names(data_list)=='echosounder_dataset']<- 'Cruise'
      names(data_list)[names(data_list)=='distance']<- 'Log'
      names(data_list)[names(data_list)=='frequency']<- 'Beam'
      names(data_list)[names(data_list)=='sa_by_acocat']<- 'AcousticCategory'
      names(data_list)[names(data_list)=='ch_type']<- 'ChannelReference'
      names(data_list)[names(data_list)=='sa']<- 'NASC'
      
      
      
      
      
      
      
      #################################################################
      #   Reorder the leevels to AcousticCategory > ChannelReference  #
      #################################################################
      merged <- merge(data_list$ChannelReference, data_list$AcousticCategory)
      data_list$AcousticCategory <- unique(merged[, !"type"])
      data_list$ChannelReference <- unique(merged)
      #data_list$AcousticCategory$type=NULL
      #ul <- (unique(data_list$AcousticCategory))
      #mm <- merge(data_list$ChannelReference, data_list$AcousticCategory)
  #
      ##Make new list structure    
      #data_list$AcousticCategory<-ul
      #data_list$ChannelReference<-mm
      
      
      
      
      
      
      #################################################################
      #        Add cruice key to all list                             #
      #################################################################
      data_list$Cruise$CruiseKey           <- data_list$Cruise$cruise
      data_list$Log$CruiseKey              <- data_list$Cruise$cruise
      data_list$Beam$CruiseKey             <- data_list$Cruise$cruise
      data_list$AcousticCategory$CruiseKey <- data_list$Cruise$cruise
      data_list$ChannelReference$CruiseKey <- data_list$Cruise$cruise
      data_list$NASC$CruiseKey             <- data_list$Cruise$cruise
      
      
      
      
      
      
      
      
      
      #################################################################
      #         Add Keys                                              #
      #################################################################
      data_list$Log[, LogKey:= paste0(gsub(' ','T',start_time),'.000Z')]
      data_list$Beam[, LogKey:= paste0(gsub(' ','T',start_time),'.000Z')]
      data_list$AcousticCategory[, LogKey:= paste0(gsub(' ','T',start_time),'.000Z')]
      data_list$ChannelReference[, LogKey:= paste0(gsub(' ','T',start_time),'.000Z')]
      data_list$NASC[, LogKey:= paste0(gsub(' ','T',start_time),'.000Z')]
      
      
      
      
      
      
      
      
      #################################################################
      #            Add BeamKey to all list                            #
      #################################################################
      data_list$Beam[, BeamKey := paste(freq, transceiver, sep='/')]
      data_list$AcousticCategory[, BeamKey := paste(freq, transceiver, sep='/')]
      data_list$ChannelReference[, BeamKey := paste(freq, transceiver, sep='/')]
      data_list$NASC[, BeamKey := paste(freq, transceiver, sep='/')]
      
      
      
      
      
      
      
      #################################################################
      #        Add AcousticCategoryKey to all list                    #
      #################################################################
      data_list$AcousticCategory$AcousticCategoryKey  <- data_list$AcousticCategory$acocat
      data_list$ChannelReference$AcousticCategoryKey  <- data_list$ChannelReference$acocat
      data_list$NASC$AcousticCategoryKey              <- data_list$NASC$acocat
      
      
      
      
      
      
      
      
      #################################################################
      #        Add ChannelReferenceKey to all list                    #
      #################################################################
      data_list$ChannelReference$ChannelReferenceKey  <- data_list$ChannelReference$type
      data_list$NASC$ChannelReferenceKey              <- data_list$NASC$type
      
      
      
      
      
      
      
      
      ##############################################################
      #              Add NASCKey to all list                       #
      ##############################################################
      data_list$NASC$NASCKey           <- data_list$NASC$ch
      
      
      
      
      
      
      
      
      
      #################################################################
      #                       RENAME cruise level                     #
      #################################################################
      names(data_list$Cruise)[names(data_list$Cruise)=='platform'] <- 'Platform'
      
      
      
      
      
      
      
      
      #################################################################
      #                       RENAME LOG level                        #
      #################################################################
      names(data_list$Log)[names(data_list$Log)=='log_start']        <- 'Log'
      names(data_list$Log)[names(data_list$Log)=='integrator_dist'] <- 'LogDistance'
      names(data_list$Log)[names(data_list$Log)=='lon_start']       <- 'Longitude'
      names(data_list$Log)[names(data_list$Log)=='lat_start']       <- 'Latitude'
      names(data_list$Log)[names(data_list$Log)=='lon_stop']       <- 'Longitude2'
      names(data_list$Log)[names(data_list$Log)=='lat_stop']       <- 'Latitude2'
      
      
      #################################################################
      #                 ADD info in  LOG level                        #
      #################################################################
      # 2020-02-03: Removed bottom depth, as we need to decide whether to include this or not, given its arbitrary interpretation for different frequencies:
      #data_list$Beam$BottomDepth<-rowMeans(cbind(data_list$Beam$min_bot_depth,data_list$Beam$max_bot_depth),na.rm = TRUE)
      #tmp2 <- data_list$Beam[,c('LogKey','BeamKey','BottomDepth','upper_integrator_depth')]
      tmp2 <- data_list$Beam[,c('LogKey','BeamKey', 'upper_integrator_depth')]
      data_list$Log <- merge(data_list$Log,tmp2,by='LogKey')
      
      data_list$Log[, EDSU := paste(data_list$Cruise$CruiseKey, LogKey, sep='/')]
      
      data_list$Log[, DateTime:= paste0(gsub(' ','T',start_time),'.000Z')]
      
      data_list$Log$LogOrigin <- "start"
      
      data_list$Log$LogOrigin2 <- "end"
      
      data_list$Log$LogDuration <- as.numeric(
          as.POSIXct(data_list$Log$stop_time, format="%Y-%m-%d %H:%M:%S") - 
        as.POSIXct(data_list$Log$start_time, format="%Y-%m-%d %H:%M:%S"), 
        units ="secs"
    )
      
      # Add NA as BottomDepth, since bottom depth in NMDEchosounder1 is defined as a start and stop value per frequency, and not one single value per Log, as in ICESAcoustic and as intended in StoxAcoustic. We choose to set these as NA and rather wait for any requests on the BottomDepth, which will call for a decision on how to interpret the bottom depth information in NMDEchosounder1 (confronting LSSS etc.):
      data_list$Log$BottomDepth <- NA
      
      
      
      
      #################################################################
      #                       RENAME Frequency level                  #
      #################################################################
      names(data_list$Beam)[names(data_list$Beam)=='freq'] <- 'Frequency'
    
      
      
      
      
      
      
      #################################################################
      #                       RENAME AcousticCatecory level           #
      #################################################################
      data_list$AcousticCategory$AcousticCategory <- data_list$AcousticCategory$AcousticCategoryKey
      
      
      
      
      
      #################################################################
      #                       RENAME ChannelReference level           #
      #################################################################
      data_list$ChannelReference$ChannelReference <- data_list$ChannelReference$ChannelReferenceKey
      
      data_list$ChannelReference$ChannelReferenceType <- data_list$ChannelReference$type
      data_list$ChannelReference$ChannelReferenceDepth <- ifelse(data_list$ChannelReference$ChannelReferenceType == "P", 0, NA) # Hard coded to the surface for pelagic channels ("P") of the LUF20, and NA for bottom channels ("B"):
      data_list$ChannelReference$ChannelReferenceOrientation <- ifelse(data_list$ChannelReference$ChannelReferenceType == "P", 180, 0) # Hard coded to vertically downwards for pelagic channels ("P") of the LUF20, and vvertically upwards for bottom channels ("B"):
      
      
      
      #################################################################
      #                RENAME NASC level                              #
      #################################################################
      names(data_list$NASC)[names(data_list$NASC)=='sa'] <- 'NASC'
      data_list$NASC$Channel <- data_list$NASC$ch
      
      
      
      
      
      
      
      #################################################################
      #                Adding depth to list                           #
      #################################################################
      #This info is not avaliable in nmd echosounder, and is hacked here
      #
      #TODO: 
      #     - Do stuff to the bottom mode
      #temp <- merge(data_list$Beam[,c('upper_integrator_depth','LogKey')],data_list$Log[,c('pel_ch_thickness','LogKey')],by='LogKey')
      data_list$NASC <- merge(data_list$NASC, data_list$Log[,c('upper_integrator_depth','pel_ch_thickness','LogKey','BeamKey')],by=c('LogKey','BeamKey'))
      
      
      data_list$NASC$MinChannelRange <- data_list$NASC$pel_ch_thickness * (as.integer(data_list$NASC$ch) - 1)
      data_list$NASC$MaxChannelRange <- data_list$NASC$pel_ch_thickness * as.integer(data_list$NASC$ch)
      
      
      
      
      #Fiks upper integration depth for pelagic
      #Denne maa Dobbelsjekkes
      data_list$NASC[(data_list$NASC$MinChannelRange<data_list$NASC$upper_integrator_depth)&(data_list$NASC$ChannelReferenceKey=='P'),]$MinChannelRange<-data_list$NASC[(data_list$NASC$MinChannelRange<data_list$NASC$upper_integrator_depth)&(data_list$NASC$ChannelReferenceKey=='P'),]$upper_integrator_depth
      
      
      
      data_list$ChannelReference[data_list$ChannelReference$ChannelReferenceKey=='B']
      
      
      
      # Temporary change class of the Longitude2 and Latitude2 to double, due to error in the xsd:
      data_list$Log$Latitude2 <- as.double(data_list$Log$Latitude2)
      data_list$Log$Longitude2 <- as.double(data_list$Log$Longitude2)
      
      
      
      
    }else{
      
      #################################################################
      # Description: protocol to convert ICESacoustic to StoxAcoustic #
      #################################################################
      
      
      #################################################################
      #                       RENAME general level                    #
      #################################################################
      names(data_list)[names(data_list)=='Data']<- 'NASC'
      
      
      
      
      
      
      #################################################################
      #         Fiks to correct time format, and add to key           #
      #################################################################
      data_list$Log[, LogKey:= paste0(gsub(' ','T',Time),'.000Z')]
      data_list$Log[, EDSU:= paste(LocalID,LogKey,sep='/')]
      
      
      
      
      
      
      #################################################################
      #                   MAKE other general level                    #
      #################################################################
      tmp <- merge(data_list$Sample,data_list$NASC)
      tmp <- merge(tmp,data_list$Log[,c('Distance','Time','LogKey','Origin')],by='Distance')
      names(tmp)[names(tmp)=="Instrument"]='ID'
      names(tmp)[names(tmp)=="Value"]='NASC'
      
      # Category can be in either EchoType of SaCategory
      tmp[, AcousticCategory:=ifelse(is.na(SaCategory), EchoType, SaCategory)]
      
      
      
      #apply beam level, and add Beam key to all
      tmp_beam<-merge(tmp,data_list$Instrument, by='ID')

      # Sanity check, we can't have missing instrument records/linkage
      if(nrow(tmp_beam) == 0) {
        stop("StoxAcoustic: There is something wrong in the instrument records (input format: ICES Acoustic)")
      }

      tmp_beam$BeamKey <- tmp_beam$Frequency
      data_list$Beam<-unique(tmp_beam[,!c('NASC','ChannelDepthUpper','AcousticCategory','Type','Unit','SvThreshold')])
      tmp$BeamKey <- tmp_beam$BeamKey
      
      
      
          
      #apply acoustic catecory, and add Key to all
      data_list$AcousticCategory <- tmp
      data_list$AcousticCategory$AcousticCategoryKey <- tmp$AcousticCategory
      tmp$AcousticCategoryKey<- tmp$AcousticCategory
      
      
      
      
      
      #Apply channel, and apply key to all
      tmp$ChannelReferenceType <- 'P'
      tmp$ChannelReferenceKey <- tmp$ChannelReferenceType
      tmp$ChannelReferenceDepth <- ifelse(tmp$ChannelReferenceType == "P", 0, NA) # Hard coded to the surface for pelagic channels ("P") of the LUF20, and NA for bottom channels ("B"):
      tmp$ChannelReferenceOrientation <- ifelse(tmp$ChannelReferenceType == "P", 180, 0) # Hard coded to vertically downwards for pelagic channels ("P") of the LUF20, and vvertically upwards for bottom channels ("B"):      
      
      data_list$ChannelReference <- tmp
      
      
      
      #Apply channel, and apply key to all
      tmp$NASCKey <- paste(tmp$ChannelDepthUpper,tmp$ChannelDepthLower,sep='/')
      tmp$Channel <- NA
      data_list$NASC <- tmp
      
      
      
      
      
      
      
      #################################################################
      #                       RENAME variables                        #
      #################################################################
      names(data_list$Cruise)[names(data_list$Cruise)=='platform'] <- 'Platform'
      names(data_list$Log)[names(data_list$Log)=='Longitude'] <- 'Longitude'
      names(data_list$Log)[names(data_list$Log)=='Latitude'] <- 'Latitude'
      names(data_list$Log)[names(data_list$Log)=='LongitudeStop'] <- 'Longitude2'
      names(data_list$Log)[names(data_list$Log)=='LatitudeStop'] <- 'Latitude2'
      #names(data_list$Log)[names(data_list$Log)=='BottomDepth'] <- 'BottomDepth'
      names(data_list$Log)[names(data_list$Log)=='Distance'] <- 'Log'
      names(data_list$Log)[names(data_list$Log)=='Time'] <- 'DateTime'
      names(data_list$Log)[names(data_list$Log)=='Origin'] <- 'LogOrigin'
      
      names(data_list$NASC)[names(data_list$NASC)=='ChannelDepthUpper'] <- 'MinChannelRange'
      names(data_list$NASC)[names(data_list$NASC)=='ChannelDepthLower'] <- 'MaxChannelRange'
      
      
      #add integration distance
      data_list$Log<-merge(data_list$Log,data_list$Beam[,c('PingAxisInterval','LogKey')])
      names(data_list$Log)[names(data_list$Log)=='PingAxisInterval'] <- 'LogDistance'
      
      data_list$Log$LogOrigin2 <- "end"
      data_list$Log$LogDuration <- NA
      
      ####Bugfiks since StopLat and lon do not exist yet
      data_list$Log$Longitude2 <- NA
      data_list$Log$Latitude2 <- NA

      # Remove duplicates in Log and Beam
      data_list$Log <- unique(data_list$Log)
      data_list$Beam <- unique(data_list$Beam)

      #################################################################
      #        Add cruice key to all list                             #
      #################################################################
      data_list$Cruise[, CruiseKey:= LocalID]
      data_list$Log[, CruiseKey:= LocalID]
      data_list$Beam[, CruiseKey:= LocalID]
      data_list$AcousticCategory[, CruiseKey:= LocalID]
      data_list$ChannelReference[, CruiseKey:= LocalID]
      data_list$NASC[, CruiseKey:= LocalID]
      
      
    }
    
    
  
    #add effective log distance
    data_list$Log$EffectiveLogDistance <- data_list$Log$LogDistance
    
    #################################################################
    #                REMOVE undefined stoxacoustic variables        #
    #################################################################
    data_list<-data_list[c('Cruise','Log','Beam','AcousticCategory','ChannelReference','NASC')]  
    
    data_list$Cruise<-data_list$Cruise[, c('CruiseKey', 'Platform')]
    # 2020-02-03: Removed BottomDepth, which is mandatory:
    data_list$Log <- data_list$Log[, c('CruiseKey', 'LogKey', 'Log', 'EDSU', 'DateTime', 'Longitude', 'Latitude', 'LogOrigin', 'Longitude2', 'Latitude2', 'LogOrigin2', 'LogDistance', 'LogDuration', 'EffectiveLogDistance', 'BottomDepth')]
    #data_list$Log <- data_list$Log[, c('CruiseKey', 'LogKey', 'Log', 'EDSU', 'DateTime', 'Longitude', 'Latitude', 'LogOrigin', 'Longitude2', 'Latitude2', 'LogOrigin2', 'LogDistance', 'LogDuration', 'EffectiveLogDistance')]
    data_list$Beam <- data_list$Beam[,c('CruiseKey', 'LogKey', 'BeamKey', 'Frequency')]
    data_list$AcousticCategory <- data_list$AcousticCategory[,c('CruiseKey', 'LogKey', 'BeamKey', 'AcousticCategoryKey', 'AcousticCategory')]
    data_list$ChannelReference <- data_list$ChannelReference[,c('CruiseKey', 'LogKey', 'BeamKey', 'AcousticCategoryKey', 'ChannelReferenceKey', 'ChannelReferenceType', 'ChannelReferenceDepth', 'ChannelReferenceOrientation')]
    
    data_list$NASC <- data_list$NASC[,c('CruiseKey', 'LogKey', 'BeamKey', 'AcousticCategoryKey', 'ChannelReferenceKey', 'NASCKey', 'Channel', 'MaxChannelRange', 'MinChannelRange', 'NASC')]
    
    
    return(data_list)
  }

  # Process Biotic data in parallel
	if(length(cores) == 0) {
	    cores <- getCores()
	}
	if(cores == 1) {
	    data_list_out <- lapply(AcousticData, StoxAcousticOne)
	}
  else {
      if(get_os() == "win") {
          cl <- makeCluster(cores)
          data_list_out <- parLapply(cl, AcousticData, StoxAcousticOne)
          stopCluster(cl)
      } else {
          data_list_out <- mclapply(AcousticData, StoxAcousticOne, mc.cores = cores)
      }
  }
  

  tableNames <- names(data_list_out[[1]])

  StoxAcousticData <- lapply(
      tableNames,
      function(name) data.table::rbindlist(lapply(data_list_out, "[[", name))
  )

  names(StoxAcousticData) <- tableNames

  #Output stox acoustic data
  return(StoxAcousticData)
  
}
  

#' Merge StoxAcousticData
#'
#' @param StoxBioticData A list of StoX acoustic data (StoX data type \code{\link{StoxAcousticData}}).
#'
#' @return An object of StoX data type \code{\link{MergedStoxAcousticData}}.
#'
#' @export
#' 
MergeStoxAcoustic <- function(StoxAcousticData) {
    mergeDataTables(StoxAcousticData, tableNames = NULL, output.only.last = TRUE)
}
