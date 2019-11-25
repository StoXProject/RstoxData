
#' Convert output from xml reader to StoxAcoustic data
#'
#' @param data_list table. The output from xmlreader.
#'
#' @return List of data.table objects containing the StoxAcoustic data.
#'
#'
#' @export
StoxAcoustic <- function(AcousticData = NULL){
	
	# For flexibility accept a list of the input data, named by the data type:
	if(is.list(AcousticData) && "AcousticData" %in% names(AcousticData)) {
		AcousticData <- AcousticData$AcousticData
	}
	
  #Define the output 
  data_list_out <- c()
  
  
  
  #Loop through all platforms
  for (data_list in AcousticData){
    
    
    
    #TODO: 
    #   - Agree on a output and input format
    
    
    
    #Check which data format this is
    #TODO:
    #     - Ibrahims code should output which xsd is used, and use this as 
    #       a flag here
    ices_format <- FALSE
    if(is.null(data_list$echosounder_dataset))ices_format<- TRUE
    
    
    print(names(AcousticData))
    
    
    
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
      #              Fiks structure in nmd echosounder                #
      #################################################################
      data_list$AcousticCategory$type=NULL
      ul <- (unique(data_list$AcousticCategory))
      mm <- merge(data_list$ChannelReference, data_list$AcousticCategory)
  
      #Make new list structure    
      data_list$AcousticCategory<-ul
      data_list$ChannelReference<-mm
      
      
      
      
      
      
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
      #         Fiks to correct time format, and add to key           #
      #################################################################
      #
      # TODO
      #     -This is a quick fix assuming the format is not changed
      data_list$Log$LogKey              <- sapply(data_list$Log$start_time,function(i) paste0(gsub(' ','T',i),'.000Z'))
      data_list$Beam$LogKey             <- sapply(data_list$Beam$start_time,function(i) paste0(gsub(' ','T',i),'.000Z'))
      data_list$AcousticCategory$LogKey <- sapply(data_list$AcousticCategory$start_time,function(i) paste0(gsub(' ','T',i),'.000Z'))
      data_list$ChannelReference$LogKey <- sapply(data_list$ChannelReference$start_time,function(i) paste0(gsub(' ','T',i),'.000Z'))
      data_list$NASC$LogKey             <- sapply(data_list$NASC$start_time,function(i) paste0(gsub(' ','T',i),'.000Z'))
      
      
      
      
      
      
      
      
      #################################################################
      #            Add BeamKey to all list                            #
      #################################################################
      data_list$Beam$BeamKey             <- paste(data_list$Beam$freq,data_list$Beam$transceiver, sep='/')
      data_list$AcousticCategory$BeamKey <- paste(data_list$AcousticCategory$freq,data_list$AcousticCategory$transceiver, sep='/')
      data_list$ChannelReference$BeamKey <- paste(data_list$ChannelReference$freq,data_list$ChannelReference$transceiver, sep='/')
      data_list$NASC$BeamKey             <- paste(data_list$NASC$freq,data_list$Channel$transceiver, sep='/')
      
      
      
      
      
      
      
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
      
      
      
      
      
      
      
      
      #################################################################
      #              Add ChannelKey to all list                       #
      #################################################################
      data_list$NASC$ChannelKey           <- data_list$NASC$ch
      
      
      
      
      
      
      
      
      
      #################################################################
      #                       RENAME cruise level                     #
      #################################################################
      names(data_list$Cruise)[names(data_list$Cruise)=='platform'] <- 'Platform'
      
      
      
      
      
      
      
      
      #################################################################
      #                       RENAME LOG level                        #
      #################################################################
      names(data_list$Log)[names(data_list$Log)=='log_start']        <- 'Log'
      names(data_list$Log)[names(data_list$Log)=='integrator_dist'] <- 'Distance'
      names(data_list$Log)[names(data_list$Log)=='lon_start']       <- 'StartLongitude'
      names(data_list$Log)[names(data_list$Log)=='lat_start']       <- 'StartLatitude'
      names(data_list$Log)[names(data_list$Log)=='lon_stop']       <- 'StopLongitude'
      names(data_list$Log)[names(data_list$Log)=='lat_stop']       <- 'StopLatitude'
      
      
      
      
      
      
      
      #################################################################
      #                 ADD info in  LOG level                        #
      #################################################################
      data_list$Beam$StartBottomDepth<-rowMeans(cbind(data_list$Beam$min_bot_depth,data_list$Beam$max_bot_depth),na.rm = TRUE)
      tmp2 <- data_list$Beam[,c('LogKey','BeamKey','StartBottomDepth')]
      data_list$Log <- merge(data_list$Log,tmp2,by='LogKey')
      
      data_list$Log$EDSU <- paste(data_list$Cruise$Platform,data_list$Log$Log,sep='/')
      
      
      
      
      
      
      
      
      #################################################################
      #                       RENAME Frequency level                  #
      #################################################################
      names(data_list$Beam)[names(data_list$Beam)=='freq'] <- 'Frequency'
    
      
      
      
      
      
      
      #################################################################
      #                       RENAME AcousticCatecory level           #
      #################################################################
      names(data_list$AcousticCategory)[names(data_list$AcousticCategory)=='acocat'] <- 'AcousticCategory'
      
      
      
      
      
      
      
      
      #################################################################
      #                RENAME NASC level                              #
      #################################################################
      names(data_list$NASC)[names(data_list$NASC)=='sa'] <- 'NASC'
      
      
      
      
      
      
      
      
      #################################################################
      #                Adding depth to list                           #
      #################################################################
      #This info is not avaliable in nmd echosounder, and is hacked here
      #
      #TODO: 
      #     - Do stuff to the bottom mode
      temp <- merge(data_list$Beam[,c('upper_integrator_depth','LogKey')],data_list$Log[,c('pel_ch_thickness','LogKey')],by='LogKey')
      data_list$NASC <- merge(data_list$NASC,temp,by='LogKey')
      
      data_list$NASC$MinRange<-data_list$NASC$pel_ch_thickness*(as.integer(data_list$NASC$ch)-1)
      data_list$NASC$MaxRange<-data_list$NASC$pel_ch_thickness*(as.integer(data_list$NASC$ch))
      
      
      
      
      #Fiks upper integration depth for pelagic
      #Denne maa Dobbelsjekkes
      data_list$NASC[(data_list$NASC$MinRange<data_list$NASC$upper_integrator_depth)&(data_list$NASC$ChannelReferenceKey=='P'),]$MinRange<-data_list$NASC[(data_list$NASC$MinRange<data_list$NASC$upper_integrator_depth)&(data_list$NASC$ChannelReferenceKey=='P'),]$upper_integrator_depth
      
      
      
      data_list$ChannelReference[data_list$ChannelReference$ChannelReferenceKey=='B']
      
      
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
      data_list$Log$LogKey <- sapply(data_list$Log$Time,function(i) paste0(gsub(' ','T',i),'.000Z'))
      data_list$Log$EDSU <- paste(data_list$Log$CruiseKey,data_list$Log$LogKey,sep='/')
      
      
      
      
      
      
      #################################################################
      #                   MAKE other general level                    #
      #################################################################
      tmp <- merge(data_list$Sample,data_list$NASC)
      tmp <- merge(tmp,data_list$Log[,c('Distance','Time','LogKey')],by='Distance')
      names(tmp)[names(tmp)=="Instrument"]='ID'
      names(tmp)[names(tmp)=="Value"]='NASC'
      names(tmp)[names(tmp)=="SaCategory"]='AcousticCategory'
      
      
      
      
      #apply beam level, and add Beam key to all
      tmp_beam<-merge(tmp,data_list$Instrument, by='ID')
      tmp_beam$BeamKey <- tmp_beam$Frequency
      data_list$Beam<-unique(tmp_beam[,!c('NASC','ChannelDepthUpper','AcousticCategory','Type','Unit','SvThreshold')])
      tmp$BeamKey <- tmp_beam$BeamKey
      
      
      
          
      #apply acoustic catecory, and add Key to all
      data_list$AcousticCategory <- tmp
      data_list$AcousticCategory$AcousticCategoryKey <- tmp$AcousticCategory
      tmp$AcousticCategoryKey<- tmp$AcousticCategory
      
      
      
      
      
      #Apply channel, and apply key to all
      data_list$ChannelReference <- tmp
      data_list$ChannelReference$ChannelReferenceKey <- 'P'
      tmp$ChannelReferenceKey<- 'P'
      
      
      
      
      #Apply channel, and apply key to all
      data_list$NASC <- tmp
      data_list$NASC$ChannelKey <- paste(tmp$ChannelDepthUpper,tmp$ChannelDepthLower,sep='/')
      
      
      
      
      
      
      
      #################################################################
      #                       RENAME variables                        #
      #################################################################
      names(data_list$Cruise)[names(data_list$Cruise)=='platform'] <- 'Platform'
      names(data_list$Log)[names(data_list$Log)=='Longitude'] <- 'StartLongitude'
      names(data_list$Log)[names(data_list$Log)=='Latitude'] <- 'StartLatitude'
      names(data_list$Log)[names(data_list$Log)=='LongitudeStop'] <- 'StopLongitude'
      names(data_list$Log)[names(data_list$Log)=='LatitudeStop'] <- 'StopLatitude'
      names(data_list$Log)[names(data_list$Log)=='BottomDepth'] <- 'StartBottomDepth'
      names(data_list$Log)[names(data_list$Log)=='Distance'] <- 'Log'
      names(data_list$NASC)[names(data_list$NASC)=='ChannelDepthUpper'] <- 'MinRange'
      names(data_list$NASC)[names(data_list$NASC)=='ChannelDepthLower'] <- 'MaxRange'
      
      
      #add integration distance
      data_list$Log<-merge(data_list$Log,data_list$Beam[,c('PingAxisInterval','LogKey')])
      names(data_list$Log)[names(data_list$Log)=='PingAxisInterval'] <- 'Distance'
      
      
      
      ####Bugfiks since StopLat and lon do not exist yet
      data_list$Log$StopLongitude <- NA
      data_list$Log$StopLatitude <- NA
      
      
      
      
      
      #################################################################
      #        Add cruice key to all list                             #
      #################################################################
      data_list$Cruise$CruiseKey           <- data_list$Cruise$LocalID
      data_list$Log$CruiseKey              <- data_list$Cruise$LocalID
      data_list$Beam$CruiseKey             <- data_list$Cruise$LocalID
      data_list$AcousticCategory$CruiseKey <- data_list$Cruise$LocalID
      data_list$ChannelReference$CruiseKey <- data_list$Cruise$LocalID
      data_list$NASC$CruiseKey             <- data_list$Cruise$LocalID
      
      
      
      
    }
    
    
  
    
    #################################################################
    #                REMOVE undefined stoxacoustic variables        #
    #################################################################
    data_list<-data_list[c('Cruise','Log','Beam','NASC','AcousticCategory','ChannelReference')]  
    
    data_list$Cruise<-data_list$Cruise[,c('Platform','CruiseKey')]
    data_list$Log <- data_list$Log[,c('Log','Distance','StartLongitude','StopLongitude','StartLatitude','StopLatitude','EDSU','CruiseKey','LogKey')]
    data_list$Beam <- data_list$Beam[,c('Frequency','CruiseKey','LogKey','BeamKey')]
    data_list$AcousticCategory <- data_list$AcousticCategory[,c('AcousticCategory','CruiseKey','LogKey','BeamKey','AcousticCategoryKey')]
    data_list$ChannelReference <- data_list$ChannelReference[,c('CruiseKey','LogKey','BeamKey','AcousticCategoryKey','ChannelReferenceKey')]
    
    data_list$NASC <- data_list$NASC[,c('CruiseKey','LogKey','BeamKey','AcousticCategoryKey',
                                                    'ChannelReferenceKey','ChannelKey','MaxRange','MinRange','NASC')]
    
    
    
    #bind platforms
    if(is.null(data_list_out)){
      data_list_out=data_list}
    else{
    data_list_out <- Map(rbind,data_list_out,data_list)
    }
    
  }
  
  
  
  
  #Output stox data
  return(data_list_out)
    
}
  