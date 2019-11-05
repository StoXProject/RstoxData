
#' Convert output from xml reader to StoxAcoustic
#'
#' @param data_list table. The output from xmlreader.
#' @param converter file. Instruction file to convert values
#'
#' @return List of data.table objects containing the "flattened" XML data.
#'
#'
#' @export
StoxAcoustic <- function(data_list = NULL,converter = NULL){

  
  
  #Check which data format this is
  ices_format <- FALSE
  if(is.null(data_list$echosounder_dataset))ices_format<- TRUE
  
  
  
  
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
    ul <- (unique(dl$AcousticCategory))
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
    names(data_list$Log)[names(data_list$Log)=='log_start'] <- 'Log'
    names(data_list$Log)[names(data_list$Log)=='integrator_dist '] <- 'Distance'
    names(data_list$Log)[names(data_list$Log)=='lon_start '] <- 'StartLatitude'
    names(data_list$Log)[names(data_list$Log)=='lat_start '] <- 'StartLongitude '
    
    
    #Fiks StartBottomDepth, where should we get this information from? 
    data_list$Log$StartBottomDepth <- NA
    data_list$Log$EDSU <- paste(data_list$Cruise$Platform,data_list$Log$Log,sep='/')
    
    
    
    
    
    
    #################################################################
    #                       RENAME Frequency level                  #
    #################################################################
    names(data_list$Beam)[names(data_list$Beam)=='freq'] <- 'Frequency'
    
    
    
    
    
    
    
    
    
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
    #     - The first channel should start at upper_integration_depth
    #       For now this will have value of 0
    temp <- merge(data_list$Beam,data_list$Log)
    temp <- merge(data_list$Log,data_list$NASC)
    temp$MinRange<-temp$pel_ch_thickness*(as.integer(temp$ChannelKey)-1)
    temp$MaxRange<-temp$pel_ch_thickness*(as.integer(temp$ChannelKey))
    temp2<-temp[,c('MinRange','MaxRange','ChannelKey','ChannelReferenceKey','BeamKey','LogKey','CruiseKey')]
    
    
    data_list$NASC<-merge(data_list$NASC,temp2)
    
  }
  

  
  
  
  
  
  
  
  return(data_list)
    
}
  