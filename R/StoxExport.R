
write2ICESacoustic_CSV <- function(Acoustic,save=T){
  
  for(aco in Acoustic){
    if(aco$metadata$useXsd=='icesAcoustic'){
      
      hl<-c()
      hl$Instrument <- 'Instrument'
      hl$Header <- 'Record'
      tmp <- data.frame(aco$Instrument)
      names(tmp)<-paste0('Instrument',names(tmp))
      hl <- cbind(hl,tmp)
      HInst <- format(hl, trim=TRUE, width=0)
      
      hl<-c()
      hl$Calibration <- 'Calibration'
      hl$Header <- 'Record'
      tmp <- data.frame(aco$Calibration)
      names(tmp)<-paste0('Calibration',names(tmp))
      hl <- cbind(hl,tmp)
      HCal <- format(hl, trim=TRUE, width=0)
      
      hl<-c()
      hl$DataAcquisition <- 'DataAcquisition'
      hl$Header <- 'Record'
      tmp <- data.frame(aco$DataAcquisition)
      names(tmp)<-paste0('DataAcquisition',names(tmp))
      hl <- cbind(hl,tmp)
      HDatA <- format(hl, trim=TRUE, width=0)
      
      hl<-c()
      hl$DataProcessing <- 'DataProcessing'
      hl$Header <- 'Record'
      tmp <- data.frame(aco$DataProcessing)
      names(tmp)<-paste0('DataProcessing',names(tmp))
      hl <- cbind(hl,tmp)
      HDatP <- format(hl, trim=TRUE, width=0)
      
      hl<-c()
      hl$Cruise <- 'Cruise'
      hl$Header <- 'Record'
      hl$CruiseSurvey <- aco$Survey[2]
      tmp <- data.frame(aco$Cruise)
      names(tmp)<-paste0('Cruise',names(tmp))
      hl <- cbind(hl,tmp)
      HCru <- format(hl, trim=TRUE, width=0)
      
      
      hl<-c()
      hl$Data <- 'Data'
      hl$Header <- 'Record'
      tmp_log <- unique(data.frame(aco$Log))
      names(tmp_log)[names(tmp_log)=='LocalID']<-'CruiseLocalID'
      names(tmp_log)[names(tmp_log)!='CruiseLocalID']<-paste0('Log',names(tmp_log)[names(tmp_log)!='CruiseLocalID'])
      tmp_sample <- data.frame(aco$Sample)
      names(tmp_sample)[names(tmp_sample)=='LocalID']<-'CruiseLocalID'
      names(tmp_sample)[names(tmp_sample)=='Distance']<-'LogDistance'
      names(tmp_sample)[names(tmp_sample)=='Instrument']<-'InstrumentID'
      names(tmp_sample)[names(tmp_sample)=='Calibration']<-'CalibrationID'
      names(tmp_sample)[names(tmp_sample)=='DataAcquisition']<-'DataAcquisitionID'
      names(tmp_sample)[names(tmp_sample)=='DataProcessing']<-'DataProcessingID'
      names(tmp_sample)[!names(tmp_sample)%in%c('CruiseLocalID','LogDistance','InstrumentID','CalibrationID','DataAcquisitionID','DataProcessingID')] <-paste0('Sample',names(tmp_sample)[!names(tmp_sample)%in%c('CruiseLocalID','LogDistance','InstrumentID','CalibrationID','DataAcquisitionID','DataProcessingID')])
      
      
      tmp_data <- data.frame(aco$Data)
      names(tmp_data)[names(tmp_data)=='LocalID']<-'CruiseLocalID'
      names(tmp_data)[names(tmp_data)=='Distance']<-'LogDistance'
      names(tmp_data)[names(tmp_data)=='ChannelDepthUpper']<-'SampleChannelDepthUpper'
      names(tmp_data)[!names(tmp_data)%in%c('CruiseLocalID','LogDistance','SampleChannelDepthUpper')]<-paste0('Data',names(tmp_data)[!names(tmp_data)%in%c('CruiseLocalID','LogDistance','SampleChannelDepthUpper')])
      
      
      tmp_sub <-(merge(tmp_data,tmp_sample, by=intersect(names(tmp_data),names(tmp_sample))))
      tmp_sub <-merge(tmp_log,tmp_sub,by=intersect(names(tmp_log),names(tmp_sub)))
      hl<-cbind(hl,tmp_sub)
      HDat <- format(hl, trim=TRUE, width=0)
      
      tmp <- list(Instrument=HInst,
                  Calibration=HCal,
                  DataAcquisition=HDatA,
                  DataProcessing=HDatP,
                  Cruise=HCru,
                  Data=HDat
      )
      
      if(save==T){
        filename = gsub('.xml','.csv',aco$metadata$file)
        if(file.exists(filename))file.remove(filename)
        suppressWarnings(lapply(tmp, write.table, file=filename, append=TRUE, row.names=FALSE, quote=FALSE, sep=","))
        }
      
    }else{warning('only ices acoustic format is allowed')}

  }
  
  return(tmp)
}
