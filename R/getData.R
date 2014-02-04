##OLD CODE##
getMeData <-
function(measurementTypeID=NULL){
  baseURL<-'http://localhost:2947/api/ValueByType'
  if(is.null(measurementTypeID)){
    out <- fromJSON(baseURL)
    out <- ldply(out, data.frame) 
  } else if(class(measurementTypeID)=='character') {
    #url<-paste(baseURL,'?gname=',measurementTypeID,sep='')
    #out <- json2dataframe(url)
    stop('Cannot take character. Use getMeasurementTypes to look up MeasurementTypeID')
  } else if(class(measurementTypeID)=='numeric'){
    mt <- getMeasurementTypes()
    if(measurementTypeID %in% mt$Id){
      type <- mt$Name[mt$Id==measurementTypeID]
      print(paste('Getting data for measurement type',measurementTypeID,'(',type,')'))
    } else {
      stop(paste(measurementTypeID, 'is not a vaild measurement type'))
    }    
    url <- paste(baseURL,'?id=',measurementTypeID,sep='')
    out <- json2dataframe(url)
  } else{
    stop('argument must be numeric or charater or NULL')
  }
  # Add measurement type ID as name
  MTs <- getMeasurementTypes()
  out$MeasurementType <- MTs$Name[match(out$MeasurementTypeID,MTs$Id)]
  
  # Create one row per measurement
  out <- dcast(out,MeasurementType+MeasurementSetID+Genus+Species+SubSpecies
                   +MSW93Binomial+MSW05Binomial+AuthorityText
                   ~ValueType,value.var='MValue')
  if(nrow(out)==0) stop('Oops, the YouTheria web portal returned no data. Contact the site admin')
  return(out)
}
