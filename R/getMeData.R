##OLD CODE##
getMeData <-
function(measurementTypeID=NULL){
  baseURL<-'http://localhost:2947/api/ValueByType'
  if(is.null(measurementTypeID)){
    out <- fromJSON(baseURL,default.size=1000, depth=3)
    out <- ldply(out, data.frame, stringsAsFactors=FALSE) 
  } else if(class(measurementTypeID)=='character') {
    #url<-paste(baseURL,'?gname=',measurementTypeID,sep='')
    #out <- json2dataframe(url)
    stop('Cannot take character. Use getMeasurementTypes to look up MeasurementTypeID')
  } else if(class(measurementTypeID)=='numeric'){
    if(length(measurementTypeID)>1) measurementTypeID <- paste(measurementTypeID,collapse=',')
    mt <- getMeasurementTypes()
    if(sum(measurementTypeID %in% mt$Id)==length(measurementTypeID %in% mt$Id)){
      type <- mt$Name[match(measurementTypeID,mt$Id)]
      if(length(type)>1) type <- paste(type, collapse = ', ')
      if(length(measurementTypeID)>1) measurementTypeID <- paste(measurementTypeID, collapse = ',')
      print(paste('Getting data for measurement type',measurementTypeID,'(',type,')'))
    } else {
      stop(paste(measurementTypeID[!measurementTypeID %in% mt$Id], 'is not a vaild measurement type'))
    }    
    url <- paste(baseURL,'?id=',measurementTypeID,sep='')
    print(url)
    out <- fromJSON(url,default.size=1000, depth=3)
    out <- ldply(out, data.frame, stringsAsFactors=FALSE) 
  } else{
    stop('argument must be numeric or charater or NULL')
  }
  # Add measurement type ID as name
  if(!exists('mt')) mt <- getMeasurementTypes()
  out$MeasurementType <- mt$Name[match(out$MeasurementTypeID,mt$Id)]
  
  # Create one row per measurement
  out <- dcast(out,MeasurementType+MeasurementSetID+StudyUnitID+Genus+Species+SubSpecies
                   +MSW93Binomial+MSW05Binomial+AuthorityText
                   ~ValueType,value.var='MValue')
  if(nrow(out)==0) stop('Oops, the YouTheria web portal returned no data. Contact the site admin')
  return(out)
}
