#' Get Measurement Types
#' 
#' Retrieves a \code{data.frame} of measurement types available from Youtheria.
#' 
#' @param measurementType If \code{NULL} (default), then all measurement types are
#'        returned. Can also be 'numeric' or 'character' (or a list of either type)
#'        and will filter by Id and Name respectivly in the resulting data.frame.
#' @return A dataframe of measurement types giving their Id and Name        
#' @export
#' @examples
#' \dontrun{
#' 
#' getMeasurementTypes()
#' getMeasurementTypes('Body Mass')
#' getMeasurementTypes(c('Body Mass','Limb Length'))
#' getMeasurementTypes(1)
#' getMeasurementTypes(1:3)
#' }


getMeasurementTypes <-
function(measurementType = NULL){

  if(is.null(measurementType)){
    
    out <- runURL(type = 't')
    
  } else if(class(measurementType)=='character') {
    
    url<-paste('?MeasurementType=',measurementType,sep='')
    out <- runURL(URL = url, type = 't')
    
  } else if(class(measurementType) == 'numeric' | class(measurementType) == 'integer'){
    
    url<-paste('?id=',measurementType,sep='')
    out <- runURL(URL = url, type = 't')
    
  } else{
    
    stop('argument must be numeric, integer, charater or NULL')
    
  }
  return(out)
}
