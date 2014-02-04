#' Get location information from YouTheria
#' 
#' Retrieves location information store on YouTheria
#' 
#' @param country Character specifying the country within which to search for locations.
#' for a list of countries used getCountries().
#' @param StudyUnitId Numeric specifying the StudyUnitId to search for
#' @return A dataframe in which each rows gives the details of a study unit
#' @export        
#' @examples
#' \dontrun{
#' 
#' getLocData(country = 'India')
#' 
#' }
getLocData <-
  function(country=NULL,StudyUnitId=NULL){
       
    if(!is.null(StudyUnitId)&!is.null(country)) stop('Cannot use both StudyUnitId and country at the same time')
    
    countryURL <- NULL
    StudyUnitIdURL <- NULL
    
    if(!is.null(country)){
      
      countryURL <- paste(country, collapse = ',',sep='')
      
    }
    
    if(!is.null(StudyUnitId)){
      
      StudyUnitIdURL <- paste(StudyUnitId, collapse = ',',sep='')
      
    }
    
    out <- runURL(paste('?id=', StudyUnitIdURL,
                 '&country=', countryURL, sep=''), 'l')
    
    if(length(out)==0){
      if(!is.null(StudyUnitId)) warning('No Data returned for this StudyUnitId(s).')
      if(!is.null(country)) warning('No Data returned for this country(s). Ensure you have capitalised appropriately')
    }
   
    return(out)
  }
