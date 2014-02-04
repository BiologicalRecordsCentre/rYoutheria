#' Get a table of trait measurements from YouTheria
#' 
#' Retrieves a \code{data.frame} of trait measurements with facilities to select by
#' location, species name and/or measurement type.
#' 
#' @param measurementType Measurement types to collect data for. If \code{NULL} (default),
#'        all measurement types are returned. Can also be 'numeric' or 'character' (or a
#'        list of either type) and will filter by MeasurementTypeID and the measurement
#'        respectivly. MeasurementTypeIDs and names can be found using getMeasurementTypes().
#' @param MSW93Binomial Character giving the latin name of a species (or list of species)
#'        for which measurements are required. Naming should follow Mammal Species of the 
#'        World 1993.
#' @param MSW05Binomial Character giving the latin name of a species (or list of species)
#'        for which measurements are required. Naming should follow Mammal Species of the 
#'        World 2005.
#' @param country Character specifying the country from which you wish to collect data.
#'        If \code{NULL} all data is retrieved
#' @param StudyUnitId Numeric specifying the StudyUnitId from which you wish to collect data.
#'        If \code{NULL} all data is retrieved
#' @param locationData Logial dictating whether location information should be added to the
#'        output. Defualt is \code{FALSE} but is set to \code{TRUE} if either StudyUnitId or
#'        country are specified.
#' @param locationOnly Logical only used if locationData is TRUE. If \code{TRUE} data
#'        is only be returned if it has location information.
#' 
#' @return A \code{data.frame} with each row giving a trait measurement        
#' @export
#' @examples
#' \dontrun{
#' 
#' #Select measurement type by id
#' getMeasurementData(1)
#' getMeasurementData(c(1:3))
#' 
#' #Select measurement type by name
#' getMeasurementData('Body Mass')
#' getMeasurementData(c('Body Mass','Population Density'))
#' 
#' #Select by measurement type and species name
#' getMeasurementData(measurementType = 1,
#'                    MSW93Binomial=c('Pongo pygmaeus','Peroryctes raffrayana'))
#' 
#' 
#' }
getMeasurementData <-
  function(measurementType = NULL,
           MSW93Binomial = NULL,
           MSW05Binomial = NULL,           
           country=NULL,
           StudyUnitId=NULL,
           locationData = FALSE,
           locationOnly = TRUE
           ){
      
    measurementTypeURL <- NULL
    MSW93BinomialURL <- NULL
    MSW05BinomialURL <- NULL
    
    if(!is.null(measurementType)){
            
        if(is.numeric(measurementType)){
          if(!exists('MTs')) MTs <- getMeasurementTypes()
          bad <- measurementType[!measurementType %in% MTs$Id]
          if(length(bad)==length(measurementType)){
            stop('All measurement types specified are invalid: Measurement Type ID(s) ',bad,' is/are not known. Use getMeasurementTypes() to find out what is appropriate.', sep='')
          } else if(length(bad)!=0){
            warning('Some measurement types unknown: Measurement Type ID(s) ',bad,' is/are not known. Use getMeasurementTypes() to find out what is appropriate.', sep='')
          }         
        }
        
        if(is.character(measurementType)){
          if(!exists('MTs')) MTs <- getMeasurementTypes()
          bad <- measurementType[!measurementType %in% MTs$Name]
          if(length(bad)==length(measurementType)){
            stop('All measurement types specified are invalid: Measurement Type ID(s) ',bad,' is/are not known. Use getMeasurementTypes() to find out what is appropriate.', sep='')
          } else if(length(bad)!=0){
            warning('Some measurement types unknown: Measurement Type ID(s) ',bad,' is/are not known. Use getMeasurementTypes() to find out what is appropriate.', sep='')
          }           
          measurementType <- MTs$Id[MTs$Name %in% measurementType]
        }
          
        measurementTypeURL <- paste(measurementType, collapse = ',', sep='')
      
    }
    
    if(!is.null(MSW93Binomial) & !is.null(MSW05Binomial)) stop ('Cannot filter by MSW05Binomial and MSW93Binomial. Choose one or the other')
    
    if(!is.null(MSW93Binomial)){
      
      MSW93BinomialURL <- paste(MSW93Binomial, collapse = ',',sep='')
            
    }
    
    if(!is.null(MSW05Binomial)){
     
      MSW05BinomialURL <- paste(MSW05Binomial, collapse = ',',sep='')
     
    }
       
    URL <- paste('?id=', measurementTypeURL,
                 '&MSW93Binomial=', MSW93BinomialURL,
                 '&MSW05Binomial=', MSW05BinomialURL, sep='')
     
    if(!is.null(country) | !is.null(StudyUnitId)) locationData = TRUE
    
    if(locationData){
      
      out <- runURL(URL,'m')
      loc_data <- getLocData(country, StudyUnitId)
      out <- merge(out, loc_data, all.x = !locationOnly, all.y = F)
      
    } else {
      out <- runURL(URL,'m')
    }
    
    if(length(out) == 0) warning('No data was returned. Check species names and locations are correct')
    
    #Report species that are missing from the results be requested
    if(!is.null(MSW93Binomial)){
      missing <- MSW93Binomial[!MSW93Binomial %in% unique(out$MSW93Binomial)]
      if(length(missing)>0) warning(paste('There were no results returned for the following species:', paste(missing, collapse=', ')))    
    }
    if(!is.null(MSW05Binomial)){
      missing <- MSW05Binomial[!MSW05Binomial %in% unique(out$MSW05Binomial)]
      if(length(missing)>0) warning(paste('There were no results returned for the following species:', paste(missing, collapse=', ')))    
    }
    
    return(out)
  }
