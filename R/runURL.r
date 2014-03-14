runURL <-
  function(URL='', type=NULL){
    
    if(is.null(URL)) stop('URL is NULL')
    if(is.null(type)) stop('type is NULL')
    
    apiURL <- 'http://www.utheria.org/api/'
    
    if(type=='m') baseURL <- paste(apiURL, 'ValueByType', sep = '')
    if(type=='l') baseURL <- paste(apiURL, 'Location', sep = '')
    if(type=='t') baseURL <- paste(apiURL, 'Measurement', sep = '')
    if(type=='g') baseURL <- paste(apiURL, 'Genus', sep = '')
    if(type=='c') baseURL <- paste(apiURL, 'Country', sep = '')
    
    fullURL <- paste(baseURL,URL,sep='')
    
    if(length(fullURL) == 1){
      
      out <- executeURL(fullURL, type)
    
    } else {
      
      outMulti <- lapply(fullURL, executeURL, type)
      out <- ldply(outMulti, data.frame, stringsAsFactors=FALSE)
      
    }
        
    return(out)
    
  }


executeURL<-
  function(fullURL, type){
    
    print(fullURL)
    out <- fromJSON(fullURL)
    
    if(length(out) != 0){
      if(class(out[[1]]) == 'list'){
        out <- ldply(out, data.frame, stringsAsFactors=FALSE)
      } else {
        out <- as.data.frame(out)
      }
    } else {
      #warning('Some combinations of search terms returned no data')
    }
    return(out)
  }