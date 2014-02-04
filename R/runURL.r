runURL <-
  function(URL='', type=NULL){
    
    if(is.null(URL)) stop('URL is NULL')
    if(is.null(type)) stop('type is NULL')
    
    if(type=='m') baseURL<-'http://localhost:2947/api/ValueByType'
    if(type=='l') baseURL<-'http://localhost:2947/api/Location'
    if(type=='t') baseURL<-'http://localhost:2947/api/Measurement'
    if(type=='g') baseURL<-'http://localhost:2947/api/Genus'
    if(type=='c') baseURL<-'http://localhost:2947/api/Country'
    
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