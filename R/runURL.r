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
    # replace spaces in the URL with the html '%20'
    fullURL <- gsub(' ', '%20', fullURL)
    
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
    
    #print(fullURL)
    resp <- getURL(fullURL)
    out <- fromJSON(resp, depth = 3, default.size = 1000)
    
    if(length(out) != 0){
      if(class(out[[1]]) == 'list'){
        if(type == 'm'){ # Use a special, fast, reformatter
          # Unique is added as there is some duplication in the database
          #######################################
          ##      THIS IS A TEMPORARY FIX      ##
          #######################################
          out <- YoutheriaToDF(out)
          out1 <- unique(out)
          tab1 <- table(out$MeasurementSetID)
          tab2 <- table(out1$MeasurementSetID)
          dups <- as.numeric(names(tab1[!tab1==tab2]))
          for(i in dups){            
            tempOut <- out1[out1$MeasurementSetID==i,]
            tabTemp <- names(table(tempOut$ValueType)[table(tempOut$ValueType)>1])  
            for(j in tabTemp){              
              rnam <- row.names(out1[out1$MeasurementSetID==i & out1$ValueType==j,])
              out <- out1[!row.names(out1) %in% rnam[2:length(rnam)],]
            }            
          }
          #######################################
          ##      THIS IS A TEMPORARY FIX      ##
          #######################################
        } else {
          out <- ldply(out, data.frame, stringsAsFactors=FALSE)
        } 
      } else {
        out <- as.data.frame(out)
      }
      
    } else {
      out <- NULL
    }
    return(out)
  }