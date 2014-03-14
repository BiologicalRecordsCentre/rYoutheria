#' Get a table of genera
#' 
#' Retrieves a \code{data.frame} of generas available from Youtheria.
#' 
#' @param genus If \code{NULL} (default), then a lis of all genera are
#'        returned. Can also be 'numeric' or 'character' (or a list of either type)
#'        and will filter by Id and Name respectivly in the resulting data.frame.
#' @return A dataframe of genera giving their Id and Name
#' @export       
#' @examples
#' \dontrun{
#' 
#' getGenus()
#' getGenus('Antilocapra')
#' getGenus(c('Antilocapra','Damaliscus'))
#' getGenus(1)
#' getGenus(1:5)
#' }

getGenus <-
function(genus=NULL){

  if(is.null(genus)){
    
    out <- runURL(type = 'g')
    
  } else if(class(genus)=='character') {
    
    URL <- paste('?gname=',genus,sep='')
    out <- runURL(URL = URL, type = 'g')
        
  } else if(class(genus)=='numeric' | class(genus) == 'integer'){
    
    URL <- paste('?id=',genus,sep='')
    out <- runURL(URL = URL, type = 'g')
    
  } else{
    
    stop('argument must be numeric, integer, charater or NULL')
    
  }
  return(out)
}
