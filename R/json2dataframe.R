json2dataframe <-
function(url){
  out <- fromJSON(url)
  if(length(out)==0) stop('No data returned from this query')
   if(length(out[[1]])>1){
     out <- ldply(out, data.frame)
   } else {
     out <- as.data.frame(out)
   }
  return(out)
}
