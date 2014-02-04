##OLD CODE##
getSpeciesData <-
function(species = NULL, dictionary = NULL){
  
  if(is.null(species)) stop('Species name(s) required')
  if(is.null(dictionary)) stop('Dictionary must be defined as "MSW93" or "MSW05". This specifies the mammal species of the world reference guide to use')
  if(!dictionary %in% c('MSW93','MSW05')) stop('Dictionary must be either MSW93 or MSW05')
  out <- getMeData()
  # Subset using given dictionary
  if(dictionary=='MSW93') out <- out[out$MSW93Binomial %in% species,]
  if(dictionary=='MSW05') out <- out[out$MSW05Binomial %in% species,]
  # remove NA columns
  out <- out[,colSums(is.na(out))<nrow(out)]
  if(nrow(out)==0) stop('No results for this species. Check spelling')
  return(out)  
}
