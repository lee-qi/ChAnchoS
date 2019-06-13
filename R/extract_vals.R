#' Extract True and Estimated Values
#' Function that extracts time series of quantities (truth and estimates) from \code{\link{read_rep}}
#'
#' @param quantname El nombre de la cantidad para buscar en el archivo report
#' @param truerep Una lista de \code{\link{read_rep}} que es del modelo baso
#' @param allreps Una lista de listas con valores estimados de los modelos estimaciones
#' @param nsim El numero de replicas
#' 
#' @return Named list of "tru" and "est" values
#'
#' @export
#' @examples
#' \dontrun{
#' reps <- get_reps(OMfolder,simrepfile,truerepfile)
#' extract_vals(reps,'Biomasatotal')
#' }

extract_vals <- function(replist,quantname,nsim=Nsim) {
  truerep<-replist$truerep
  allreps<-replist$allreps
  
  eval(substitute(truevals <- truerep$x, list(x=quantname)))
  
  if(sum(dim(truevals))==0)
    N <- length(truevals)
  if(sum(dim(truevals))>0)
    stop("quantname needs to be a vector, not a matrix")
  
  estvals <- matrix(nrow=nsim,ncol=N)
  
  for(i in 1:nsim)
    eval(substitute(estvals[i,] <- allreps[[i]]$x, list(x=quantname)))
  
  ret <- list(tru=truevals,est=estvals)
  return(ret)
}