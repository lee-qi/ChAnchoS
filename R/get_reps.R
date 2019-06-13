#' Read in Report Files
#' Tiene que estar en la carpeta de "Dropbox/Simulador\ modelo\ Inpesca"
#'
#' @param OMfolder Folder containing runs
#' @param simrepfile Base name of each simulation rep file (.
#' @param truerepfile final year to plot from. Default value is 2018.
#' 
#' @return Rdata file with \code{allreps} and \code{truerep}
#'
#' @export
#' @examples
#' \dontrun{
#' 	OMfolder <- "Evalua\ Estimador_INPESCA_SARD"
#'	simrepfile <- "sar_sim" # "mae_sim"
#'	truerepfile <- "sar_base.rep"
#'  reps <- get_reps(OMfolder,simrepfile,truerepfile)
#' }

get_reps <- function(OMfolder,simrepfile,truerepfile) {
	repdat <- here::here(OMfolder,"Reports.RData")
	testdat <- here::here(OMfolder,paste0(simrepfile,1,".rep"))
	if(ifelse(file.exists(repdat),file.info(repdat)$mtime < file.info(testdat)$mtime,TRUE)) {
		library(foreach)
		doParallel::registerDoParallel()
		
		Nsim <- sum(grepl(paste0(simrepfile,".*rep"),list.files(OMfolder)))
		
		allreps <- foreach(i = 1:Nsim) %dopar%  {
			read_rep(here::here(OMfolder,paste0(simrepfile,i,".rep")))
		}
		
		doParallel::stopImplicitCluster()
		
		truerep <- read_rep(here::here(OMfolder,truerepfile))
		save(allreps, truerep, file=repdat)
	}
	temp<-mget(load(repdat))
	return(temp)
}
