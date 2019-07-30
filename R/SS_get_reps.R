#' Efficiently Read in Report Files from SS
#' Tiene que estar en la carpeta de "Dropbox/Simulador\ modelo\ Inpesca"
#' Por ahora solo esta 
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

SS_get_reps <- function(OMfolder) {
	full.path <- here::here(OMfolder)
	repdat <- file.path(full.path,"Reports.RData")
	bootfolder <- list.files(full.path)[grep("Bootstrap", list.files(full.path))[1]]

	testdat <- file.path(full.path,bootfolder,"Report.sso")

	if(ifelse(file.exists(repdat),file.info(repdat)$mtime < file.info(testdat)$mtime,TRUE)) {
		rep.dir <- file.path(full.path,"Reports")
		
		library(dplyr)
		library(r4ss)
		repnames <- list.files(rep.dir)
		keyvec <- repnames %>% stringr::str_remove("Report") %>% stringr::str_remove(".sso")
		Nboot <- length(repnames)
		
		truerep <- SS_output(dir=here::here(OMfolder,bootfolder))
		
		reps <- SSgetoutput(keyvec=keyvec,dirvec=rep.dir,
							getcovar=F,getcomp=F,forecast=F)
		
		reps$truerep <- truerep
		
		sumreps <- SSsummarize(reps)

		save(sumreps, file=repdat)
	}
	temp<-mget(load(repdat))
	return(temp$sumreps)
}
