#' Extract values from SS report files
#' Function that extracts quantities from SSsummarize output
#'
#' @param repfiles Una lista de \code{\link{SSsummarize}}
#' @param quantname El nombre de la cantidad para buscar en el archivo report
#' 
#' @return List of tru and est values
#'
#' @export
#' @examples
#' \dontrun{
#' plot_RE(extract_vals(reps,'Biomasatotal'))
#' }

SS_extract_vals <- function(repfiles,quantname) {
	Nsim <- length(repfiles$modelnames)-1

	eval(substitute(temp <- repfiles$x, list(x=quantname)))

	tru <- which(colnames(temp)=="truerep")

	truevals <- temp[,tru]

	estvals <- temp[,c(1:(tru-1))]
	
	ret <- list(tru=truevals,est=t(estvals))
	return(ret)
		
}