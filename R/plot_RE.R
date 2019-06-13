#' Plot Relative Errors
#' Function that plots time series of relative errors based on output from \code{\link{extract_vals}}
#'
#' @param vals list output from \code{\link{extract_vals}}
#' @param endyr final year to plot from. Default value is 2018.
#' 
#' @return Box and whiskers plot of relative errors
#'
#' @export
#' @examples
#' \dontrun{
#' plot_RE(extract_vals(reps,'Biomasatotal'))
#' }

plot_RE <- function(vals,qname,endyr=2018) {
  suppressMessages(library(ggplot2))
  theme_set(theme_minimal())
  truth <- vals$tru
  estimates <- vals$est
  
  REs <- as.data.frame((estimates - truth)/truth)
  colnames(REs) <- (endyr-dim(REs)[2]+1):endyr
  
  ggplot(stack(REs),aes(x = ind, y = values)) +
    geom_boxplot() +
    xlab("Year") +
    ylab("Relative Error") +
    ggtitle(qname)
}