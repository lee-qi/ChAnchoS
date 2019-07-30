#' Plot Relative Errors
#' Function that plots time series of relative errors based on output from \code{\link{extract_vals}}
#'
#' @param vals list output from \code{\link{extract_vals}}
#' @param qname name of quantity being plotted
#' @param endyr final year to plot from. Default value is 2019.
#' 
#' @return Box and whiskers plot of relative errors
#'
#' @export
#' @examples
#' \dontrun{
#' plot_RE(extract_vals(reps,'Biomasatotal'))
#' }

plot_REs <- function(vals,qname,endyr=2019) {
  if(!"patchwork" %in% installed.packages()) warning("patchwork package not found. Please run:\ndevtools::install_github(\"thomasp85/patchwork\")")
  suppressMessages(library(ggplot2))
  suppressMessages(library(patchwork))

  theme_set(theme_minimal())

  truth <- vals$tru
  estimates <- vals$est

  yrs <- (endyr-length(truth)+1):endyr
  yrs2plot <- c(range(yrs)[1]-1,range(yrs)[2]+1)

  ests <- data.frame(estimates)
  colnames(ests) <- names(truth) <- yrs

  est <- ests %>%
          gather(year) %>%
          mutate(year=as.integer(year)) %>%
          group_by(year)

  tru <- data.frame(year=yrs,truth=truth)

  REs <- est %>%  
          nest() %>%
          left_join(tru) %>%
          unnest() %>%
          mutate(RE=(value-truth)/truth) %>%
          group_by(year)

  f1 <- ggplot() +
          geom_boxplot(data=est,mapping=aes(x=year, y=value, group=year)) +
          geom_line(data=tru,mapping=aes(x=year,y=truth),colour="red")+
          labs(title=qname, y = "Valor") + 
          scale_x_continuous(name="Año",limits=yrs2plot)
  
  f2 <- ggplot(REs,aes(x = year, y = RE, group=year)) +
          geom_boxplot() +
          geom_hline(yintercept=0,colour="red") + 
          labs(title=qname, y = "Error") + 
          scale_x_continuous(name="Año",limits=yrs2plot)

  f3 <- ggplot(REs,aes(x = RE)) +
          geom_histogram() +
          geom_vline(xintercept=0,colour="red") +
          labs(title="Error (todos los años)",
          x ="Error", y = "Frecuencia")

  f4 <- ggplot(REs %>% filter(year>(endyr-3)),aes(x = RE)) +
        geom_histogram() +
        geom_vline(xintercept=0,colour="red") +
        labs(title="Error (ultimos tres años)",
        x ="Error", y = "Frecuencia") 

  f1 + f2 + f3 + f4 +
          plot_layout(ncol=2)
}