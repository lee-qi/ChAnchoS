#' Get Relative Errors
#' Function that plots time series of relative errors based on output from \code{\link{extract_vals}}
#'
#' @param vals list output from \code{\link{extract_vals}}
#' @param endyr final year to plot from. Default value is 2019.
#' @param Nyears number of final years to calculate for. Default value is 3.
#' @param Ndigits number of digits to round the values to. Default value is 3.
#' 
#' @return Data frame of eight values
#'
#' @export
#' @examples
#' \dontrun{
#' plot_RE(extract_vals(reps,'Biomasatotal'))
#' }

get_REs <- function(vals,endyr=2018,Nyears=3,Ndigits=3) {

  truth <- vals$tru[!is.na(vals$tru)]
  estimates <- vals$est

  yrs <- (endyr-length(truth)+1):endyr
  yrs2plot <- c(range(yrs)[1]-1,range(yrs)[2]+1)

  ests <- data.frame(estimates)
  colnames(ests) <- names(truth) <- yrs

  est <- ests %>%
          gather(year) %>%
          mutate(year=as.integer(year)) %>%
          remove_missing() %>%
          group_by(year)

  tru <- data.frame(year=yrs,truth=truth)

  REs <- est %>%  
          nest() %>%
          left_join(tru) %>%
          unnest() %>%
          mutate(RE=(value-truth)/truth)

  restot <- REs %>% summarise(MARE_Total=str_c(round(mean(RE),Ndigits)," (",round(sd(RE),Ndigits),")"), 
                           MRE_Total=str_c(round(sum(abs(RE))/n(),Ndigits)," (", round(sd(abs(RE)),Ndigits),")"))

  resfin <- REs %>% filter(year%in%rev(year)[1:Nyears]) %>%
              summarise(MARE_Final=str_c(round(mean(RE),Ndigits)," (",round(sd(RE),Ndigits),")"), 
                           MRE_Final=str_c(round(sum(abs(RE))/n(),Ndigits)," (", round(sd(abs(RE)),Ndigits),")"))

  res <- bind_cols(restot,resfin)

  return(res)
}