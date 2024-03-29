% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_rep.R
\name{read_rep}
\alias{read_rep}
\title{Read ADMB Report Files
The following reads a report file. Then the 'A' object contains a list structure with all the elemements in the report file. 
  
Para leer los archivos reportes de ADMB}
\usage{
read_rep(fn)
}
\arguments{
\item{fn}{El filepath para el archivo de reporte}
}
\value{
Named list of "tru" and "est" values
}
\description{
The part in quotations becomes the list name.
Created By (Steven Martell){https://www.admb-project.org/tools/r/reading-report-files-using-r}
}
\examples{
\dontrun{
extract_vals('Biomasatotal')
}
}
