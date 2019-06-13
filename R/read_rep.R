#' Read ADMB Report Files
#' The following reads a report file. Then the 'A' object contains a list structure with all the elemements in the report file. 
#'   
#' Para leer los archivos reportes de ADMB
#' 
#' The part in quotations becomes the list name.
#' Created By Steven Martell
#'
#' @param fn El filepath para el archivo de reporte
#' 
#' @return Named list of "tru" and "est" values
#'
#' @export
#' @examples
#' \dontrun{
#' extract_vals('Biomasatotal')
#' }


read_rep <- function(fn)
{
  options(warn=-1)  #Suppress the NA message in the coercion to double
  
  
  ifile=scan(fn,what="character",flush=TRUE,blank.lines.skip=FALSE,quiet=TRUE)
  idx=sapply(as.double(ifile),is.na)
  vnam=ifile[idx] #list names
  nv=length(vnam) #number of objects
  A=list()
  ir=0
  for(i in 1:nv)
  {
    ir=match(vnam[i],ifile)
    if(i!=nv) irr=match(vnam[i+1],ifile) else irr=length(ifile)+1 #next row
    dum=NA
    if(irr-ir==2) dum=as.double(scan(fn,skip=ir,nlines=1,quiet=TRUE,what=""))
    if(irr-ir>2) dum=as.matrix(read.table(fn,skip=ir,nrow=irr-ir-1,fill=TRUE))
    
    if(is.numeric(dum))#Logical test to ensure dealing with numbers
    {	
      newname<-vnam[i]
      if(sum(grep("\\$",vnam[i]))>0) newname<-gsub("\\$","",vnam[i])
      A[[newname]]=dum
    }
  }
  options(warn=0)
  
  return(A)
}