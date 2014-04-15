#' Stata webuse command
#' 
#' TODO
#' @param x The name of the dataset, as character
#' @return The output from read.dta
#' @export
#' @importFrom foreign read.dta


webuse <- function(x){
  if(!is.character(x)) stop("'x' should be character")
  link <- paste("http://www.stata-press.com/data/r12/", x, ".dta", sep="")  
  
  out <- read.dta(link)
  
  return(out)
  
}