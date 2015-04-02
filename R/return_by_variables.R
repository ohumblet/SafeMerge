return_by_variables <- function(by, by.i) {

  if( !is.null(by) ) {
    return(by)
    } else if( !is.null(by.i) ) {
      return(by.i)
    } else stop("Both by and by.i were NULL.")

}


