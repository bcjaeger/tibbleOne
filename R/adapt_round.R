
#' Adaptive rounding for tables
#' @param x a numeric vector
#' @return a character vector comprising rounded values.
#' @examples
#' adapt_round(c(0.12, 10.12, 100.12))
#' @export


adapt_round <- function(x){

  if(is_empty(x)) return("NA")

  output <- rep( "NA", vec_size(x) )

  if(all(is.na(x))) return(rep("NA", vec_size(x)))

  if(!is.numeric(x)) stop("x should be numeric", call. = FALSE)

  x_abs <- abs(x)

  loop_index <- which(!is.na(x))

  for(i in loop_index){

    if(x_abs[i] < 10) {
      dig = 2
    } else if(x_abs[i] < 100){
      dig = 1
    } else {
      dig = 0
    }

    output[i] <- format(
      round(x[i], dig),
      nsmall = dig,
      big.mark = ','
    )

  }

  output

}
