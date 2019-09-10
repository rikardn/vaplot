#' Compute variability attribution
#'
#' This function uses the data structure prepared by the prepare functions and performs the actual
#' variability attribution computation. The function will return a data frame that can be used to
#' generate a plot.
#'
#' @param va_input An input data structure as produced by the prepare_va_* functions
#' @param conditioning A function or list determining in which order the computation should occur
#' @param idv The independent variable column
#' @param dvid The dependent variable id column
#'
#' @return A data frame
#' @export
compute_va <- function(va_input, conditioning = default_conditioning, idv = TIME, facets = NULL){
  # determine conditioning order
  idv <- rlang::enquo(idv)
  facets <- rlang::enquo(facets)
  if(is.function(conditioning)) cond_order <- conditioning(va_input)
  var_calc_lf(va_input, cond_order, idv = idv, facets = facets)
}


#' Determine conditioning order
#'
#' @param va_input An input data structure as produced by the prepare_va_* functions
#'
#' @return A list of variable names
#' @export
default_conditioning <- function(va_input){
  order <-colnames(va_input$omega)
  names(order) <- order
  return(order)
}
