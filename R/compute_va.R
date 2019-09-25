#' Compute variability attribution
#'
#' This function uses the data structure prepared by the prepare functions and performs the actual
#' variability attribution computation. The function will return a data frame that can be used to
#' generate a plot.
#'
#' @param va_input An input data structure as produced by the prepare_va_* functions
#' @param conditioning A function or list determining in which order the computation should occur
#' @param idv The independent variable column
#' @param facets Columns to be used for facetting
#'
#' @return A data frame
#' @export
compute_va <- function(va_input, conditioning = default_conditioning, idv = "TIME", facets = NULL){
  # determine conditioning order
  idv <- rlang::enquo(idv)
  facets <- rlang::enquo(facets)
  if(is.function(conditioning)) cond_order <- conditioning(va_input)
  var_calc_lf(va_input, cond_order, idv = idv, facets = facets)
}


#' Determine conditioning order
#'
#' These functions automatically generate a conditioning order based on the available information.
#'
#' @param va_input An input data structure as produced by the prepare_va_* functions
#'
#' @return A list of variable names
#' @export
default_conditioning <- function(va_input){
  # have covariates first
  if(any(va_input$variable_types=="covariate")){
    cov_vars <- which(va_input$variable_types=="covariate")
    non_cov_vars <- which(va_input$variable_types!="covariate")
    order <- va_input$variable_names[c(cov_vars, non_cov_vars)]
    names(order) <- va_input$variable_labels[c(cov_vars, non_cov_vars)]
  }else{
    order <- va_input$variable_names
    names(order) <- va_input$variable_labels
  }
  return(order)
}
