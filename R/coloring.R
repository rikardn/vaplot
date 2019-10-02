color_like_hadley <- function(result, ...){
  var_names <- get_variability_cols(result)
  nvars <- length(var_names)
  hues <-  seq(15, 375, length = nvars)
  colors <- hcl(h = hues, l = 65, c = 100)
  names(colors) <- var_names
  return(colors)
}

#' Select VA plot coloring
#'
#' These functions generate color palettes for the plotting functions
#'
#' @param result A va_result object
#' @param ... Additional arguments
#'
#' @return A vector of colors (in hexadecimal representation)
#' @name coloring
NULL

#' @export
#' @describeIn coloring Default
coloring_default <- function(result, ...){
  var_names <- get_variability_cols(result)
  colors <- colorspace::qualitative_hcl(n = length(var_names), h1 = 0, h2 = 260, c1 = 80, l1 = 60)
  names(colors) <- var_names
  colors[get_ruv_cols(result)] <- rgb(0.5,0.5,0.5)
  return(colors)
}

#' @export
#' @describeIn coloring Highlights covariates
coloring_highlight_covs <- function(result, ...){
  var_names <- get_variability_cols(result)
  nvars <- length(var_names)
  cov_index <- var_names %in% get_cov_dependent_cols(result)
  colors <- colorspace::qualitative_hcl(n = nvars,  h1 = 0, h2 = 260, c1 = 80, l1 = 60)
  names(colors) <- var_names
  colors[!cov_index] <- colors[!cov_index] %>%
     colorspace::desaturate(amount = 0.5) %>%
      colorspace::lighten(amount = 0.3)
  colors[get_ruv_cols(result)] <- rgb(0.7,0.7,0.7)
  return(colors)
}
