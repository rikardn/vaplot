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
  nvars <- length(var_names)
  colors <- colorspace::qualitative_hcl(n = nvars, h1 = 10, h2 = -350, c1 = 50, l1 = 80)
  names(colors) <- var_names
  return(colors)
}

#' @export
#' @describeIn coloring Highlights covariates
coloring_highlight_covs <- function(result, ...){
  var_names <- get_variability_cols(result)
  nvars <- length(var_names)
  cov_index <- purrr::map_lgl(result$variability_sources$variable_types, ~any(.x=="covariate"))
  colors <- colorspace::qualitative_hcl(n = nvars, h1 = 10, h2 = -350, c1 = 50, l1 = 80)
  colors[!cov_index] <- colors[!cov_index] %>%
     colorspace::desaturate(amount = 0.5) %>%
      colorspace::lighten(amount = 0.3)
  names(colors) <- names(result$variable_types)
  return(colors)
}
