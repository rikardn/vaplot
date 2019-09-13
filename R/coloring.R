color_like_hadley <- function(result, ...){
  var_names <- names(which(result$column_types == "variability"))
  nvars <- length(var_names)
  hues = seq(15, 375, length = nvars + 1)
  colors <- hcl(h = hues, l = 65, c = 100)[1:nvars]
  names(colors) <- var_names
  return(colors)
}
