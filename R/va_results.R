va_results <- function(table, column_types, variability_sources){
  return(
    structure(
      list(
        table = table,
        column_types = column_types,
        variability_sources = variability_sources
      ),
      class = 'va_results'
    ))
}

#' @export
print.va_results <- function(x, ...){
  cat("VA results from a linearized model\n")
  invisible(x)
}
