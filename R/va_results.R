va_results <- function(table, column_types){
  return(
    structure(
      list(
        table = table,
        column_types = column_types
      ),
      class = 'va_results'
    ))
}

#' @export
print.va_results <- function(x, ...){
  cat("VA results from a linearized model\n")
  invisible(x)
}
