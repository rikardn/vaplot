va_results <- function(table, column_names){
  return(
    structure(
      list(
        table = table,
        column_names = column_names
      ),
      class = 'va_results'
    ))
}

#' @export
print.va_results <- function(x, ...){
  cat("VA results from a linearized model\n")
  invisible(x)
}
