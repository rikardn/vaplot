va_input <- function(column_names, theta, omega,
                     sigma, derivative_data, input_file,
                     variable_types, variable_labels){
  if(missing(variable_types)) variable_types <- rep("iiv-re", NROW(omega))
  if(missing(variable_labels)) {
    variable_labels <- glue::glue("ETA({i})", i = seq_len(NROW(omega)))
  }
  return(
    structure(
      list(
        column_names = column_names,
        theta = theta,
        omega = omega,
        sigma = sigma,
        derivative_data = derivative_data,
        input_file = input_file,
        variable_types = variable_types,
        variable_labels = variable_labels
      ),
      class = 'va_input'
    ))
}

#' @export
print.va_input <- function(x, ...){
  cat("VA input from a linearized NONMEM run\n")
  invisible(x)
}


#' @export
summary.va_input <- function(object, ...){
  cat("VA input from a linearized NONMEM run:\n")
  cat("\tInput file:\t", basename(object$input_file),"\n")
  cat("\tNumber of ETA:\t", nrow(object$omega),"\n")
  cat("\tNumber of EPS:\t", nrow(object$sigma),"\n")
  cat("\tVariables:\t", paste(object$column_names, collapse = ", "), "\n")
  invisible(object)
}
