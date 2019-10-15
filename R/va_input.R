va_input <- function(column_names, theta, omega,
                     sigma, derivative_data, input_file,
                     variable_types, variable_labels,
                     variable_names){
  if(missing(variable_types)) variable_types <- rep("iiv-re", NROW(omega))
  if(missing(variable_labels)) {
    variable_labels <- glue::glue("ETA({i})", i = seq_len(NROW(omega)))
  }
  if(missing(variable_names)) variable_names <- paste0("ETA", seq_len(NROW(omega)))
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
        variable_labels = variable_labels,
        variable_names = variable_names
      ),
      class = 'va_input'
    ))
}

#' Update Omega/Sigma matrix
#'
#' These functions allows to update the Omega and Sigma matricies associated with VA input structure.
#'
#' @param input A VA input as produced by the prepare_va_* functions
#' @param omega A covariance matrix
#' @param sigma A covariance matrix
#'
#' @return An updated VA input structure
#' @export
update_omega <- function(input, omega){
  if(NROW(input$omega)!=NROW(omega) || NCOL(input$omega)!=NCOL(omega))
     ui_error("The new Omega matrix needs to have the same dimensions as the old one.")
  input$omega <- omega
  return(input)
}

#' @rdname update_omega
update_sigma <- function(input, sigma){
  if(NROW(input$sigma)!=NROW(sigma) || NCOL(input$sigma)!=NCOL(sigma))
    ui_error("The new Sigma matrix needs to have the same dimensions as the old one.")
  input$sigma <- sigma
  return(input)

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
