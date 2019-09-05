
lower_tri_vec_to_mat <-  function(vec){
  dim <- (sqrt(1+4*length(vec)*2)-1)/2
  mat <- matrix(0, dim, dim)
  mat[upper.tri(mat, T)] <- vec
  mat[lower.tri(mat, T)] <- t(mat)[lower.tri(mat, T)]
  return(mat)
}

# function to drop the named rows and columns in a square matrix
drc <- function(x, drop){
  if(NROW(x) == NCOL(x)){
    vars_to_select <- colnames(x)[!colnames(x) %in% drop]
    return(x[vars_to_select, vars_to_select, drop = F])
  }
  stop("Only square matricies supported")
}
# function to drop the named elements in a vector
dr <- function(x, drop){
  vars_to_select <- names(x)[!names(x) %in% drop]
  return(x[vars_to_select, drop = F])
}

src <- function(x, select){
  if(NROW(x) == NCOL(x)){
    vars_to_select <- select[select %in% colnames(x)]
    return(x[vars_to_select, vars_to_select, drop = F])
  }
  stop("Only square matricies supported")
}

sr <- function(x, select){
  vars_to_select <- select[select %in% names(x)]
  return(x[vars_to_select, drop = F])
}

dsrc <- function(x, drop_rows, select_cols){
  if(NROW(x) == NCOL(x)){
    rows_to_select <- colnames(x)[!colnames(x) %in% drop_rows]
    columns_to_select <- select_cols[select_cols %in% colnames(x)]
    return(x[rows_to_select, columns_to_select, drop = F])
  }
  stop("Only square matricies supported")
}

set_rcnames <- function(m, names) {
  rownames(m) <- colnames(m) <- names
  return(m)
}

# extracts patterns using regex and converts to integers
extract_int <- function(x, regex) {
  regexpr(regex, x, perl = T) %>%
    regmatches(x, m = .) %>%
    as.integer()
}
