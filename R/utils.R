
lower_tri_vec_to_mat <-  function(vec){
  dim <- (sqrt(1+4*length(vec)*2)-1)/2
  mat <- matrix(0, dim, dim)
  mat[upper.tri(mat, T)] <- vec
  mat[lower.tri(mat, T)] <- t(mat)[lower.tri(mat, T)]
  return(mat)
}
