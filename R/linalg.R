
schur_complement <- function(M, block_index){
  ginv(M)[-block_index, -block_index] %>% ginv()
  #  M[-block_index, -block_index] - M[block_index, -block_index] %*% MASS::ginv(M[block_index, block_index]) %*% M[-block_index, block_index]
}

schur_complement_named <- function(M, block_names) {
  ginv(M) %>% drc(block_names) %>% ginv()
}

# var(f(x)) with Var(x)=sigma
var_lf <- function(dfdx, sigma) t(dfdx) %*% sigma %*% dfdx

# var(f(x)|x_1) with Var(x) = sigma
condvar_lf <- function(dfdx, sigma, given)  t(dr(dfdx, given)) %*% drc(sigma, given) %*% dr(dfdx, given)

ginv <- function(X){
  Xinv <- MASS::ginv(X)
  colnames(Xinv) <- rownames(Xinv) <- colnames(X)
  return(Xinv)
}
# var_from_vars <- function(dfdx, sigma, vars){
#     if(!any(colnames(sigma) %in% vars)) return(0)
#     x <- sr(dfdx, vars)+t(dr(dfdx, vars))%*%dsrc(sigma, vars, vars)%*%MASS::ginv(src(sigma, vars))
#     x %*% src(sigma, vars) %*% t(x)
# }
