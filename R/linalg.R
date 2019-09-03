
schur_complement <- function(M, block_index){
  solve(M)[-block_index, -block_index] %>% solve()
  #  M[-block_index, -block_index] - M[block_index, -block_index] %*% MASS::ginv(M[block_index, block_index]) %*% M[-block_index, block_index]
}

# var(f(x)) with Var(x)=sigma
var_lf <- function(dfdx, sigma) t(dfdx) %*% sigma %*% dfdx

# var(f(x)|x_1) with Var(x) = sigma
condvar_lf <- function(dfdx, sigma, given)  t(dr(dfdx, given)) %*% drc(sigma, given) %*% dr(dfdx, given)
