
schur_complement <- function(M, block_index){
  solve(M)[-block_index, -block_index] %>% solve()
  #  M[-block_index, -block_index] - M[block_index, -block_index] %*% MASS::ginv(M[block_index, block_index]) %*% M[-block_index, block_index]
}
