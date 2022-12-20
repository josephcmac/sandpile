library(extraDistr)

require(lattice)


rm(list = ls())

n_rows <- 80

sandPileInit <- function(n_rows) {
  matrix(data = rcat(n_rows^2, p=c(1/4, 1/4, 1/4, 1/4)) - 1, nrow = n_rows, 
                ncol = n_rows )
}

topple <- function(M, n_rows, i, j) {
  M[i,j] <- M[i,j] - 4
  if (i > 1) {
    M[i-1,j] <- M[i-1,j] + 1
  }
  if (i < n_rows) {
    M[i+1,j] <- M[i+1,j] + 1
  }
  if (j > 1) {
    M[i,j-1] <- M[i,j-1] + 1
  }
  if (j < n_rows) {
    M[i,j+1] <- M[i,j+1] + 1
  }  
  M
}

findExcess <- function(M) {
  data.frame( which(M>3, arr.ind = T) )
}

evolveStep <- function(M, n_rows) {
  L <- findExcess(M)
  L_length <-length( L[,1] )
  if (L_length > 0) {
    for (k in 1:L_length) {
      i <- L[k,1]
      j <- L[k,2]
      M <- topple(M, n_rows, i, j)
    }
  }
  M
}

evolve <- function(M, n_rows) {
  N <- evolveStep(M, n_rows)
  while(max(N - M) > 0.01) {
    M <- N
    N <- evolveStep(M, n_rows)
  }
  N
}

grainSand <- function(n_rows) {
  H <- matrix(data = sapply(1:n_rows^2, function(x) 0), nrow = n_rows, 
         ncol = n_rows );
  H[floor(n_rows/2), floor(n_rows/2)] <- 1;
  H
}

H <- grainSand(n_rows)

M <- sandPileInit(n_rows)


for (i in 1:10000) {
  M <- M + H;
  M <- evolve(M, n_rows);
  print(levelplot(M));
  Sys.sleep(0.5);
}




