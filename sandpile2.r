library(extraDistr)

require(tidyverse)


rm(list = ls())

n_rows <- 50

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

n_cells <- n_rows*n_rows

S <- c()

for (i in 1:5000) {
  M <- M + H;
  N <- evolve(M, n_rows);
  temp <- n_cells - sum(  M == N  );
  if (temp != 0) {
    S <- append(S, log(temp));
  }
  M <- N;
}

rm(temp)
c <- length(S)
min_magnitude <- 30
max_magnitude <- 100
i_lenght <- 1/15
L <- data.frame( magnitude = (min_magnitude:max_magnitude)/max_magnitude, 
                 freq = sapply(min_magnitude:max_magnitude, function(i) length(S[S >= i_lenght*i]))/c )

lmodel <- lm(formula = freq ~ magnitude, data = L)

L <- data.frame( magnitude = L$magnitude, 
                 freq = L$freq,
                 pred = lmodel$fitted.values) 


ggplot(L, aes(x = magnitude, y = value, color = variable)) + 
  geom_point(aes(y = freq, col = "real")) + 
  geom_line(aes(y = pred, col = "prediction"))

summary(lmodel)
