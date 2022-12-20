library(Rcpp)

rm(list = ls())

evalCpp("2+3")

myFunct <- cppFunction("
            int f(int x) {
            if (x == 0){
              return 1;
            }
            else {
              if (x == 1) {
                return 1;
              }
              else {
                return f(x-1) + f(x-2);
                } 
              }
            }")


sapply(1:10, myFunct) 



