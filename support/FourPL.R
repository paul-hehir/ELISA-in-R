# https://janalin.github.io/analyse-ELISA/calibration.html

# Four PL function 
fourPL <- function(x, Amin, Amax,log2EC50,B) {
  Amin + ((Amax - Amin) / (1 + 2^(-B * (x - log2EC50))))
}

fourPL_sample <- function(x, Amin, Amax,log2EC50,B,E) {
  Amin + ((Amax - Amin) / (1 + 2^(-B * (x - log2EC50 + E))))
}
