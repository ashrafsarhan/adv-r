brute_force_knapsack <- function(x, W){
  n = nrow(x)
  maxv = 0
  ev = c()
  
  for(i in 1:(2^n)){
    iv = intToBits(i)
    sv = 0
    sw = 0
    tv = c()
    for(j in 1:n){
      if(iv[j] == 1){
        sv = sv + x[j, ]$v
        sw = sw + x[j, ]$w
        tv = c(tv, j)
      }
    }

    if((sv > maxv) && (sw <= W)){
      maxv = sv
      ev = tv
    }
  }
  
  list(value=round(maxv), elements=ev)
}

# brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500)
