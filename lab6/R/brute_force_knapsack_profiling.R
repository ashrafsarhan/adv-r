library(profvis)

# generate data
set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

x = knapsack_objects[1:12,]
W = 3500

#Not optimized code
profvis({
  n = nrow(x)  # number of items
  maxv = 0     # max value
  ev = c()     # element vector
  
  for(i in 0:(2^n-1)){
    iv = intToBits(i)
    sv = 0     # sum value
    sw = 0     # sum weight
    tv = c()   # temporary value
    v_vect <- c()
    w_vect <- c()
    for(j in 1:n){
      if(iv[j] == 1){
        v_vect <- c(v_vect, x[j,'v'])
        w_vect <- c(w_vect, x[j,'w'])
        tv <- c(tv, j)
      }
    }
    sv = sum(v_vect)
    sw = sum(w_vect)
    
    if((sv > maxv) && (sw <= W)){
      maxv = sv
      ev = tv
    }
  }
  
  rslt <- list(value=round(maxv), elements=ev)
  print(rslt)
})
