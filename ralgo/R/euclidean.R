#Name: Yumeng Li
#Liuid: yumli241

#1.1 Write the R code
#1.1.1 euclidean()

euclidian <- function(a,b) {
  x = abs(a)
  y = abs(b)
  if((round(x) == x) && (round(y) == y)){
    while(abs(x-y) != 0) {
      z = abs(x-y)
      x = min(x,y)
      y = z
      if(abs(x) == abs(y)){
        break()
      }
    }
    abs(x)
  }else{
    print("a, b must be intergers!") 
  }
}

#euclidian(123612, 13892347912)
#euclidian(123612, -13892347912)
#euclidian(-100, 1000)
#euclidian(100, 1000)
