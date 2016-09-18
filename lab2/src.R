#732A94_lab2
name <- 'Ashraf Sarhan'
liuid <- 'ashsa762'

#1.1 Conditional statements
#1.1.1
#The function should return either ‘‘Player 1 wins!’’, ‘‘Player 2 wins!’’ or ‘‘Draw!’’.
sheldon_game <- function(player1, player2) {
  #Choices
  rock <- 'rock'
  paper <- 'paper'
  scissors <- 'scissors'
  lizard <- 'lizard'
  spock <- 'spock'
  choices <- c(rock, paper, scissors, lizard, spock)
  if(!(player1 %in% choices) || !(player2 %in% choices)) {
    stop() 
  } else {
    p1_win <- 'Player 1 wins!'
    p2_win <- 'Player 2 wins!'
    no_win <- 'Draw!'
    if (player1 == rock && player2 == scissors) { #Rock crushes Scissors 
      return(p1_win)
    }else if(player2 == rock && player1 == scissors) {
      return(p2_win)  
      } else if (player1 == scissors && player2 == paper) { #Scissors cuts Paper 
        return(p1_win)  
      } else if (player2 == scissors && player1 == paper) {
        return(p2_win)
      } else if (player1 == paper && player2 == rock) { #Paper covers Rock 
        return(p1_win)  
      } else if (player2 == paper && player1 == rock) {
        return(p2_win)
      } else if (player1 == rock && player2 == lizard) { # Rock crushes Lizard 
        return(p1_win)  
      } else if (player2 == rock && player1 == lizard) {
        return(p2_win)
      } else if (player1 == lizard && player2 == spock) { #Lizard poisons Spock 
        return(p1_win)
      } else if(player2 == lizard && player1 == spock) {
        return(p2_win)
      } else if (player1 == spock && player2 == scissors) { #Spock smashes Scissors
        return(p1_win)
      } else if (player2 == spock && player1 == scissors) {
        return(p2_win)
      } else if (player1 == scissors && player2 == lizard) { #Scissors decapitates Lizard 
        return(p1_win)
      } else if (player2 == scissors && player1 == lizard) {
        return(p2_win)
      } else if (player1 == lizard && player2 == paper) { #Lizard eats Paper
        return(p1_win)
      } else if (player2 == lizard && player1 == paper) {
        return(p2_win)
      } else if (player1 == paper && player2 == spock) { #Paper disproves Spock 
        return(p1_win)
      } else if (player2 == paper && player1 == spock) {
        return(p2_win)
      } else if (player1 == spock && player2 == rock) { #Spock vaporizes Rock
        return(p1_win)
      } else if (player2 == spock && player1 == rock) {
        return(p2_win)
      } else {
        return(no_win)
      }
    }
  }

#1.2 for loops
#1.2.1
my_moving_median <- function(x, n, ...) {
  if(!is.numeric(x) || !is.numeric(n)) {
    stop() 
  } else {
    na.rm <- list(...)$na.rm
    if (is.null(na.rm)) {
      na.rm <- FALSE
    }
    vect_size <- length(x)
    widow_size <- n
    mvm <- c()
    for (i in 1:vect_size) {
      window <- c()
      for (j in 0:widow_size) {
        elem_idx <- i+j 
        if(elem_idx <= vect_size) {
          elem <- x[elem_idx]
          window <- c(window, elem)
        }else {
          return(mvm)
        }
      }
      md <- median(window, na.rm = na.rm)
      mvm <- c(mvm, md)
    }
  }
}

#1.2.2
for_mult_table <- function(from, to) {
  if(!is.numeric(from) || !is.numeric(to) || from < 0 || to < 0) {
    stop() 
  } else {
    range <- c(from:to)
    dim <- (to-from)+1
    mat = matrix(0, nrow = dim, ncol = dim)
    for (i in 1:dim) {
      r <- range * range[i]
      mat[i,] <- r
    }
    #Set rows names
    rownames(mat) <- range
    #Set cols names
    colnames(mat) <- range
    return(mat)
  }
}

#1.3 while loops
#1.3.1
find_cumsum <- function(x, find_sum) {
  if(!is.numeric(x) || !is.numeric(find_sum)) {
    stop() 
  } else {
    cumsum <- cumsum(x)
    cumsum_size <- length(cumsum)
    i <- 1
    while(i <= cumsum_size){
      curr_cumsum <- cumsum[i]
      if(curr_cumsum > find_sum){
        return(curr_cumsum)
      }
      i <- i+1
    }
    return(cumsum[cumsum_size])
  }
}

#1.3.2
while_mult_table <- function(from, to) {
  if(!is.numeric(from) || !is.numeric(to) || from < 0 || to < 0) {
    stop() 
  } else {
    range <- c(from:to)
    dim <- (to-from)+1
    mat = matrix(0, nrow = dim, ncol = dim)
    i <- 1
    while (i <= dim) {
      r <- range * range[i]
      mat[i,] <- r
      i <- i+1
    }
    #Set rows names
    rownames(mat) <- range
    #Set cols names
    colnames(mat) <- range
    return(mat)
  }
}

#1.4 repeat and loop controls
#1.4.1
repeat_find_cumsum <- function(x, find_sum) {
  if(!is.numeric(x) || !is.numeric(find_sum)) {
    stop() 
  } else {
    cumsum <- cumsum(x)
    cumsum_size <- length(cumsum)
    i <- 1
    repeat {
      curr_cumsum <- cumsum[i]
      if(curr_cumsum > find_sum){
        return(as.numeric(curr_cumsum))
      }
      i <- i+1
      if(i > cumsum_size) {
        break
      }
    }
    return(as.numeric(cumsum[cumsum_size]))
  }
}

#1.4.2
repeat_my_moving_median <- function(x, n, ...) {
  if(!is.numeric(x) || !is.numeric(n)) {
    stop() 
  } else {
    na.rm <- list(...)$na.rm
    if (is.null(na.rm)) {
      na.rm <- FALSE
    }
    vect_size <- length(x)
    widow_size <- n
    mvm <- c()
    i <- 1
    repeat {
      window <- c()
      j <- 0
      repeat {
        if (j <= widow_size) {
          elem_idx <- i+j
          if(elem_idx <= vect_size) {
            elem <- x[elem_idx]
            window <- c(window, elem)
          }else {
            return(mvm)
          }
          j <- j+1 
        }else {
          j <- 0
          break
        }
      }
      md <- median(window, na.rm = na.rm)
      mvm <- c(mvm, md)
      i <- i+1
    }
  }
}

#1.5 Environment
#TODO to be reviewed
#1.5.1
in_environment <- function(env) {
  content <- ls(env)
  return(content)
}

#1.6 Functionals
#1.6.1
cov <- function(X) {
  if(!is.data.frame(X)) {
    stop() 
  } else {
    vect <- lapply(X, function(x) {
      sd(x)/mean(x)
    })
    cols <- names(vect)
    rslt <- c()
    for (c in cols) {
      rslt[[c]] <- vect[[c]]
    }
    return(rslt)
    }
}

#1.7 Closures
moment <- function(i) {
  if (!is.numeric(i)) {
    stop('i args must be numeric')
  }
  mom <- i
  f <- function(x) {
    if (mom == 0) {
      return(1)
    } else if (mom == 1) {
      return(mean(x))
    } else if (mom == 2) {
      return(var(x))
    } else {
      mean <- mean(x)
      return(sum((x-mean)^mom)/var(x)^mom)
    }
  }
}