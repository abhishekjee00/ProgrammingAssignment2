## Put comments here that give an overall description of what your
## functions do

## Function returns mod 3 and if the computed value is 0 then return value is set to 3
mod3 <- function(x = numeric()) {
  if (x%%3 == 0) { 
    ret_value <- 3 
  } else { 
    ret_value <- x%%3 
  }
  ret_value
}

## Function computes inverse of 3x3 Matrix
inverse <- function(m = martix()) {
  if (nrow(m) == 2) {
    m_inverse <- solve(m)
  } else {
    m_inverse <- matrix(NA,3,3)    
    m_det <- det(m)
    m_transpose <- t(m)
    
    for (i in 1:3) {
      for (j in 1:3) {
        r1 <- mod3(i+1)
        r2 <- mod3(i+2)
        c1 <- mod3(j+1)
        c2 <- mod3(j+2)
        m_inverse[i,j] <- det(m_transpose[c(r1,r2),c(c1,c2)])/m_det
      }
    } 
  }
  m_inverse
}

## Functions make cache matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  
  
  set <- function(y) {
      x <<- y
      m <<- NULL      
  }  
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  m <- x$get()
  if (nrow(m) != ncol(m)) {
    message("Not a balanced matrix")
    message("Cache not set")
    return(m)
  } else if (nrow(m) > 3) {
    message("Function does inverse of 2x2 or 3x3 matrix")
    message("Cache not set")
    return(m)
  } else if (nrow(m) < 2 ) {
    message("Function does inverse of 2x2 or 3x3 matrix")
    message("Cache not set")
    return(m)
  }
    
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- inverse(data, ...)
  x$setinverse(m)
  m
}
