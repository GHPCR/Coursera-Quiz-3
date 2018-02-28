
## This function uses Construction Functions to enhance processing time to calculate the Inverse of a 
## square matrix by avoiding unnecessary recalculations. To do so, the following 2 functions have been 
## created:

## 1) makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  Inv <- NULL
  
  set <- function(y){
    
    x <<- y
    Inv <<- NULL    
  }

  get <- function() { 
    x
  }
  
  setinverse <- function (solve) {
    Inv <<- solve
  }
  
  getinverse <- function() {
    Inv
  }
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}



## 2) cacheSolve: 
##    This function computes the inverse of the special "matrix" returned by 
##    makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed),
##    then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  Inv <- x$getinverse()
  
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
 
  data <- x$get()
  
  Inv <- data^-1
    
  x$setinverse(Inv)
  
  Inv
  
}
