## This R script contains two functions that are used to create a special matrix
## object that stores a matrix and cache's its inverse.



## The makeCacheMatrix function takes a matrix as its input and creates 
## a special matrix object which have 4 functions for getting and setting matrix 
## and its inverse and 1 data variable to store its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  solution <- NULL
    
  getmatrix <- function(){x}
  setsolution <- function(solve) { solution <<- solve}
  
  getsolution <- function() {solution}
  setmatrix <- function(y = matrix()) {
    x <<- y
    solution <<- NULL
  }
  
  list(setmatrix = setmatrix,
       getmatrix = getmatrix,
       setsolution = setsolution,
       getsolution = getsolution)
  
}


## The cacheSolve function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
 
  solve <- x$getsolution()
  
  if(!is.null(solve)){
    message("getting cached data")
    return(solve)
  }
  
  tosolve <- x$getmatrix()
  solve <- solve(tosolve)
  x$setsolution(solve)
  solve
  
}
