## R Programming Assignment 2: Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## default the return variable to null
  inverseM<-NULL
  
  ## function to set the value of the matrix
  set <- function(y){
    x<<-y
    inverseM<<- NULL
  }
  
  ## returns the value of the input matrix
  get <- function() {x}
  
  ## caches the inverted matrix after it has been found
  setInverse<-function(inverse){
    inverseM<<-inverse
  }
  
  ## returns the value of the inverted matrix
  getInverse<-function() inverseM
  
  ##store the functions to apply to the makeCacheMatrix function
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## calls the getInverse function in makeCacheMatrix and sets inverseM equal to the result      
  inverseM <- x$getInverse()
  
  ##checkes if inverse has already been found and cached
  if(!is.null(inverseM)){
    message("retrieving cached data")
    return (inverseM)
  }
  
  ## retrieves the initial matrix
  origionalM <-x$get()
  
  ## solves for the inverse matrix
  inverseM<-solve(origionalM)
  
  ## calls the setInverse function so the inverse will be cached
  x$setInverse(inverseM)
  
  ##returns the inverse
  inverseM
  
}

