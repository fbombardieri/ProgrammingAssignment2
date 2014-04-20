## The following functions allow to cache the inverse of a matrix in order to be able 
## to re-acquire the cached inverse matrix without the need to recompute it

## makeCacheMatrix creates a special "matrix" object that can cache its inverse,
## it provides a list of functions to: 
## 1) set the matrix
## 2) get the matrix
## 3) set the inverse matrix
## 4) get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {

  ## Initializing inverse of the matrix
  inv <- NULL
  
  ## Function to set the original matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Function to get the original matrix
  get <- function() x
  
  ## Function to set the inverse of the original matrix
  set.inverse <- function(inverse) inv <<- inverse
  
  ## Function to get the inverse of the original matrix
  get.inverse <- function() inv
  
  ## List of the functions
  list( set = set, 
        get = get,
        set.inverse = set.inverse,
        get.inverse = get.inverse )
}


## cacheSolve returns the cached inverse of the matrix x if this is available,
## otherwise the function computes and caches the inverse of the matrix and returns it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## Tries to retrieve the cached inverse of the matrix
  inv <- x$get.inverse()
  
  ## Check if the cached value is available
  if(!is.null(inv)) {
  
    ## Cached value found, returning it
    message("getting cached data")
    return(inv)
  }
  
  ## The inverse of the matrix has to be computed, retrieving the original matrix
  data <- x$get()
  
  ## Computing, caching and returning the inverse of the matrix
  inv <- solve(data, ...)
  
  x$set.inverse(inv)
  
  inv
}
