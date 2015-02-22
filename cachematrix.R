## Contains two functions to manage a cache for the inverse of a matrix.
## In conjunction both functions serve to calculate the inverse 
## matrix only once and keep it in memory.

## makeCacheMatrix Creates a new object containing a matrix and 
## an empty reference to its inverse matrix.
## 
## Contains four useful functions: 
##   
## 1) set(y): it sets y as the matrix referenced. Also it deletes the cache for the previous matrix
## 2) get(): it obtains the matrix associated with this object
## 3) set_inv(inverse): it sets inverse as the associated inverse matrix for x
## 4) get_inv(): it returns the associated inverse matrix of x, if exists. Otherwise returns NULL

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set_inv <- function(inverse) inv <<- inverse
  get_inv <- function() inv
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}


## cacheSolve It returns the inverse matrix of x.
## It checks if x contains a cache of its inverse, if it's present in returns it.
## Otherwise it calculates the inverse, saves a reference to it into x (through the method set_inv)
## and then returns it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$get_inv()
  if(!is.null(inv)) {
    message("Getting cached inverse matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$set_inv(inv)
  inv
}
