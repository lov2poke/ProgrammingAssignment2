# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # This sets the value of the matrix:
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # This gets the value of the matrix:
  get <- function() x
  
  # This sets the value of the inverse:
  setInverse <- function(inverse) inv <<- inverse
  
  # This sets the value of the inverse:
  getInverse <- function() inv
  
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# This function creates the inverse of the special "matrix" returned by makeCacheMatrix.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  # This returns it from the cache if the inverse is already calculated:
  if (!is.null(inv)) {
    message("Getting Cached Data")
    return(inv)
  }
  
  # Otherwise, this will calculate the inverse:
  data <- x$get()
  inv <- solve(data, ...)
  
  # This sets the value of the inverse in the cache:
  x$setInverse(inv)
  
  inv
}
