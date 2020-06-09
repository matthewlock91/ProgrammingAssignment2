
## The functions help to save time in finding the inverse of a matrix, by seeing if it has already been calculated and retrieving it if so. If not, it is cached for future. 


## This function makes a special 'matrix', actually a list containing functions to set and get the values and inverses of the specified matrix.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(solve) inv <<- solve
      getinv <- function() inv
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function will call the relevant inverse solving functions and applies to the specified matrix, caching the result.
## If there already exists an inverse, this will be used instead (before recalculating it).

cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached result")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
      
}


