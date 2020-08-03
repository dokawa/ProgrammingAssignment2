

## A function that calculates the inverse of a matrix and cache the value, 
## returning the cached value if called again

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(matrix) {
    x <<- matrix
    inverse <<- NULL
  }
  get <- function() x
  set_inverse <- function(value) inverse <<- value
  get_inverse <- function() inverse
  list(get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## Receive the cache_matrix function and return the inverse of the matrix if in cache or
## calculate otherwise

## To test:
## m <- rbind(c(1, -1/4), c(-1/4, 1))
## mc <- makeCacheMatrix(m)
## mc <- makeCacheMatrix(m)
## should see the message "getting cached data" executing the second time

cacheSolve <- function(cache_matrix) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- cache_matrix$get_inverse()
  
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- cache_matrix$get()
  inverse <- solve(data, ...)
  cache_matrix$set_inverse(inverse)
  inverse
}
