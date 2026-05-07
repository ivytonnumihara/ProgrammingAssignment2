## Put comments here that give an overall description of what your
## functions do 
## Response: This function will "cache" its inverse or in other words, 
## it remembers inverse of a matrix and cache it 
## so that it doesn't have to compute it repeatedly

## Write a short comment describing this function
## Response: The functions below allow the following:
##set — update the matrix and clear the cache
##get — return the matrix
##setInverse — store the inverse
##getInverse — retrieve the cached inverse
 
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
  
      set <- function(y) {
          x <<- y
        inv <<- NULL
      }
  
  get <- function() x
  
  setInverse <- function(inverse) inv <<- inverse
  
  getInverse <- function() inv
  
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}


## Write a short comment describing this function
## the cacheSolve checks whether the inverse is already cached, if so it returns the cached version
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
  
        if (!is.null(inv)) {
            message("getting cached inverse")
            return(inv)
        }
  
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
