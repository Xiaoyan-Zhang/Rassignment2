## makecacheMatrix creates a special matrix object that can cache its inverse
## cachesSolve use makeCacheMatrix to get/set inverted matrix in cache

makeCacheMatrix <- function(x = matrix()) {
  
  #initialize
  cache <- NULL
  set <- function(y){
    x <<- y
    cache <<- NULL
  }
  
  get <- function() x
  
  SetMatrix <- function(inverse) cache <<- inverse
  
  getInverse <- function() cache
  
  # return the created functions to the working environment
  list(set = set, get = get, SetMatrix = SetMatrix, getInverse = getInverse)

}


# cacheSolve computes the inverse of the special matrix 
# returned by makeCacheMatrix above. If the inverse has
# already been caculated, then retrieve the inverse from 
# the cache

cacheSolve <- function(x, ...) {
        # attempt to get the inverse of the matrix in cache
        cacheInv <- x$getInverse()
        
        # return inverted matrix from cache if exists
        # or create matrix in working environment
        if(!is.null(cacheInv)){
          message("getting cached data")
          return(cacheInv)
        }
        
        # otherwise, calculate the inverse
        mat.data = x$get()
        cacheInv = solve(mat.data, ...)
        
        # set the value of the inverse in the cache
        x$setMatrix(cacheInv)
        
        # return result
        return(cacheInv)

}
