makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  ## Set the value
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse<- function(solve) i <<- solve 
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  ## If in cache, returned the cached version
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## If not in cache, get it
  data <- x$get()
  i <- solve(data, ...)
  
  ## Store value in cache
  x$setinverse(i)
  i
}
