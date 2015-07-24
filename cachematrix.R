## makeCacheMatrix creates a special "matrix" object 
## then, cacheSolve calculates the inverse of the matrix.
## If the matrix inverse has already been calculated, it will instead 
## find it in the cache and return it, and not calculate it again

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) m <<-inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve returns the inverse of the special "matrix" created by makeCacheMatrix.
## If the inverse has already been calculated, cacheSolve retrieves it, while if
## not, it computes, caches, and returns it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  m <- x$getinverse()
  
  if (!is.null(m)) {
    message("getting cached inverse of a matrix")
    return(m)
  } 
  
  else {
    m <- solve(x$get())
    x$setinverse(m)
    return(m)
  }
  
}
