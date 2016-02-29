## The following two functions, makeCacheMatrix and cachesolve, work 
## in conjunction to cache the inverse of a matrix


## makeCacheMatrix creates a list of four functions
## set, get, setsolve, and getsolve
## that will be called by cachesolve
## Note that this function rewrites the cached value as NULL every time it is called

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve checks to see if the inverse of a matrix has already been 
## cached, and if so returns that cached inverse.  if it has not been
## cached, it computes the matrix inversion and stores the inverse as a
## cached value


cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}



