## This pair of functions enables the inverse of a matrix to be stored in a cache
## so that if it is needed repeatedly, it can be fetched rather than calculated every time.

## makeCacheMatrix takes a matrix x as its argument and creates an object with four getting 
## and setting functions and two variables, x (the original matrix) and m (either the inverse
## matrix if it has already been calculated, or NULL if not).  

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}
  


## cacheSolve takes the makeCacheMatrix object and first checks whether the inverse has already
## been calculated.  If so, it gets and returns the already-calculated m; if not,
## it calculates and returns the inverse as m (using the setinv function from makeCacheMatrix)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  
}
