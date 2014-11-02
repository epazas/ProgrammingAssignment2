## There are 2 slightly different implementations
## The first (cacheSolve) returns the inverse 
## of a given matrix returning a cached value, if exists.
## If not, performs the calculation and stores the result
## The second (cacheSolveEx) returns the inverse 
## of a given matrix returning a cached value, if exists.
## If not, performs the calculation and stores the result
## Apart from checking for a cached value, it performs an
## equality check with the given original matrix that was
## initially used for the calculation

## Calculates and stores the inverse of a matrix, without
## making validity checks on the input

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


## Returns the inverse of a matrix, checking
## for an existing cached value

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting inverse from cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

## Returns the inverse of a matrix, checking
## for an existing cached value
## Performs an extra check against the given y input that 
## points to the original matrix used in construction of x 

cacheSolveEx <- function(x, y, ...) {  
  data <- x$get()
  if (matequal(data, y)){
    m <- x$getinv()
    if(!is.null(m)) {
      message("getting inverse from cached data")
      return(m)
    }
  }
  else
    message("original matrix has been changed, recalculating")
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

## Calculates equality check between 2 matrices

matequal <- function(x, y){
  is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
}
