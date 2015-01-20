## These two functions handle the caching of time-consuming computation 
## of a matrix's inverse

## makeCacheMatrix creates a special vector, which is really a list
## containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse

makeCacheMatrix <- function(x = numeric()) {
  mI <- NULL
  set <- function(y){
    x <<- y
    mI <<- NULL
  }
  get <- function() x
  setMInverse <- function(matrixInverse) mI <<- matrixInverse
  getMInverse <- function() mI
  list( set= set, get= get,
        setMInverse= setMInverse,
        getMInverse= getMInverse)
}

## cacheSolve calculates the inverse of the special "vector" created 
## with the above function.
## It first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value 
## of the inverse in the cache via the setMInverse function.

cacheSolve <- function(x, ...) {
  mI <- x$getMInverse()
  if(!is.null(mI)){
    message("getting cached data")
    return (mI)
  }
  data <- x$get()
  mI <- solve(data)
  x$setMInverse(mI)
  mI
}
