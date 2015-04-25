## functions to compute the inverse of a matrix 
## and to cache the value for later use
## if a matrix inverse has already been computed, 
## we used the cached value

## returns a list containing functions to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  # we will store the calculated cached inverse matrix here
  inverseCache <- NULL
  
  # set and get for the input matrix
  set <- function(y) {
    x <<- y
    inverseCache <<- NULL
  }
  get <- function() x
  
  # set and get for the cached inverse matrix
  setCache <- function(inverseMatrix) inverseCache <<- inverseMatrix
  getCache <- function() inverseCache
  
  # return the list of functions
  list(set = set, get = get,
       setCache = setCache,
       getCache = getCache)
}


## Calculates the inverse of a matrix, store it in the cache, 
## and return the inverse.
## If the matrix was already solved, we return the cached value 

cacheSolve <- function(x) {
  
  # check if the inverse is in the cache, return if found
  inverseMatrix <- x$getCache()
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  
  # there was no cache hit, so get the matrix and 
  # calculate the inverse
  data <- x$get()
  inverseMatrix <-solve(data)
  x$setCache(inverseMatrix)
  inverseMatrix
}