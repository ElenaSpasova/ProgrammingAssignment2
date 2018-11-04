## These functions work together to provide functionality to calculate inverted matrices and to cache them for later, faster access. 

## The function "makeCacheMatrix" creates an object with two properties: one for the orginal matrix, and one - for the inverted matrix.  Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverted_matrix <- NULL
  set <- function(y) {
    x <<- y
    inverted_matrix <<- NULL
  }
  get <- function() x
  setinverted <- function(new_inverted_matrix) inverted_matrix <<- new_inverted_matrix
  getinverted <- function() inverted_matrix
  list(set = set, get = get,
       setinverted = setinverted,
       getinverted = getinverted)
}


## "cacheSolve" is a function retrieving the cached inverted matrix (in case there is one) 
## or calculates the inverted matrix and caches it (in that case the message "calculating new inverted matrix" is shown). 

cacheSolve <- function(x, ...) {
  inverted_matrix <- x$getinverted()
  if(!is.null(inverted_matrix)) {
    message("getting cached data")
    return(inverted_matrix)
  }
  message("calculating new inverted matrix")
  data <- x$get()
  inverted_matrix <- solve(data, ...)
  x$setinverted(inverted_matrix)
  inverted_matrix
}
