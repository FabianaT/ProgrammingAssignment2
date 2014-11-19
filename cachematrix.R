
## makeCacheMatrix: saves the original matrix and the inversed matrix

makeCacheMatrix <- function(originalMatrix = matrix()) {
  inversedMatrix <- NULL
  
  ## Get & Set for the original matrix to be inversed
  set <- function(newMatrix) {
    originalMatrix <<- newMatrix
    inversedMatrix <<- NULL
  }
  get <- function() originalMatrix
  
  ## Get & Set for the inversed matrix
  setInverse <- function(inverse) inversedMatrix <<- inverse
  getInverse <- function() inversedMatrix
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Return a matrix that is the inverse of 'x'
## First, check is inverse matrix was already calculated. In case it was calculated, return the cache value.
## In case it was not calculated yet, calculates the inversed matrix of a given matrix and saved on the cache.

cacheSolve <- function(x) {
  ## get inverse matrix from the cache
  inversedMatrix <- x$getInverse()
  
  if(!is.null(inversedMatrix)) {
    ## if inverse matrix is at the cache, it is returned.
    message("getting cached data")
    return(inversedMatrix)
  }
  
  ## if inverse matrix is not at the cache, it is calculated to be returned.
  ## before returning it, it is saved on the cache
  
  originalMatrix <- x$get()             ## obtains the matrix the be inversed.
  inversedMatrix <- solve(originalMatrix)        ## calculate the inverse matrix
  x$setInverse(inversedMatrix)        ## save inversed matrix on cache
  
  inversedMatrix                               ## return inversed matrix
}
