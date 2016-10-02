## The following functions are designed to both solve the inverse of inputted matrices (if invertible) and
## to cache/store the information for that invertible matrix once the computation is done. This is being done
## so as to make it easier to retrieve solutions if the solution has already been computed previously.

## makeCacheMatrix take a given matrix and sets up a list that stores the information for that given matrix
## and it's inverse for later use

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # create object m within the makeCacheMatrix environment
  set <- function(y) { # function sets/stores matrix wi
    x <<- y # set 
    m <<- NULL # set m to NULL in PARENT environment (makeCacheMtrix environment
  }
  get <- function() x # gets x
  setsolve <- function(inverse) m <<- inverse # sets the inverse
  getsolve <- function() m # gets the inverse 
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve) # list created that set matrix and solution for easier future retrieval
}


## Returns a matrix that is the inverse of 'x'; if x already previously computed
## then solution is retrieved from list of makeCacheMatrix

cacheSolve <- function(x, ...) {
  m <- x$getsolve() # retrieve getsolve from list
  if(!is.null(m)) { # check to see if x is in list
    message("getting cached data") # if TRUE, then solution retrieved
    return(m) # return solution and bypass repeated calculation
  }
  data <- x$get() # if not, then data is gathered and solved
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
