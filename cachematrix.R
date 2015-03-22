## Yanal Altidoka, Assignment #2
##A pair of functions that cache the inverse of a matrix


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {   #creates a new function
    x <<- y
    m <<- NULL
  }
  get <- function() x   #calls the function
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,setsolve = setsolve,getsolve = getsolve)

}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()          #retreives the inverse value
  if(!is.null(m)) {          #checks to see if value is already cached
    message("getting cached data")   #sends message out
    return(m)                        #returns cached inverse value
  }
  data <- x$get()                   #else: gets the inverse value
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
