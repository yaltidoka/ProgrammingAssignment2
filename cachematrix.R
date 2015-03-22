## Yanal Altidoka, Assignment #2
##A pair of functions that cache the inverse of a matrix


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#initialization of the function which requires a numeric vector as its only argument

makeCacheMatrix <- function(x = matrix()) {   
  m <- NULL                                #initializing a local variable to the function (m) to NULL. 
  set <- function(y) {                     #'y' in this case is the numeric arg passed into makeCacheMatrix
    x <<- y                                #Set 'x' for the function enviromnent to 'y'
    m <<- NULL                             #Set 'm' for the 'makeVector' environment to NULL
  }
  get <- function() x                      #calls the function
  setsolve <- function(solve) m <<- solve  #setting the cached (m) to NULL
  getsolve <- function() m                 #function which returns the cached mean value
  list(set = set, get = get,setsolve = setsolve,getsolve = getsolve)
                                           #Lists out the values of the functions in the makeCacheMatrix
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
                                          ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()                       #retreives the inverse value
  if(!is.null(m)) {                       #checks to see if value is already cached
    message("getting cached data")        #sends message out
    return(m)                             #returns cached inverse value
  }
  data <- x$get()                         #else: gets the inverse value
  m <- solve(data, ...)
  x$setsolve(m)                           #assigned the calculated inverse to the makeCacheMatrix
  m                                       #returns the calculated inverse
}
