##This function creates a special "matrix" object that can cache its inverse
## Creates a special "matrix" object that can cache its inverse.
#VALERIA MARTINEZ 
makeCacheMatrix <- function(x = matrix()) {
  m  <- NULL
  set  <- function(y){                      #sets the value of a vector 
    x <<- y
    m <<- NULL 
  }
  get  <- function() x  #gets the value of a vector 
  setinverse  <- function(inverse) m  <<- inverse   #set the value of the mean
  getinverse  <- function() m                       #get the value of the mean
  list(set= set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
  
}


## Computes the inverse. If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cach

cacheSolve <- function(x, ...) {
  m  <- x$getinverse()
  if (!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data  <- x$get()
  m  <- solve(data, ...)
  x$setinverse(m)
  m
}
