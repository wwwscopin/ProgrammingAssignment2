## This function create a special matrix that can cache it's inverse.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
        x<<-y 
        m<<-NULL
        }

        get<-function()x #get matrix
        setinv<-function(inv) m<<-inv # Set inverse of matrix
        getinv<-function() m # get inverse of matrix
        list(set=set, get=get, setinv=setinv, getinv=getinv )
}


## This function computes the inverse of the special matrix returned by makeCacheMatrix above, 
## If the inverse already existed, then retrieve it from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getinv()
        if(!is.null(m)) {
        message("getting cached inverse matrix")
        return(m)
        }
        data<- x$get()
        m<- solve(data,...) #get inverse
        x$setinv(m)
        m
}
