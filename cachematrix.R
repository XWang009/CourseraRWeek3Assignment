## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

        invMatr <- NULL 
        
        set <-  function(y){
                x <<- y
                invMatr <<- NULL
        }
        
        get <- function() x 
        setInvMatr <- function(invMatrix) invMatr <<- invMatrix
        getInvMatr <- function() invMatr
        list( set = set, get = get, setInvMatr = setInvMatr, getInvMatr = getInvMatr)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        invMatr <- x$getInvMatr()
        
        if(!is.null(invMatr)){
                message("getting cached data")
                return(invMatr)
        }
        
        data <- x$get()
        invMatr <- solve(data, ...)
        x$setInvMatr(invMatr)
        invMatr
}
