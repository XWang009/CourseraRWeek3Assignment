## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function

##XW: the following funtion are used to create a list object 
## that takes matrix as the input and stores/cache's its inverse

makeCacheMatrix <- function(x = matrix()) {

        invMatr <- NULL 
                                             # set the value of the matrix
        set <-  function(y){                 
                x <<- y
                invMatr <<- NULL
        }
                                             # get the value of the matrix
        get <- function() x 
                                             # set the value of the inverse
        setInvMatr <- function(invMatrix) invMatr <<- invMatrix
                                             # get the value of the inverse
        getInvMatr <- function() invMatr
        
        list( set = set, get = get, setInvMatr = setInvMatr, getInvMatr = getInvMatr)
}


## Write a short comment describing this function
## XW: Function below checks to see if the inverse is existed. if it is, return that value. 
## XW: If not, the function calculates the inverse of the matrix getting from the first funtion. 

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
