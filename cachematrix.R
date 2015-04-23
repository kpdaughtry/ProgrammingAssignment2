## The function makeCacheMatrix takes an argument of a matrix and creates a
## list of functions for storing the values of both the original matrix and the
## inverse of the matrix. The purpose of this kind of function is to reduce
## computation time for long processes by storing the results in a variable
## that can be called in less time than the full computation.

makeCacheMatrix <- function(x = matrix()) {
        z <- NULL
        set <- function(y = matrix()) {
                x <<- y
                z <<- NULL
        }
        
        get <- function() {x}
        setinverse <- function(inverse) {z <<- inverse}
        getinverse <- function() {z}
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        

}


## This function takes the argument of makeCacheMatrix and does two things.
## First it checks for a previously stored inverse(z), if TRUE then it returns
        ## that value.
## Second, if the (z) has no value it inverts the the matrix(x) from 
        ## makeCacheMatrix and stores it in the value(z) for future use.

cacheSolve <- function(x, ...) {
        z <- x$getinverse()
        if(!is.null(z)) {
                message("Retrieving cached data")
                return(z)
        }
        data <- x$get()
        z <- solve(data)
        x$setinverse(z)
        z
        
        
        ## Return a matrix that is the inverse of 'x'
}
