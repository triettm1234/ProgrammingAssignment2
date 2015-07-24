## makeCacheMatrix creates a structure that holds  a matrix and 
## its inverse. It has 4 interfacing functions: set(), get(), 
## setinverse(), and getinverse(). 
##
## set() and get() interact with the matrix while setinverse() 
## and getinverse() interact with the matrix's inverse.

makeCacheMatrix <- function(x = matrix()) {

        i <- NULL #inverse matrix
        
        set <- function(y) {
                x <<- y
                i < NULL
        }
        
        get <- function () x
        setinverse <- function (y) i <<- y
        getinverse <- function () i
        
        list (set = set, get = get, #return interface
              setinverse = setinverse, 
              getinverse = getinverse)
}


## Calculates and returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        
        i <- x$getinverse()
        
        if (!is.null(i)) return(i) #return i if value is already calculated
        
        data <- x$get() 
        i <- solve(data, ...) #calculate inverse
        
        x$setinverse(i)
        i
}

###################################################################