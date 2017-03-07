## Put comments here that give an overall description of what your
## functions do

# These functions cache the inverse of a matrix so that if an inverse matrix has already been
# calculated, it can be called from local memory instead of being recomputed. 



## Write a short comment describing this function
# This function creates a special "matrix" which is a list containing a function to 
# a) set the value of the matrix, b) get the value of the matrix, 
# c) set the value of the inverse matrix, and d) get the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
# This function calculates the inverse matrix of the special "matrix" created with 
# makeCacheMatrix. It first checks to see if the inverse matrix has already been calculated
# though. If so, it instead prints out the previously calculated inverse matrix. If not, 
# it calculates the inverse matrix and stores is locally in memory as m.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
