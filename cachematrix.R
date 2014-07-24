#this file contains 2 functions one represents a special matrix with it's cached inverce
#and the second one computes the inverse of the special matrix


#This function creates a special "matrix" object that can cache its inverse.
#it returns a list containing functions to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse matrix
#get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    xInv <- NULL
    set <- function(y) {
        x <<- y
        xInv <<- NULL #reset inverse is the matrix changes
    }
    get <- function() x
    setinverse <- function(inverse) xInv <<- inverse
    getinverse <- function() xInv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix
#ussage:
#   x <- makeCacheMatrix(matrix(nrow=2,ncol=2, c(1,2,3,4)))
#   cacheSolve(x)
#   cacheSolve(x) - will use the cached version and print "getting cached data"
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
