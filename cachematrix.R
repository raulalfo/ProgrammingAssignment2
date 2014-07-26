## Put comments here that give an overall description of what your
## functions do
# Funtions to calculate the inverse of a matrix with good performance
# Typical use: cm <- makeCacheMatrix(x) and then cacheSolve(cm) where
# x is an inverse matrix 

## Write a short comment describing this function
# Create a matrix object that can cache the inverse to improve the performance
# If you don't pass a parameter it create a matrix of dimension 1,1 and value 1
# you can change it's value with the function set
makeCacheMatrix <- function(x = matrix()) {
    invs <- NULL
    set <- function(y) {
        x <<- y
        invs <<- NULL  
    }
    get <- function() x
    setinverse <- function(inverse) invs <<- inverse
    getinverse <- function() invs
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Write a short comment describing this function
# Get a inverse of matrix x. The x value is a cache matrix
# that you can create with makeCacheMatrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invs <- x$getinverse()
    if(!is.null(invs)) {
        message("getting cached data")
        return(invs)
    }
    data <- x$get()
    invs <- solve(data)
    x$setinverse(invs)
    invs
}
