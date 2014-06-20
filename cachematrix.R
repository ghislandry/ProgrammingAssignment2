## Put comments here that give an overall description of what your
## functions do

## Creates a special "matrix" that is a list containing a function to set the value of the matrix
# a function to get the value of the matrix, set the value of the inverse matrix
# get the inverse matrix
##

makeCacheMatrix <- function(x = matrix()) {
        
        mat <- NULL
        set <- function(y) {
                x <<- y
                mat <<- NULL
        }
        
        get <- function() x
        setinverse <- function(solve) mat <<- solve
        getinverse <- function() mat
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function
# The cacheSolve function compute the inverse of the special matrix made with the 
# makeCacheMatrix function. Prior to computing the inverse, it checks whether the 
# inverse has already been computed and returns that value. Otherwise, it computes 
# the inverse of the matrix and sets the value of the inverse in the cache via the 
# setinverse function.
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat <- x$getinverse()
        if(!is.null(mat)){
                message("getting cached data")
                return(mat)
        }
        
        data <- x$get()
        mat <- solve(data, ...)
        x$setinverse(mat)
        mat
}

## Sample test code ####
# mdat1 <- matrix(rnorm(10000, 1 , 3), nrow = 100, ncol=100)
# x <- makeCacheMatrix(mdat1)
# invt <- cacheSolve(x)
# invt <- cacheSolve(x)
## Sample output ####
# getting cached data
##
#
