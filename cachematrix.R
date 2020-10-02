## Put comments here that give an overall description of what your
## functions do
## The following code is written in reference to the function that calculates mean 
## of the special vector
## The function below will also determine inverse of the matrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        mat_inv <- NULL
        set <- function(y) {
                x <<- y
                mat_inv <<- NULL
                
        }
        get <- function() x
        setinverse <- function(inv) mat_inv <<- inv
        getinverse <- function() mat_inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function

## The main objective of the function below if to solve the inverse of the matrix
## If the inverse of the matrix of already determined in the above function then the function will
##return the solution
## If the inverse of the matrix is not determined then cacheSolve function will determine
##the inverse of the input matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat_inv <- x$getinverse()
        if(!is.null(mat_inv)) {
                message("getting cached data")
                return(mat_inv)
        }
        data <- x$get()
        mat_inv <- inv(data, ...)
        x$setinverse(mat_inv)
        mat_inv
}
