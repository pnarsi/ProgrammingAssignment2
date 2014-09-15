## cachematrix.R 
 
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(new_matrix)
        # check to see if new matrix is the same as the old matrix
        if(!identical(new_matrix,x)){
            # if the new matrix is different, clear the cached inverse
            inverse <<- NULL
            x <<- new_matrix
        }
    get <- function () x
    setinverse <- function(new_inverse) inverse <<- new_inverse
    getinverse <- function() inverse
    list(set = set, get = get, setinverse = setinverse, 
         getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)){
        message("getting cached data")
        return(inverse)
    }
    
    matrix <- x$get()
    inverse <- solve(matrix, ...)
    x$setinverse(inverse)
    inverse
        ## Return a matrix that is the inverse of 'x'
}
