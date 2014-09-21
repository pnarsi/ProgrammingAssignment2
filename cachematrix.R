## cachematrix.R 
## cachematrix.R provides a pair of functions that enables the caching 
## of the inverse of a matrix so that if the inverse of a matrix has 
## previously been calculated, it returns that cached value instead of 
## re-calculating the inverse 

## makeCacheMatrix provides a wrapper around a standard R matrix.
## enabling the inverse of the matrix to be stored along with the matrix 
## itself. It provides a list of function to set and get both the original 
## matrix as well as the cached matrix 
##
## parameters
## x - the standard R matrix to be wrapped around
##
## return
## a list that contains the four sub-functions defined in this makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    # set function - assigns a new matrix to the CacheMatrix
    set <- function(new_matrix)
        # check to see if new matrix is the same as the old matrix
        if(!identical(new_matrix,x)){
            # if the new matrix is different, clear the cached inverse
            inverse <<- NULL
            x <<- new_matrix
        }
    
    # get function - returns the stored matrix
    get <- function () x
    
    # setinverse function - used to set the inverse of the matrix
    setinverse <- function(new_inverse) inverse <<- new_inverse
    
    # getinverse function - used to get stored inverse of the matrix.
    # returns NULL if no cached inverse is available
    getinverse <- function() inverse
    list(set = set, get = get, setinverse = setinverse, 
         getinverse = getinverse)
}


## cacheSolve returns the inverse of a matrix. If available, the function
## returns the cached value of the inverse. Otherwise it computes the inverse,
## set the inverse parameter of the inputed makeMatrix

## parameters
## x - a list that was created using the makeCacheMatrix  function.
##
## return
## a standard R matrix that is the inverse of the matrix inputed into makeCacheMatrix 

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    
    # check if inverse exists. If so return the cache value
    if(!is.null(inverse)){
        message("getting cached data")
        return(inverse)
    }
    
    matrix <- x$get()
    
    # solve the inverse
    inverse <- solve(matrix, ...)
    
    # set the inverse of the input x list
    x$setinverse(inverse)
    
    # return the inverse matrix
    inverse
        ## Return a matrix that is the inverse of 'x'
}
