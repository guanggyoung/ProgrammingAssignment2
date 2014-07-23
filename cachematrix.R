## These functions comstruct a special "matrix" object
## in which you can get the matrix itself as well as
## its inverse if the matrix has not been changed.
## By caching the inverse of the matrix, it would save
## a lot of time when the matrix inversion should be
## performed repeatly.

## This is the function to construct the special "matrix"
## which can cache its inversion.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL   #used to store the cached inverse of the matrix
	set <- function(y){
	    x <<- y
		inv <<- NULL
	}
    get <- function() x
	set_inverse <- function(inverse) inv <<- inverse
	get_inverse <- function() inv 
	list(set=set,get=get,set_inverse=set_inverse,get_inverse=get_inverse)
}


## This functon computes the inverse of the 
## special "matrix" returned by makeCacheMatrix.
# Return a matrix that is the inverse of 'x'.
cacheSolve <- function(x, ...) { 
    inv <- x$get_inverse()
	if(!is.null(inv) ){
	    message("getting the cached inverse")
		return (inv)
	}
    matrix_data <- x$get()
    inv <- solve(matrix_data, ... )
    x$set_inverse(inv)
    inv    
}
