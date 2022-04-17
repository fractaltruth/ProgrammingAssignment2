## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()){
        matrix_inverse <- NULL
        set <- function(y){
                x <<- y
                matrix_inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) matrix_inverse <<- inverse
        getinverse <- function() matrix_inverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
        matrix_inverse <- x$getinverse()
        if(!is.null(matrix_inverse)) {
                message("getting inverse matrix from cache")
                return(matrix_inverse)
        }
        data <- x$get()
        matrix_inverse <- solve(data, ...)
        x$setinverse(matrix_inverse)
        matrix_inverse
}