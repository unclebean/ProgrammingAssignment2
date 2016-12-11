## The functions to handle matrix inverse cache feature. Right now we only can handle a square matrix, if you pass non-square to makeCacheMatrix when you call cacheSolve will get error.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(newMatrix){
        x <- newMatrix
        inverse <- NULL
    }
    get <- function() x
    setInverse <- function(newInverse) inverse <<- newInverse
    getInverse <- function() inverse

    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)){
        return(inverse)
    }else{
        currentMatrix <- x$get()
        inverse <- solve(currentMatrix)
        x$setInverse(inverse)
        return(inverse)
    }

}
