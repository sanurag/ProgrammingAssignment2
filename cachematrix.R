## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    setMatrix <- function(y){
            x <<- y
            inv <<- NULL
    }
    getMatrix <- function(){
        x
    }
    setInv <- function(Inv){
        inv <<- Inv
    }
    getInv <- function(){
        inv
    }
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInv = setInv,
         getInv = getInv)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    Inv <- x$getInv()
    if(!is.null(Inv)) {
        message("getting cached data")
        return(Inv)
    }
    data <- x$getMatrix()
    
    if (det(data) != 0){
        Inv <- solve(data)
    }
    else{
        message("Inverse of the matrix is not possible")
        return()
    }
    x$setInv(Inv)
    Inv
}
