## define two functions that create an object that hold a matrix and its inverse
## but only calculates the inverse when the matrix changes

## makeCacheMatrix:
##      create an object that holds a matix and its inverse and provides methods 
##      to get and set them.
makeCacheMatrix <- function(theMatrix = matrix()) {
    ## initialise the inverse
    theInverse <- NULL
    ## the matrix setter function
    set <- function(aMatrix) {
        theMatrix <<- aMatrix
        theInverse <<- NULL
    }
    ## the matrix getter function
    get <- function() theMatrix
    ## the inverse setter function
    setinverse <- function(inverse) theInverse <<- inverse
    ## the inverse getter function
    getinverse <- function() theInverse
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}

## cacheSolve:
##      calculate the inverse only when the inverse is NULL
cacheSolve <- function(theMatrix, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- theMatrix$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    matrix <- theMatrix$get()
    inverse <- solve(matrix, ...)
    theMatrix$setinverse(inverse)  ## set the inverse 
    inverse
}

## some test data
## xx <- matrix(c(2,5,6,4,3,7,8,9,3), nrow=3, ncol=3, byrow=TRUE)