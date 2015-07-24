## makeCacheMatrix() - creates cache object with methods to set & get matrix 
## and inverse data.
## 
## cacheSolve() returns inverse matrix for given cache object.
##
## 


## makeCacheMatrix() creates object with following "methods":
##  - get(), return matrix data stored in object
##  - set(matrix), set matrix stored in object
##  - setinverse(inverse), set inverse for matrix stored in object
##  - getinverse(), return inverse for matrix stored in object

makeCacheMatrix <- function(x = matrix()) {
    ## Value to store inverse
    i <- NULL
    ## Method to get orginal matrix
    get <- function() x
    ## Method to set orginal matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    ## Method to set inverse matrix
    setinverse <- function(inverse) i <<- inverse
    ## Method to get inverse matrix
    getinverse <- function() i
    
    ## Return list object containing object "methods"
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve() returns inverse matrix value for matrix 
## object created using makeCacheMatrix() function

cacheSolve <- function(x, ...) {
        ## Get inverse value from matrix object
        i <- x$getinverse()
        ## If inverse != null, return cached data
        if(!is.null(i)) {
            message("getting cached data")
            return(i)
        }
        ## Get matrix data
        data <- x$get()
        ## Calculate inverse
        i <- solve(data, ...)
        ## Store inverse into matrix object
        x$setinverse(i)
        ## Return a matrix that is the inverse of 'x'
        i
}

createMatrix <- function() {
    ## Create and return matrix for testing
    matrix( c(7, 0, -3, 2, 3, 4, 1, -1, -2), nrow=3, ncol=3)
}
