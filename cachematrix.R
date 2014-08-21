## Script cachematrix contains the functions makeCacheMatrix and cacheSolve, used to cache
## a matrix inverse. 

## makeCacheMatrix creates a matrix object (actually a list) from the matrix x, 
## with sub-functions:
## 1. set. Changes the "matrix part" of the object. If the inverse has been cached previously
## it is cleared from the cache. 
## 2. get. Extracts the "matrix part" of the object
## 3. setinv. Changes the "inverse matrix part" of the object. 
## 4. getinv. Extracts the "inverse matrix part" of the object. 

makeCacheMatrix <- function(x = matrix()) {
    ## when the matrix object is initalised, the inverse m is assigned NULL. 
    m <- NULL
    ## set changes the matrix part of the object, and assigns the inverse part NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## function cacheSolve returns the inverse of a matrix object x. The matrix is assumed to be 
## invertible. 
## If the inverse of the matrix object x has already been calculated, the function returns
## the data stored in memory instead of computing the inverse again. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ##Tries to retrieve the inverse of matrix x. 
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ##If the inverse is not stored in memory, the matrix part of x is retrived into data.
    data <- x$get()
    ##Inverse computed using solve.
    m <- solve(data, ...)
    ##Inverse of data stored in the matrix object x. 
    x$setinv(m)
    m
    
}
