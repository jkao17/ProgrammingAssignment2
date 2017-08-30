## Cachematrix.R
##
## The combination of makeCacheMatrix and cacheSolve computes and caches the inverse of a
## square invertible matrix.  It takes advantage of lexical scoping to pass information
## about the matrix and its inverse matrix between the two functions.
##
## notes are based on my understanding of Leonard Greski's Demystifying makeVector()
## https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md 


## makeCacheMatrix creates a matrix inv that can cache the inverse of matrix "x"
##  matrix are saved into a list vector as follows:
##  1. set: set the value of the matrix(x)
##  2. get: get the value of the matrix(x)
##  3. setinv: set the value of the inverse matrix(inv)
##  4. getinv: get the value of the inverse matrix(inv)

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL                               ## initialize matrix inv
    set <- function(y) {
        x <<- y                               ## assign input x to parent environment
        inv <<- NULL                          ## assign NULL to inv in the parent environment 
                                              ## and clear prior execution of cacheSolve()
    }
    get <- function() x                       ## retrieve x from parent environment
    setinv <- function(solve) inv <<- solve   ## set the results of the inverse matrix inv using solve()
    getinv <- function() inv                  ## assign new matrix inv in the parent environment
    
    list(set    = set,
         get    = get,
         setinv = setinv,
         getinv = getinv)                     
}


##  cacheSolve computes the inverse of the "matrix" from makeCacheMatrix
##  If the inverse has already been calculated (and the matrix has
##  not changed), then the inverse is retrieved from the cache.  Otherwise
##  the inverse matrix is calculated and stored in the setinv function.


cacheSolve <- function(x, ...) {

    ## Return a matrix inv that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        print('getting inverse matrix from cached data')
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
    
}


