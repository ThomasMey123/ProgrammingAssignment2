## The functions in this file allow create a special object that stores a matix and caches its inverse 
## 
## Usage sample:
## m<-matrix(c(1,1,0,1),2,2)
## m1=makeCacheMatrix(m)
## mi1<-cacheSolve(m1)
## 
## Test with
## r<-m1$get() %*% mi1  # Matrix multiplication
## r # Should return a unit matrix 
##
## Larger matrices (where caching can be tested) could be generated with e.g. m<-matrix(runif(1000000,0,1),1000,1000)


## Accept a matrix as argument, store the matrix and return a special object to access the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    seti <- function(inv) i <<- inv
    geti <- function() i
    list(set = set, get = get,
         seti = seti,
         geti = geti)
}


## Accect a special object referring to a matrix (as created with makeCacheMatrix) and return its inverse
## The inverse is calculated once only and the cache is returned subsequently
## Write a short comment describing this function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$geti()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    mat <- x$get()
    i <- solve(mat)
    x$seti(i)
    i   
}
