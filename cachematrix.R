## Coursera R Programming: Lexical Scoping Programming Assignment
## Author: jbkellner
## Created: May 11, 2014
## Last modified: May 11, 2014

## Summary Description: this file includes two functions, 
## makeCacheMatrix and cacheSolve, that together cache the inverse of a matrix

## These are the commands used to execute the script
## a<-makeCacheMatrix()     initialize
## a$set(matrix(1:4,2,2))   set the matrix (should be square to be invertible)
## a$get                    get the matrix
## cacheSolve(a)            calculate the inverse
## cacheSolve(a)            when it is called back use the cached inverse

## To check that the output from cacheSolve(a) is an inverse of a matrix
## testsolution <- cacheSolve(a)
## testsolution%*%solve(testsolution)
## this should output an identity matrix with ones on the diagonal
## Additional description of matrix inverse: 
## http://mathworld.wolfram.com/MatrixInverse.html



## The function makeCacheMatrix creates a special "matrix" object that can 
## cache its inverse using the solve function. This special matrix is a list 
## containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse
## Note: For this assignment, assume that the matrix supplied is always invertible.


makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y  
          m <<- NULL
     }
     get <- function() x
     setinverse <- function(solve) m <<- solve
     getinverse <- function() m
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}



## The function cacheSolve computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve should retrieve the inverse 
## from the cache.


cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     m <- x$getinverse()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setinverse(m)
     m
}
