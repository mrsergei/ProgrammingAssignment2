#!/usr/bin/env R
## (c) Sergei V Rousakov

## In this scrupt you will find two functions that are used to create a special object 
## that stores a square matrix and cache's its inverse.
##

## Function makeCachematrix() is a list containing a function to:
## 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix
## 
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Function cacheSolve() calculates the inverse of the special "matrix" returned by 
## makeCacheMatrix() function above. However, it first checks to see if the inverse matrix has 
## already been calculated. If so, it gets the inverse matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse matrix of the data using solve(x) function and 
## sets the value of the inverse matrix in the cache via the setinv function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
