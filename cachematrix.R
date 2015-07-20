## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The makeCacheMatrix function:
## 1) takes an R object of type matrix as an input parameter
## 2) declares an attribute, called inv, to store the inverse of the input (matrix) parametr 
## 3) has two setter and getter functions to manipulate and access its attributes
## 4) creates a list consisting of all the setters and getters to be used by the cacheSolve function
## Usage: one needs to declare an instance of this function or class and pass it as an
## argument to the cacheSolve function below. The function assumes no change in the matrix 
## and feeds the previous value unless its set function is invoked. 

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setinv <- function(invert) {
        inv <<- invert
    }
    
    getinv <- function() {
        inv
    }
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## The cacheSolve function:
## 1) first checks if the inv attribute of the makeCacheMatrix is NULL; if so,
## 2) accesses the input matrix, takes inverse of it and saves the result in inv attribute
## 3) otherwise, takes the cached value and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting cached data ...")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
