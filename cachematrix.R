## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    solution = NULL
    set = function(y) {
        # syntax note: double arrows change the variable in the parent namespace
        x <<- y # assign a new matrix to X
        solution <<- NULL # reset the solution, because x has changed
    }
    get = function() {
        x # return the matrix X
    }
    setSolution <- function(newSolution) {
        solution <<- newSolution # store a new inverse of X
    }
    getSolution <- function() {
        solution # return the inverse of X, as currently stored
    }
    list(set = set
        ,get = get
        ,setSolution = setSolution
        ,getSolution = getSolution
        )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
