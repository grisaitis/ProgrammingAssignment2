## These functions provide a framework for managing the computation of inverting
# a matrix.

## this function returns a list of functions for managing the  inversion of a
# matrices. It's sort of like defining a class (with attributes and methods),
# but R doesn't have class definitions, and this is the next best thing.

## The specific methods this list has are:
# * get() - returns the matrix you want to invert
# * set(y) - changes the matrix you want to invert, to y (a matrix object)
# * getSolution() - returns the inverse of the matrix, if stored.
# * setSolution(newSolution) - changes the stored inverse of the matrix


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
    solution = x$getSolution()
    if(!is.null(solution)) {
        message("getting cached data")
        return(solution)
    }
    data <- x$get()
    solution <- solve(data, ...)
    x$setSolution(solution)
    solution
}
