## These functions provide a framework for managing the computation of inverting
# a matrix. The nice functionality this offers is that it prevents redundant
# computation. In other words, if you've already calculated the inverse of a
# matrix, these functions save that result and, when you want the inverse at
# some later point in time, these functions return the already-computed result,
# instead of recalculating the solution.


## The makeCacheMatrix() function returns a list of functions for managing the
# inversion of a matrix.
# The specific methods this list has are:
# * get() - returns the matrix you want to invert
# * set(y) - changes the matrix you want to invert, to y (a matrix object)
# * getSolution() - returns the inverse of the matrix, if stored.
# * setSolution(newSolution) - changes the stored inverse of the matrix
# This function is similar to defining a class (with attributes and methods).
# Since R doesn't have class definitions, we use this approach instead.

makeCacheMatrix <- function(x = matrix()) {
    # initialize a variable that will contain the inverse of X
    solution = NULL

    # create methods for getting/setting the matrix X and the inverse of X.
    set = function(y) {
        # syntax note: double arrows change the variable in the parent namespace
        x <<- y # assign a new matrix to X
        solution <<- NULL # reset the solution, because x has changed
    }
    get = function() {
        x # return the matrix X
    }
    setSolution = function(newSolution) {
        solution <<- newSolution # store a new inverse of X
    }
    getSolution = function() {
        solution # return the inverse of X, as currently stored
    }

    # return a list object containing these methods
    list(set = set
        ,get = get
        ,setSolution = setSolution
        ,getSolution = getSolution
        )
}


## This function returns the inverse of a matrix, using a makeCacheMatrix()
# return object. It's useful in that it abstracts away the operations of either
# computing the solution or loading a cached solution that was computed
# previously. It automates this by first asking: is there a cached solution?
# In other words: has the matrix already been inverted? If yes, it stops there
# and returns the cached result that was computed in the past. If not, it
# computes the inverse, caches this solution in the "matrix object", and
# returns the solution (i.e. the matrix inverse).

cacheSolve <- function(x, ...) {
    # returns the inverse of the matrix stored in x.
    # x must be an object returned by makeCacheMatrix().
    # the additional arguments (`...`) are arguments for the built-in solve()
    # function in R.
    solution = x$getSolution()
    # check if the matrix inverse has already been calculated
    if(!is.null(solution)) {
        # if yes, then return the cached answer
        message("getting cached data")
        return(solution)
    }
    # otherwise, calculate the inverse, which involves...
    # get the matrix we want to invert
    xValue = x$get()
    # invert the matrix
    solution = solve(xValue, ...)
    # cache the result
    x$setSolution(solution)
    # return the result
    solution
}
