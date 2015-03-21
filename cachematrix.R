## @author  Malcolm Chu
## @course  Data Science by John Hopkins University
## @class   R Programming
## @task    Programming Assignment 2
## @description: Write a pair of functions that cache the inverse of a matrix.
## @url     https://class.coursera.org/rprog-012/human_grading/view/courses/973493/assessments/3/

## @name makeCacheMatrix
## @description This function creates a special "matrix" object that can cache 
##   its inverse.
makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    get <- function() x
    setinversematrix <- function(inversematrix) im <<- inversematrix
    getinversematrix <- function() im
    list(set = set, get = get,
         setinversematrix = setinversematrix,
         getinversematrix = getinversematrix)
}

## @name cacheSolve
## @description This function computes the inverse of the special "matrix"
##   returned by makeCacheMatrix above. If the inverse has already been
##   calculated (and the matrix has not changed), then cacheSolve will retrieve
##   the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    im <- x$getinversematrix()
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    matrix <- x$get()
    im <- solve(matrix)
    x$setinversematrix(im)
    im
}