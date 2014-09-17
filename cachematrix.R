## Put comments here that give an overall description of what your
## functions do

## ERROR TRAPPING
## Since the problem description states to assume matrix is always invertable
## no testing of the input matrix for validity will be done.


## This function creates an object with methods to cache the value of a matrix
## and its calculated inverse to prevent the need to recalculate unless the matrix
## changes

makeCacheMatrix <- function(x = matrix()) {
        ## Needs 4 functions, set, get, setinverse, getinverse
        ## Should check if the matrix is new, if not retain the old cache of
        ## the inverse for extra computational efficientcy i.e. repeated
        ## calls setting the matrix from the output of another function

}


## This function will return the inverse of a matrix cached in the makeCacheMatrix
## function or trigger the calculation if not already done

cacheSolve <- function(x, ...) {
        ## Get the inverse using the method $getinverse
        ## Check if inverse exists
                ## If inverse exists then output
        ## If no inverse exists obtain raw matrix
        ## Caculate inverse of matrix
        ## Store inverse in cache and return the inverse
}
