## This script stores a pair of functions, makeCacheMatrix and cacheSolve. These functions are designed to be used in
## conjunction with each other. Specifically, makeCacheMatrix() will generate a pointer to an object that has the
## $set, $get, $setinverse and $getinverse methods. This object can then be used to cache a matrix and its inverse
## to speed up computation. This pointer should be used by setting the initial matrix using the $set(matrix) method
## followed by calling the cacheSolve(pointer) function to return the cached inverse or solve for it if the inverse
## has not already been solved for. Therefore, once a matrix has been stored in the pointer object one should call
## cacheSolve(pointer) rather than solve(matrix) in your code to increase efficientcy where the inverse of the same
## matrix is repeatedly being solved for.

## ERROR TRAPPING NOTE
## Since the problem description states to assume matrix is always invertable
## no testing of the input matrix for validity will be done.


## This function creates an object with methods to cache the value of a matrix
## and its calculated inverse to prevent the need to recalculate unless the matrix
## changes
makeCacheMatrix <- function(x = matrix()) {
        # This function will act as a placeholder to store a matrix in an internal variable 'x' and its inverse in an
        # internal variable 'inv'. Rather than recalculate the inverse, one can use the cacheSolve(pointer) command to
        # calculate the inverse, if and only if the inverse has not already been solved. To make the job easier the
        # object will NULL the stored inverse 'inv' every time a new matrix is stored.
        #
        # Usage:  my_pointer <- makeCacheMatrix()
        # Methods:  "my_pointer$set(matrix)" to set a new stored matrix
        #           "my_pointer$get(matrix)" to get the stored matrix
        #           "my_pointer$setinverse(matrix)" to set an inverse for the stored matrix
        #           "my_pointer$getinverse(matrix)" to get the stored stored inverse for the stored matrix
  
        # Create a quick internal function to compare two matricies to help potentially speed up the code
        matequal <- function(x, y){
          # This function takes two matricies and returns a logical value of TRUE if they are identical to each other
          # in values and dimensions
          is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
        }
        
        # When the function is used to create new object we set the inverse 'inv' to a NULL because it must be as we
        # don't have x yet
        inv <- NULL
        
        # Create the function to set the value of the matrix that we will hold he inverse for
        set <- function(y) {
          # This function takes a matrix 'y' and will update the stored matrix to match only if it is different. If it
          # is different then the stored inverse 'inv' is removed.
          
          # Check if the incoming matrix is new?
          if(!matequal(x,y)){
            # If it is new then we NULL the stored inverse 'inv'
            inv <<- NULL
          }
          # Update the stored matrix if and only if 'y' is different from the stored matrix 'x'
          x <<- y
        }
        
        # Create the $get function to retrieve the stored matrix 'x'
        get <- function() x
        
        # Create the $setinverse function to set the stored inverse 'inv'
        setinverse <- function(inverse) inv <<- inverse
        
        # Create the $getinverse function to retrieve the stored inverse 'inv'
        getinverse <- function() inv
        
        # Create the methods to expose the internal functions to the outside of this environment
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function will return the inverse of a matrix cached in the makeCacheMatrix
## function or trigger the calculation if not already done
cacheSolve <- function(x, ...) {
        # This function exploits the chaching capacity of the makeCacheMatrix() function to calculate the inverse
        # of a matrix if and only if it has not already been calculated and stored for future use. This version
        # will provide verbose feedback about the calculation of the inverse.
        #
        # Usage: inverse <- cacheSolve(makeCacheMatrix_pointer)
        
        # First, try to get the inverse
        inv <- x$getinverse()
        
        # Check if a NULL was returned
        if(!is.null(inv)) {
          # If a NULL was not returned then exit the function returning the cached inverse 'inv'
          message("getting cached inverse")
          return(inv)
        }
        
        # As the inverse has not been calculated at this stage, get the original matrix
        data <- x$get()
        message("calculating new inverse for storage")
        # Use the original matrix to solve for the inverse matrix
        inv <- solve(data, ...)
        # Store the inverse solution in the makeCachedMatrix object
        x$setinverse(inv)
        # Return the solved inverse
        inv
}
