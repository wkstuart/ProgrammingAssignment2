##  The two functions, makeCacheMatrix and cacheSolve, together
##  provide a mechanism for reducing the computational burden of repeatedly
##  recalculating the inverse of a matrix by caching the inverse matrix. 
##
##  The functions work only with square, non-singular matrices.
##
##  The function makeCacheMatrix() can be called in the following way:
## 
##  > myMatrix <- makeCacheMatrix(matrix(rnorm(9), 3, 3))
## 
##  Applying the cacheSolve function to myMatrix for the first time
##  will compute and cache the inverse matrix.
## 
##  > cacheSolve(myMatrix)
## 
##  Subsequent calls to cacheSolve(myMatrix) will return the cached inverse
##  matrix.
##   
##  A call to the function get() will display the matrix that has been created.
## 
##  > myMatrix$get()
## 
##              [,1]       [,2]       [,3]
##  [1,]  0.06511834 -1.1423613 -0.6310917
##  [2,] -0.45886403 -0.9485919  0.9374725
##  [3,]  2.02090121  1.2171553 -0.3866329## 
##
##  A call to the function set() will replace the original matrix and delete
##  its calculated inverse (if one exists).
##
##
##
##
##  The following function makeCacheMatrix returns a list of 4 functions,
##  get(), set(), setinverse() and getinverse() in a single object.

makeCacheMatrix <- function(x = matrix()) {
      ## create an object to contain both the matrix x and its inverse
      inverse <- NULL
      set <- function(y) {
            x <<- y
            inverse <<- NULL
      }
      get <- function() x
      setinverse <- function(x) inverse <<- x
      getinverse <- function() inverse
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

##
##  The function cacheSolve() uses the functions within the object 
##  created with makeCacheMatrix() to store the computed inverse matrix
##  within that object.
## 
##  An inverse matrix can only be calculated for square, non-singular matrices.
##  The Solve function will end with an error if the matrix is 
##  singular. (To see the error, you can create a singular matrix using 
##  matrix(1:16, 4, 4).)
## 
##  Since the class assignment specifically says to assume that the matrix 
##  is invertible (not singular), the cacheSolve function does not check to see that
##  the matrix is not singular. A check to verify that the determinant of the 
##  matrix is not zero could prevent passing a singular matrix to the 
##  Solve function.
## 
## 
cacheSolve <- function(x, ...) {
      ## return a matrix that is the inverse of the matrix x (created with makeCacheMatrix)
      inverse <- x$getinverse()
      if(!is.null(inverse)) {
            message("getting cached inverse")
            return(inverse)
      }
      data <- x$get()
      inverse <- solve(data)
      x$setinverse(inverse)
      inverse
}
