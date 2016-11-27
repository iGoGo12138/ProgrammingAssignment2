## Assignment for coursera course - R programming week 3 
## detailed description for functions, see below


## makeCacheMatrix 
## This function creates a special "matrix" object that can cache its inverse.
## intake a invertible matrix (assumption for this 
## assignment) and return a list of functions, decribed below
## $set change or set the matrix
## $get print the current matrix 
## $setinverse matually assign or "overwrite" the inversion matrix
## $getinverse print out the inversion matrix currently stored (could be wrong!)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
}


## cacheSolve
## intake the special "matrix", actually a list from function makeCacheMatrix
## output the inversion of matrix
## if the cahce value exist already, direct read and print that value
## if not, use built-in solve function to compute the inversion and print

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
