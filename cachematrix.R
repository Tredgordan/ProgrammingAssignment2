## Put comments here that give an overall description of what your
## functions do
#
# makeCacheMatrix(x=matrix()) 
#   creates an "CacheMatrix" structure 
#   to store a matrix and its inverse
# cacheSolve(x, ...)
#   sets and returns inverse matrix for a "CacheMatrix"

# Examples:
#    cm<-makeCacheMatrix(matrix(c(1,2,0,2,1,0,0,0,3), nrow=3, ncol=3))
#    cm<-makeCacheMatrix()
#    cm$set(matrix(c(1,2,0,2,1,0,0,0,3), nrow=3, ncol=3))
#    cm$get()
#    cacheSolve(cm)
#    cm$getinverse()%*%cm$get()

## Write a short comment describing this function
#
# This function returns a list of functions that store 
# a matrix and it's inverse (we assume it exists)
# 

makeCacheMatrix <- function(x = matrix()) {
	# inverse is not set initially
	inv <- NULL
  set <- function(y) {
          x <<- y
          inv <<- NULL
        } 
	
	#return the current matrix
  get <- function() x

	#set the inverse
  setinverse <- function(inverse) inv <<- inverse

	#return the inverse
  getinverse <- function() inv

	#return the list of functions
  list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function
# sets and returns the inverse for a "CacheMatrix" structure 

cacheSolve <- function(x, ...) {

  ## Return a matrix that is the inverse of 'x'
        im<-x$getinverse()
        if(!is.null(im)){
          message("getting cached data")
          return(im)
        }

  #compute and set inverse, solve throws an error if matrix is non-invertible
        x$setinverse( solve(x$get()) )      
}
