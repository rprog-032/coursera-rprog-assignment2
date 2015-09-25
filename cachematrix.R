
#The first function, makeCacheMatrix creates a special "matrix" object that can cache its inverse.
# set the value of the vector
# get the value of the vector
# set the value of the mean
# get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y) {
     x <<- y
     inv <<- NULL
   }
   get <- function() x
   setinverse <- function(inverse) inv <<- inverse
   getinverse <- function() inv
   list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
 }

# The function matrix.cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the
# inverse from the cache.
# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
   inv <- x$getinverse()
   if(!is.null(inv)) {
     message("getting cached data")
     return(inv)
   }
   data <- x$get()
   inv <- solve(data)
   x$setinverse(inv)
   inv
 }

## Sample run:
# x = rbind(c(1, 2), c(3, 4))
# m = makeCacheMatrix(x)
# m$get()
#       [,1] [,2]
# [1,]    1    2
# [2,]    3    4

## No cache in the first run
# > cacheSolve(m)
#       [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5


# Retrieving from the cache in the second run
# > cacheSolve(m)
# getting cached data
#      [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5
 
