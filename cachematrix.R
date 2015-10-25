## a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}


###Test
###> m <- diag(nrow = 5, ncol = 5)
# m[1,2]=1
# m[1,3]=5
# m[1,1]=10
# a = makeCacheMatrix(m)
# b = cacheSolve(a)
#
# a$get()
# [,1] [,2] [,3] [,4] [,5]
# [1,]   10    1    5    0    0
# [2,]    0    1    0    0    0
# [3,]    0    0    1    0    0
# [4,]    0    0    0    1    0
# [5,]    0    0    0    0    1
#
# b
# [,1] [,2] [,3] [,4] [,5]
# [1,]  0.1 -0.1 -0.5    0    0
# [2,]  0.0  1.0  0.0    0    0
# [3,]  0.0  0.0  1.0    0    0
# [4,]  0.0  0.0  0.0    1    0
# [5,]  0.0  0.0  0.0    0    1


