## These two functions are used to create a special object that stores a "matrix" vector and cache's its inverse.

## The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the mean
## 4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## The following function creates the inverse of the special "vector" created with the above function. 
## However, it first checks to see if the inverse has already been created. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it creates the inverse of the data and sets the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv   ## Return a matrix that is the inverse of 'x'
}

