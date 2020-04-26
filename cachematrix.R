# Here are 2 connected functions. The second one computes the inverse of a matrix and then stores the values in the environment of the first function. Whenever there is a new matix, the second function does recomputation. If the matrix has not changed, then the second function returnes cached data.

# In that function we get a list of 4 functions. One of them — getinverse - stores the data of an inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get<- function() x
  setinverse<- function(inverse) inv <<- inverse
  getinverse<- function() inv
  list (set=set, get=get, 
        setinverse=setinverse,
        getinverse=getinverse)
}


# In this function we return a matrix that is the inverse of "x". We return cached datd if "x" has not changed.

cacheSolve <- function(x, ...) {
  inv<- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data<- x$get()
  inv<- solve(data, ...)
  x$setinverse(inv)
  inv
}
