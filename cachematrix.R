## Put comments here that give an overall description of what your
## functions do

#1.makeCacheMatrix helps in storing a matrix and it's computed inverse in the cache. 
#2.cacheSolve checks if any value of inverse is there in the cache and if yes then it displays it's value . Otherwise, it computes the
#inverse of the matrix stored in the cache and stores the computed value in the i variable present inside the setinverse function.


## Write a short comment describing this function

##makeCacheMatrix takes a matrix as an argument. The getmatrix function shows the content of the matrix passed as an argument.
#setmatrix helps in overwriting the existing values of the matrix with new values. setinverse takes the value of inverse as an 
#argument. This value of inverse is the value stored in cache . The <<- operator is used to assign a value to an object 
#in an environment that is different from it's current environment. This is done so that changes made in variables "x" and "i" are saved,
#since now they are assigned in the same environment in which they are defined. 
#getinverse gives the value of the inverse of the matrix.


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function


##cacheSolve is a function that assigns the inverse value of the matrix inputted by the user .
#It first checks if a computed value of inverse has been inputted by the user or not. If it has been inputted, then it displays the value of the inverse
#and the programme ends there.This is done to prevent recomputing of the already computed inverse value. If no value of inverse has been 
#inputted then it finds the inverse of the matrix present in the cache(this matrix is stored in cache using makeCacheMatrix function) 
#and displays the result. The computed inverse is also assigned to inv variable present inside the set_inverse function.


cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}