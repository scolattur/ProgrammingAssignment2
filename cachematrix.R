## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # initalizing inverse = NULL
  
  #function to set matrix y to x + reset inverse cache 
  set <- function(y) {
    x <<- y # assigning new matrix y to x 
    inv <<- NULL

}

  get <- function()x #retriving current matrix (x)
  setinv <- function(inverse)inv <<- inverse # set inverse to inverse
  getinv <- function(){
    inver <- solve(x)
    inver%*%x
  } 
  # retriving cached inverse
  list(set = set, get = get, 
       setinv = setinv,
       getinv = getinv)
}

## Write a short comment describing this function
#code for second function cacheSolve (computing inverse matrix returned by first function)
cacheSolve <- function(x, ...) {
  inv <- x$getinv() #retriving inverse
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv) #if the inverse is not NULL, the cached inverse will be returned
  }
  data <- x$get()  ## Return a matrix that is the inverse of 'x'
  inv <- solve(data, ...)
  x$setin(inv)
  inv
}
