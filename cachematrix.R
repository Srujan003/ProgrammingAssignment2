## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(a = matrix()) {
  inv <- NULL
  set <- function(y)
  {
    a <<- b
    inv <<- NULL
  }
  get <- function(){a}
  setInverse <- function(inverse){inv<<-inverse}
  getInverse <- function(){inv}
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(a, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- a$getInverse()
  if(!is.null(inv))
  {
    message("Getting cached data")
    return (inv)
  }
  mat <- a$get()
  inv <- solve(mat,...)
  a$setInverse(inv)
  inv
}
