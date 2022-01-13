## The first function, makeCacheMatrix creates a special "matrix",
## which is really a list containing a function to
## 1.get the matrix of the vector
## 2.get the inverse matrix 
## If the input has already had a inverse matrix, it will be cached


## This function creates a special "matrix" object 
## that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverser <<- NULL
  }
  ## To justify whether there is already a inverse matrix in the input
  if(exists("inverse")){
    inverse <<- inverse
    message("we already have one inverse!")
  }
  else{
    inverse <- NULL 
    message("inverse is not here")
  }
  get <- function() x
  setinverse <- function(inverse_matrix) inverse <<- inverse_matrix
  getinverse <- function() inverse
  list(get=get,setinverse=setinverse,getinverse=getinverse)
}


## cacheSolve:This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()# Return a matrix that is the inverse of 'x'
  if(!is.null(inverse)){
    message("already exists!")
    return(inverse)
    }
  a_matrix <- x$get()
  inverse <- solve(a_matrix)
  inverse
}
