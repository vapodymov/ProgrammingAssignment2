## Below are two functions that are used to create 
## a special object that stores a numeric matrix and cache's its inverse value

## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

## It is assumed that x is a square invertible matrix

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  
  ## setting the value of the matrix
  set <- function(y) {  
    x <<- y
    inv_x <<- NULL
  }
  
  ## getting the value of the matrix
  get <- function() x   
  
  ## setting the value of the inverse matrix
  setinverse <- function(inverse) inv_x <<- inverse  
  
  ## getting the value of the inverse matrix
  getinverse <- function() inv_x                 
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The second function calculates the inverse of the special "matrix" created 
## with the above function. However, it first checks to see if the inverse has 
## already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of 
## the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_x <- x$getinverse()
  
  ## If the inverse has already been calculated,
  ## get the inverse from the cache and skip the computation
  if(!is.null(inv_x)) {     
    message("getting cached data")
    return(inv_x)     
  }
  
  ## getting data
  data <- x$get()
  
  ## calculating the inverse
  inv_x <- solve(data, ...)  
  
  ## setting the value of the inverse in the cache
  x$setinverse(inv_x)          
  inv_x
}
