## This programe includes two functions ”makeCacheMatrix” and "cacheSolve"
## that are used to create a special object that stores a numeric matrix 
## and cache's its inverse. 

##The following function ”makeCacheMatrix” creates a special “matrix”   
## nd sets the values of the matrix gets the value of the matrix, 
## ets the value of the inverse of the matrix and gets the value 
## of the inverseof the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve  
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function ("cacheSolve") calculates the inverseof the special "matrix"
## created with the above function. However, it first checks to see if the inverse has
## already been calculated. If so, it gets the inverse from the cache and skips the 
## computation. Otherwise, it calculates the inverse of the matrix and sets the value 
## of the inverse in the cache via the "setinverse" function.

cacheSolve <- function(x, ...) {
          m <- x$getinverse()        
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }   
  data <- x$get()  
  m <- solve(data, ...) 
  x$setinverse(m)       
  m
}
