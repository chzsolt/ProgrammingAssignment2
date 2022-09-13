## This function creates an R object that stores a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL               ## initializes i 
  set <- function(y) { 
    x <<- y               ## assigns the input argument to x in the parent environment
    i <<- NULL            ## clears any value of i that had been cached by a prior execution of cacheSolve()
  }
  get <- function() {x}   ## returns x from parent environment 
  setinverse <- function(inverse) {
    i <<- inverse         ## assigns the input argument to i in the parent environment
    } 
  getinverse <- function() {i}  ## gets the value of i
  list(set = set, get = get,    ## assigns the functions as an element within a list()
       setinverse = setinverse, ## and returns it to the parent environment
       getinverse = getinverse)
}


## This function requires an argument that is returned by makeCacheMatrix()
## in order to retrieve the inverse from the cached value that is stored in 
## the makeCacheMatrix() object's environment.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()   ## calls the getinverse() function on the input object
  if(!is.null(i)) {     ## if i is not equal to NULL, cached inverse can return
    message("getting cached data") 
    return(i)
  }
  data <- x$get()       ## when is statement is FALSE cacheSolve() function
  i <- solve(data, ...) ## calculates inverse and uses setinverse() funtion 
  x$setinverse(i)       ## to store it in object i
  i
}

