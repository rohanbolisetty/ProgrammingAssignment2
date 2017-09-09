## Put comments here that give an overall description of what your
## functions do
# The makeCacheMatrix gives the input matrix 4 functions to help in the cache process
# The cacheSolve returns the inverse if already computed else it computes it and sets
# the inverse so that it need not be computed again and directly be used

## Write a short comment describing this function
# This function takes in the matrix and assigns it 4 values in a way that if the
#inverse already exists, it is registered in the getinv value of the matrix
# you can also change the matrix directly using the set function

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<- function(y){
    x<<-y
    m<<-NULL
  }
  get<- function() x
  setinv <-function(inv) m<<-inv
  getinv <- function() m
  list(set = set,get = get, setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function
# This function checks if the inverse is already computed and if yes , returns a 
#message saying getting cached data and gives the inverse, otherwise is computes
# the inverse and sets the setinv value of the matrix to the inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m<- solve(data, ...)
  x$setinv(m)
  m
}
