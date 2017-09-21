## Put comments here that give an overall description of what your
## functions do

## Caching datatype for caching inverse types of matrices

makeCacheMatrix <- function(x = matrix()) {
  c <- NULL
  setM <- function(m){
    x <<- m
    c <<- NULL
  }
  getM <- function()x
  setI <- function(i) c <<- i
  getI <- function()i
  list(setMatrix = setM, getMatrix = getM, 
       getInverse = getI,setInverse = setI)
}


## Attempte to lookup the inverse in a cache, otherwise compute inverse and store in cache

cacheSolve <- function(x, ...) {
  m<-x$getInverse() 
  if(!is.null(m))return(m)
  data<-x$get()
  m<-solve(data) %*% data
  x$setInverse(m)
  m
  
}
