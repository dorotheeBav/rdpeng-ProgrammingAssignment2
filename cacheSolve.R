#This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
  m<-NULL
  y<- NULL
  set<-function(y) #set the value of the matrix
    {
    x<<-y
    m<<-NULL
    }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  #creates a list to house the four functions
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x=matrix(), ...) 
{
  m<-x$getmatrix()
  if(!is.null(m)) # To check if cachesolve has run   befor check that the matrix hasn't changed
    {
    return(m)
  }
  matrix <- x$get() # get a value of the input matrix 
  m<-solve(matrix, ...)# compute the value of the inverse of the input matrix
  x$setmatrix(m) # run the setinverse function on the inverse to cache the inverse
  m # return the inverse ! 
}

x = rbind(c(5, 8), c(3, 5))
m = makeCacheMatrix(x)
m$get()
cacheSolve(m)