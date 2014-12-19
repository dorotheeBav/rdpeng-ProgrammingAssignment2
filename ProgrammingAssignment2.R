makeCacheMatrix <- function(x = matrix()) 
{
  m<-NULL
  set<-function(y)
    {
    x<<-y
    m<<-NULL
    }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

cacheSolve <- function(x=matrix(), ...) 
{
  m<-x$getmatrix()
  if(!is.null(m))
    {
    return(m)
  }
  matrix <- x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}

x = rbind(c(5, 8), c(3, 5))
m = makeCacheMatrix(x)
m$get()
cacheSolve(m)