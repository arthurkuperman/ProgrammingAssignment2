## Creates a "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  n<-NULL
  set<-function(z){
    x<<-z
    n<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) n<<- solve
  getmatrix<-function() n
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

## Computes the inverse. If the inverse has already been calculated,
## then the cachesolve retrieves the inverse from the cach
cacheSolve <- function(x=matrix(), ...) {
  n<-x$getmatrix()
  if(!is.null(n)){
    message("get data cached")
    return(n)
  }
  matrix <- x$get()
  n<-solve(matrix, ...)
  x$setmatrix(n)
  n
}