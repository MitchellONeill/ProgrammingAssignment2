##cachematrix.R 
##This file creates a special matrix object which will solve for the inverse  
##of a matrix and store that value in a cach. Future attempts to solve for 
##this inverse will retrieve the answer from the cache, saving on computation

##Create a special 'matrix object' that stores a vector, its inverse
##and a list of methods for retrieving that matrix and its inverse

makeCacheMatrix <- function (x=matrix()) {       ##creates the matrix object from a matrix x 
  m <- NULL                                      ##sets/resets m to null 
  set <- function (y){                           
    x <<- y                                      ##copies y into x in the parent environment
    m <<- NULL                                   ##set m to null in the parent environment
  }
  get <- function()x                             ##function that retrieves matrix from x
  SetInverse <- function(solve) m <<- solve      ##stores inverse matrix m
  getInverse <- function() m                     ##returns stored inverse matrix m
  list(set=set, get=get, SetInverse=SetInverse,  ##creates a list which has variables which can 
       getInverse = getInverse )                 ##be referenced to access to m
}

##returns the inverse of a cached matrix object created above 

cacheSolve <- function(x, ...) {
  m <- x$getInverse()                            ##searces for a saved inverse value of x 
  if(!is.null(m)){
         message ("getting cached data")         ##if a cached inverse exists the function 
         return(m)                               ##finishes and returns the inverse matrix
    }
  data <- x$get()                                ##otherwise, retrieves matrix and solves
  m <- solve(data, ...)                          ##for the inverse
  x$SetInverse(m)                                ##the inverse is tored in the SetInverse
  m                                              ##inverse matrix is returned
}
