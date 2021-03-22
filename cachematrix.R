# makeCacheMatrix creates a special "matrix" object that can cache the input 
# matrix and its inverse.

makeCacheMatrix<-function(m=matrix()){
  # Initialize the inverse property.
  inv<-NULL
  # set the value of matrix
  set<-function(y){
    m<<-y
    inv<<-NULL
  }
  # get the value of matrix
  get<-function()m
  
  # set the inverse of the matrix
  setinverse<-function(inverse) inv<<-inverse
  # get the inverse of the matrix 
  getinverse<-function() inv
  
  # wrap up a list of the methods
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}

# If the inverse has already been calculated, the "cacheSolve" should retrieve
# the inverse from the cache, otherwise, the "cacheSolve" should calculate with 
# solve function.

catheSolve<- function(x,...){
  # Return a matrix is the inverse of 'x'   
  inv<-x$getinverse() 
  # Return the inverse if already calculated
  if(!is.null(inv)){
    message("getting cached matrix") 
    return(inv) 
  }
  # get the matrix from makeCacheMatrix
  mat<-x$get()
  # Calculate the inverse by solve
  inv<- solve(mat,...)
  # set the inverse to 'x'
  x$setinverse(inv)
  # return the inverse of the matrix
  inv
}