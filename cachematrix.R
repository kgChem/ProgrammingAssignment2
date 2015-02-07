## USAGE: From a matrix first create a cache matrix with
## cm<-makeCacheMatrix(x = matrix())
## then compute or retreive  the inverse wtih cacheSolve(cm)

## Create a construct that contains a matrix, its inverse
## and methods for getting and setting 

makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL
  set<-function(y){ #set clears the matrix and its inverse
    x<<-y # Need <<- to modify variable in parent closure
    inverse<<-NULL #Initialize inverse to nothing
  }
  get<-function() x# Return the matrix itself
  getInv<-function() inverse # Retreive the stored inverse
  setInv<-function(inverted) inverse<<-inverted #save the inverted matrix to inverse
  list(set=set, get=get, getInv=getInv, setInv=setInv)
}

## Check if the inverse matrix is cached. If not compute it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse<- x$getInv() #Call the getInv memthod associated with x
  if(!is.null(inverse)){
    message("retrieving cached data...")
    return(inverse) #Exit here if inverse was cached
  }
  ##Time to calculate the inverse matrix
  m1<-x$get() #m1 is the normal matrix
  inverse<-solve(m1,...) #calculate the inverse with solve
  x$setInv(inverse) #write the inverted matrix to the cache
  inverse #return the inverse matrix
}

## Uncomment the following lines to run a test
#testmatrix<-cbind(c(1,0,0),c(0,1,0),c(1,0,1))
#cm<-makeCacheMatrix(testmatrix)
#testmatrix
#cacheSolve(cm)
#testmatrix %*% cacheSolve(cm)
#testmatrix2<-cbind(c(4,3),c(3,2))
#cm2<-makeCacheMatrix(testmatrix2)
#testmatrix2
#cacheSolve(cm2)
#testmatrix2 %*% cacheSolve(cm2)

