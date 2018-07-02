## Put comments here that give an overall description of what your
## functions do
## Actually I take matrix x convert it into a special matrix or kind of list let's call it z in which i store the value of matrix 
##and also store the value of its inverse
## Then I call cacheSolve on this  z list try to figure out its inverse value is already computed or not if it already computed 
## then I will get it easily by Calling z$getinverse()
## Write a short comment describing this function
## It will take a matrix convert it into a list 
makeCacheMatrix <- function(x = matrix()) {
            i<- NULL
            set<- function(y){
                x<<-y
                i<<-NULL }
            get<-function() x
            setinverse<-function(solve) i<<-solve
            getinverse<- function() i
            
            list(set =set,get=get,setinverse = setinverse , getinverse = getinverse)
            
            
}


## Write a short comment describing this function
## This function will search the z and find out it contain inverse of x or its null. if it is null then it will calculate inverse of x by calling solve funtion 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     i<- x$getinverse()
     if(!is.null(i)){
       message("getting cached data")
       return(i)
     }
     data<-x$get()
     i<-solve(data, ...)
     x$setinverse(i)
     i
     
     
}
