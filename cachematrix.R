##This function stores a matrix object and caches its inverse. 
##It also enables the user to retrieve the original matrix or the inverse as needed.


makeCacheMatrix <- function(x = matrix()) {


        i<-NULL
        set<-function(y){
                x<<-y
                i<<-NULL
        }
        
        get<-function()x
        setinverse<-function(inv) i<<-inv
        getinverse<-function() i
        list(set=set, get=get,
             setinverse=setinverse,
             getinverse=getinverse)
        }

## This function calculates the inverse of a matrix object created by makeCacheMatrix and retrieves its value.
##It also checks if the inverse has been previously calculated, if that is the case it retrieves its value.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        i<-x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        matrix<-x$get()
        i<-solve(matrix,...)
        x$setinverse(i)
        i

}
