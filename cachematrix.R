## This pair of functions will cache the inverse of a matrix

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        ##initialize m
        m<- NULL
        ##creates set function
        set<- function(y) {
                x<<- y
                m<<-NULL
        }
        ##creates get function
        get<-function()x
        
        ##calculates inverse
        setsolve <-function(solve) m<<-solve
        getsolve<-function()m
        list(set =set, get=get,
             setsolve =setsolve,
             getsolve = getsolve)
}


## This function computes the inverse of the special matrix returned by 
##MakeCacheMatrix above.  If the inverse has already been calculated (and the
##matrix has not changed), then the cacheSolve function should retrieve
##the inverse from the cache.

cacheSolve <- function(x, ...) {
        ##calls getsolve
        m<-x$getsolve()
        ##checks to see if there is cached data
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        ##if no cached data, then find inverse value
        data<-x$get()
        m<-solve(data, ...)
        x$setsolve(m)
        ## Return a matrix that is the inverse of 'x'
        m
}
