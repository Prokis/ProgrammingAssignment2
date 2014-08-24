## makeCacheMAtrix
## The function that creates a special "matrix" object that can cache its inverse
## The object does not calculate the inverse but rather saves it inside
## The object saves the matrix to a variable x and its inverse to a variable k
## The returned object (list) contains the following methods:
## set: sets the matrix and resets the cached inverse matrix
## get: returns the matrix
## setInverse: saves the value of the solve function
## getInverse: returns the value of the cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        k<-NULL
        set<-function(y){
                x<<-y
                k<<-NULL
        }
        get<-function(){
                x
        }
        setSolve<-function(solve){
                k<<-solve
        }
        getSolve<-function(){
                k
        }
        list(set=set,
             get=get,
             setSolve=setSolve,
             getSolve=getSolve)
}


## cacheSolve
## The function that gets the inverse matrix from a special object created by the makeCacheMAtrix function
## It takes the object as an argument 'x'
## With an if statetment checks if the value of the inverse matrix is already cached
## If it is it returns the cached value
## Otherwise, the function calculates the inverse of the matrix saved in 'x'
## It saves it into 'x' cache using the 'setSolve' method and returns the result

cacheSolve <- function(x, ...) {
        k<-x$getSolve()
        if (!is.null(k)){
                message("getting cached data")
                return(k)
        }
        data<-x$get()
        k<-solve(data,...)
        x$setSolve(k)
        k
}
