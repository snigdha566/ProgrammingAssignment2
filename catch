## The process of cache is used to save intermediate data files, when the process of loading or manipulating data is time-consuming. 
## Matrix inverse is usually time-consuming 
## A value is assigned to an object (special "matrix") in an environment that is different from the current environment 
## If needed, this value can be retrived
## This will save time in opposite to compute again


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function()x
    setInverse<-function(inverse)m<<-inverse
    getInverse<-function()m
    list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
    
} 



## This function will compute the inverse of the special "matrix" create by makeCacheMatrix
## If the inverse has been calculate and the matrix has not changed, the cachesolved will retrieve the inverse from the cache

cacheSolve <- function(x,...) {
    
    m<-x$getInverse()
    if(!is.null(m)) {
        message("Getting Cached Matrix")
        return(m)
    }
    data<-x$get()
    m<-solve(data,...)
    x$setInverse(m)
    m
}
