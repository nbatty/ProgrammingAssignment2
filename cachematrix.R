## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#start by initializing two objects, x and nvrs
makeCacheMatrix <- function(x = matrix()) {
        nvrs<- NULL
        set <- function(y) {
                #the double headed arrow assigns the value of y to x not in the current environment
                #I believe it passes it to the parent environment, assigning x to now be y
                x<<- y
                #clears nvrs cached data should there be something present
                #Same way I solved the third part of the last assignment
                #think like clearing a cache for a phone app
                nvrs<<- NULL
        }
        #'gets' the matrix for lack of a better term
        get <- function() x
        #sets the inverse value
        setnvrs <- function(solve) nvrs <<- solve
        #retrieves the inverse
        getnvrs <- function() nvrs
        #create a list, assigning each of the functions above a name allowing them to be called easier in the cacheSolve function
        list(set = set, get = get,
             setnvrs = setnvrs,
             getnvrs = getnvrs)

}


## Write a short comment describing this function
#Now this is a function that when provided the makeCacheMatrix function as its argument,
# will output our actual inverse matrix. 
cacheSolve <- function(x, ...) {
        ## check to see if there is an already cached 'answer'
        nvrs <- x$getnvrs()
        if(!is.null(nvrs)) {
                message("getting cached data")
                return(nvrs)
        }
        #if there is no data, then this will calculate the inverse
        data <- x$get()
        nvrs <- solve(data, ...)
        x$setnvrs(nvrs)
        nvrs
}
