## I have written two functions, the first one which takes in a matrix and returns a list of
## functions as output, each of which either sets or returns the inverse of the matrix. The 
## output is provided as an argument to the second function, which then either calculates the 
## inverse or retrieves it from the cache (the cache is saved non-locally)

## makeCacheMatrix takes in a matrix and returns a list of 4 functions as an output,
## which either edit the matrix, return it, or compute & return its inverse, which is saved
## non-locally in a variable called my_cache. The resulting func_list can be provided as an
## argument to the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
        my_cache <- NULL
        set <- function(y) {
                x <<- y
                my_cache <<- NULL
        }
        get <- function() {return(x)}
        setmean <- function(x) {my_cache <<- solve(x)}
        getmean <- function() {my_cache}
        func_list = list(set = set, get = get, setmean = setmean, getmean = getmean)
}


## The cacheSolve function takes in the output of makeCacheMatrix as an argument and prints out
## the inverse of the matrix, either from the saved value in the cache or by computing it, 
## both using the output functions of makeCacheMatrix.

cacheSolve <- function(func_list, ...) {
        my_cache <- func_list$getmean()
        if(!is.null(my_cache)) {
                message("getting cached data")
                return(my_cache)
        }
        dataframe <- func_list$get()
        my_cache <- solve(dataframe, ...)
        func_list$setmean(my_cache)
        my_cache
}
