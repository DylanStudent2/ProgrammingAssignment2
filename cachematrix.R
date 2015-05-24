#!/usr/bin/env Rscript


#Time to make a function dedicated to saving a result
makeCacheMatrix <- function(value = matrix(1:9, nrow = 3, ncol = 3)){
   cache <- NULL

   #Have a new matrix replace the previous one, where 
   #We don't know the inverse of, so we clear the cache.
   set <- function(newValue){
      value <<- newValue 
      cache <<- NULL
   }

   get <- function() value

   #After a solve function is used, the result should
   #be passed through this function to update the cache.
   setInverse <- function(solveResult){
         cache <<- solveResult 
   }

   getInverse <- function() cache

   list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


cacheSolve <- function(data, ...){
   result <- data$getInverse()
   
   if(!is.null(result) ){
      print("Same Inverse returned")
      return(result)
   }

   value <- data$get()
   result <- solve(value, ...) 
   data$setInverse(result)
   result

}


