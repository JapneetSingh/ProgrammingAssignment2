##The purpose of this code is to develop a cache to store the inverse of a matrix, so that it does not have to be 
##calculated repeatedly.This process is especially useful in case where it may be required to compute the inverse 
##repeatedly, like in a loop,where the values of the initial matrix don't change much
##We use two functions to complete this process 
##1)makeCacheMatrix():- this function serves to create a matrix that will act as the cache matrix. 
##It accepts a square matrix that is invertible as an input and gives a list consisting of four functions as the output 
##The calculated inverse matrix is stored here after being calculated for the first time.
##
##2)cacheSolve():- this function is used to calculate the inverse of the matrix input as an argument into the 
##makeCacheMatrix() and then store its output in the in the same if not already stored. 
##It accepts the list created in makeCache Matrix() as the argument and  gives out the inverse of the matrix as the output.



##makeCacheMatrix() function accepts the square matrix as input. Its body consists of four differenct functions:
##set()- to make changes the original matrix. Accepts the changed matrix as input
##get()-gets the original matrix the inverse for which is stored in the cache
##setinv()-Used to store the Inverse matrix in cache after calculation
##getinv()-used to obtain the inverse stored in the cache
##Output for this function consists of a list consisting the above four functions.

makeCacheMatrix <- function(mat = matrix()) {
        inv<- NULL
        set<-function(mat1){
                mat<<-mat1
                inv<<-NULL
                }
        get<-function(){mat}
        setinv<-function(mat2){inv<<- mat2}
        getinv<-function(){inv}
        list(set=set,get=get,setinv=setinv,getinv=getinv)
}


##cacheSolve() uses the list created by makeCacheMatrix() as the input.
##It starts by checking if any value is stored in the inverse using the getinv() function. 
##If the inverese was calculated in a previous iteration and stored in the cache it will use that as the output.
##If the value is not stored in cache, it will obtain the original matrix using get()function,then use solve() function 
##to calculate the inverse which in turn is then stored in the cache matrix using the setinv() function. 
 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse<-x$getinv()
        if(!is.null(inverse)){
                print("Data available in cache. Retrieving....")
                return(inverse)
                }
        
        matr<-x$get()
        inverse<-solve(matr,...)
        x$setinv(inverse)
        inverse
}
