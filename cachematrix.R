##this is the first function, makeCacheMatrix()
##it creates the special matrix object that we will use to find the inverse

makeCacheMatrix<-function(x=matrix()){
      m<-NULL
      set<-function(y){
            x<<-y
            m<<-NULL
      }
      get<-function() x
      setInverse<-function(inverse) m<<-inverse
      getInverse<-function()m
      list(set=set,get=get,
           setInverse=setInverse,
           getInverse=getInverse)
}

##this is the second function, cacheSolve()
##it takes the matrix in makeCacheMatrix and finds the inverse or returns the cached inverse

cacheSolve<-function(x,...){
      m<-x$getInverse()
      if(!is.null(m)){
            message("getting cached data")
            return(m)
      }
      data<-x$get()
      m<-solve(data,...)
      x$setInverse(m)
      m
}

##example of solution

x<-makeCacheMatrix(matrix(rnorm(40*40),40,40))
cacheSolve(x)

#run cacheSolve(x) again to retrieve the now cached value of the inverse matrix

cacheSolve(x)