#	makeCacheMatrix stores a matrix and also its inverse
#		but it only calculates the inverse once it's requested
#		if set is called to change the stored matrix in makeCacheMatrix
#	it checks that it's not equal before clearing the cache
# cacheSolve calls x$get_inverse()
	
makeCacheMatrix <- function(new_ = matrix()){
	m_orig	<- x
	m_inv		<- NULL
	params	<- NULL

	#sets a new matrix to evaluate
	set <- function(y){
		if(!identical(m_orig,y) ){
			#set value of matrix
			m_orig	<<- y
			
			#reset inverse calculation and solve options to trigger re-evaluation
			m_inv		<<- NULL
			params	<<- NULL
		} #end if changed
	} #end set
	
	#gets current matrix
	get <- function(){ m_orig }

	#calculates inverse on first call or after a change to matrix or solve options
	set_inverse <- function(...){
		#checks if we need to recalculate inverse
		tmp <- print(as.list(match.call()))
		if(is.null(m_inv)||!identical(tmp,params)){
			params <<- tmp
			#calculate inverse
			message("Calculating inverse...")
			m_inv <<- solve(m_orig,...)
		} else{
			message("Already calculated...")
		}#end if changed
	} #end set_inverse
	
	#gets the matrix inverse via solve() 
	get_inverse <- function(...){
		set_inverse() #we might as well always call, because it will exit when not changed
		message("Returning inverse")
		m_inv
	} #end get_inverse
	
	#i don't know what this does but it seems to be important. I guess it registers publically available elements
	list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse )

} #end makeCacheMatrix


#cacheSolve just gets the inverse, because 
# makeCacheMatrix already does all caching and prevents recalculation
# including check for parameter change in solve
cacheSolve <- function(x, ...) {
	i <- x$get_inverse(...)
	i
}
