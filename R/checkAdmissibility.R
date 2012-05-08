# TODO: Add comment
# 
# Author: srazbash
###############################################################################

checkAdmissibility<-function(opt.env, box.cox=NULL, small.phi=NULL, ar.coefs=NULL, ma.coefs=NULL, tau=0, bc.lower=0, bc.upper=1) {
	#Check the range of the Box-Cox parameter
	if(!is.null(box.cox)) {
		if((box.cox <= bc.lower) | (box.cox >= bc.upper)) {
			#print("box-cox")
			return(FALSE)
		}
	}
	
	#Check the range of small.phi
	if(!is.null(small.phi)) {
		if(((small.phi < .8) | (small.phi > 1))) {
			#print("small-phi")
			return(FALSE)
		}
	
	}
	
	#Check AR part for stationarity
	if(!is.null(ar.coefs)) {
		#print("as.coefs")
		arCheck <- function(ar) {
			p <- max(which(c(1, -ar) != 0)) - 1
			if (!p) 
				return(TRUE)
			all(Mod(polyroot(c(1, -ar[1L:p]))) > 1)
		}
		if(!arCheck(ar.coefs)) {
			#print("ar")
			return(FALSE)
		}
	}
	
	#Check MA part for invetibility
	if(!is.null(ma.coefs)) {
		#print("ma.coefs")
		maInvert <- function(ma) {
			q <- length(ma)
			q0 <- max(which(c(1, ma) != 0)) - 1L
			if (!q0) 
				return(ma)
			roots <- polyroot(c(1, ma[1L:q0]))
			ind <- Mod(roots) < 1
			if (all(!ind)) 
				return(ma)
			if (q0 == 1) 
				return(c(1/ma[1L], rep(0, q - q0)))
			roots[ind] <- 1/roots[ind]
			x <- 1
			for (r in roots) x <- c(x, 0) - c(0, x)/r
			c(Re(x[-1L]), rep(0, q - q0))
		}
		inverted.ma<-maInvert(ma.coefs)
		if(all(inverted.ma != ma.coefs)) {
			#print("ma")
			return(FALSE)
		}
	}
	
	#Check the eigen values of the D matrix
	D.eigen.values<-eigen(opt.env$D, symmetric=FALSE, only.values=TRUE, EISPACK=TRUE)$values
	
	return(all(abs(D.eigen.values) < 1+1e-10))
	
	
}
