# These functions make the w, F, x and g matrices
# 
# 
# Author: srazbash
###############################################################################

makeTBATSFMatrix <- function(alpha, beta=NULL, small.phi=NULL, seasonal.periods=NULL, k.vector=NULL, gamma.bold.matrix=NULL, ar.coefs=NULL, ma.coefs=NULL) {
	
	#1. Alpha Row
	F <- matrix(1,nrow=1,ncol=1)
	if(!is.null(beta)) {
		F <- cbind(F, matrix(small.phi,nrow=1,ncol=1))
	}
	if(!is.null(seasonal.periods)) {
		tau <- sum(k.vector)*2
		zero.tau <- matrix(0,nrow=1, ncol=tau)
		F <- cbind(F,zero.tau)
	}
	if(!is.null(ar.coefs)) {
		p <- length(ar.coefs)
		ar.coefs <- matrix(ar.coefs, nrow=1, ncol=p)
		alpha.phi <- alpha*ar.coefs
		F <- cbind(F, alpha.phi)
	}
	if(!is.null(ma.coefs)) {
		q <- length(ma.coefs) 
		ma.coefs <- matrix(ma.coefs,nrow=1,ncol=q)
		alpha.theta <- alpha*ma.coefs
		F <- cbind(F,alpha.theta)
	}
	
	#2. Beta Row
	if(!is.null(beta)) {
		beta.row <- matrix(c(0,small.phi),nrow=1,ncol=2)
		if(!is.null(seasonal.periods)) {
			beta.row <- cbind(beta.row, zero.tau)
		}
		if(!is.null(ar.coefs)) {
			beta.phi <- beta*ar.coefs
			beta.row <- cbind(beta.row, beta.phi)
		}
		if(!is.null(ma.coefs)) {
			beta.theta <- beta*ma.coefs
			beta.row <- cbind(beta.row, beta.theta)
		}
		F <- rbind(F, beta.row)
	}
	
	#3. Seasonal Row
	if(!is.null(seasonal.periods)) {
		seasonal.row <- t(zero.tau)
		if(!is.null(beta)) {
			seasonal.row <- cbind(seasonal.row,seasonal.row)
		}
		
		
		#Make the A matrix
		A <- matrix(0,tau,tau)
		last.pos <- 0
		for(i in 1:length(k.vector)) {
			if(seasonal.periods[i] != 2) {
				C <- .Call("makeCIMatrix", k_s = as.integer(k.vector[i]), m_s = as.double(seasonal.periods[i]), PACKAGE = "forecast")
			} else {
				C <- matrix(0,1,1)	
			}
			S <- .Call("makeSIMatrix", k_s = as.integer(k.vector[i]), m_s = as.double(seasonal.periods[i]), PACKAGE = "forecast")
			
			#C <- matrix(0,k.vector[i],k.vector[i])
			#for(j in 1:k.vector[i]) {
			#	l <- round((2*pi*j/seasonal.periods[i]), digits=15)
			#	C[j,j] <- cos(l)
			#}
			#S <- matrix(0,k.vector[i],k.vector[i])
			#for(j in 1:k.vector[i]) {
			#	S[j,j] <- sin(2*pi*j/seasonal.periods[i])
			#}
			#print(C)
			#print(S)
			Ai <- .Call("makeAIMatrix", C_s = C, S_s = S, k_s = as.integer(k.vector[i]), PACKAGE = "forecast")
			A[(last.pos+1):(last.pos+(2*k.vector[i])), (last.pos+1):(last.pos+(2*k.vector[i]))] <- Ai
			last.pos <- last.pos+(2*k.vector[i])
			
				
			
		}
		seasonal.row <- cbind(seasonal.row,A)
		
		if(!is.null(ar.coefs)) {
			B <- t(gamma.bold.matrix) %*% ar.coefs
			seasonal.row <- cbind(seasonal.row, B)
		}
		if(!is.null(ma.coefs)) {
			C <- t(gamma.bold.matrix) %*% ma.coefs
			seasonal.row <- cbind(seasonal.row, C)
		}
		F <- rbind(F, seasonal.row)
	}
	
	#4. AR() Rows
	if(!is.null(ar.coefs)) {
		#p <- length(ar.coefs)
		ar.rows <- matrix(0,nrow=p,ncol=1)
		if(!is.null(beta)) {
			ar.rows <- cbind(ar.rows, ar.rows)
		}
		if(!is.null(seasonal.periods)) {
			ar.seasonal.zeros <- matrix(0,nrow=p,ncol=tau)
			ar.rows <- cbind(ar.rows, ar.seasonal.zeros)
		}
		ident <- diag((p-1))
		ident <- cbind(ident, matrix(0,nrow=(p-1),ncol=1))
		ar.part <- rbind(ar.coefs,ident)
		ar.rows <- cbind(ar.rows, ar.part)
		
		if(!is.null(ma.coefs)) {
			ma.in.ar <- matrix(0,nrow=p,ncol=q)
			ma.in.ar[1,] <- ma.coefs
			ar.rows <- cbind(ar.rows,ma.in.ar)
		}
		
		F <- rbind(F,ar.rows)
	}
	
	#5. MA() Rows
	if(!is.null(ma.coefs)) {
		ma.rows <- matrix(0,nrow=q,ncol=1)
		if(!is.null(beta)) {
			ma.rows <- cbind(ma.rows,ma.rows)
		}
		if(!is.null(seasonal.periods)) {
			ma.seasonal <- matrix(0, nrow=q, ncol=tau)
			ma.rows <- cbind(ma.rows,ma.seasonal)
		}
		if(!is.null(ar.coefs)) {
			ar.in.ma <- matrix(0, nrow=q, ncol=p)
			ma.rows <- cbind(ma.rows,ar.in.ma)
		}
		ident <- diag((q-1))
		ident <- cbind(ident,matrix(0,nrow=(q-1),ncol=1))
		ma.part <- rbind(matrix(0,nrow=1,ncol=q),ident)
		ma.rows <- cbind(ma.rows,ma.part)
		F <- rbind(F,ma.rows)
	}
	return(F)
}


#makeWMatrix <- function(small.phi=NULL, seasonal.periods=NULL, ar.coefs=NULL, ma.coefs=NULL) {
#
#	the.list <- .Call("makeBATSWMatrix", smallPhi_s = small.phi, sPeriods_s = as.integer(seasonal.periods), arCoefs_s = ar.coefs, maCoefs_s = ma.coefs, PACKAGE = "forecast")
#
#	
#	return(the.list)
#	
#}

#makeGMatrix <- function(alpha, beta=NULL, gamma.vector=NULL, seasonal.periods=NULL, p=0, q=0) {
#	li <- .Call("makeBATSGMatrix", alpha, beta, gamma.vector, as.integer(seasonal.periods), as.integer(p), as.integer(q), PACKAGE="forecast")
#	
#	return(li)
#}


makeFMatrix <- function(alpha, beta=NULL, small.phi=NULL, seasonal.periods=NULL, gamma.bold.matrix=NULL, ar.coefs=NULL, ma.coefs=NULL) {
	
	#1. Alpha Row
	F <- matrix(1,nrow=1,ncol=1)
	if(!is.null(beta)) {
		F <- cbind(F, matrix(small.phi,nrow=1,ncol=1))
	}
	if(!is.null(seasonal.periods)) {
		tau <- sum(seasonal.periods)
		zero.tau <- matrix(0,nrow=1, ncol=tau)
		F <- cbind(F,zero.tau)
	}
	if(!is.null(ar.coefs)) {
		p <- length(ar.coefs)
		ar.coefs <- matrix(ar.coefs, nrow=1, ncol=p)
		alpha.phi <- alpha*ar.coefs
		F <- cbind(F, alpha.phi)
	}
	if(!is.null(ma.coefs)) {
		q <- length(ma.coefs) 
		ma.coefs <- matrix(ma.coefs,nrow=1,ncol=q)
		alpha.theta <- alpha*ma.coefs
		F <- cbind(F,alpha.theta)
	}

	#2. Beta Row
	if(!is.null(beta)) {
		beta.row <- matrix(c(0,small.phi),nrow=1,ncol=2)
		if(!is.null(seasonal.periods)) {
			beta.row <- cbind(beta.row, zero.tau)
		}
		if(!is.null(ar.coefs)) {
			beta.phi <- beta*ar.coefs
			beta.row <- cbind(beta.row, beta.phi)
		}
		if(!is.null(ma.coefs)) {
			beta.theta <- beta*ma.coefs
			beta.row <- cbind(beta.row, beta.theta)
		}
		F <- rbind(F, beta.row)
	}
	
	#3. Seasonal Row
	if(!is.null(seasonal.periods)) {
		seasonal.row <- t(zero.tau)
		if(!is.null(beta)) {
			seasonal.row <- cbind(seasonal.row,seasonal.row)
		}
		
		
		#Make the A matrix
		for(i in seasonal.periods) {
			if(i == seasonal.periods[1]) {
				a.row.one <- matrix(0,nrow=1,ncol=i)
				a.row.one[i] <- 1
				a.row.two <- cbind(diag((i-1)), matrix(0,nrow=(i-1),ncol=1))
				A <- rbind(a.row.one,a.row.two)
			} else {
				old.A.rows <- dim(A)[1]
				old.A.columns <- dim(A)[2]
				a.row.one <- matrix(0,nrow=1,ncol=i)
				a.row.one[i] <- 1
				a.row.two <- cbind(diag((i-1)), matrix(0,nrow=(i-1),ncol=1))
				Ai <- rbind(a.row.one,a.row.two)
				A <- rbind(A, matrix(0, nrow=dim(Ai)[1], ncol=old.A.columns))
				A <- cbind(A, matrix(0,nrow=dim(A)[1],ncol=dim(Ai)[2]))
				A[((old.A.rows+1):(old.A.rows+dim(Ai)[1])),((old.A.columns+1):(old.A.columns+dim(Ai)[2]))] <- Ai
			}
		}
		seasonal.row <- cbind(seasonal.row,A)
		
		if(!is.null(ar.coefs)) {
			B <- t(gamma.bold.matrix) %*% ar.coefs
			seasonal.row <- cbind(seasonal.row, B)
		}
		if(!is.null(ma.coefs)) {
			C <- t(gamma.bold.matrix) %*% ma.coefs
			seasonal.row <- cbind(seasonal.row, C)
		}
		F <- rbind(F, seasonal.row)
	}
	
	#4. AR() Rows
	if(!is.null(ar.coefs)) {
		#p <- length(ar.coefs)
		ar.rows <- matrix(0,nrow=p,ncol=1)
		if(!is.null(beta)) {
			ar.rows <- cbind(ar.rows, ar.rows)
		}
		if(!is.null(seasonal.periods)) {
			ar.seasonal.zeros <- matrix(0,nrow=p,ncol=tau)
			ar.rows <- cbind(ar.rows, ar.seasonal.zeros)
		}
		ident <- diag((p-1))
		ident <- cbind(ident, matrix(0,nrow=(p-1),ncol=1))
		ar.part <- rbind(ar.coefs,ident)
		ar.rows <- cbind(ar.rows, ar.part)
		
		if(!is.null(ma.coefs)) {
			ma.in.ar <- matrix(0,nrow=p,ncol=q)
			ma.in.ar[1,] <- ma.coefs
			ar.rows <- cbind(ar.rows,ma.in.ar)
		}
		
		F <- rbind(F,ar.rows)
	}

	#5. MA() Rows
	if(!is.null(ma.coefs)) {
		ma.rows <- matrix(0,nrow=q,ncol=1)
		if(!is.null(beta)) {
			ma.rows <- cbind(ma.rows,ma.rows)
		}
		if(!is.null(seasonal.periods)) {
			ma.seasonal <- matrix(0, nrow=q, ncol=tau)
			ma.rows <- cbind(ma.rows,ma.seasonal)
		}
		if(!is.null(ar.coefs)) {
			ar.in.ma <- matrix(0, nrow=q, ncol=p)
			ma.rows <- cbind(ma.rows,ar.in.ma)
		}
		ident <- diag((q-1))
		ident <- cbind(ident,matrix(0,nrow=(q-1),ncol=1))
		ma.part <- rbind(matrix(0,nrow=1,ncol=q),ident)
		ma.rows <- cbind(ma.rows,ma.part)
		F <- rbind(F,ma.rows)
	}
	return(F)
}


makeXMatrix <- function(l, b=NULL, s.vector=NULL, d.vector=NULL, epsilon.vector=NULL) {
	x.transpose <- matrix(l, nrow=1, ncol=1)
	if(!is.null(b)) {
		x.transpose <- cbind(x.transpose, matrix(b, nrow=1, ncol=1))
	}
	if(!is.null(s.vector)) {
		x.transpose <- cbind(x.transpose,matrix(s.vector,nrow=1,ncol=length(s.vector)))
	}
	
	if(!is.null(d.vector)) {
		x.transpose <- cbind(x.transpose,matrix(d.vector,nrow=1,ncol=length(d.vector)))
	}
		
	if(!is.null(epsilon.vector)) {
		x.transpose <- cbind(x.transpose,matrix(epsilon.vector,nrow=1,ncol=length(epsilon.vector)))
	}
	
	x <- t(x.transpose)
	return(list(x=x, x.transpose=x.transpose))
}

