# TODO: Add comment
# 
# Author: srazbash
###############################################################################

unParameteriseTBATS<-function(param.vector, control) {
	#print(control)
	if(control$use.box.cox) {
		lambda<-param.vector[1]
		alpha<-param.vector[2]
		if(control$use.beta) {
			if(control$use.damping) {
				small.phi<-param.vector[3]
				beta<-param.vector[4]
				gamma.start<-5
			} else {
				small.phi<-1
				beta<-param.vector[3]
				gamma.start<-4
			}
		} else {
			small.phi<-NULL
			beta<-NULL
			gamma.start<-3
		}
		if(control$length.gamma > 0) {
			gamma.one.vector<-param.vector[gamma.start:(gamma.start+(control$length.gamma/2)-1)]
			gamma.two.vector<-param.vector[(gamma.start+(control$length.gamma/2)):(gamma.start+(control$length.gamma)-1)]
			final.gamma.pos<-gamma.start+control$length.gamma-1
		} else {
			gamma.one.vector<-NULL
			gamma.two.vector<-NULL
			final.gamma.pos<-gamma.start-1
		}
		if(control$p != 0) {
			ar.coefs<-param.vector[(final.gamma.pos+1):(final.gamma.pos+control$p)]
		} else {
			ar.coefs<-NULL
		}
		if(control$q != 0) {
			ma.coefs<-param.vector[(final.gamma.pos+control$p+1):length(param.vector)]
		} else {
			ma.coefs<-NULL
		}
	} else {
		lambda<-NULL
		alpha<-param.vector[1]
		if(control$use.beta) {
			if(control$use.damping) {
				small.phi<-param.vector[2]
				beta<-param.vector[3]
				gamma.start<-4
			} else {
				small.phi<-1
				beta<-param.vector[2]
				gamma.start<-3
			}
		} else {
			small.phi<-NULL
			beta<-NULL
			gamma.start<-2
		}
		if(control$length.gamma > 0) {
			gamma.one.vector<-param.vector[gamma.start:(gamma.start+(control$length.gamma/2)-1)]
			gamma.two.vector<-param.vector[(gamma.start+(control$length.gamma/2)):(gamma.start+(control$length.gamma)-1)]
			final.gamma.pos<-gamma.start+control$length.gamma-1
		} else {
			gamma.one.vector<-NULL
			gamma.two.vector<-NULL
			final.gamma.pos<-gamma.start-1
		}
		if(control$p != 0) {
			ar.coefs<-param.vector[(final.gamma.pos+1):(final.gamma.pos+control$p)]
		} else {
			ar.coefs<-NULL
		}
		if(control$q != 0) {
			ma.coefs<-param.vector[(final.gamma.pos+control$p+1):length(param.vector)]
		} else {
			ma.coefs<-NULL
		}
	}
	return(list(lambda=lambda, alpha=alpha, beta=beta, small.phi=small.phi, gamma.one.v=gamma.one.vector, gamma.two.v=gamma.two.vector, ar.coefs=ar.coefs, ma.coefs=ma.coefs))
}

makeParscale<-function(control) {
	#print(control)
	if(control$use.box.cox) {
		parscale<-c(.001, .01)
	} else {
		parscale<-.01
	}
	if(control$use.beta) {
		if(control$use.damping) {
			parscale<-c(parscale, 1e-2, 1e-2)
		} else {
			parscale<-c(parscale, 1e-2)
		}
	}
	if(control$length.gamma > 0) {
		parscale<-c(parscale, rep(1e-5, control$length.gamma))	
	}
	
	if((control$p != 0) | (control$q != 0)) {
		parscale<-c(parscale, rep(1e-1, (control$p + control$q)))
	}
	#print(parscale)
	return(parscale)
}
##############################################################################################################################################################################################
##BATS related stuff below
########################################
makeParscaleBATS<-function(control) {
	#print(control)
	if(control$use.box.cox) {
		parscale<-c(.001, .1)
	} else {
		parscale<-.1
	}
	if(control$use.beta) {
		if(control$use.damping) {
			parscale<-c(parscale, 1e-2, 1e-2)
		} else {
			parscale<-c(parscale, 1e-2)
		}
	}
	if(control$length.gamma > 0) {
		parscale<-c(parscale, rep(1e-2, control$length.gamma))	
	}
	
	if((control$p != 0) | (control$q != 0)) {
		parscale<-c(parscale, rep(1e-1, (control$p + control$q)))
	}
	#print(parscale)
	return(parscale)
}


parameterise<-function(alpha, beta.v=NULL, small.phi=1, gamma.v=NULL, lambda=NULL, ar.coefs=NULL, ma.coefs=NULL) {
	#print("urg")
	#print(lambda)
	if(!is.null(lambda)) {
		param.vector<-cbind(lambda, alpha)
		use.box.cox<-TRUE
	} else {
		#print("hello")
		param.vector<-alpha
		use.box.cox<-FALSE
		#print(use.box.cox)
	}
	if(!is.null(beta.v)) {
		use.beta<-TRUE
			if(is.null(small.phi)) {
				use.damping<-FALSE
			} else if(small.phi != 1) {
				param.vector<-cbind(param.vector, small.phi)
				use.damping<-TRUE
			} else {
				use.damping<-FALSE
			}
		param.vector<-cbind(param.vector, beta.v)
	} else {
		use.beta<-FALSE
		use.damping<-FALSE
	}
	if(!is.null(gamma.v)) {
		gamma.v<-matrix(gamma.v, nrow=1, ncol=length(gamma.v))
		param.vector<-cbind(param.vector, gamma.v)
		length.gamma<-length(gamma.v)
	} else {
		length.gamma<-0
	}
	if(!is.null(ar.coefs)) {
		ar.coefs<-matrix(ar.coefs, nrow=1, ncol=length(ar.coefs))
		param.vector<-cbind(param.vector, ar.coefs)
		p<-length(ar.coefs)
	} else {
		p<-0
	}
	if(!is.null(ma.coefs)) {
		ma.coefs<-matrix(ma.coefs, nrow=1, ncol=length(ma.coefs))
		param.vector<-cbind(param.vector, ma.coefs)
		q<-length(ma.coefs)
	} else {
		q<-0
	}
	#print(use.box.cox)
	control<-list(use.beta=use.beta, use.box.cox=use.box.cox, use.damping=use.damping, length.gamma=length.gamma, p=p, q=q)
	return(list(vect=as.numeric(param.vector), control=control))
}

unParameterise<-function(param.vector, control) {
	#print(control)
	if(control$use.box.cox) {
		lambda<-param.vector[1]
		alpha<-param.vector[2]
		if(control$use.beta) {
			if(control$use.damping) {
				small.phi<-param.vector[3]
				beta<-param.vector[4]
				gamma.start<-5
			} else {
				small.phi<-1
				beta<-param.vector[3]
				gamma.start<-4
			}
		} else {
			small.phi<-NULL
			beta<-NULL
			gamma.start<-3
		}
		if(control$length.gamma > 0) {
			gamma.vector<-param.vector[gamma.start:(gamma.start+control$length.gamma-1)]
			final.gamma.pos<-gamma.start+control$length.gamma-1
		} else {
			gamma.vector=NULL
			final.gamma.pos<-gamma.start-1
		}
		if(control$p != 0) {
			ar.coefs<-param.vector[(final.gamma.pos+1):(final.gamma.pos+control$p)]
		} else {
			ar.coefs<-NULL
		}
		if(control$q != 0) {
			ma.coefs<-param.vector[(final.gamma.pos+control$p+1):length(param.vector)]
		} else {
			ma.coefs<-NULL
		}
	} else {
		lambda<-NULL
		alpha<-param.vector[1]
		if(control$use.beta) {
			if(control$use.damping) {
				small.phi<-param.vector[2]
				beta<-param.vector[3]
				gamma.start<-4
			} else {
				small.phi<-1
				beta<-param.vector[2]
				gamma.start<-3
			}
		} else {
			small.phi<-NULL
			beta<-NULL
			gamma.start<-2
		}
		if(control$length.gamma > 0) {
			gamma.vector<-param.vector[gamma.start:(gamma.start+control$length.gamma-1)]
			final.gamma.pos<-gamma.start+control$length.gamma-1
		} else {
			gamma.vector=NULL
			final.gamma.pos<-gamma.start-1
		}
		if(control$p != 0) {
			ar.coefs<-param.vector[(final.gamma.pos+1):(final.gamma.pos+control$p)]
		} else {
			ar.coefs<-NULL
		}
		if(control$q != 0) {
			ma.coefs<-param.vector[(final.gamma.pos+control$p+1):length(param.vector)]
		} else {
			ma.coefs<-NULL
		}
	}
	return(list(lambda=lambda, alpha=alpha, beta=beta, small.phi=small.phi, gamma.v=gamma.vector, ar.coefs=ar.coefs, ma.coefs=ma.coefs))
}
