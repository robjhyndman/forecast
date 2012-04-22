###############################################################################

#TBATS code
cutWTBATS<-function(use.beta, w.tilda.transpose, seasonal.periods, p=0, q=0) {
	mask.vector<-numeric(length(seasonal.periods))
	i<-length(seasonal.periods)
	while(i > 1) {
		for(j in 1:(i-1)) {
			if((seasonal.periods[i] %% seasonal.periods[j]) == 0) {
				mask.vector[j]<-1
			}
		}
		i<-i-1
	}
	
	w.pos.counter<-1
	w.pos<-1
	if(use.beta) {
		w.pos<-w.pos+1
	}
	for(s in seasonal.periods) {
		if(mask.vector[w.pos.counter] == 1) {
			w.tilda.transpose<-w.tilda.transpose[,-((w.pos+1):(w.pos+s))]
		} else if(mask.vector[w.pos.counter] < 0) { 
			#Cut more than one off
			w.pos<-w.pos+s
			w.tilda.transpose<-w.tilda.transpose[,-c((w.pos+mask.vector[w.pos.counter]+1):w.pos)]
			w.pos<-w.pos+mask.vector[w.pos.counter]
		} else {
			w.pos<-w.pos+s
			w.tilda.transpose<-w.tilda.transpose[,-w.pos]
			w.pos<-w.pos-1
		}
		w.pos.counter<-w.pos.counter+1
	}
	if((p != 0) | (q != 0)) {
		end.cut<-ncol(w.tilda.transpose)
		start.cut<-end.cut-(p+q)+1
		w.tilda.transpose<-w.tilda.transpose[,-c(start.cut:end.cut)]	
		
	}
	
	return(list(matrix=w.tilda.transpose, mask.vector=mask.vector))
}

#BATS code below
#########
cutW<-function(use.beta, w.tilda.transpose, seasonal.periods, p=0, q=0) {
	mask.vector<-numeric(length(seasonal.periods))
	i<-length(seasonal.periods)
	while(i > 1) {
		for(j in 1:(i-1)) {
			if((seasonal.periods[i] %% seasonal.periods[j]) == 0) {
				mask.vector[j]<-1
			}
		}
		i<-i-1
	}
	if(length(seasonal.periods) > 1) {
		for(s in length(seasonal.periods):2) {
			for(j in (s-1):1) {
				hcf<-findGCD(seasonal.periods[s], seasonal.periods[j])
				if(hcf != 1) {
					if((mask.vector[s] != 1) & (mask.vector[j] != 1)) {
						mask.vector[s]<-hcf*-1
						
					}
				}
			}
		}
	}
	w.pos.counter<-1
	w.pos<-1
	if(use.beta) {
		w.pos<-w.pos+1
	}
	for(s in seasonal.periods) {
		if(mask.vector[w.pos.counter] == 1) {
			w.tilda.transpose<-w.tilda.transpose[,-((w.pos+1):(w.pos+s))]
		} else if(mask.vector[w.pos.counter] < 0) { 
			#Cut more than one off
			w.pos<-w.pos+s
			w.tilda.transpose<-w.tilda.transpose[,-c((w.pos+mask.vector[w.pos.counter]+1):w.pos)]
			w.pos<-w.pos+mask.vector[w.pos.counter]
		} else {
			w.pos<-w.pos+s
			w.tilda.transpose<-w.tilda.transpose[,-w.pos]
			w.pos<-w.pos-1
		}
		w.pos.counter<-w.pos.counter+1
	}
	if((p != 0) | (q != 0)) {
		end.cut<-ncol(w.tilda.transpose)
		start.cut<-end.cut-(p+q)+1
		w.tilda.transpose<-w.tilda.transpose[,-c(start.cut:end.cut)]	
		
	}
	
	return(list(matrix=w.tilda.transpose, mask.vector=mask.vector))
}


calcSeasonalSeeds<-function(use.beta, coefs, seasonal.periods, mask.vector, p=0, q=0) {
	x.pos.counter<-1
	sum.k<-0
	if(use.beta) {
		x.pos<-2
		new.x.nought<-matrix(coefs[1:2],nrow=2,ncol=1)
	} else {
		x.pos<-1
		new.x.nought<-matrix(coefs[1],nrow=1,ncol=1)
	}
	x.pos.counter<-1
	for(s in seasonal.periods) {
		if(mask.vector[x.pos.counter] == 1) {
			#Make a vector of zeros
			season<-matrix(0, nrow=s, ncol=1)
			new.x.nought<-rbind(new.x.nought, season)
		} else if(mask.vector[x.pos.counter] < 0) { 
			extract<-coefs[(x.pos+1):(x.pos+s+mask.vector[x.pos.counter])]
			#print("extract:")
			#print(extract)
			#Find k
			k<-sum(extract)
			#update sum.k
			sum.k<-sum.k+k/s
			#create the current.periodicity vector
			current.periodicity<-extract-k/s
			current.periodicity<-matrix(current.periodicity, nrow=length(current.periodicity), ncol=1)
			additional<-matrix(-k/s, nrow=(-1*mask.vector[x.pos.counter]), ncol=1)
			current.periodicity<-rbind(current.periodicity, additional)
			new.x.nought<-rbind(new.x.nought, current.periodicity)
			x.pos<-x.pos+s+mask.vector[x.pos.counter]
		}else {
			#Find k
			k<-sum(coefs[(x.pos+1):(x.pos+s-1)])
			#update sum.k
			sum.k<-sum.k+k/s
			#create the current.periodicity vector
			current.periodicity<-coefs[(x.pos+1):(x.pos+s-1)]-k/s
			current.periodicity<-c(current.periodicity, -k/s)
			current.periodicity<-matrix(current.periodicity, nrow=length(current.periodicity), ncol=1)
			new.x.nought<-rbind(new.x.nought, current.periodicity)
			x.pos<-x.pos+s-1
		}
		#Adjust L(t)
		x.pos.counter<-x.pos.counter+1
	}
	#print(new.x.nought)
	#Lastly, get the arma error seed states, if they exist.
	if((p != 0) | (q != 0)) {
		arma.seed.states<-numeric((p+q))
		arma.seed.states<-matrix(arma.seed.states, nrow=length(arma.seed.states), ncol=1)
		#Final value of x.nought
		x.nought<-rbind(new.x.nought, arma.seed.states)
	} else {
		x.nought<-new.x.nought
	}
	return(x.nought)
}

findGCD<-function(larger, smaller) {
	remainder<-larger %% smaller
	if(remainder != 0) {
		return(findGCD(smaller, remainder))
	} else {
		return(smaller)
	}
}

