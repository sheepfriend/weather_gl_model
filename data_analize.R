spell.temp<-function(x){
	record<-c()
	k<-1
	j<-1
	while(k<=length(x)){
		up<-0
		down<-0
		up.total<-0
		down.total<-0
		while(k<length(x) & x[k]<=x[k+1]){
			up<-up+1
			up.total<-up.total+x[k+1]-x[k]
			k<-k+1
		}
		if(k==length(x)){
			record<-rbind(record,c(up,down,up.total,down.total))
			return(record)
		}
		while(k<length(x) & x[k]>=x[k+1]){
			down<-down+1
			down.total<-down.total+x[k+1]-x[k]
			k<-k+1
		}
		record<-rbind(record,c(up,down,up.total,down.total))
	}
	return(record)
}

freq<-function(x){
	level<-as.numeric(levels(factor(x)))
	record<-c()
	for(i in 1:length(level)){
		if(level[i]!=0){
			record<-rbind(record,c(level[i],length(which(x==level[i]))))
		}
	}
	record[,2]<-record[,2]/sum(record[,2])
	record<-rbind(c(0,0),record)
	return(record)
}

LU<-function(x){
	if(length(x)==1){n<-1}
	else{n<-length(x[,1])}
	L<-x
	if(n>1){
		L[1,1]<-sqrt(L[1,1])
		L[2:n,1]<-L[2:n,1]/L[1,1]
		L[2:n,2:n]<-L[2:n,2:n]-L[2:n,1]%o%L[2:n,1]
		return(cbind(L[,1],rbind(rep(0,n-1),LU(L[2:n,2:n]))))
	}
	else{return(sqrt(L))}
}

dist<-function(x,y){
	return(sqrt(sum((x-y)^2)))
}


updown<-function(x){
	up<-0
	down<-0
	up.total<-0
	down.total<-0
	up.record<-c()
	down.record<-c()
	for(i in 2:length(x)){
		if(x[i-1]-x[i]<0){
			up<-up+1
			up.total<-up.total+x[i]-x[i-1]
			up.record<-c(up.record,x[i]-x[i-1])
		}
		else{
			down<-down+1
			down.total<-down.total+x[i]-x[i-1]
			down.record<-c(down.record,x[i]-x[i-1])
		}
	}
	return(list(c(up,up.total,down,down.total),up.record,down.record))
}
n