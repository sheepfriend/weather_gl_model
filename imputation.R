missing.max<-function(x){
	miss.max<-0
	k<-1
	j<-1
	while(k<=length(x)){
		temp<-0
		while(x[k]==-9999 & k<=length(x)){
			k<-k+1
			temp<-temp+1
		}
		if(temp>miss.max){
			miss.max<-temp
			j<-k
		}
		if(k==length(x)){return(c(miss.max,j))}
		while(x[k]!=-9999 & k<=length(x)){k<-k+1}
	}
	return(c(miss.max,j))
}

#subsitute single missing data with the avarage temperature between that day(for TMAX and TMIN)
insert<-function(x,num,data){
	if(num==1){return(rbind(x,data))}
	else if(num==length(data[,1])){return(rbind(data,x))}
	else{return(rbind(data[1:(num-1),],x,data[num:length(data[,1]),]))}
}

match<-function(x){
	temp<-data.frame(matrix(0,nrow=41*365,ncol=length(x[1,])))
	temp[,4]<-rep(1:365,41)
	for(i in 1:41){temp[(i-1)*365+1:365,8]<-1972+i}
	for(i in 1:length(x[,1])){
		p<-which(x[i,"DATE"]==temp[,4] & x[i,"year"]==temp[,8])
		temp[p,]<-x[i,]
	}
	names(temp)<-names(x)
	p<-which(temp[,"LATITUDE"]==0)
	if(length(p)!=0){
		for(i in p){
			q<-which(x[,"DATE"]==temp[i,"DATE"])[1]
			temp[i,]<-x[q,]
		}
	}
	return(temp)
}

suc<-function(x){
	result<-c()
	k<-length(x)
	print(k)
	i<-1
	while(i<=k){
		j<-0
		while(i<=k){
			if(x[i]!=-9999){i<-i+1}
			else{break()}
		}
		if(i==k+1){return(result)}
		while(i<=k){
			if(x[i]==-9999){i<-i+1;j<-j+1}
			else{break()}
		}
		if(j!=0){result<-rbind(result,c(i-j,i-1,j))}
	}
	return(result)
}

missing.temp<-function(x,what){
	record<-matrix(0,nrow=365,ncol=41)
	temp<-suc(x[,what])
	if(length(temp)!=0){
		for(i in 1:length(temp[,1])){
			end<-temp[i,1]-1
			if(i==1){start<-1}
			else{start<-temp[i-1,1]+1}
			record[start:end]<-(end-start+1):1
		}
		temp<-cbind(temp,temp[,3]-round(temp[,3]/365)*365)
		for(i in 1:length(temp[,1])){
			a<-which(record[temp[i,4],]>=temp[i,3])
			if(length(a)!=0){
				a<-(sample(a,size=1)-1)*365+temp[i,4]
				x[temp[i,1]:temp[i,2],what]<-x[a+0:(temp[i,2]-temp[i,1]),what]
			}
		}
	}
	return(x)
}
