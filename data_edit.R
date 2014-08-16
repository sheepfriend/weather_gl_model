day<-function(x){
	month<-matrix(c(1,0,2,31,3,28,4,31,5,30,6,31,7,30,8,31,9,31,10,30,11,31,12,30),ncol=2,byrow=T)
	month[,2]<-cumsum(month[,2])
	months<-round(x/100)
	days<-x-months*100
	for(i in 1:12){
		temp<-which(months==month[i,1])
		days[temp]<-days[temp]+month[i,2]
	}
	return(days)
}
lag<-function(x){
	level<-levels(factor(x[,1]))
	y<-c()
	for(i in 1:length(level)){
		temp1<-which(x[,1]==level[i])
		temp<-x[temp1,]
		temp<-cbind(temp,rbind(rep(0,3),x[temp1[1:(length(temp1)-1)],5:7]))
		temp<-cbind(temp,temp[,5]>0)
		temp<-temp[-1,]
		y<-rbind(y,temp)
	}
	return(y)
}
