#read data
d<-read.csv("/Users/sheepfriend/Dropbox/pampa.csv",stringsAsFactor=F)
d<-d[,-1]
d<-d[which(d[,6]>=19730601),]

#record the number of locations
level<-levels(factor(d[,1]))

#plot a map to see the locations
library(mapdata)
map("world","Argentina",xlim=c(-65,-55),ylim=c(-40,-25),fill=T,col="gray90")
map("world","Uruguay",add=T,fill=T,col="gray50")
map("world","Brazil",add=T,fill=T,col="gray70")
map("world","Paraguay",add=T,fill=T,col="gray60")
place<-c()
for(i in 1:length(level)){
	temp<-which(d[,1]==level[i])
	place<-rbind(place,c(d[temp[1],5],d[temp[1],4]))
}
points(place)
map.axes()

#change the date to day and delete year
d<-cbind(d,d[,6]-round(d[,6]/10000)*10000)
names(d)[10]<-"year"
d[,6]<-d[,6]-round(d[,6]/10000)*10000
d<-d[-which(d[,6]==229),]

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

#add paramaters
d[,6]<-day(d[,6])
d<-d[,-c(2,3)]
rain<-1-d[,5]==0
season1<-cos(2*pi*d[,4]/365)
season2<-sin(2*pi*d[,4]/365)
d<-cbind(d,rain,season1,season2)

#create another col to make autogressive relation
lag<-function(x){
	level<-levels(factor(x[,1]))
	y<-c()
	for(i in 1:length(level)){
		temp1<-which(x[,1]==level[i])
		temp<-x[temp1,]
		print(head(temp))
		temp<-cbind(temp,rbind(rep(0,3),x[temp1[1:(length(temp1)-1)],5:7]))
		temp<-temp[-1,]
		y<-rbind(y,temp)
	}
	return(y)
}
d<-lag(d)
names(d)[12:14]<-c("PRCPpre","TMAXpre","TMINpre")

#compute the max length of successive -9999
missing.max<-function(x){
	miss.max<-0
	k<-1
	while(k<=length(x)){
		temp<-0
		while(x[k]==-9999 & k<=length(x)){
			k<-k+1
			temp<-temp+1
		}
		if(temp>miss.max){miss.max<-temp}
		if(k==length(x)){return(miss.max)}
		while(x[k]!=-9999 & k<=length(x)){k<-k+1}
	}
	return(miss.max)
}

#subsitute single missing data with the avarage temperature between that day(for TMAX and TMIN)
missing.temp<-function(x){
	start<-1
	while(x[start]==-9999){start<-start+1}
	k<-start
	for(k in (start+1):(length(x)-1)){
		if(x[k]==-9999 & x[k-1]!=-9999 & x[k+1]!=-9999){x[k]<-(x[k-1]+x[k+1])/2}
	}
	return(x)
}

#delete the data with successive missing data
d[,"TMIN"]<-missing.temp(d[,"TMIN"])
d[,"TMAX"]<-missing.temp(d[,"TMAX"])
d[,"TMINpre"]<-missing.temp(d[,"TMINpre"])
d[,"TMAXpre"]<-missing.temp(d[,"TMAXpre"])
d<-d[which(d[,"TMAX"]!=-9999 & d[,"TMIN"]!=-9999),]
d<-d[which(d[,"TMAXpre"]!=-9999 & d[,"TMINpre"]!=-9999),]

#count the number of spells of cold and hot day
spell.temp<-function(x){
	record<-c()
	k<-1
	while(k<=length(x)){
		up<-0
		down<-0
		while(k<length(x) & x[k]<=x[k+1]){
			k<-k+1
			up<-up+1
		}
		if(k==length(x)){
			record<-rbind(record,c(up,down))
			return(record)
		}
		while(k<length(x) & x[k]>=x[k+1]){
			down<-down+1
			k<-k+1
		}
		record<-rbind(record,c(up,down))
	}
	return(record)
}

fit<-list()
for(i in 1:length(level)){
	temp<-d[which(d[,1]==level[i]),]
	fit[[i]]<-glm(I(PRCP<0)~season1+season2+I(PRCPpre)+year,data=temp)
}

#regression for TMAX and TMIN
fit.max<-list()
fit.min<-list()
for(i in 1:length(level)){
	temp<-d[which(d[,1]==level[i]),]
	fit.max[[i]]<-list()
	fit.min[[i]]<-list()
	fit.max[[i]][[1]]<-glm(TMAX~TMAXpre+TMINpre+season1+season2+DATE,data=temp)
	fit.min[[i]][[1]]<-glm(TMIN~TMAXpre+TMINpre+season1+season2+DATE,data=temp)
	fit.max[[i]][[2]]<-spell.temp(predict(fit.max[[i]][[1]]))
	fit.min[[i]][[2]]<-spell.temp(predict(fit.min[[i]][[1]]))
	fit.max[[i]][[3]]<-spell.temp(temp[,"TMAX"])
	fit.min[[i]][[3]]<-spell.temp(temp[,"TMIN"])
}

#compute the frequency for cold and hot spells
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

#simulation
d<-cbind(d,0,0,0,0,0,0)
names(d)[15:20]<-c("WMAX","preMAX","todayMAX","WMIN","preMIN","todayMIN")
for(i in 1:1){
	temp<-which(d[,1]==level[i])
	d[temp[1],"preMAX"]<-d[temp[1],"TMAXpre"]
	d[temp[1],"preMIN"]<-d[temp[1],"TMINpre"]
	sigma.max<-sqrt(fit.max[[i]][[1]]$dev/fit.max[[i]][[1]]$df.res)
	sigma.min<-sqrt(fit.min[[i]][[1]]$dev/fit.min[[i]][[1]]$df.res)
	coef.max<-fit.max[[i]][[1]]$coef
	coef.min<-fit.min[[i]][[1]]$coef
	d[temp,"WMAX"]<-rnorm(length(temp))*sigma.max
	d[temp,"WMIN"]<-rnorm(length(temp))*sigma.min
	for(j in 1:(length(temp))){
		print(j)
		d[temp[j],"todayMAX"]<-coef.max%*%c(1,d[temp[j],"preMAX"],d[temp[j],"preMIN"],d[temp[j],"season1"],d[temp[j],"season2"],d[temp[j],"DATE"])+d[temp[j],"WMAX"]
		d[temp[j],"todayMIN"]<-coef.max%*%c(1,d[temp[j],"preMAX"],d[temp[j],"preMIN"],d[temp[j],"season1"],d[temp[j],"season2"],d[temp[j],"DATE"])+d[temp[j],"WMIN"]
		if(j!=length(temp)){
			d[temp[j+1],c("preMAX","preMIN")]<-d[temp[j],c("todayMAX","todayMIN")]
		}
	}
}

#compute the covariance matrix for all locations 30days/matrix
#change the data in a format convenient for further calculation
level1<-level
level<-matrix(0,ncol=3,nrow=length(level1))
for(i in 1:length(level[,1])){level[i,2]<-length(which(d[,1]==level1[i]))}
level[,3]<-cumsum(c(0,level[,2]))[1:9]+1

len<-round(min(level[,2])/30)
cov.max<-array(0,length(level[,1])^2*len)
cov.min<-array(0,length(level[,1])^2*len)
dim(cov.max)<-c(len,length(level[,1]),length(level[,1]))
dim(cov.min)<-c(len,length(level[,1]),length(level[,1]))

data.max<-array(0,30*length(level[,1])*len)
data.min<-array(0,30*length(level[,1])*len)
dim(data.max)<-c(len,length(level[,1]),30)
dim(data.min)<-c(len,length(level[,1]),30)
for(i in 1:len){
	temp1<-c()
	temp2<-c()
	for(j in 1:length(level[,1])){
		temp<-level[j,3]+((i-1)*30):(i*30-1)
		temp1<-cbind(temp1,d[temp,"TMAX"])
		temp2<-cbind(temp2,d[temp,"TMIN"])
	}
	data.max[i,,]<-temp1
	data.min[i,,]<-temp2
}

#compute the covariance matrix
for(i in 1:len){
	cov.max[i,,]<-var(t(data.max[i,,]))
	cov.min[i,,]<-var(t(data.min[i,,]))
}