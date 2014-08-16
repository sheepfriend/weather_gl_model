#read data
source("data_edit.r")
source("imputation.r")
source("data_analize.r")
d<-read.csv("/Users/sheepfriend/Dropbox/pampa.csv",stringsAsFactor=F)
d<-d[,-1]
d<-d[which(d[,6]>=19730601 & d[,6]<20140000),]

#record the number of locations
level<-levels(factor(d[,1]))

#change the date to day and delete year
d<-cbind(d,round(d[,6]/10000))
names(d)[10]<-"year"
d[,6]<-d[,6]-round(d[,6]/10000)*10000
d<-d[-which(d[,6]==229),]

#add paramaters
d[,6]<-day(d[,6])
d<-d[,-c(2,3)]

record<-list()

for(i in 1:9){
	print(i)
	record[[i]]<-match(d[which(d[,"STATION"]==level[i]),])
	record[[i]]<-missing.temp(record[[i]],"TMIN")
	record[[i]]<-missing.temp(record[[i]],"TMAX")
	temp1<-which(record[[i]][,"TMIN"]==-9999)
	temp2<-which(record[[i]][,"TMAX"]==-9999)
	if(length(temp1)>1){record[[i]]<-missing.temp(record[[i]],"TMIN")}
	if(length(temp2)>1){record[[i]]<-missing.temp(record[[i]],"TMAX")}
	rain<-record[[i]][,5]>0
	season1<-cos(2*pi*record[[i]][,4]/365)
	season2<-sin(2*pi*record[[i]][,4]/365)
	record[[i]]<-cbind(record[[i]],rain,season1,season2)
	record[[i]]<-lag(record[[i]])
	names(record[[i]])[12:15]<-c("PRCPpre","TMAXpre","TMINpre","rainpre")
}

sam<-d
d<-c()
for(i in 1:length(level)){d<-rbind(d,record[[i]])}
rain<-1-sam[,5]==0
season1<-cos(2*pi*sam[,4]/365)
season2<-sin(2*pi*sam[,4]/365)
sam<-cbind(sam,rain,season1,season2)

sam<-lag(sam)
names(sam)[12:15]<-c("PRCPpre","TMAXpre","TMINpre","rainpre")
sam[,"rain"]<-sam[,"PRCP"]>0
sam[,"rainpre"]<-sam[,"rainpre"]+0
sam<-sam[which(sam[,"PRCP"]!=-9999 & sam[,"PRCPpre"]!=-9999),]
sam[,"rain"]<-sam[,"rain"]+0

fit.bino<-list()
fit.norm<-list()

for(i in 1:length(level)){
	fit.norm[[i]]<-glm(PRCP~PRCPpre+season1+season2,data=sam[which(sam[,"STATION"]==level[i]),])
	fit.bino[[i]]<-glm(rain~rainpre+season1+season2,data=sam[which(sam[,"STATION"]==level[i]),],family=binomial(link="probit"))
}

for(i in 1:9){
	coef<-fit.bino[[i]]$coef
	record[[i]][,"rain"]<-record[[i]][,"rain"]+0
	names(record[[i]])[15]<-"rainpre"
	record[[i]][,"rainpre"]<-record[[i]][,"rainpre"]+0
	for(j in 1:length(record[[i]][,1])){
		if(record[[i]][j,"PRCP"]==-9999){
			temp<-c(record[[i]][j,"rainpre"],record[[i]][j,"season1"],record[[i]][j,"season2"])
			temp<-coef%*%c(1,temp)
			if(temp>0){
				record[[i]][j,"rain"]<-1
				if(j<length(record[[i]],1)){
					record[[i]][j+1,"rainpre"]<-1
				}
			}
		}
	}
}

#regression for TMAX and TMIN
fit.max<-list()
fit.min<-list()
for(i in 1:length(level)){
	temp<-record[[i]][1:10000,]
	fit.max[[i]]<-list()
	fit.min[[i]]<-list()
	fit.max[[i]][[1]]<-glm(TMAX~TMAXpre+TMINpre+season1+season2+DATE+year+rain,data=temp)
	fit.min[[i]][[1]]<-glm(TMIN~TMAXpre+TMINpre+season1+season2+DATE+year+rain,data=temp)
	fit.max[[i]][[2]]<-spell.temp(predict(fit.max[[i]][[1]]))
	fit.min[[i]][[2]]<-spell.temp(predict(fit.min[[i]][[1]]))
	fit.max[[i]][[3]]<-spell.temp(temp[,"TMAX"])
	fit.min[[i]][[3]]<-spell.temp(temp[,"TMIN"])
}

#compute the covariance matrix for all locations 30days/matrix
#change the data in a format convenient for further calculation
level1<-level
level<-matrix(0,ncol=3,nrow=length(level1))
for(i in 1:length(level[,1])){
	level[i,2]<-length(which(d[,1]==level1[i]))
	level[i,3]<-which(d[,"STATION"]==level1[i])[1]
}

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
	start<-(i-1)*30+1
	end<-i*30
	for(j in 1:length(level[,1])){
		temp1<-cbind(temp1,fit.max[[j]][[1]]$res[start:end])
		temp2<-cbind(temp2,fit.min[[j]][[1]]$res[start:end])
	}
	data.max[i,,]<-temp1
	data.min[i,,]<-temp2
}

#compute the covariance matrix
for(i in 1:len){
	cov.max[i,,]<-var(t(data.max[i,,]))
	cov.min[i,,]<-var(t(data.min[i,,]))
}

coef.max<-c()
coef.min<-c()
for(i in 1:length(level[,1])){
	coef.max<-rbind(coef.max,fit.max[[i]][[1]]$coef)
	coef.min<-rbind(coef.min,fit.min[[i]][[1]]$coef)
}

cova.max<-list()
cova.min<-list()
for(i in 1:len){
	cova.max[[i]]<-LU(cov.max[i,,])
	cova.min[[i]]<-LU(cov.min[i,,])
}

#generate random normal variables together
rnorm.max<-matrix(rnorm(length(level[,1])*30*len),ncol=length(level[,1]),nrow=len*30)
rnorm.min<-matrix(rnorm(length(level[,1])*30*len),ncol=length(level[,1]),nrow=len*30)

for(i in 1:len){
	start<-(i-1)*30
	end<-i*30-1
	which.matrix<-start:end
	rnorm.max[which.matrix,]<-rnorm.max[which.matrix,]%*%t(cova.max[[1]])
	rnorm.min[which.matrix,]<-rnorm.min[which.matrix,]%*%t(cova.min[[1]])
}

#simulation with spatial covariance
for(i in 10001:(len*30)){
	if(round(i/30)*30==i){print(i/30)}
	temp<-level[,3]+i-1
	if(i==1){
		d[temp,"preMAX"]<-d[temp,"TMAXpre"]
		d[temp,"preMIN"]<-d[temp,"TMINpre"]
	}
	d[temp,"WMAX"]<-rnorm.max[i,]
	d[temp,"WMIN"]<-rnorm.min[i,]
	d[temp,"todayMAX"]<-diag(coef.max%*%rbind(1,d[temp,"preMAX"],d[temp,"preMIN"],d[temp,"season1"],d[temp,"season2"],d[temp,"DATE"])+d[temp,"WMAX"])
	d[temp,"todayMIN"]<-diag(coef.min%*%rbind(1,d[temp,"preMAX"],d[temp,"preMIN"],d[temp,"season1"],d[temp,"season2"],d[temp,"DATE"])+d[temp,"WMIN"])
	if(j!=len*30){
			d[temp+1,c("preMAX","preMIN")]<-d[temp,c("todayMAX","todayMIN")]
	}
}

dist.matrix<-matrix(0,length(level[,1]),length(level[,1]))
for(i in 1:length(level[,1])){
	for(j in 1:length(level[,1])){
		dist.matrix[i,j]<-dist(d[level[i,3],2:3],d[level[j,3],2:3])
	}
}

fit.max.new<-list()
fit.min.new<-list()

for(i in 1:length(level[,1])){
	max.which<-which(fit.max[[i]][[1]]$res[1:10000]>0)
	min.which<-which(fit.min[[i]][[1]]$res[1:10000]>0)
	max.num<-length(max.which)/len/30
	min.num<-length(min.which)/len/30
	max.right<-fit.max[[i]][[1]]$res[max.which]
	max.right<-c(max.right,-max.right)
	max.left<-fit.max[[i]][[1]]$res[-max.which]
	max.left<-c(max.left,-max.left)
	min.right<-fit.min[[i]][[1]]$res[min.which]
	min.right<-c(min.right,-min.right)
	min.left<-fit.min[[i]][[1]]$res[-min.which]
	min.left<-c(min.left,-min.left)
	max.right<-sqrt(var(max.right))
	max.left<-sqrt(var(max.left))
	min.right<-sqrt(var(min.right))
	min.left<-sqrt(var(min.left))
	fit.max.new[[i]]<-c(max.num,max.left,max.right)
	fit.min.new[[i]]<-c(min.num,min.left,min.right)
}

#generate the adjusted random normal
rnorm.max<-matrix(0,ncol=length(level[,1]),nrow=len*30)
rnorm.min<-matrix(0,ncol=length(level[,1]),nrow=len*30)

for(i in 1:length(level[,1])){
	rnorm.max[,i]<-rnorm.new(len*30,fit.max.new[[i]][1],fit.max.new[[i]][2],fit.max.new[[i]][3])
	rnorm.min[,i]<-rnorm.new(len*30,fit.min.new[[i]][1],fit.min.new[[i]][2],fit.min.new[[i]][3])
}

corr.max<-list()
corr.min<-list()

for(i in 1:len){
	temp1<-matrix(0,9,9)
	temp2<-matrix(0,9,9)
	diag(temp1)<-sqrt(1/diag(cov.max[i,,]))
	diag(temp2)<-sqrt(1/diag(cov.min[i,,]))
	corr.max[[i]]<-temp1%*%cova.max[[i]]
	corr.min[[i]]<-temp2%*%cova.min[[i]]
}

for(i in 1:len){
	start<-(i-1)*30
	end<-i*30-1
	which.matrix<-start:end
	rnorm.max[which.matrix,]<-rnorm.max[which.matrix,]%*%t(corr.max[[1]])
	rnorm.min[which.matrix,]<-rnorm.min[which.matrix,]%*%t(corr.min[[1]])
}

#simulate
for(i in 10001:(30*len)){
	if(round(i/30)*30==i){print(i/30)}
	temp<-level[,3]+i-1
	if(i==10001){
		d[temp,"preMAX"]<-d[temp,"TMAXpre"]
		d[temp,"preMIN"]<-d[temp,"TMINpre"]
	}
	d[temp,"WMAX"]<-rnorm.max[i,]
	d[temp,"WMIN"]<-rnorm.min[i,]
	d[temp,"todayMAX"]<-diag(coef.max%*%rbind(1,d[temp,"preMAX"],d[temp,"preMIN"],d[temp,"season1"],d[temp,"season2"],d[temp,"DATE"])+d[temp,"WMAX"])
	d[temp,"todayMIN"]<-diag(coef.min%*%rbind(1,d[temp,"preMAX"],d[temp,"preMIN"],d[temp,"season1"],d[temp,"season2"],d[temp,"DATE"])+d[temp,"WMIN"])
	print(d[temp,"todayMAX"])
	if(j!=len*30){
			d[temp+1,c("preMAX","preMIN")]<-d[temp,c("todayMAX","todayMIN")]
	}
}

corr.table.max<-array(0,dim=c(333,9,9))
corr.table.min<-array(0,dim=c(333,9,9))
corr.table.max.new<-array(NA,dim=c(107,9,9))
corr.table.min.new<-array(NA,dim=c(107,9,9))
dist.table<-rep(0,36)
k<-1
for(i in 1:9){
	for(j in i:9){
		if(i!=j){
			dist.table[k]<-dist.matrix[i,j]
			k<-k+1
		}
	}
}
for(m in 1:333){
	for(i in 1:length(level[,1])){
		for(j in i:length(level[,1])){
			start<-(m-1)*30
			end<-m*30-1
			if(i!=j){
				corr.table.max[m,i,j]<-sqrt(var(d[level[i,3]+start:end,"TMAX"]-d[level[j,3]+start:end,"TMAX"]))
				corr.table.min[m,i,j]<-sqrt(var(d[level[i,3]+start:end,"TMIN"]-d[level[j,3]+start:end,"TMIN"]))
				if(m<=100 & (end+9990<=level[i,2] & end+9990<=level[j,2])){
					corr.table.max.new[m,i,j]<-sqrt(var(d[(level[i,3]+len*30-1+start:end),"TMAX"]-d[(level[j,3]+len*30-1+start:end),"TMAX"]))
					corr.table.min.new[m,i,j]<-sqrt(var(d[(level[i,3]+len*30-1+start:end),"TMIN"]-d[(level[j,3]+len*30-1+start:end),"TMIN"])
				}
			}
		}
	}
}

table.max<-matrix(0,nrow=333,ncol=36)
table.min<-matrix(0,nrow=333,ncol=36)
table.max.new<-matrix(NA,nrow=107,ncol=36)
table.min.new<-matrix(NA,nrow=107,ncol=36)
k<-1
for(i in 1:length(level[,1])){
	for(j in i:length(level[,1])){
		if(i<j){
			table.max[,k]<-corr.table.max[,i,j]
			table.min[,k]<-corr.table.min[,i,j]
			table.max.new[,k]<-corr.table.max.new[,i,j]
			table.min.new[,k]<-corr.table.min.new[,i,j]
			k<-k+1
		}
	}
}
table.max<-table.max[,order(dist.table)]
table.min<-table.min[,order(dist.table)]
table.max.new<-table.max.new[,order(dist.table)]
table.min.new<-table.min.new[,order(dist.table)]
dist.table<-sort(dist.table)

