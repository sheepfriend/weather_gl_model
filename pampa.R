#read data
d<-read.csv("/Users/sheepfriend/Dropbox/pampa.csv",stringsAsFactor=F)
d<-d[,-1]
d<-d[which(d[,6]>=19730601 & d[,6]<20140000),]

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
	place<-rbind(place,c(d[temp[1],"LONGITUDE"],d[temp[1],"LATITUDE"]))
}
points(place)
map.axes()

#change the date to day and delete year
d<-cbind(d,round(d[,6]/10000))
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

#compute the max length of successive -9999
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
	for(i in p){
		q<-which(x[,"DATE"]==temp[i,"DATE"])[1]
		temp[i,]<-x[q,]
	}
	return(x)
}

suc<-function(x){
	result<-c()
	k<-length(x)
	i<-1
	while(i<=k){
		j<-0
		while(i<=k){if(x[i]!=-9999)i<-i+1}
		if(i==k){return(result)}
		while(i<=k){if(x[i]==-9999)i<-i+1;j<-j+1}
		if(j!=0){result<-rbind(result,c(i-j,i+1,j))}
	}
	return(result)
}

missing.temp<-function(x,what){
	record<-matrix(0,nrow=365,ncol=41)
	temp<-suc(x[,what])
	for(i in 1:length(temp[,1])){
		end<-temp[i,1]-1
		if(i==1){start<-1}
		else{start<-temp[i-1,1]+1}
		record[start:end]<-(end-start+1):1
	}
	temp<-cbind(temp,temp[,3]-round(temp[,3]/365)*365) 
	for(i in 1:length(temp[,1])){
		a<-which(record[temp[i,4],]>=temp[i,3])
		if(length(a)==0){print(i)}
		a<-(sample(a,size=1)-1)*365+temp[i,4]
		x[temp[i,1]:temp[i,2],what]<-x[a+0:(temp[i,2]-temp[i,1]),what]
	}
	return(x)
}

record<-list()

for(i in 3:length(level)){
	print(i)
	record[[i]]<-match(d[which(d[,"STATION"]==level[i]),])
	record[[i]]<-missing.temp(record[[i]],"TMIN")
	record[[i]]<-missing.temp(record[[i]],"TMAX")
	temp1<-which(record[[i]][,"TMIN"]==-9999)
	temp2<-which(record[[i]][,"TMAX"]==-9999)
	if(length(temp1)!=0){record[[i]]<-missing.temp(record[[i]],"TMIN")}
	if(length(temp2)!=0){record[[i]]<-missing.temp(record[[i]],"TMAX")}
}
d<-c()
for(i in 1:length(level)){d<-rbind(d,recrod[[i]])}


#delete the data with successive missing data

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
names(d)[12:14]<-c("PRCPpre","TMAXpre","TMINpre")\

#count the number of spells of cold and hot day
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

#regression for TMAX and TMIN
fit.max<-list()
fit.min<-list()
for(i in 1:length(level)){
	temp<-d[which(d[,1]==level[i])[1:10000],]
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
for(i in 1:length(level)){
	temp<-which(d[,1]==level[i])
	d[temp[10001],"preMAX"]<-d[temp[10001],"TMAXpre"]
	d[temp[10001],"preMIN"]<-d[temp[10001],"TMINpre"]
	sigma.max<-sqrt(fit.max[[i]][[1]]$dev/fit.max[[i]][[1]]$df.res)
	sigma.min<-sqrt(fit.min[[i]][[1]]$dev/fit.min[[i]][[1]]$df.res)
	coef.max<-fit.max[[i]][[1]]$coef
	coef.min<-fit.min[[i]][[1]]$coef
	d[temp,"WMAX"]<-rnorm(length(temp))*sigma.max
	d[temp,"WMIN"]<-rnorm(length(temp))*sigma.min
	for(j in 10001:(length(temp))){
		d[temp[j],"todayMAX"]<-coef.max%*%c(1,d[temp[j],"preMAX"],d[temp[j],"preMIN"],d[temp[j],"season1"],d[temp[j],"season2"],d[temp[j],"DATE"])+d[temp[j],"WMAX"]
		d[temp[j],"todayMIN"]<-coef.min%*%c(1,d[temp[j],"preMAX"],d[temp[j],"preMIN"],d[temp[j],"season1"],d[temp[j],"season2"],d[temp[j],"DATE"])+d[temp[j],"WMIN"]
		if(j!=length(temp)){
			d[temp[j+1],c("preMAX","preMIN")]<-d[temp[j],c("todayMAX","todayMIN")]
		}
	}
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

#LU factorization
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

#simulate random variables with covariance matrix
rnorm.multi<-function(cov){
	if(length(cov)==1){return(rnorm(1))}
	else{return(cov%*%rnorm(length(cov[,1])))}
}

#get correlation matrix
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

#find the relationship btw distance and temperature correlations
corr<-function(x){
	temp<-matrix(0,length(x[,1]),length(x[,1]))
	diag(temp)<-sqrt(1/diag(x))
	return(temp%*%x%*%temp)
}
cor.max<-cov.max
cor.min<-cov.min
record.max<-matrix(0,ncol=2,nrow=81*len)
record.min<-matrix(0,ncol=2,nrow=81*len)

for(i in 1:len){
	start<-(i-1)*81+1
	end<-i*81
	temp1<-corr(cor.max[i,,])
	temp2<-corr(cor.min[i,,])
	cor.max[i,,]<-temp1
	cor.min[i,,]<-temp2
	dim(temp1)<-81
	dim(temp2)<-81
	record.max[start:end,2]<-temp1
	record.min[start:end,2]<-temp2
}

dist<-function(x,y){
	return(sqrt(sum((x-y)^2)))
}
dist.matrix<-matrix(0,length(level[,1]),length(level[,1]))
for(i in 1:length(level[,1])){
	for(j in 1:length(level[,1])){
		dist.matrix[i,j]<-dist(d[level[i,3],2:3],d[level[j,3],2:3])
	}
}
dim(dist.matrix)<-81
record.max[,1]<-rep(dist.matrix,len)
record.min[,1]<-rep(dist.matrix,len)
plot(record.max)
plot(record.min)

#the hot spell result is not satisfying, the effect of rainfall cannot be ignored
#there is possibility that TMIN and TMAX are influenced by rainfall in different ways
#since the PRCP data has nearly half is missing, only pay attention to the awailable ones.

rain<-function(d){
	temp<-which(d[,"PRCP"]!=-9999)
	d<-d[temp,]
	record<-matrix(0,ncol=4,nrow=length(which(d[,"PRCP"]>0)))
	k<-0
	for(i in 2:(length(temp))){
		if(d[i,"PRCP"]!=0){
			k<-k+1
			record[k,]<-c(i,d[i-1,"TMAX"]-d[i,"TMAX"],d[i-1,"TMIN"]-d[i,"TMIN"],d[i,"PRCP"])
		}
	}
	return(record)
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

#finding: the distribution for residual is not normal:
#Pr(X>0)>0.5 but E(X)=0
#way to solve: binomial whether - or +
#then simulate seperately, using normal
#thus the variance for up and down should be record seperately
#in terms of covariance, ni ma, there is some covariance among the residual of the 9 locations

rnorm.new<-function(x,percen,left,right){
	if(x<=0){return(FALSE)}
	else if(x==1){
		if(runif(1)<=percen){return(abs(rnorm(1))*right)}
		else{return(-abs(rnorm(1))*left)}
	}
	else{
		ran<-runif(x)
		temp<-which(ran<=percen)
		if(length(temp>0)){
			ran[temp]<-abs(rnorm(length(temp)))*right
			ran[-temp]<--abs(rnorm(x-length(temp)))*left
		}
		else{ran<--abs(rnorm(x))*left}
		return(ran)
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

for(i in 1:9){
	temp<-spell.temp(d[10001:10360+level[i,3]-1,"todayMIN"])
	plot(freq(temp[,1]),type='l',xlim=c(0,8),ylim=c(0,0.6))
	par(new=T)
	temp<-spell.temp(d[10001:10360+level[i,3]-1,"TMIN"])
	plot(freq(temp[,1]),type='l',xlim=c(0,8),ylim=c(0,0.6),col='red')
	par(new=T)
}

for(i in 1:9){
	temp<-spell.temp(d[10001:10360+level[i,3]-1,"todayMAX"])
	plot(freq(temp[,1]),type='l',xlim=c(0,8),ylim=c(0,0.6))
	par(new=T)
	temp<-spell.temp(d[10001:10360+level[i,3]-1,"TMAX"])
	plot(freq(temp[,1]),type='l',xlim=c(0,8),ylim=c(0,0.6),col='red')
	par(new=T)
}

fit.bino<-list()
fit.norm<-list()
for(i in 1:length(level[,1])){
	fit.norm[[i]]<-glm(PRCP~PRCPpre+season1+season2,data=sam[which(sam[,"STATION"]==level1[i]),])
	fit.bino[[i]]<-glm(rain~rainpre+season1+season2,data=sam[which(sam[,"STATION"]==level1[i]),],family=binomial(link="probit"))
}

