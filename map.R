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