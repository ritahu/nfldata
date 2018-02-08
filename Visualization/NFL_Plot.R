setwd("/Users/user/Desktop/Classes/STAT 425/Projects/")
library(ggplot2)
load("NFL_Data.Rdata")

##Plot 1
var.plot<-function(Team.name,var){
  variable<-names(NFL_Data)
  index1<-which(variable%in%"Team")
  index2<-which(NFL_Data[,index1]%in%Team.name)
  index3<-which(variable%in%var)
  range<-range(as.numeric(as.character(NFL_Data[,index3])))
  var.pl<-as.numeric(as.character(NFL_Data[index2,index3]))
  plot(2002:2015,var.pl, pch = 20, cex = 0.4,xlab="Season",ylim=c(range[1],range[2]),ylab=var,main=paste(var,"of",Team.name,sep=" "))
  points(2002:2015, var.pl, pch = 20, col="firebrick3",cex = 1.2)
  points(2002:2015, var.pl, col = "lightsteelblue4", pch = 19, type = "l", lwd = 2.6)
}
##eg.
##var.plot("Denver","pct")
##var.plot("Buffalo","Rec")

##Plot 2
comp.var.plot<-function(Team.name1,Team.name2,var){
  variable<-names(NFL_Data)
  index1<-which(variable%in%"Team")
  index21<-which(NFL_Data[,index1]%in%Team.name1)
  index22<-which(NFL_Data[,index1]%in%Team.name2)
  index3<-which(variable%in%var)
  range<-range(as.numeric(as.character(NFL_Data[,index3])))
  var.pl1<-as.numeric(as.character(NFL_Data[index21,index3]))
  var.pl2<-as.numeric(as.character(NFL_Data[index22,index3]))
  plot(rep(2002:2015,2),c(var.pl1,var.pl2), pch = 20, cex = 0.4,xlab="Season",ylim=c(range[1],range[2]),ylab=var,main=paste(var,"of",Team.name1,"and",Team.name2,sep=" "))
  points(rep(2002:2015,2), c(var.pl1,var.pl2), pch = 20, col="aquamarine4",cex = 1)
  points(2002:2015, var.pl1, col = "lightsteelblue4", pch = 19, type = "l", lwd = 2.3)
  points(2002:2015, var.pl2, col = "firebrick3", pch = 19, type = "l", lwd = 2.3)
  legend("topleft",lty=c(1,1),lwd=2, col=c("lightsteelblue4","firebrick3"), legend=c(Team.name1,Team.name2),cex=1.2,bty="n")
}
##eg.
##comp.var.plot("Buffalo","Denver","pct")
##comp.var.plot("Seattle","Denver","Pass.FD")

##Plot 3
NFL_barplot<-function(Team.name,var,season){
  x=NFL_Data[NFL_Data$Season==season,][,"Team"]
  y=as.numeric(as.character(NFL_Data[NFL_Data$Season==season,][,var]))
  data4barplot<-data.frame(x=x,y=y)
  data4barplot<-data4barplot[order(data4barplot$y,decreasing=F),]
  index<-which(data4barplot[,1]%in%Team.name)
  x=data4barplot$x
  y=data4barplot$y
  if (index==1){
    y1<-c(0,y[2:32])
    y2<-c(y[1],rep(0,31))
  }  else 
  {
    y1<-c(y[1:(index-1)],0,y[-(1:index)])
    y2<-c(rep(0,index-1),y[index],rep(0,32-index))
  }
  d4bp<-data.frame(x=x,y1=y1,y2=y2,y=y)
  ggplot(d4bp,aes(x = x)) +
    geom_bar(aes(y = y1), stat = "identity",fill="dodgerblue4") + 
    geom_bar(aes(y = y2), stat = "identity",fill = "firebrick3") +
    scale_x_discrete(limits=d4bp$x) +
    coord_flip() +
    labs(x = "Team", y = var, title = paste(var, " among all team (Season ", season,")", sep="")) +
    theme(panel.grid =element_blank()) +
    theme_bw()
}
##eg.
##NFL_barplot("Kansas City","Rush.FD", 2006)

##Plot 4
NFL_BubbleChart<-function(yr,var1,var2){
  PCT=as.numeric(as.character(NFL_Data$pct))
  NFL_Data1<-data.frame(NFL_Data,PCT=PCT)
  ind=which(NFL_Data$Season==yr)
  p=ggplot(aes(x=NFL_Data1[ind,var1],y=NFL_Data1[ind,var2],size=PCT,colour=Team),pch=16,data=NFL_Data1[ind,])
  p+geom_point(show.legend = FALSE)+
    scale_size_continuous(range=c(0,12)) +
    xlim(range(NFL_Data1[ind,var1])[1]*0.9,range(NFL_Data1[ind,var1])[2]*1.1)+ylim(range(NFL_Data1[ind,var2])[1]*0.9,range(NFL_Data1[ind,var2])[2]*1.1)+
    annotate("text", x=range(NFL_Data1[ind,var1])[2]*1.015, y=range(NFL_Data1[ind,var2])[1]*1.06, label = yr,size=15,color="grey")+
    labs(x = var1, y = var2) +
    theme(panel.grid =element_blank()) +
    theme_bw()
}
##eg.
##NFL_BubbleChart(2014,"Pass.TD","T.YD")

## Dynamic Bubble Chart
## Just For FUN!
drawit<-function(yr){
  PCT=as.numeric(as.character(NFL_Data$pct))
  NFL_Data1<-data.frame(NFL_Data,PCT=PCT)
  ind=which(NFL_Data$Season==yr)
  p=ggplot(aes(x=T.YD,y=Rec,size=PCT,colour=Team),pch=16,data=NFL_Data1[ind,])
  p+geom_point(show.legend = FALSE)+
    scale_size_continuous(range=c(0,12)) +
    xlim(range(NFL_Data1[ind,"T.YD"])[1]*0.9,range(NFL_Data1[ind,"T.YD"])[2]*1.1)+ylim(range(NFL_Data1[ind,"Rec"])[1]*0.9,range(NFL_Data1[ind,"Rec"])[2]*1.1)+
    annotate("text", x=range(NFL_Data1[ind,"T.YD"])[2]*1.015, y=range(NFL_Data1[ind,"Rec"])[1]*1.06, label = yr,size=20,color="grey")+
    labs(x = "T.YD", y = "Rec") +
    theme(panel.grid =element_blank()) +
    theme_bw()
}
finaldraw=function(a,b)
{
  for (i in 1:100)
    print(drawit(a))
  for (i in a:b)
    print(drawit(i))
  for (i in 1:100)
    print(drawit(b))
}
library(animation)
oopts = ani.options(ffmpeg = "/Users/user/Downloads/ffmpeg")
saveVideo({
  finaldraw(2002,2015)
  ani.options(interval = 0.1, nmax = 50)
}, video.name = "animation.mp4", other.opts = "-b 500k")
