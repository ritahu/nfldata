name1<-function(year){
  file_name<-paste("NFL_",year,".Rdata",sep="")
  file_name
}
name2<-function(year){
  file_name<-paste("NFL_win_",year,".Rdata",sep="")
  file_name
}
for(j in 2002:2015){
  load(name1(j))
  load(name2(j))
  Team.name1<-a$Team
  t1<-gsub("NY","New York",Team.name1)
  team.name<-gsub("Los Angeles","St. Louis Rams",t1)
  nr<-rep(NA,32)
  for(i in 1:32){
    nr[i]<-grep(team.name[i],rg$Team)
  }
  gd<-data.frame(cbind(nr,team.name))
  t3<-gd[order(gd$nr),2][c(1,12,23,27:32,2:11,13:22,24:26)]
  pct<-rep(NA,32)
  for(i in 1:32){
    pct[team.name%in%t3[i]]=rg$Pct[i]
  }
  data.total<-cbind(a,pct,Season=rep(j,32))
  data4model<-rbind(data4model,data.total)
}
NFL_Data<-data4model[-1,]
save(NFL_Data,file="NFL_Data.Rdata")
load("NFL_Data.Rdata")
