# Last Updated: Jan 2018

install.packages("rvest")
library(rvest)

nfl_pct<-function(year){
  a<-c("East","South","West","North")
  b<-c("NFC")
  c<-c("AFC")
  d<-paste(b,"_",a,sep="")
  url1<-paste("https://en.wikipedia.org/wiki/Template:",year,"_",d,"_standings",sep="")
  d<-paste(c,"_",a,sep="")
  url2<-paste("https://en.wikipedia.org/wiki/Template:",year,"_",d,"_standings",sep="")
  l<-c(url1,url2)
  table.total=list(NULL)
  length(table.total)=8
  for(i in 1:8){
    url<-l[i]
    table <- url %>%
      read_html() %>%
      html_nodes(xpath='//*[@id="mw-content-text"]/table') %>%
      html_table()
    table.total[[i]]<-table[[1]][2:5,]
  }
  rg<-table.total[[1]][,c(1,5)]
  colnames(rg)<-c("Team","Pct")
  for(i in 2:8){
    re<-table.total[[i]][,c(1,5)]
    colnames(re)<-c("Team","Pct")
    rg<-rbind(rg,re)
  }
  file_name<-paste("NFL_win_",year,".Rdata",sep="")
  save(rg,file=file_name)
}

nfl_data<-function(year){
  glossary<-c("total","downs","passing","rushing","receiving","returning")
  link<-paste("http://www.espn.com/nfl/statistics/team/_/stat/",glossary,"/year/",year,sep="")
  table.glossary=list(NULL)
  length(table.glossary)=6
  for(i in 1:6){
    url<-link[i]
    table <- url %>%
    read_html() %>%
    html_nodes(xpath='//*[@id="my-teams-table"]/div/div[2]/table') %>%
    html_table(fill = TRUE,header=T)
    table.glossary[[i]]<-table[[1]][,-1]
  }
  Variable<-c("Team","T.YD","Pass.YD","Rush.YD","T.FD","Rush.FD","Pass.FD","Pen.FD","Pass.ATT","Pass.COMP","Pass.PCT","Pass.YPA","Pass.Lng","Pass.TD","Pass.INT","PASS.SACK","Rush.ATT","Rush.YPA","Rush.Lng","Rush.TD","Rush.FUM","Rec","Rec.YD","Rec.AVG","Rec.Lng","Rec.TD","Rec.FUM")
  TEAM<-table.glossary[[1]]$TEAM
  Team_stat<-function(x){
    Team=x
    for(i in 1:6){
      Team<-cbind(Team,table.glossary[[i]][which(table.glossary[[i]][,1]==x),-1])
    }
    sel<-Team[,c(1,2,4,6,10:13,23,24,25,27,28:31,35,37:39,41,43,44,45,46,47,49)]
    colnames(sel)<-Variable
    sel
  }
  data<-apply(matrix(TEAM,ncol=1),1,Team_stat)
  a<-data[[1]]
  for(i in 2:32){
    a<-rbind(a,data[[i]])
  }
  file_name<-paste("NFL_",year,".Rdata",sep="")
  save(a,file=file_name)
}




