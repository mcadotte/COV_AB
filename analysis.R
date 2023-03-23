library(picante)

setwd("~/Dropbox/2018WorkingFiles/Marc2018/NutNet/Catford meeting/COV-ab_SEM")
source("COV.AB.SES.R")


##nutnet data - need full cover file (too big to upload to githhub)
nut.dat<-read.csv("~/Dropbox/2018WorkingFiles/Marc2018/NutNet/Catford meeting/COV-ab_SEM/full-cover-2022-11-15.csv")

nut.con<-nut.dat[nut.dat$trt=="Control",] #just control plots
nut.con<-nut.con[nut.con$Family!="NULL",] #removing mosses, litter, etc.

max.year<-tapply(nut.con$year_trt,nut.con$site_code,max)#find max year
year7<-max.year[max.year>6]#data for siteswith more than 6 years
sites7<-names(year7)#sites with more than 6 years


plot.time.mat<-list()
#turn sites with >7 years into yearxspecies matrix in a list
for (i in 1:length(sites7)){
  site.tmp<-nut.con[nut.con$site_code==sites7[i],]
  spp<-unique(site.tmp$Taxon)
  blocks<-unique(site.tmp$block)
  
  tmp.list<-list()
  for (j in 1:length(blocks)){
    site.tmp.block<-site.tmp[site.tmp$block==blocks[j],]
    pc<-site.tmp.block[,c("year_trt","max_cover","Taxon")]
    tmp.list[[j]]<-sample2matrix(pc)
    names(tmp.list)[j]<-paste(sites7[i],blocks[j],sep=".")
  }
  plot.time.mat<-c(plot.time.mat,tmp.list)
}

COVs<-list()
SES.out<-list()
for (i in 1:length(plot.time.mat)){
    tmp<-SES.COV.AB(plot.time.mat[[i]],99)
    SES.out[[i]]<-tmp[[1]]
    COVs[[i]]<-tmp[[2]]
    names(SES.out)[i]<-names(plot.time.mat)[i]
    names(COVs)[i]<-names(plot.time.mat)[i]
}

minY<-min(unlist(lapply(SES.out,function(X) min(X$SES.coef.ab))),na.rm=TRUE)
maxY<-max(unlist(lapply(SES.out,function(X) max(X$SES.coef.ab))),na.rm=TRUE)

minX<-min(unlist(lapply(SES.out,function(X) min(X$SES.cov))),na.rm=TRUE)
maxX<-max(unlist(lapply(SES.out,function(X) max(X$SES.cov))),na.rm=TRUE)


quartz()
plot(0,0,xlim=c(-10,maxX),ylim=c(minY,maxY),type="n",
     xlab="SES.COV",ylab="SES.ab.coef")

lapply(SES.out,function(X) points(X$SES.cov,X$SES.coef.ab,pch=19,col="grey85"))

abline(v=0,lwd=2,lty="dashed",col="grey65")
abline(h=0,lwd=2,lty="dashed",col="grey65")



