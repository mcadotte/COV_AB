library(picante)

####covariance-abundance SEMs

#time (rows) by species (cols) matrix

#function calculating SES covariance and SES aboundance trend (lm coef), 
#X is time by species matrix
SES.COV.AB<-function(X,iter=999){
  #observed average covariances   
   cov.ob<-cov(X)
  var<-diag(cov.ob)
  diag(cov.ob)<-0
  ave0<-function(x) return(mean(x[x!=0],na.rm=TRUE))
  ave.cov<-apply(cov.ob,1,ave0)
  
  #observed abundance trend
  
  coef.lm<-function(X){
    coefs<-NULL
    Y<-1:nrow(X)
    for (i in 1:ncol(X)){
      tmp.lm<-lm(X[,i]~Y)
      coefs[i]<-tmp.lm$coefficients[2]
      
    }
    names(coefs)<-names(X)
    return(coefs)
  }
  
  coef.obs<-coef.lm(X)
 
  
  #randomizer for both nullcov and nullab
  null.cov<-matrix(0,nrow=iter,ncol=length(ave.cov))
  null.coef<-matrix(0,nrow=iter,ncol=length(ave.cov))
 
   for (i in 1:iter){
    X.null<-randomizeMatrix(X)
    tmp.null<-cov(X.null)
    diag(tmp.null)<0
    null.cov[i,]<-apply(tmp.null,1,ave0)
    
    null.coef[i,]<-coef.lm(X.null)
    
  }
  colnames(null.cov)<-names(X)
  colnames(null.coef)<-names(X)
  
  null.cov.ave<-apply(null.cov,2,mean)
  null.cov.sd<-apply(null.cov,2,sd)
  
  null.coef.ave<-apply(null.coef,2,mean)
  null.coef.sd<-apply(null.coef,2,sd)
  
  return(list(SES.analysis=data.frame(ab.coef.obs=coef.obs,
                    SES.coef.ab=((coef.obs-null.coef.ave)/null.coef.sd),
                    ave.cov,
                    SES.cov=((ave.cov-null.cov.ave)/null.cov.sd),
                    var),
              Observed.covariances=cov.ob))
}



