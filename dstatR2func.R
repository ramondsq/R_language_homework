##### 《数据统计分析与R语言编程》
##### 暨南大学 王斌会 2015.5.1  
##### 可用于R3.x版

Stats<-function(x)
{
  if(is.vector(x))
     S=data.frame(n=length(x),min=min(x),max=max(x),
       mean=mean(x),sd=sd(x),median=median(x),IQR=IQR(x))
  else  
     S=data.frame(n=nrow(x),min=apply(x,2,min),max=apply(x,2,max),
       mean=apply(x,2,mean),sd=apply(x,2,sd),
       median=apply(x,2,median),IQR=apply(x,2,IQR))
  S
}

EDA<-function(x){ 
  par(mfrow=c(2,2),mar=c(4,3,2,1),cex=0.8)   # 同时做4个图
  hist(x,main='Histgram',ylab='')            # 直方图 
  dotchart(x,main='Dotchart',ylab='')        # 点图
  boxplot(x,horizontal=T,main='Boxplot')     # 箱式图
  qqnorm(x,xlab='',ylab='');qqline(x)        # 正态概率图
  par(mfrow=c(1,1))                          # 恢复单图
}


Ftab<-function(X){ #自定义计数频数表函数
  f=table(X); 
  S=sum(f);
  P=f/S*100
  T=cbind(' 例数'=f,'构成比(%)'=round(P,2))
  print(rbind(T,'合计'=c(S,100)))
  invisible(f)
}

Freq<-function(X){ #自定义计量频数表函数
  H=hist(X,xlab='',main='')
  f=H$counts
  p=f/sum(f)*100; cp=cumsum(p)
  freq=data.frame(m=H$mids,f=f,p=p,cp=round(cp,2))
  names(freq)=c('组中值','频数','频率(%)','累计频数(%)')
  freq
}

z.conf.plot<-function(conf.level=0.95){
  x=seq(-3,3,0.1)
  curve(dnorm(x),-3,3)                    
  legend(-1.3,0.3,paste("1-a=",conf.level),bty="n")
  a=1-conf.level
  za=qnorm(c(a/2,1-a/2))
  abline(v=c(za),lty=3)
  za
}

z.conf.int <- function(x,sigma,conf.level=0.95){    
  n = length(x)
  a=1-conf.level 
  Za=qnorm(1-a/2)
  se=sigma/sqrt(n)
  c(mean(x)- se*Za, mean(x) + se*Za)
}

t.conf.plot<-function(n,conf.level=0.95){
  x=seq(-4,4,0.1)
  curve(dt(x,n-1),-4,4)
  legend(-1.7,0.2,paste("1-a=",conf.level),bty="n")
  a=1-conf.level
  ta=qt(c(a/2,1-a/2),n-1)
  abline(v=c(ta),lty=3)
  ta
}

t.conf.int <- function(x,conf.level=0.95) {  
  n = length(x)
  xbar = mean(x)
  a=1-conf.level 
  ta=qt(1-a/2,n-1)
  s=sd(x)
  se=s/sqrt(n)
  c(mean(x)- se* ta, mean(x)+se *ta)
}

t.test1<-function(x,mu=0){  
  n=length(x)
  xbar=mean(x)
  s=sd(x)
  se=s/sqrt(n)
  t=(xbar-mu)/se
  df=n-1
  p=2*pt(t,n-1)
  list(t=t,df=df,p=p)  # cat('t=',t,'df=',df,'p-value=',p)
}

P_value<-function(cdf, x, df=numeric(0), side=0){
   k=length(df)
   P=switch(k+1, cdf(x), cdf(x, df), cdf(x, df[1], df[2]),cdf(x, df[1], df[2], df[3]))
   if (side<0)     P
   else if (side>0) 1-P
   else 
   if (P<1/2)   2*P 
     else        2*(1-P)
}

var.test2<-function(x, y, mu=c(Inf, Inf), side=0){
   n1=length(x); n2=length(y)
   if (all(mu<Inf)){
      Sx2=sum((x-mu[1])^2)/n1; Sy2=sum((y-mu[2])^2)/n2
      df1=n1; df2=n2
   }
   else{
      Sx2=var(x); Sy2=var(y); df1=n1-1; df2=n2-1
   }
   F=Sx2/Sy2
   P=P_value(pf, F, df=c(df1, df2), side=side)
   list(S1=Sx2, S2=Sy2,df1=df1, df2=df2, F=F, p_value=P)
}

t.test2<-function(x, y, sigma=c(-1, -1), var.equal=FALSE, side=0){
   n1=length(x); n2=length(y)
   xb=mean(x); yb=mean(y)
   if (all(sigma>=0)){
      z=(xb-yb)/sqrt(sigma[1]^2/n1+sigma[2]^2/n2)
      list(mu=xb-yb, df=n1+n2, Z=z, p_value= P_value(pnorm, z, side=side))
   }
   else{
      if (var.equal ==  TRUE){
         Sw=sqrt(((n1-1)*var(x)+(n2-1)*var(y))/(n1+n2-2))
         t=(xb-yb)/(Sw*sqrt(1/n1+1/n2))
         df=n1+n2-2
      }
      else{
         S1=var(x); S2=var(y)
         df=(S1/n1+S2/n2)^2/(S1^2/n1^2/(n1-1)+S2^2/n2^2/(n2-1))
         t=(xb-yb)/sqrt(S1/n1+S2/n2)
      }
      list(mu=xb-yb, df=df, t=t, p_value= P_value(pt,t,df,side))
   }
}

stock.sim<-function(N=100,File='',Seed=123){
  # N=100; Seed=123
  set.seed(Seed)
  R1=rbinom(N,1,0.3)+1; #sex=table(R1);sex
  R2=round(rnorm(N,40,10),0);R2[R2<18]<- 0.0; R2[R2>70]<- 0.0; 
  #age=table(R2);age
  R3=rbinom(N,2,0.75)+1; #result=table(R3);result
  R4=rbinom(N,3,0.5)+1; #method=table(R4);method
  R5=rbinom(N,1,0.6)+1; #risk=table(R5);risk
  R6=rbinom(N,1,0.85)+1; #post=table(R6);post
  R7=round(runif(N,1,1100),1);R7[sample(1:N,1)]<-0
  #fund=table(as.integer(R7));fund; sum(fund)
  R8=rbinom(N,7,0.5)+1; #job=table(R8);job
  R9=rbinom(N,7,0.7)+1; #edu=table(R9);edu
  RX=data.frame(sex=R1,age=R2,result=R3,method=R4,risk=R5,
                post=R6,fund=R7,job=R8,edu=R9)
  if(File=='') RX
  else write.csv(RX,file=File,row.names=FALSE)
}
