sim.clt<-function(n){
    x = 0
     for(i in 1:1000){
         k = sample(y,n,rep=F)
         x=k+x
     }
     x = x/1000
     hist(x,prob=T,main=paste("样本容量=",n), xlab="月收入样本均值")
 }