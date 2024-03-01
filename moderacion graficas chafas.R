x <- c(-1.2455,0.0985,1.1307,-1.2455,0.0985,1.1307,-1.2455,0.0985,1.1307)
w <- c(-0.9259,-0.9259,-0.9259,-0.0351,-0.0351,-0.0351,1.0785, 1.0785,1.0785)
y <- c(-0.3305,-0.0317,0.1978,-0.4188,0.053,0.4153,-0.5291,0.1588,0.6872)

plot(y=y,x=w,pch=15)
plot(y=y,x=w,pch=15,col="black", xlab="EDAD", ylab="MEMORIA")
legend.txt<-c("IDEREE LOW","IDEREE HIGH","IDEREE MEDIUM")
legend("topleft",legend=legend.txt,lty=c(1),lwd=c(2), col=c("blue","red","darkgreen"))
lines(w[x== -1.2455],y[x== -1.2455],lwd=2,lty=1,col="blue")
lines(w[x== 1.1307],y[x== 1.1307],lwd=2,lty=1,col="red")
lines(w[x== 0.0985],y[x== 0.0985],lwd=2,lty=1,col="darkgreen")


x <- c(-0.9207,
       0.034,
       0.9521,
       -0.9207,
       0.034,
       0.9521,
       -0.9207,
       0.034,
       0.9521)

w <- c(-0.9657,
       -0.9657,
       -0.9657,
       -0.2827,
       -0.2827,
       -0.2827,
       1.1403,
       1.1403,
       1.1403)

y <- c(0.0438,
       -0.193,
       -0.4207,
       0.0648,
       -0.0574,
       -0.1749,
       0.1085,
       0.2252,
       0.3374)

plot(y=y,x=w,pch=15)
plot(y=y,x=w,pch=15,col="black", xlab="SUPRESION", ylab="MEMORIA")
legend.txt<-c("CRIQ LOW","CRIQ HIGH","CRIQ MEDIUM")
legend("topleft",legend=legend.txt,lty=c(1),lwd=c(2), col=c("blue","red","darkgreen"))
lines(w[x== -0.9207],y[x== -0.9207],lwd=2,lty=1,col="blue")
lines(w[x== 0.9521],y[x== 0.9521],lwd=2,lty=1,col="red")
lines(w[x== 0.034],y[x== 0.034],lwd=2,lty=1,col="darkgreen")




