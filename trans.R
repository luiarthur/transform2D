# Scaling Matrix (S)
# |scalex  shearx  0|
# |sheary  scaley  0|
# |     0       0  1|

# Rotation Matrix (R)
rotate <- function(m,a=0) {
  R <- matrix(c(cos(a),sin(a),0,
               -sin(a),cos(a),0,
               0,0,1),3,byrow=T)
  m%*%R
}

# Addition Matrix(A)
plus <- function(m,dx=0,dy=0) {
  A <- diag(3)
  A[3,1] <- dx
  A[3,2] <- dy
  m%*%A
}

# Shear (B) 
shear <- function(m,shx=0,shy=0) {
  B <- diag(3)
  B[2,1] <- shx
  B[1,2] <- shy
  m%*%B
}

n <- 30
x <- c(rep(0,n),seq(0,1,len=n),rep(1,n),seq(1,0,len=n))
y <- c(seq(0,1,len=n),rep(1,n),seq(1,0,len=n),rep(0,n))

m <- cbind(matrix(c(x,y),ncol=2,byrow=F),1)
colnames(m) <- c("x","y","1")

# Original
plot(0,type="n",ylim=c(-3,3),xlim=c(-3,3)); abline(v=0,h=0)
#for (a in seq(0,2*pi,len=7))
#points(rotate(plus(m,1,-.5),a)[,1:2],col="red",pch=20,cex=.1)
#points(rotate(plus(m,1,-.5),0)[,1:2],col="red",pch=20,cex=1)

nl <- function(m,ax=-1,y.slope=1) {
  x <- m[,1]
  y <- m[,2]
  ym <- mean(y)
  xm <- mean(x)
  new.y <- y
  new.x <- x
  
  new.y[which(y==max(y))] <- max(y) + (new.x[which(y==max(y))]-min(new.x)) *  y.slope
  new.y[which(y==min(y))] <- min(y) + (new.x[which(y==min(y))]-min(new.x)) * -y.slope

  ax <- -min(x)/max(new.y)^2
  new.x[which(x==min(x))] <- 1.5*min(x) + ax * (y[which(x==min(x))])^2
  #new.x[which(x==max(x))] <- max(y) + (y[which(x==max(x))]^2)*ax
  cbind(new.x,new.y,1)
}
plot(0,type="n",ylim=c(-3,3),xlim=c(-3,3)); abline(v=0,h=0)
points(nl(plus(m,1,-.5),-.3,.2)[1:2],col="red",pch=20,cex=.1)
