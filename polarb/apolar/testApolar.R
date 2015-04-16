source("apolar.R")
dfx <- data.frame(family=rep(c("I", "II", "III", "IV")[sort(sample(1:4, 10, TRUE))], 5), 
                  item=rep(LETTERS[1:10], 5), 
                  score=rep(letters[1:5], each=10), 
                  value=rep((1:5), each=10) + rnorm(50, 0, .5))

p <- apolar(dfx, familyLabel=TRUE, direction="inwards", 
              binSize=0.3, spaceBar=0, spaceItem=0.2, spaceFamily=1.0, innerRadius=0.1, nguides=5)
print(p)

ggplot(dfx)+geom_rect()

gen.df <- function(n=100,b0=c(1,11,20),b1=2) {
  k <- length(b0)
  x <- rnorm(k*n,3)
  y <- x*b1 + diag(k)%x%rep(1,n)%*%b0 + rnorm(n)
  dfx <- data.frame(x,y,z=rep(1:k,each=n))
  dfx
}

dfx <- gen.df(b0=seq(1,100,len=10))

ggplot(dfx,aes(x,y))+geom_point()
ggplot(dfx,aes(x,y))+geom_point()+coord_polar()
ggplot(dfx,aes(y,x))+geom_point()
ggplot(dfx,aes(y,x))+geom_point()+coord_polar()

p <- ggplot(dfx,aes(y,x))+geom_point()+coord_polar(start=0)
p
p+xlim(0,2*pi)
#p <- ggplot(dfx,aes(x))+geom_density()+coord_polar();p # Works
#p <- ggplot(dfx)+geom_rect(aes(xmin=-3,xmax=3,ymin=-3,ymax=3,fill=z))+coord_polar();p #Stupid...
#multiplot(p,p)


