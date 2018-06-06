##Exponetial functions in R

x<-seq(0,1,0.01)
y<-dexp(x,10)
ggplot(data.frame(x,y),aes(x=x,y=y)) + geom_line()

#Attraction
x<-seq(0,1,0.01)
lambda=10
y=exp(-lambda*x)
ggplot(data.frame(x,y),aes(x=x,y=y)) + geom_line()

#Repulsion
x<-seq(0,1,0.01)
lambda=10
y=-exp(-lambda*x)+1
ggplot(data.frame(x,y),aes(x=x,y=y)) + geom_line()
