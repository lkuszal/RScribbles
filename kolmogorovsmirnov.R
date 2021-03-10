a=c(27, 11, 16, 10, 17, 10, 30, 9, 29 )
b=c(14, 21, 7, 19, 30, 29, 11)
a=sort(a)
b=sort(b)
d=sort(c(unique(c(a,b))))
a=as.character(a)
y=c()
z=c()
for(x in d){
  y=c(y,length(which(a==x)))
  z=c(z,length(which(x==b)))
}
ysum=c()
zsum=c()
n=1
for(x in y){
  ysum=c(ysum,sum(y[1:n]))
  n=n+1
}
n=1
for(x in z){
  zsum=c(zsum,sum(z[1:n]))
  n=n+1
}
ysum1=ysum*(1/length(a))
zsum1=zsum*(1/length(b))
wynik=abs(ysum1-zsum1)
data.frame(d,y,z,ysum,zsum,ysum1,zsum1,wynik)
max(wynik)