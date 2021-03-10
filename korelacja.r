y=c(3.60,4.65,5.20,1.86,3.06,1.36,2.46,3.93,5.80,6.35)
z=c(3.56,4.59,5.13,1.84,3.02,1.35,2.43,3.89,5.72,6.26)
sry=mean(y)
srz=mean(z)
gora=0
dol1=0
dol2=0
for(x in seq(1,length(y))){
  a=(y[x]-sry)
  b=(z[x]-srz)
  gora=gora+(a*b)
  dol1=dol1+a^2
  dol2=dol2+b^2
}
R=gora/sqrt(dol1*dol2)
T=R/sqrt(1-R^2)*sqrt(length(y)-2)
#wspó³czynnik korelacji:
print(R)
#istotnoœæ wspó³czynnika:
print(T)