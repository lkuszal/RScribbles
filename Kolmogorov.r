Dane=c(0.18,0.56,0.87,1.37,2.46)
dystr0=c()
dystr1=c()
dystr2=c()
a=1/length(Dane)
teor1=a
teor2=0
for(x in Dane){
  dystr0=c(dystr0,pexp(x,1))
  dystr1=c(dystr1,teor1)
  dystr2=c(dystr2,teor2)
  teor1=teor1+a
  teor2=teor2+a
}
max(abs(c(dystr1-dystr0,dystr2-dystr0)))