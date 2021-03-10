#kendall tau test
x=c(1,3,2,8,5,4,1)
y=c(12,5,2,6,1,8,11)
cor.test(x,y,method="kendall")
Kandell=function(x,y){
  assertthat::are_equal(length(x),length(y))
  n=length(x)
  con=0
  dis=0
  for(i in seq(1,n-1)){
    for(j in seq(i+1,n)){
      if (!((x[i]==x[j]) || (y[i]==y[j]))){
        if((x[i]>x[j])==(y[i]>y[j])){
          con=con+1
        }
        else{
          dis=dis+1
        }
      }
    }
  }
  Tau=(con-dis)/(n*(n-1)/2)
  Z=3*(con-dis)/sqrt(n*(n-1)*(2*n+5)/2)
  for(q in seq(1,10)){
    if (Z>pnorm(0.05/q)){
      return(c(0.05/(q-1),Z,Tau))
    }
  }
  return(c(0.05/(q-1),Z,Tau))
}

Kandell(x,y)