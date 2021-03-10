d1=c(5.6, 6.2, 6.7, 5.8, 5.8)
d2=c(5.8, 4.8, 6.1, 5.4, 5.7)
d3=c(4.2, 4.8, 5.2, 5.1, 4.8, 4.3)

d0=c(d1,d2,d3)
s0=mean(c(d1,d2,d3))
s1=mean(d1)
s2=mean(d2)
s3=mean(d3)
#SST
a=0
for(x in d0){
  a=a+(x-s0)^2
}
print(a)
#SSA
print(length(d1)*(s0-s1)^2+length(d2)*(s0-s2)^2+length(d3)*(s0-s3)^2)
#SSE
a=0
for(x in d1){
  a=a+(x-s1)^2
}
for(x in d2){
  a=a+(x-s2)^2
}
for(x in d3){
  a=a+(x-s3)^2
}
print(a)
