library(tidyr)
library(ggplot2)
library(MLmetrics)
inp=read.csv("C:/Users/Latul/Downloads/owid-covid-data.csv")
dane=inp[inp$location=="Japan",]
dane$train=ifelse(as.Date(dane$date)<max(as.Date(dane$date))-30,TRUE,FALSE)
dane$new_cases=replace_na(dane$new_cases, 0)
#faza 1: 01-22 - 03-22;0.18,16.3 SIR 126475192 835 431  MAPE 0.505
#faza 2: 03-22 - 05-22;0.52,1.85 SIR 126460025 235 16198   MAPE 0.317
#faza 3: 05-22 - 07-22;0.2,6.1 SIR 126450027 2057 24395 MAPE 0.045
#faza 4: 07-22 - 09-22; 0.55 1.79 SIR 126395895 1182 79402 MAPE 0.039
#faza 5: 09-22 - 11-22; 0.28,3.965 SIR 126344835 6212 125432 MAPE 0.012
SIR2=function(b,g){
  populacja=126476458
  start= as.Date("2020-09-22")
  ending = as.Date("2021-01-16")
  S=126396420
  I=1170
  R=78869
  beta=b
  gamma=g
  dane$S[dane$date==start]=S
  dane$I[dane$date==start]=I
  dane$R[dane$date==start]=R
  currentday=start
  for(x in seq(ending-start)){
    SN=(-1*beta*dane$S[dane$date==currentday]*dane$I[dane$date==currentday])/populacja
    IN=beta*dane$S[dane$date==currentday]*dane$I[dane$date==currentday]/populacja - (1/gamma)*dane$I[dane$date==currentday]
    RN=1/gamma*dane$I[dane$date==currentday]
    currentday=currentday+1
    dane$S[dane$date==currentday]=SN+dane$S[dane$date==currentday-1]
    dane$I[dane$date==currentday]=IN+dane$I[dane$date==currentday-1]
    dane$R[dane$date==currentday]=RN+dane$R[dane$date==currentday-1]
  }
  print("SIR")
  print(dane$S[dane$date==currentday])
  print(dane$I[dane$date==currentday])
  print(dane$R[dane$date==currentday])
  print("OCENA")
  print(MAPE(populacja-dane$S[dane$date<ending & dane$date>start],dane$total_cases[dane$date<ending & dane$date>start]))
  print(RAE(populacja-dane$S[dane$date<ending & dane$date>start],dane$total_cases[dane$date<ending & dane$date>start]))
  print(RMSE(populacja-dane$S[dane$date<ending & dane$date>start],dane$total_cases[dane$date<ending & dane$date>start]))
  #ggplot(dane[dane$date<ending & dane$date>start,],aes(x=as.Date(date),group=1))+
  #  geom_line(aes(y=populacja-S,color="red"))+
  #  geom_line(aes(y=total_cases))+
  #  scale_y_continuous(name="Suma zakazen")+
  #  scale_x_date(name ="Data")+
  #  theme(legend.position = "none")
  ggplot(dane[dane$date<ending & dane$date>start,],aes(x=as.Date(date),group=1))+
    #geom_line(aes(y=S/populacja,color="blue"))+
    geom_line(aes(y=I/populacja,color="red"))+
    geom_line(aes(y=R/populacja,color="green"))+
    scale_y_continuous(name="SI")+
    scale_x_date(name ="Data")+
    theme(legend.position = "none")
}



SIR3=function(beta,gamma,mi=1519/79773){
  populacja=126476458
  start= as.Date("2020-09-22")
  ending = as.Date("2020-11-22")
  S=126396420
  I=1170
  R=78869-1519
  D=1519
  dane$S[dane$date==start]=S
  dane$I[dane$date==start]=I
  dane$R[dane$date==start]=R
  dane$D[dane$date==start]=D
  currentday=start
  for(x in seq(ending-start)){
    SN=(-beta*dane$S[dane$date==currentday]*dane$I[dane$date==currentday])/populacja
    IN=beta*dane$S[dane$date==currentday]*dane$I[dane$date==currentday]/populacja - gamma*dane$I[dane$date==currentday]-mi*dane$I[dane$date==currentday]
    RN=gamma*dane$I[dane$date==currentday]
    DN=mi*dane$I[dane$date==currentday]
    currentday=currentday+1
    dane$S[dane$date==currentday]=SN+dane$S[dane$date==currentday-1]
    dane$I[dane$date==currentday]=IN+dane$I[dane$date==currentday-1]
    dane$R[dane$date==currentday]=RN+dane$R[dane$date==currentday-1]
    dane$D[dane$date==currentday]=DN+dane$D[dane$date==currentday-1]
  }
  print("SIRD")
  print(dane$S[dane$date==currentday])
  print(dane$I[dane$date==currentday])
  print(dane$R[dane$date==currentday])
  print(dane$D[dane$date==currentday])
  print("OCENA")
  print(MAPE(populacja-dane$S[dane$date<ending & dane$date>start],dane$total_cases[dane$date<ending & dane$date>start]))
  print(RAE(populacja-dane$S[dane$date<ending & dane$date>start],dane$total_cases[dane$date<ending & dane$date>start]))
  print(RMSE(populacja-dane$S[dane$date<ending & dane$date>start],dane$total_cases[dane$date<ending & dane$date>start]))
  ggplot(dane[dane$date<ending & dane$date>start,],aes(x=as.Date(date),group=1))+
    geom_line(aes(y=populacja-S,color="red"))+
    geom_line(aes(y=total_cases))+
    scale_y_continuous(name="Suma zakazen")+
    scale_x_date(name ="Data")+
    theme(legend.position = "none")
  #ggplot(dane[dane$date<ending & dane$date>start,],aes(x=as.Date(date),group=1))+
    #geom_line(aes(y=S/populacja,color="blue"))+
    #geom_line(aes(y=I/populacja,color="red"))+
    #geom_line(aes(y=R/populacja,color="green"))+
    #geom_line(aes(y=D/populacja,color="black"))+
    #scale_y_continuous(name="SI")+
    #scale_x_date(name ="Data")+
    #theme(legend.position = "none")
}