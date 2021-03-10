Tablica=c(0.5475, 0.3325, 0.2347, 0.1586, 0.0922, 0.0303)
Dane=c(7.15, 7.17, 7.18, 7.26, 7.33, 7.48, 7.71, 7.88, 7.97, 8.00, 8.03, 8.08)
#kod
Dane=sort(Dane)
Licznik=1
Roznice=c()
n=floor(length(Dane)/2)
while(Licznik!=n+1){
  Roznice=c(Roznice,(Dane[length(Dane)+1-Licznik]-Dane[Licznik]))
  Licznik=Licznik+1
}
Wymnozone=Roznice*Tablica
Wariancja=c()
for(x in Dane){
  Wariancja=c(Wariancja,(x-mean(Dane))^2)
}
Wynik=sum(Wymnozone)^2/sum(Wariancja)
print(Wynik)
qqnorm(Dane);qqline(Dane)