# Biblioteki
library(ggplot2)
library(tidyverse)
library(zoo)
# Zapis naszych danych
tab<-read.csv(file="gus.csv",sep=";",dec=",",encoding = "UTF-8")
# Przegląd danych
View(tab)
names(tab)
names(tab)[1:8]
# Zmiany nazwy kolumn 
names(tab)[3:16]<-paste0("Bułka_Pszenna",c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))
names(tab)[17:30]<-paste0("Mięso_Wieprzowe_Bez_Kości",c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))
names(tab)[31:44]<-paste0("Kiełbasa_Wędzona",c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))
names(tab)[45:58]<-paste0("Filety_Z_Morszczuka_Mrożone",c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))
names(tab)[59:72]<-paste0("Karp_Świeży",c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))
names(tab)[73:86]<-paste0("Podkoszulek_Męski_Bawełniany_Bez_Rękawa",c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))
names(tab)[87:100]<-paste0("Rajstopy_Damskie_Gładkie_15Den",c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))
names(tab)[101:114]<-paste0("Spodnie_Jeans",c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))
names(tab)[115:128]<-paste0("Czyszczenie_Chemiczne_Garnituru_Męskiego",c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))
names(tab)[129:142]<-paste0("Olej_Napędowy",c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))
# Podgląd
View(tab)
# Przypisanie produktów do województw oraz przypisanie lat do całości
woj<-rep(c("Polska","dolnoslaskie","kujawsko-pomorskie","lubelskie","lubuskie","lodzkie","malopolskie",
           "mazowieckie","opolskie","podkarpackie","podlaskie","pomorskie","slaskie","swietokrzyskie","warminsko-mazurskie","wielkopolskie","zachodniopomorskie"),12*140)
mies<-rep(c("sty","lut","mar","kwi","maj","cze","lip","sie","wrz","paz","lis","gru"),each=168)
Bułka_Pszenna<-c(tab[,3],tab[,4],tab[,5],tab[,6],tab[,7],tab[,8],tab[,9],tab[,10],tab[,11],tab[,12],tab[,13],tab[,14],tab[,15],tab[,16])
Mięso_Wieprzowe_Bez_Kości<-c(tab[,17],tab[,18],tab[,19],tab[,20],tab[,21],tab[,22],tab[,23],tab[,24],tab[,25],tab[,26],tab[,27],tab[,28],tab[,29],tab[,30])
Kiełbasa_Wędzona<-c(tab[,31],tab[,32],tab[,33],tab[,34],tab[,35],tab[,36],tab[,37],tab[,38],tab[,39],tab[,40],tab[,41],tab[,42],tab[,43],tab[,44])
Filety_Z_Morszczuka_Mrożone<-c(tab[,45],tab[,46],tab[,47],tab[,48],tab[,49],tab[,50],tab[,51],tab[,52],tab[,53],tab[,54],tab[,55],tab[,56],tab[,57],tab[,58])
Karp_Świeży<-c(tab[,59],tab[,60],tab[,61],tab[,62],tab[,63],tab[,64],tab[,65],tab[,66],tab[,67],tab[,68],tab[,69],tab[,70],tab[,71],tab[,72])
Podkoszulek_Męski_Bawełniany_Bez_Rękawa<-c(tab[,73],tab[,74],tab[,75],tab[,76],tab[,77],tab[,78],tab[,79],tab[,80],tab[,81],tab[,82],tab[,83],tab[,84],tab[,85],tab[,86])
Rajstopy_Damskie_Gładkie_15Den<-c(tab[,87],tab[,88],tab[,89],tab[,90],tab[,91],tab[,92],tab[,93],tab[,94],tab[,95],tab[,96],tab[,97],tab[,98],tab[,99],tab[,100])
Spodnie_Jeans<-c(tab[,101],tab[,102],tab[,103],tab[,104],tab[,105],tab[,106],tab[,107],tab[,108],tab[,109],tab[,110],tab[,111],tab[,112],tab[,113],tab[,114])
Czyszczenie_Chemiczne_Garnituru_Męskiego<-c(tab[,115],tab[,116],tab[,117],tab[,118],tab[,119],tab[,120],tab[,121],tab[,122],tab[,123],tab[,124],tab[,125],tab[,126],tab[,127],tab[,128])
Olej_Napędowy<-c(tab[,129],tab[,130],tab[,131],tab[,132],tab[,133],tab[,134],tab[,135],tab[,136],tab[,137],tab[,138],tab[,139],tab[,140],tab[,141],tab[,142])
###
tab[,2]
tab[,2]<-as.character(tab[,2])
woj<-rep(tab[,2],14)
woj1<-rep(woj,12)
length(woj1)
rok<-rep(c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"),each=17)
rok1<-rep(rok,12)
length(rok1)
mies<-rep(c("sty","lut","mar","kwi","maj","cze","lip","sie","wrz","paź","lis","gru"),each=238)
mon<-factor(mies, levels=c("sty","lut","mar","kwi","maj","cze","lip","sie","wrz","paź","lis","gru"), ordered = TRUE)
# Przypisanie danych z tabeli do odpowiednich kolumn
j=0
Bułka_Pszenna<-c(tab[,j+3],tab[,j+4],tab[,j+5],tab[,j+6],tab[,j+7],tab[,j+8],tab[,j+9],tab[,j+10],tab[,j+11],tab[,j+12],tab[,j+13],tab[,j+14],tab[,j+15],tab[,j+16])
Mięso_Wieprzowe_Bez_Kości<-c(tab[,j+17],tab[,j+18],tab[,j+19],tab[,j+20],tab[,j+21],tab[,j+22],tab[,j+23],tab[,j+24],tab[,j+25],tab[,j+26],tab[,j+27],tab[,j+28],tab[,j+29],tab[,j+30])
Kiełbasa_Wędzona<-c(tab[,j+31],tab[,j+32],tab[,j+33],tab[,j+34],tab[,j+35],tab[,j+36],tab[,j+37],tab[,j+38],tab[,j+39],tab[,j+40],tab[,j+41],tab[,j+42],tab[,j+43],tab[,j+44])
Filety_Z_Morszczuka_Mrożone<-c(tab[,j+45],tab[,j+46],tab[,j+47],tab[,j+48],tab[,j+49],tab[,j+50],tab[,j+51],tab[,j+52],tab[,j+53],tab[,j+54],tab[,j+55],tab[,j+56],tab[,j+57],tab[,j+58])
Karp_Świeży<-c(tab[,j+59],tab[,j+60],tab[,j+61],tab[,j+62],tab[,j+63],tab[,j+64],tab[,j+65],tab[,j+66],tab[,j+67],tab[,j+68],tab[,j+69],tab[,j+70],tab[,j+71],tab[,j+72])
Podkoszulek_Męski_Bawełniany_Bez_Rękawa<-c(tab[,j+73],tab[,j+74],tab[,j+75],tab[,j+76],tab[,j+77],tab[,j+78],tab[,j+79],tab[,j+80],tab[,j+81],tab[,j+82],tab[,j+83],tab[,j+84],tab[,j+85],tab[,j+86])
Rajstopy_Damskie_Gładkie_15Den<-c(tab[,j+87],tab[,j+88],tab[,j+89],tab[,j+90],tab[,j+91],tab[,j+92],tab[,j+93],tab[,j+94],tab[,j+95],tab[,j+96],tab[,j+97],tab[,j+98],tab[,j+99],tab[,j+100])
Spodnie_Jeans<-c(tab[,j+101],tab[,j+102],tab[,j+103],tab[,j+104],tab[,j+105],tab[,j+106],tab[,j+107],tab[,j+108],tab[,j+109],tab[,j+110],tab[,j+111],tab[,j+112],tab[,j+113],tab[,j+114])
Czyszczenie_Chemiczne_Garnituru_Męskiego<-c(tab[,j+115],tab[,j+116],tab[,j+117],tab[,j+118],tab[,j+119],tab[,j+120],tab[,j+121],tab[,j+122],tab[,j+123],tab[,j+124],tab[,j+125],tab[,j+126],tab[,j+127],tab[,j+128])
Olej_Napędowy<-c(tab[,j+129],tab[,j+130],tab[,j+131],tab[,j+132],tab[,j+133],tab[,j+134],tab[,j+135],tab[,j+136],tab[,j+137],tab[,j+138],tab[,j+139],tab[,j+140],tab[,j+141],tab[,j+142])

j=140
Bułka_Pszenna<-c(Bułka_Pszenna,tab[,j+3],tab[,j+4],tab[,j+5],tab[,j+6],tab[,j+7],tab[,j+8],tab[,j+9],tab[,j+10],tab[,j+11],tab[,j+12],tab[,j+13],tab[,j+14],tab[,j+15],tab[,j+16])
Mięso_Wieprzowe_Bez_Kości<-c(Mięso_Wieprzowe_Bez_Kości,tab[,j+17],tab[,j+18],tab[,j+19],tab[,j+20],tab[,j+21],tab[,j+22],tab[,j+23],tab[,j+24],tab[,j+25],tab[,j+26],tab[,j+27],tab[,j+28],tab[,j+29],tab[,j+30])
Kiełbasa_Wędzona<-c(Kiełbasa_Wędzona,tab[,j+31],tab[,j+32],tab[,j+33],tab[,j+34],tab[,j+35],tab[,j+36],tab[,j+37],tab[,j+38],tab[,j+39],tab[,j+40],tab[,j+41],tab[,j+42],tab[,j+43],tab[,j+44])
Filety_Z_Morszczuka_Mrożone<-c(Filety_Z_Morszczuka_Mrożone,tab[,j+45],tab[,j+46],tab[,j+47],tab[,j+48],tab[,j+49],tab[,j+50],tab[,j+51],tab[,j+52],tab[,j+53],tab[,j+54],tab[,j+55],tab[,j+56],tab[,j+57],tab[,j+58])
Karp_Świeży<-c(Karp_Świeży,tab[,j+59],tab[,j+60],tab[,j+61],tab[,j+62],tab[,j+63],tab[,j+64],tab[,j+65],tab[,j+66],tab[,j+67],tab[,j+68],tab[,j+69],tab[,j+70],tab[,j+71],tab[,j+72])
Podkoszulek_Męski_Bawełniany_Bez_Rękawa<-c(Podkoszulek_Męski_Bawełniany_Bez_Rękawa,tab[,j+73],tab[,j+74],tab[,j+75],tab[,j+76],tab[,j+77],tab[,j+78],tab[,j+79],tab[,j+80],tab[,j+81],tab[,j+82],tab[,j+83],tab[,j+84],tab[,j+85],tab[,j+86])
Rajstopy_Damskie_Gładkie_15Den<-c(Rajstopy_Damskie_Gładkie_15Den,tab[,j+87],tab[,j+88],tab[,j+89],tab[,j+90],tab[,j+91],tab[,j+92],tab[,j+93],tab[,j+94],tab[,j+95],tab[,j+96],tab[,j+97],tab[,j+98],tab[,j+99],tab[,j+100])
Spodnie_Jeans<-c(Spodnie_Jeans,tab[,j+101],tab[,j+102],tab[,j+103],tab[,j+104],tab[,j+105],tab[,j+106],tab[,j+107],tab[,j+108],tab[,j+109],tab[,j+110],tab[,j+111],tab[,j+112],tab[,j+113],tab[,j+114])
Czyszczenie_Chemiczne_Garnituru_Męskiego<-c(Czyszczenie_Chemiczne_Garnituru_Męskiego,tab[,j+115],tab[,j+116],tab[,j+117],tab[,j+118],tab[,j+119],tab[,j+120],tab[,j+121],tab[,j+122],tab[,j+123],tab[,j+124],tab[,j+125],tab[,j+126],tab[,j+127],tab[,j+128])
Olej_Napędowy<-c(Olej_Napędowy,tab[,j+129],tab[,j+130],tab[,j+131],tab[,j+132],tab[,j+133],tab[,j+134],tab[,j+135],tab[,j+136],tab[,j+137],tab[,j+138],tab[,j+139],tab[,j+140],tab[,j+141],tab[,j+142])


j=11*140  # od 2 do 11
Bułka_Pszenna<-c(Bułka_Pszenna,tab[,j+3],tab[,j+4],tab[,j+5],tab[,j+6],tab[,j+7],tab[,j+8],tab[,j+9],tab[,j+10],tab[,j+11],tab[,j+12],tab[,j+13],tab[,j+14],tab[,j+15],tab[,j+16])
Mięso_Wieprzowe_Bez_Kości<-c(Mięso_Wieprzowe_Bez_Kości,tab[,j+17],tab[,j+18],tab[,j+19],tab[,j+20],tab[,j+21],tab[,j+22],tab[,j+23],tab[,j+24],tab[,j+25],tab[,j+26],tab[,j+27],tab[,j+28],tab[,j+29],tab[,j+30])
Kiełbasa_Wędzona<-c(Kiełbasa_Wędzona,tab[,j+31],tab[,j+32],tab[,j+33],tab[,j+34],tab[,j+35],tab[,j+36],tab[,j+37],tab[,j+38],tab[,j+39],tab[,j+40],tab[,j+41],tab[,j+42],tab[,j+43],tab[,j+44])
Filety_Z_Morszczuka_Mrożone<-c(Filety_Z_Morszczuka_Mrożone,tab[,j+45],tab[,j+46],tab[,j+47],tab[,j+48],tab[,j+49],tab[,j+50],tab[,j+51],tab[,j+52],tab[,j+53],tab[,j+54],tab[,j+55],tab[,j+56],tab[,j+57],tab[,j+58])
Karp_Świeży<-c(Karp_Świeży,tab[,j+59],tab[,j+60],tab[,j+61],tab[,j+62],tab[,j+63],tab[,j+64],tab[,j+65],tab[,j+66],tab[,j+67],tab[,j+68],tab[,j+69],tab[,j+70],tab[,j+71],tab[,j+72])
Podkoszulek_Męski_Bawełniany_Bez_Rękawa<-c(Podkoszulek_Męski_Bawełniany_Bez_Rękawa,tab[,j+73],tab[,j+74],tab[,j+75],tab[,j+76],tab[,j+77],tab[,j+78],tab[,j+79],tab[,j+80],tab[,j+81],tab[,j+82],tab[,j+83],tab[,j+84],tab[,j+85],tab[,j+86])
Rajstopy_Damskie_Gładkie_15Den<-c(Rajstopy_Damskie_Gładkie_15Den,tab[,j+87],tab[,j+88],tab[,j+89],tab[,j+90],tab[,j+91],tab[,j+92],tab[,j+93],tab[,j+94],tab[,j+95],tab[,j+96],tab[,j+97],tab[,j+98],tab[,j+99],tab[,j+100])
Spodnie_Jeans<-c(Spodnie_Jeans,tab[,j+101],tab[,j+102],tab[,j+103],tab[,j+104],tab[,j+105],tab[,j+106],tab[,j+107],tab[,j+108],tab[,j+109],tab[,j+110],tab[,j+111],tab[,j+112],tab[,j+113],tab[,j+114])
Czyszczenie_Chemiczne_Garnituru_Męskiego<-c(Czyszczenie_Chemiczne_Garnituru_Męskiego,tab[,j+115],tab[,j+116],tab[,j+117],tab[,j+118],tab[,j+119],tab[,j+120],tab[,j+121],tab[,j+122],tab[,j+123],tab[,j+124],tab[,j+125],tab[,j+126],tab[,j+127],tab[,j+128])
Olej_Napędowy<-c(Olej_Napędowy,tab[,j+129],tab[,j+130],tab[,j+131],tab[,j+132],tab[,j+133],tab[,j+134],tab[,j+135],tab[,j+136],tab[,j+137],tab[,j+138],tab[,j+139],tab[,j+140],tab[,j+141],tab[,j+142])
# Utworzenie ramki danych z danymi
dane<-data.frame(woj=woj1, rok=rok1, mies=mies, mon=mon, Bułka_Pszenna=Bułka_Pszenna, Mięso_Wieprzowe_Bez_Kości=Mięso_Wieprzowe_Bez_Kości,
                 Kiełbasa_Wędzona=Kiełbasa_Wędzona, Filety_Z_Morszczuka_Mrożone=Filety_Z_Morszczuka_Mrożone, Karp_Świeży=Karp_Świeży, Podkoszulek_Męski_Bawełniany_Bez_Rękawa=Podkoszulek_Męski_Bawełniany_Bez_Rękawa,
                 Rajstopy_Damskie_Gładkie_15Den=Rajstopy_Damskie_Gładkie_15Den, Spodnie_Jeans=Spodnie_Jeans, Czyszczenie_Chemiczne_Garnituru_Męskiego=Czyszczenie_Chemiczne_Garnituru_Męskiego, Olej_Napędowy=Olej_Napędowy)
# Uzupełniamy wszystkie 'NA' w naszych danych wartością 0 
# (ciężko będzie zastąpić dane jedną średnią gdyż mamy rozpiętość 14 lat dlatego wybrałem 0)
dane[is.na(dane)] <- 0
# Analiza względem województw 
dim(dane)
View(dane)
# Średnia cena produktów dla podanych lat
for(i in 5:14){
  print(tapply(dane[,i],dane[,2],mean))
}
# Odchylenie standardowe produktów dla podanych lat
for(i in 5:14){
  print(tapply(dane[,i],dane[,2],sd))
}
# Średnie w poszczególnych województwach
Sr_Bułka_Pszenna<-tapply(dane[,5],dane[,1],mean)
Sr_Mięso_Wieprzowe_Bez_Kości<-tapply(dane[,6],dane[,1],mean)
Sr_Kiełbasa_Wędzona<-tapply(dane[,7],dane[,1],mean,na.rm=T)
Sr_Filety_Z_Morszczuka_Mrożone<-tapply(dane[,8],dane[,1],mean)
Sr_Karp_Świeży<-tapply(dane[,9],dane[,1],mean)
Sr_Podkoszulek_Męski_Bawełniany_Bez_Rękawa<-tapply(dane[,10],dane[,1],mean)
Sr_Rajstopy_Damskie_Gładkie_15Den<-tapply(dane[,11],dane[,1],mean)
Sr_Spodnie_Jeans<-tapply(dane[,12],dane[,1],mean)
Sr_Czyszczenie_Chemiczne_Garnituru_Męskiego<-tapply(dane[,13],dane[,1],mean)
Sr_Olej_Napędowy<-tapply(dane[,14],dane[,1],mean)
# Zestawienie średnich:
srednie<-data.frame(woj=as.factor(sort(tab[,1])), Sr_Bułka_Pszenna=Sr_Bułka_Pszenna, Sr_Mięso_Wieprzowe_Bez_Kości=Sr_Mięso_Wieprzowe_Bez_Kości,
                    Sr_Kiełbasa_Wędzona=Sr_Kiełbasa_Wędzona, Sr_Filety_Z_Morszczuka_Mrożone=Sr_Filety_Z_Morszczuka_Mrożone, Sr_Karp_Świeży=Sr_Karp_Świeży, Sr_Podkoszulek_Męski_Bawełniany_Bez_Rękawa=Sr_Podkoszulek_Męski_Bawełniany_Bez_Rękawa,
                    Sr_Rajstopy_Damskie_Gładkie_15Den=Sr_Rajstopy_Damskie_Gładkie_15Den, Sr_Spodnie_Jeans=Sr_Spodnie_Jeans, Sr_Czyszczenie_Chemiczne_Garnituru_Męskiego=Sr_Czyszczenie_Chemiczne_Garnituru_Męskiego, Sr_Olej_Napędowy=Sr_Olej_Napędowy)
View(srednie)
#### Wykresy średnich ##########################################
# Bułka pszenna
ggplot(data = srednie, 
       aes(x = woj, 
           y = Sr_Bułka_Pszenna)) + 
  geom_point(color = 'red', 
             size = 1, 
             shape = 15) +
  ggtitle("Województwo v. cena Bułki pszennej") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data = srednie, 
       aes(x = woj, 
           y = Sr_Bułka_Pszenna)) +
  geom_bar(stat = 'identity', fill="lightblue", colour = "red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Mięso wieprzowe bez kości
ggplot(data = srednie, 
       aes(x = woj, 
           y = Sr_Mięso_Wieprzowe_Bez_Kości)) + 
  geom_point(color = 'red', 
             size = 1, 
             shape = 15) +
  ggtitle("Województwo v. cena Mięsa wieprzowego bez kości") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data = srednie, 
       aes(x = woj, 
           y = Sr_Mięso_Wieprzowe_Bez_Kości)) +
  geom_bar(stat = 'identity', fill="lightblue", colour = "red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Kiełbasa wędzona
ggplot(data = srednie, 
       aes(x = woj, 
           y = Sr_Kiełbasa_Wędzona)) + 
  geom_point(color = 'red', 
             size = 1, 
             shape = 15) +
  ggtitle("Województwo v. cena Kiełbasy wędzonej") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data = srednie, 
       aes(x = woj, 
           y = Sr_Kiełbasa_Wędzona)) +
  geom_bar(stat = 'identity', fill="lightblue", colour = "red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Filet z morszczuka mrożony
ggplot(data = srednie, 
       aes(x = woj, 
           y = Sr_Filety_Z_Morszczuka_Mrożone)) + 
  geom_point(color = 'red', 
             size = 1, 
             shape = 15) +
  ggtitle("Województwo v. cena Filetu z morszczuka mrożonego") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data = srednie, 
       aes(x = woj, 
           y = Sr_Filety_Z_Morszczuka_Mrożone)) +
  geom_bar(stat = 'identity', fill="lightblue", colour = "red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Karp świeży
ggplot(data = srednie, 
       aes(x = woj, 
           y = Sr_Karp_Świeży)) + 
  geom_point(color = 'red', 
             size = 1, 
             shape = 15) +
  ggtitle("Województwo v. cena Świeżego karpia") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data = srednie, 
       aes(x = woj, 
           y = Sr_Karp_Świeży)) +
  geom_bar(stat = 'identity', fill="lightblue", colour = "red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Podkoszulek męski bawełniany bez rękawa
ggplot(data = srednie, 
       aes(x = woj, 
           y = Sr_Podkoszulek_Męski_Bawełniany_Bez_Rękawa)) + 
  geom_point(color = 'red', 
             size = 1, 
             shape = 15) +
  ggtitle("Województwo v. cena Podkoszulki męskiej bawełnianej bez rękawa") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data = srednie, 
       aes(x = woj, 
           y = Sr_Podkoszulek_Męski_Bawełniany_Bez_Rękawa)) +
  geom_bar(stat = 'identity', fill="lightblue", colour = "red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Rajstopy damskie gładkie 15Den
ggplot(data = srednie, 
       aes(x = woj, 
           y = Sr_Rajstopy_Damskie_Gładkie_15Den)) + 
  geom_point(color = 'red', 
             size = 1, 
             shape = 15) +
  ggtitle("Województwo v. cena Rajstop damskich gładkich 15Den") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data = srednie, 
       aes(x = woj, 
           y = Sr_Rajstopy_Damskie_Gładkie_15Den)) +
  geom_bar(stat = 'identity', fill="lightblue", colour = "red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Spodnie jeans (6-11 lat)
ggplot(data = srednie, 
       aes(x = woj, 
           y = Sr_Spodnie_Jeans)) + 
  geom_point(color = 'red', 
             size = 1, 
             shape = 15) +
  ggtitle("Województwo v. cena Spodni typu jeans (6-11 lat)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data = srednie, 
       aes(x = woj, 
           y = Sr_Spodnie_Jeans)) +
  geom_bar(stat = 'identity', fill="lightblue", colour = "red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Czyszczenie chemiczne garnituru męskiego
ggplot(data = srednie, 
       aes(x = woj, 
           y = Sr_Czyszczenie_Chemiczne_Garnituru_Męskiego)) + 
  geom_point(color = 'red', 
             size = 1, 
             shape = 15) +
  ggtitle("Województwo v. cena Czyszczenia chemicznego garnituru męskiego") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data = srednie, 
       aes(x = woj, 
           y = Sr_Czyszczenie_Chemiczne_Garnituru_Męskiego)) +
  geom_bar(stat = 'identity', fill="lightblue", colour = "red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Olej napędowy
ggplot(data = srednie, 
       aes(x = woj, 
           y = Sr_Olej_Napędowy)) + 
  geom_point(color = 'red', 
             size = 1, 
             shape = 15) +
  ggtitle("Województwo v. cena Oleju napędowego za 1 litr") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data = srednie, 
       aes(x = woj, 
           y = Sr_Olej_Napędowy)) +
  geom_bar(stat = 'identity', fill="lightblue", colour = "red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
######################################################################

# Odchylenie standardowe w poszczególnych województwach
Sd_Bułka_Pszenna<-tapply(dane[,5],dane[,1],sd)
Sd_Mięso_Wieprzowe_Bez_Kości<-tapply(dane[,6],dane[,1],sd)
Sd_Kiełbasa_Wędzona<-tapply(dane[,7],dane[,1],sd,na.rm=T)
Sd_Filety_Z_Morszczuka_Mrożone<-tapply(dane[,8],dane[,1],sd)
Sd_Karp_Świeży<-tapply(dane[,9],dane[,1],sd)
Sd_Podkoszulek_Męski_Bawełniany_Bez_Rękawa<-tapply(dane[,10],dane[,1],sd)
Sd_Rajstopy_Damskie_Gładkie_15Den<-tapply(dane[,11],dane[,1],sd)
Sd_Spodnie_Jeans<-tapply(dane[,12],dane[,1],sd)
Sd_Czyszczenie_Chemiczne_Garnituru_Męskiego<-tapply(dane[,13],dane[,1],sd)
Sd_Olej_Napędowy<-tapply(dane[,14],dane[,1],sd)
# Zestawienie odchyleń standardowych
odchylenia<-data.frame(srednie,Sd_Bułka_Pszenna=Sd_Bułka_Pszenna, Sd_Mięso_Wieprzowe_Bez_Kości=Sd_Mięso_Wieprzowe_Bez_Kości,
                       Sd_Kiełbasa_Wędzona=Sd_Kiełbasa_Wędzona, Sd_Filety_Z_Morszczuka_Mrożone=Sd_Filety_Z_Morszczuka_Mrożone, Sd_Karp_Świeży=Sd_Karp_Świeży, Sd_Podkoszulek_Męski_Bawełniany_Bez_Rękawa=Sd_Podkoszulek_Męski_Bawełniany_Bez_Rękawa,
                       Sd_Rajstopy_Damskie_Gładkie_15Den=Sd_Rajstopy_Damskie_Gładkie_15Den, Sd_Spodnie_Jeans=Sd_Spodnie_Jeans, Sd_Czyszczenie_Chemiczne_Garnituru_Męskiego=Sd_Czyszczenie_Chemiczne_Garnituru_Męskiego, Sd_Olej_Napędowy=Sd_Olej_Napędowy)

#### Uwzględnienie odchyleń standardowych na wykresach ##########################################
# Bułka pszenna
ggplot(odchylenia, aes(x=woj, y=Sr_Bułka_Pszenna)) + 
  geom_errorbar(aes(ymin=Sr_Bułka_Pszenna-Sd_Bułka_Pszenna, 
                    ymax=Sr_Bułka_Pszenna+Sd_Bułka_Pszenna), 
                width=.1,
                col="blue") +
  geom_point(color = 'red', 
             size = 1, 
             shape = 15) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(data = odchylenia, 
       aes(x = woj, 
           y = Sr_Bułka_Pszenna)) +
  geom_bar(stat = 'identity', fill="lightblue", colour = "red") +
  geom_errorbar(aes(ymin=Sr_Bułka_Pszenna - Sd_Bułka_Pszenna, ymax=Sr_Bułka_Pszenna + Sd_Bułka_Pszenna),
                width=.2,                    # Width of the error bars
                col="darkblue") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Mięso wieprzowe bez kości
ggplot(odchylenia, aes(x=woj, y=Sr_Mięso_Wieprzowe_Bez_Kości)) + 
  geom_errorbar(aes(ymin=Sr_Mięso_Wieprzowe_Bez_Kości-Sd_Mięso_Wieprzowe_Bez_Kości, 
                    ymax=Sr_Mięso_Wieprzowe_Bez_Kości+Sd_Mięso_Wieprzowe_Bez_Kości), 
                width=.1,
                col="blue") +
  geom_point(color = 'red', 
             size = 1, 
             shape = 15) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(data = odchylenia, 
       aes(x = woj, 
           y = Sr_Mięso_Wieprzowe_Bez_Kości)) +
  geom_bar(stat = 'identity', fill="lightblue", colour = "red") +
  geom_errorbar(aes(ymin=Sr_Mięso_Wieprzowe_Bez_Kości - Sd_Mięso_Wieprzowe_Bez_Kości, ymax=Sr_Mięso_Wieprzowe_Bez_Kości + Sd_Mięso_Wieprzowe_Bez_Kości),
                width=.2,                    # Width of the error bars
                col="darkblue") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Kiełbasa wędzona
ggplot(odchylenia, aes(x=woj, y=Sr_Kiełbasa_Wędzona)) + 
  geom_errorbar(aes(ymin=Sr_Kiełbasa_Wędzona-Sd_Kiełbasa_Wędzona, 
                    ymax=Sr_Kiełbasa_Wędzona+Sd_Kiełbasa_Wędzona), 
                width=.1,
                col="blue") +
  geom_point(color = 'red', 
             size = 1, 
             shape = 15) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(data = odchylenia, 
       aes(x = woj, 
           y = Sr_Kiełbasa_Wędzona)) +
  geom_bar(stat = 'identity', fill="lightblue", colour = "red") +
  geom_errorbar(aes(ymin=Sr_Kiełbasa_Wędzona - Sd_Kiełbasa_Wędzona, ymax=Sr_Kiełbasa_Wędzona + Sd_Kiełbasa_Wędzona),
                width=.2,                    # Width of the error bars
                col="darkblue") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Filet z morszczuka mrożony
ggplot(odchylenia, aes(x=woj, y=Sr_Filety_Z_Morszczuka_Mrożone)) + 
  geom_errorbar(aes(ymin=Sr_Filety_Z_Morszczuka_Mrożone-Sd_Filety_Z_Morszczuka_Mrożone, 
                    ymax=Sr_Filety_Z_Morszczuka_Mrożone+Sd_Filety_Z_Morszczuka_Mrożone), 
                width=.1,
                col="blue") +
  geom_point(color = 'red', 
             size = 1, 
             shape = 15) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(data = odchylenia, 
       aes(x = woj, 
           y = Sr_Filety_Z_Morszczuka_Mrożone)) +
  geom_bar(stat = 'identity', fill="lightblue", colour = "red") +
  geom_errorbar(aes(ymin=Sr_Filety_Z_Morszczuka_Mrożone - Sd_Filety_Z_Morszczuka_Mrożone, ymax=Sr_Filety_Z_Morszczuka_Mrożone + Sd_Filety_Z_Morszczuka_Mrożone),
                width=.2,                    # Width of the error bars
                col="darkblue") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Karp świeży
ggplot(odchylenia, aes(x=woj, y=Sr_Karp_Świeży)) + 
  geom_errorbar(aes(ymin=Sr_Karp_Świeży-Sd_Karp_Świeży, 
                    ymax=Sr_Karp_Świeży+Sd_Karp_Świeży), 
                width=.1,
                col="blue") +
  geom_point(color = 'red', 
             size = 1, 
             shape = 15) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(data = odchylenia, 
       aes(x = woj, 
           y = Sr_Karp_Świeży)) +
  geom_bar(stat = 'identity', fill="lightblue", colour = "red") +
  geom_errorbar(aes(ymin=Sr_Karp_Świeży - Sd_Karp_Świeży, ymax=Sr_Karp_Świeży + Sd_Karp_Świeży),
                width=.2,                    # Width of the error bars
                col="darkblue") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Podkoszulek męski bawełniany bez rękawa
ggplot(odchylenia, aes(x=woj, y=Sr_Podkoszulek_Męski_Bawełniany_Bez_Rękawa)) + 
  geom_errorbar(aes(ymin=Sr_Podkoszulek_Męski_Bawełniany_Bez_Rękawa-Sd_Podkoszulek_Męski_Bawełniany_Bez_Rękawa, 
                    ymax=Sr_Podkoszulek_Męski_Bawełniany_Bez_Rękawa+Sd_Podkoszulek_Męski_Bawełniany_Bez_Rękawa), 
                width=.1,
                col="blue") +
  geom_point(color = 'red', 
             size = 1, 
             shape = 15) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(data = odchylenia, 
       aes(x = woj, 
           y = Sr_Podkoszulek_Męski_Bawełniany_Bez_Rękawa)) +
  geom_bar(stat = 'identity', fill="lightblue", colour = "red") +
  geom_errorbar(aes(ymin=Sr_Podkoszulek_Męski_Bawełniany_Bez_Rękawa - Sd_Podkoszulek_Męski_Bawełniany_Bez_Rękawa, ymax=Sr_Podkoszulek_Męski_Bawełniany_Bez_Rękawa + Sd_Podkoszulek_Męski_Bawełniany_Bez_Rękawa),
                width=.2,                    # Width of the error bars
                col="darkblue") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Rajstopy damskie gładkie 15Den
ggplot(odchylenia, aes(x=woj, y=Sr_Rajstopy_Damskie_Gładkie_15Den)) + 
  geom_errorbar(aes(ymin=Sr_Rajstopy_Damskie_Gładkie_15Den-Sd_Rajstopy_Damskie_Gładkie_15Den, 
                    ymax=Sr_Rajstopy_Damskie_Gładkie_15Den+Sd_Rajstopy_Damskie_Gładkie_15Den), 
                width=.1,
                col="blue") +
  geom_point(color = 'red', 
             size = 1, 
             shape = 15) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(data = odchylenia, 
       aes(x = woj, 
           y = Sr_Rajstopy_Damskie_Gładkie_15Den)) +
  geom_bar(stat = 'identity', fill="lightblue", colour = "red") +
  geom_errorbar(aes(ymin=Sr_Rajstopy_Damskie_Gładkie_15Den - Sd_Rajstopy_Damskie_Gładkie_15Den, ymax=Sr_Rajstopy_Damskie_Gładkie_15Den + Sd_Rajstopy_Damskie_Gładkie_15Den),
                width=.2,                    # Width of the error bars
                col="darkblue") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Spodnie jeans (6-11 lat)
ggplot(odchylenia, aes(x=woj, y=Sr_Spodnie_Jeans)) + 
  geom_errorbar(aes(ymin=Sr_Spodnie_Jeans-Sd_Spodnie_Jeans, 
                    ymax=Sr_Spodnie_Jeans+Sd_Spodnie_Jeans), 
                width=.1,
                col="blue") +
  geom_point(color = 'red', 
             size = 1, 
             shape = 15) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(data = odchylenia, 
       aes(x = woj, 
           y = Sr_Spodnie_Jeans)) +
  geom_bar(stat = 'identity', fill="lightblue", colour = "red") +
  geom_errorbar(aes(ymin=Sr_Spodnie_Jeans - Sd_Spodnie_Jeans, ymax=Sr_Spodnie_Jeans + Sd_Spodnie_Jeans),
                width=.2,                    # Width of the error bars
                col="darkblue") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Czyszczenie chemiczne garnituru męskiego
ggplot(odchylenia, aes(x=woj, y=Sr_Czyszczenie_Chemiczne_Garnituru_Męskiego)) + 
  geom_errorbar(aes(ymin=Sr_Czyszczenie_Chemiczne_Garnituru_Męskiego-Sd_Czyszczenie_Chemiczne_Garnituru_Męskiego, 
                    ymax=Sr_Czyszczenie_Chemiczne_Garnituru_Męskiego+Sd_Czyszczenie_Chemiczne_Garnituru_Męskiego), 
                width=.1,
                col="blue") +
  geom_point(color = 'red', 
             size = 1, 
             shape = 15) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(data = odchylenia, 
       aes(x = woj, 
           y = Sr_Czyszczenie_Chemiczne_Garnituru_Męskiego)) +
  geom_bar(stat = 'identity', fill="lightblue", colour = "red") +
  geom_errorbar(aes(ymin=Sr_Czyszczenie_Chemiczne_Garnituru_Męskiego - Sd_Czyszczenie_Chemiczne_Garnituru_Męskiego, ymax=Sr_Czyszczenie_Chemiczne_Garnituru_Męskiego + Sd_Czyszczenie_Chemiczne_Garnituru_Męskiego),
                width=.2,                    # Width of the error bars
                col="darkblue") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Olej napędowy
ggplot(odchylenia, aes(x=woj, y=Sr_Olej_Napędowy)) + 
  geom_errorbar(aes(ymin=Sr_Olej_Napędowy-Sd_Olej_Napędowy, 
                    ymax=Sr_Olej_Napędowy+Sd_Olej_Napędowy), 
                width=.1,
                col="blue") +
  geom_point(color = 'red', 
             size = 1, 
             shape = 15) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(data = odchylenia, 
       aes(x = woj, 
           y = Sr_Olej_Napędowy)) +
  geom_bar(stat = 'identity', fill="lightblue", colour = "red") +
  geom_errorbar(aes(ymin=Sr_Olej_Napędowy - Sd_Olej_Napędowy, ymax=Sr_Olej_Napędowy + Sd_Olej_Napędowy),
                width=.2,                    # Width of the error bars
                col="darkblue") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
######################################################################

# Analiza cen dla Polski (wszystkie lata 2006-2019)
dim(dane)
View(dane)

polska <- dane[dane$woj == "POLSKA",]
dim(polska)
View(polska)
str(polska)


polska$data <- as.yearmon(paste(polska$mies, polska$rok)) 
polska<-polska[order(polska$mies),]
polska2006<-polska[polska$rok==2006,]
polska2007<-polska[polska$rok==2007,]
polska2008<-polska[polska$rok==2008,]
polska2009<-polska[polska$rok==2009,]
polska2010<-polska[polska$rok==2010,]
polska2011<-polska[polska$rok==2011,]
polska2012<-polska[polska$rok==2012,]
polska2013<-polska[polska$rok==2013,]
polska2014<-polska[polska$rok==2014,]
polska2015<-polska[polska$rok==2015,]
polska2016<-polska[polska$rok==2016,]
polska2017<-polska[polska$rok==2017,]
polska2018<-polska[polska$rok==2018,]
polska2019<-polska[polska$rok==2019,]
########## Zmiany w czasie dla Polski #################
#### Bułka pszenna
ggplot(data = polska, 
       aes(x = data, 
           y = Bułka_Pszenna,
           color =rok)) + 
  geom_point(size = 5, 
             shape = 15) +
  ggtitle("Miesiąc v. cena Bułki pszennej") +
  scale_x_yearmon() +
  theme(axis.text.x = element_text(angle = 90, hjust = .5))

# porównanie po latach
ggplot(data = polska, 
       aes(x = mon, 
           y = Bułka_Pszenna,
           color =rok,
           group=rok)) + 
  geom_point(size = 5, 
             shape = 15) +
  ggtitle("Miesiąc v. cena Bułki pszennej") +
  scale_x_discrete( labels = c("sty","lut","mar","kwi","maj","cze","lip","sie","wrz","paź","lis","gru")) +
  theme(axis.text.x = element_text(angle = 90, hjust = .5))

ggplot(polska, aes(x=mon, y=Bułka_Pszenna, fill=mies)) + 
  geom_boxplot(alpha=0.6)
#### Mięso wieprzowe bez kości
ggplot(data = polska, 
       aes(x = data, 
           y = Mięso_Wieprzowe_Bez_Kości,
           color =rok)) + 
  geom_point(size = 5, 
             shape = 15) +
  ggtitle("Miesiąc v. cena Mięsa wieprzowego bez kości") +
  scale_x_yearmon() +
  theme(axis.text.x = element_text(angle = 90, hjust = .5))

# porównanie po latach
ggplot(data = polska, 
       aes(x = mon, 
           y = Mięso_Wieprzowe_Bez_Kości,
           color =rok,
           group=rok)) + 
  geom_point(size = 5, 
             shape = 15) +
  ggtitle("Miesiąc v. cena Mięsa wieprzowego bez kości") +
  scale_x_discrete( labels = c("sty","lut","mar","kwi","maj","cze","lip","sie","wrz","paź","lis","gru")) +
  theme(axis.text.x = element_text(angle = 90, hjust = .5))

ggplot(polska, aes(x=mon, y=Mięso_Wieprzowe_Bez_Kości, fill=mies)) + 
  geom_boxplot(alpha=0.6)
### Kiełbasa wędzona
ggplot(data = polska, 
       aes(x = data, 
           y = Kiełbasa_Wędzona,
           color =rok)) + 
  geom_point(size = 5, 
             shape = 15) +
  ggtitle("Miesiąc v. cena Kiełbasy wędzonej") +
  scale_x_yearmon() +
  theme(axis.text.x = element_text(angle = 90, hjust = .5))

# porównanie po latach
ggplot(data = polska, 
       aes(x = mon, 
           y = Kiełbasa_Wędzona,
           color =rok,
           group=rok)) + 
  geom_point(size = 5, 
             shape = 15) +
  ggtitle("Miesiąc v. cena Kiełbasy wędzonej") +
  scale_x_discrete( labels = c("sty","lut","mar","kwi","maj","cze","lip","sie","wrz","paź","lis","gru")) +
  theme(axis.text.x = element_text(angle = 90, hjust = .5))

ggplot(polska, aes(x=mon, y=Kiełbasa_Wędzona, fill=mies)) + 
  geom_boxplot(alpha=0.6)
### Filety z morszczuka mrożone
ggplot(data = polska, 
       aes(x = data, 
           y = Filety_Z_Morszczuka_Mrożone,
           color =rok)) + 
  geom_point(size = 5, 
             shape = 15) +
  ggtitle("Miesiąc v. cena Filetu mrożonego z morszczuka") +
  scale_x_yearmon() +
  theme(axis.text.x = element_text(angle = 90, hjust = .5))

# porównanie po latach
ggplot(data = polska, 
       aes(x = mon, 
           y = Filety_Z_Morszczuka_Mrożone,
           color =rok,
           group=rok)) + 
  geom_point(size = 5, 
             shape = 15) +
  ggtitle("Miesiąc v. cena Filetu mrożonego z morszczuka") +
  scale_x_discrete( labels = c("sty","lut","mar","kwi","maj","cze","lip","sie","wrz","paź","lis","gru")) +
  theme(axis.text.x = element_text(angle = 90, hjust = .5))

ggplot(polska, aes(x=mon, y=Filety_Z_Morszczuka_Mrożone, fill=mies)) + 
  geom_boxplot(alpha=0.6)
### Karp_Świeży
ggplot(data = polska, 
       aes(x = data, 
           y = Karp_Świeży,
           color =rok)) + 
  geom_point(size = 5, 
             shape = 15) +
  ggtitle("Miesiąc v. cena Karpia swieżego") +
  scale_x_yearmon() +
  theme(axis.text.x = element_text(angle = 90, hjust = .5))

# porównanie po latach
ggplot(data = polska, 
       aes(x = mon, 
           y = Karp_Świeży,
           color =rok,
           group=rok)) + 
  geom_point(size = 5, 
             shape = 15) +
  ggtitle("Miesiąc v. cena Karpia swieżego") +
  scale_x_discrete( labels = c("sty","lut","mar","kwi","maj","cze","lip","sie","wrz","paź","lis","gru")) +
  theme(axis.text.x = element_text(angle = 90, hjust = .5))

ggplot(polska, aes(x=mon, y=Karp_Świeży, fill=mies)) + 
  geom_boxplot(alpha=0.6)
### Podkoszulek męski bawełniany bez rękawa
ggplot(data = polska, 
       aes(x = data, 
           y = Podkoszulek_Męski_Bawełniany_Bez_Rękawa,
           color =rok)) + 
  geom_point(size = 5, 
             shape = 15) +
  ggtitle("Miesiąc v. cena Podkoszulki męskiej bawełnianej bez rękawa") +
  scale_x_yearmon() +
  theme(axis.text.x = element_text(angle = 90, hjust = .5))

# porównanie po latach
ggplot(data = polska, 
       aes(x = mon, 
           y = Podkoszulek_Męski_Bawełniany_Bez_Rękawa,
           color =rok,
           group=rok)) + 
  geom_point(size = 5, 
             shape = 15) +
  ggtitle("Miesiąc v. cena Podkoszulki męskiej bawełnianej bez rękawa") +
  scale_x_discrete( labels = c("sty","lut","mar","kwi","maj","cze","lip","sie","wrz","paź","lis","gru")) +
  theme(axis.text.x = element_text(angle = 90, hjust = .5))

ggplot(polska, aes(x=mon, y=Podkoszulek_Męski_Bawełniany_Bez_Rękawa, fill=mies)) + 
  geom_boxplot(alpha=0.6)
### Rajstopy damskie gładkie 15Den
ggplot(data = polska, 
       aes(x = data, 
           y = Rajstopy_Damskie_Gładkie_15Den,
           color =rok)) + 
  geom_point(size = 5, 
             shape = 15) +
  ggtitle("Miesiąc v. cena Rajstop damskich gładkich 15Den") +
  scale_x_yearmon() +
  theme(axis.text.x = element_text(angle = 90, hjust = .5))

# porównanie po latach
ggplot(data = polska, 
       aes(x = mon, 
           y = Rajstopy_Damskie_Gładkie_15Den,
           color =rok,
           group=rok)) + 
  geom_point(size = 5, 
             shape = 15) +
  ggtitle("Miesiąc v. cena Rajstop damskich gładkich 15Den") +
  scale_x_discrete( labels = c("sty","lut","mar","kwi","maj","cze","lip","sie","wrz","paź","lis","gru")) +
  theme(axis.text.x = element_text(angle = 90, hjust = .5))

ggplot(polska, aes(x=mon, y=Rajstopy_Damskie_Gładkie_15Den, fill=mies)) + 
  geom_boxplot(alpha=0.6)
### Spodnie Jeans (6-11 lat)
ggplot(data = polska, 
       aes(x = data, 
           y = Spodnie_Jeans,
           color =rok)) + 
  geom_point(size = 5, 
             shape = 15) +
  ggtitle("Miesiąc v. cena Spodni typu jeans") +
  scale_x_yearmon() +
  theme(axis.text.x = element_text(angle = 90, hjust = .5))

# porównanie po latach
ggplot(data = polska, 
       aes(x = mon, 
           y = Spodnie_Jeans,
           color =rok,
           group=rok)) + 
  geom_point(size = 5, 
             shape = 15) +
  ggtitle("Miesiąc v. cena Spodni typu jeans") +
  scale_x_discrete( labels = c("sty","lut","mar","kwi","maj","cze","lip","sie","wrz","paź","lis","gru")) +
  theme(axis.text.x = element_text(angle = 90, hjust = .5))

ggplot(polska, aes(x=mon, y=Spodnie_Jeans, fill=mies)) + 
  geom_boxplot(alpha=0.6)
### Czyszczenie chemiczne garnituru męskiego
ggplot(data = polska, 
       aes(x = data, 
           y = Czyszczenie_Chemiczne_Garnituru_Męskiego,
           color =rok)) + 
  geom_point(size = 5, 
             shape = 15) +
  ggtitle("Miesiąc v. cena Chemicznego czyszczenia garnituru męskiego") +
  scale_x_yearmon() +
  theme(axis.text.x = element_text(angle = 90, hjust = .5))

# porównanie po latach
ggplot(data = polska, 
       aes(x = mon, 
           y = Czyszczenie_Chemiczne_Garnituru_Męskiego,
           color =rok,
           group=rok)) + 
  geom_point(size = 5, 
             shape = 15) +
  ggtitle("Miesiąc v. cena Chemicznego czyszczenia garnituru męskiego") +
  scale_x_discrete( labels = c("sty","lut","mar","kwi","maj","cze","lip","sie","wrz","paź","lis","gru")) +
  theme(axis.text.x = element_text(angle = 90, hjust = .5))

ggplot(polska, aes(x=mon, y=Czyszczenie_Chemiczne_Garnituru_Męskiego, fill=mies)) + 
  geom_boxplot(alpha=0.6)
### Olej napędowy
ggplot(data = polska, 
       aes(x = data, 
           y = Olej_Napędowy,
           color =rok)) + 
  geom_point(size = 5, 
             shape = 15) +
  ggtitle("Miesiąc v. cena Oleju napędowego za 1 litr") +
  scale_x_yearmon() +
  theme(axis.text.x = element_text(angle = 90, hjust = .5))

# porównanie po latach
ggplot(data = polska, 
       aes(x = mon, 
           y = Olej_Napędowy,
           color =rok,
           group=rok)) + 
  geom_point(size = 5, 
             shape = 15) +
  ggtitle("Miesiąc v. cena Oleju napędowego za 1 litr") +
  scale_x_discrete( labels = c("sty","lut","mar","kwi","maj","cze","lip","sie","wrz","paź","lis","gru")) +
  theme(axis.text.x = element_text(angle = 90, hjust = .5))

ggplot(polska, aes(x=mon, y=Olej_Napędowy, fill=mies)) + 
  geom_boxplot(alpha=0.6)

