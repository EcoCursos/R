#
#O comando setwd Seleciona a pasta a qual irei trabalhar
#
setwd("C:/Users/Jairo Matos Rocha/Desktop/eee")
dir()#mostra o que esta dentro da pasta

contro<-read.csv("Control_tower_rad_2016.csv")# ler o arquivo csv e saval no objeto contro
soy<-read.csv("Soy_tower_rad_2016.csv" )# ler o arquivo csv e saval no objeto soy

head(contro)#Mostra as 6 primeira linhas do objeto contro

names(contro)#Mostra os nomes das colunas do objeto contro
names(soy)#Mostra os nomes das colunas do objeto soy

contro$torre='controle' #Cria a coluna torre e salva a palavra controle em toda a coluna 
soy$torre='soja' #Cria a coluna torre e salva a palavra soja em toda a coluna

torres<- rbind(contro,soy) # Junta as duas tabelas

dim(torres)# Dimenção do objeto.
View(torres)# Ver os dados dentro do objeto


names(torres)#Mostra o nomes da colunas do objeto torres

#Lwin = Ondas longas chegando
#Lwout= Ondas longas saindo
#SWin = Ondas curtas chegando
#SWout= Ondas curtas saindo (Albedo)
#LE   = Evapo transpiração
#H    = Calor Sensivel
#Renomer os nomes das colunas do objeto torres
names(torres)=c("X", "Year", "DateTime", "Day", "Hour", "LWin", "LWout", "SWin", "SWout", "LE", "H", "torre" )

#Cria a coluna mes, extraindo o mês da coluna DataTime
torres$mes=substr(torres$DateTime,6,7)
unique(torres$mes)#Agrupa os valores para mostra no console.
torres$mes=as.numeric(torres$mes)#Converte o valor para numerico

# Calcula  o Rnet e adicionado no objeto torres
# Rnet = (SWin - SWout)+(LWin- LWout)
torres$Rnet=(torres$SWin-torres$SWout)+(torres$LWin-torres$LWout)


#Histrogramas 
hist(torres$LWin)#Fazer um histrograma de  Ondas longas chegando
hist(torres$LWout)#Fazer um histrograma de  Ondas longas saindo
hist(torres$SWin)#Fazer um histrograma de  Ondas curtas chegando
hist(torres$SWout)#Fazer um histrograma de  Ondas curtas saindo (Albedo)
hist(torres$Rnet)#Fazer um histrograma de Rnet


library(ggplot2)#Carrega a biblioteca ggplot2

ggplot()
