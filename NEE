###########################
#Author: Maracahipes-Santos, Leonardo; Contributions: Silvério, Divino Vicente and Paolucci, Lucas; 
#Instituto de Pesquisa Ambiental da Amazonia (IPAM)
#Fevereiro 2017
#Última atualização fevereiro 06, 2017
rm (list = ls())
# Ecologia de Ecossistemas
path="C:\\Users\\nayan_000\\Documents\\Ecologia de Ecossistema\\"
setwd("C:\\Users\\nayan_000\\Documents\\Ecologia de Ecossistema")
dir()

# EUC = NPP/GPP # Eficiencia do uso do carbona

# t.control=read.csv("Control_tower_rad_2016.csv",h=T)
# head(t.control)
# names(t.control)
# 
# 
# t.soy=read.csv("Soy_tower_rad_2016.csv",h=T)
# head(t.soy)
# names(t.soy)
list.files()

torre <-read.csv("control_tower_2015.csv",h=T)
dim(torre)
dir()
names(torre)
dim(torre)
head(torre)
str(torre)

torre.sub = torre[,c("PPFD_1_1_1","Rg",
                     "co2_flux","qc_co2_flux",
                     "Day", "Hour")]

####
head(torre.sub)

plot(torre.sub$co2_flux)

library(ggplot2)
ggplot(torre.sub, aes(x =PPFD_1_1_1, 
                      y = co2_flux))+
  geom_point(alpha = .3,color = "darkgreen")+
  labs(x = "PAR", y="NEE")

####
torre.qual = subset(torre.sub,
                    qc_co2_flux<5&Rg>20)


ggplot(torre.qual, aes(x =PPFD_1_1_1, 
                      y = co2_flux))+
  geom_point(alpha = .3,color = "darkgreen")+
  geom_smooth()+
  labs(x = "PAR", y="NEE")


ggplot(torre.sub, aes(x = Hour, y = co2_flux))+
  geom_smooth()+
  geom_hline(yintercept = 0, type = 1)


torre.sub$Estacao = cut(torre.sub$Day,
                        seq(1,367,91),
                        labels = c("JFM",
                                   "AMJ",
                                   "JAS",
                                   "OND"))

##

ggplot(torre.sub, aes(x = Hour, y = co2_flux, color = Estacao))+
  geom_smooth()+
  geom_hline(yintercept = 0, linetype = 2)

####

ggplot(torre.sub, aes(x = Hour, y = co2_flux, color = Estacao))+
  geom_smooth()+
  facet_wrap(~Estacao)+
  geom_hline(yintercept = 0, linetype = 2)

###
  
ggplot(torre.sub, aes(x = Estacao, y = co2_flux))+
  geom_boxplot()+
  geom_violin(alpha = 0.3, color = "salmon")+
  geom_hline(yintercept = 0, linetype = 2)
 

ggplot(torre.sub, aes(x = co2_flux,
                      color = Estacao,
                      fill = Estacao))+
  geom_density(alpha = 0.2)+
  geom_vline(xintercept = 0, linetype = 2)
  


# 1 = Gráfico dois paineis (dia e noite)

torre.sub$day_nigth = ifelse(torre.sub$Hour==0.0|torre.sub$Hour==0.5|
                             torre.sub$Hour==1.0|torre.sub$Hour==1.5|
                             torre.sub$Hour==2.0|torre.sub$Hour==2.5|
                             torre.sub$Hour==3.0|torre.sub$Hour==3.5|
                             torre.sub$Hour==4.0|torre.sub$Hour==4.5|
                             torre.sub$Hour==5.0|torre.sub$Hour==5.5|
                             torre.sub$Hour==6.0|torre.sub$Hour==18.0|
                             torre.sub$Hour==18.5|torre.sub$Hour==19.0|
                             torre.sub$Hour==19.5|torre.sub$Hour==20.0|
                             torre.sub$Hour==20.5|torre.sub$Hour==21.0|
                             torre.sub$Hour==21.5|torre.sub$Hour==22.0|
                             torre.sub$Hour==22.5|torre.sub$Hour==23.0|
                             torre.sub$Hour==23.5,"Night", NA)

torre.sub$day_nigth = ifelse(torre.sub$Hour==6.5|torre.sub$Hour==7.0|
                               torre.sub$Hour==7.5|torre.sub$Hour==8.0|
                               torre.sub$Hour==8.5|torre.sub$Hour==9.0|
                               torre.sub$Hour==9.5|torre.sub$Hour==10.0|
                               torre.sub$Hour==10.5|torre.sub$Hour==11.0|
                               torre.sub$Hour==11.5|torre.sub$Hour==12.0|
                               torre.sub$Hour==12.5|torre.sub$Hour==13.0|
                               torre.sub$Hour==13.5|torre.sub$Hour==14.0|
                               torre.sub$Hour==14.5|torre.sub$Hour==15.0|
                               torre.sub$Hour==15.5|torre.sub$Hour==16.0|
                               torre.sub$Hour==16.5|torre.sub$Hour==17.0|
                               torre.sub$Hour==17.5,"Day", torre.sub$day_nigth)

plot1 = ggplot(torre.sub, aes(x = Hour, y = co2_flux, color = Estacao))+
  geom_smooth()+
  facet_wrap(~day_nigth)+
  labs(x = "Hour", y="NEE")+
  theme_bw(base_size = 20)+
  geom_hline(yintercept = 0, linetype = 2)

plot1

library(ggplot2)
library(gridExtra)
library(extrafont)

## figuras
path
tiff(filename = "Day_Nigth1.tiff",
     width = 30, height = 20, units = "cm", res = 300)

## isso acima é para salvar figura em melhor qualidade
## + esse comando dev.off() no fim.
#######
## Criar plots


grid.arrange(plot1, ncol=1)
dev.off() # dizer que finalizei meu gráfico.

######

plot2 = ggplot(torre.sub, aes(x = Hour, y = co2_flux, color = Estacao))+
  geom_boxplot()+
  facet_wrap(~day_nigth)+
  labs(x = "Hour", y="NEE")+
  theme_bw(base_size = 20)+
  # geom_violin(alpha = 0.3, color = "salmon")+
  geom_hline(yintercept = 0, linetype = 2)
plot2

library(ggplot2)
library(gridExtra)
library(extrafont)

## figuras
path
tiff(filename = "Day_Nigth2.tiff",
     width = 30, height = 20, units = "cm", res = 300)

## isso acima é para salvar figura em melhor qualidade
## + esse comando dev.off() no fim.
#######
## Criar plots


grid.arrange(plot2, ncol=1)

dev.off() # dizer que finalizei meu gráfico.


# 2 = Converter de: mmolCO2/m2/hora
# para gC/m2/dia

C=as.numeric(12)  
O2=2*16
CO2= C+O2  

## para obter o peso de Carbono  
Cmol=(CO2/12) ## = 3.666667

## uma grama equivale a:
mol=(6.02*10^23) 

## para transformar em gramas 

Cg=(6.02*10^23)/(3.666667) ## = 1.641818e+23
## mas é preciso converter pra mmol então depois da divisão acrescenta mais 10^3 ficando 1.641818e+26


## para transformar em gr por dia 
Cg=(1.641818e+26)/24  

torre.sub$c_gr_d=(torre.sub$co2_flux * 6.840908e+24)



# 6x10^23 atomos de Carbono existem em 1mol de CO2.
# partiu desse principio
ggplot(torre.sub, aes(x = Hour, y = c_gr_d, color = Estacao))+
  geom_smooth()+
  facet_wrap(~day_nigth)+
  geom_hline(yintercept = 0, linetype = 2)

ggplot(torre.sub, aes(x = Hour, y = c_gr_d, color = Estacao))+
  geom_boxplot()+
  facet_wrap(~day_nigth)+
  geom_violin(alpha = 0.3, color = "salmon")+
  geom_hline(yintercept = 0, linetype = 2)




 
# 3 = Média por estação
head(torre.sub)
est_co2=doBy::summaryBy(co2_flux~Estacao,torre.sub,FUN=c(mean,max,min,sd),na.rm=T)

est_co2

est_co2_1=doBy::summaryBy(c_gr_d~Estacao,torre.sub,FUN=c(mean,max,min,sd),na.rm=T)

est_co2_1
