Dir <- "~/Dropbox/Projects/TORRE/Dados torres tanguro/DATA/MASTER_GAP_FILLED/"
Dir_Fig <- "~/Dropbox/Manuscript/Fire_Recovery/Tower/Figures_general"

#importing files
control <- read.csv(paste0(Dir, 'Control_Gap_Filled_MDS_masterJUNE2017.csv'))
torre <- subset(control, Year == 2015)
###
###
###
str(torre)

torre <- read.csv("Control_tower_2015.csv")
dim(torre)
head(torre)
str(torre)

torre.sub <- torre[,c('PPFD_1_1_1',
                      'Rg',
                      'co2_flux',
                      'qc_co2_flux',
                      'Day',
                      'Hour')]
###
plot(torre.sub$co2_flux)

library(ggplot2)
ggplot(torre.sub, aes(x=PPFD_1_1_1,
                      y=co2_flux))+
  geom_point(alpha = .3)

#
torre.qual <- subset(torre.sub,
                     qc_co2_flux<5&
                     Rg>20)

ggplot(torre.qual, aes(x=PPFD_1_1_1,
                      y=co2_flux))+
  geom_point(alpha = .3)+
  geom_smooth()

###
###
###
ggplot(torre.sub, aes(x=Hour,
                      y=co2_flux))+
  geom_smooth()+
  geom_hline(yintercept = 0,type=1)

####
torre.sub$Estacao <- cut(torre.sub$Day,
                         seq(1,367, 91),
                         labels = c('JFM',
                                    'AMJ',
                                    'JAS',
                                    'OND'))
###
###
ggplot(torre.sub, aes(x=Hour,
                      y=co2_flux,
                      color=Estacao))+
  geom_smooth()+
  geom_hline(yintercept = 0,type=1)


ggplot(torre.sub, aes(x=Hour,
                      y=co2_flux,
                      color=Estacao))+
  geom_smooth()+
  facet_wrap(~Estacao)+
  geom_hline(yintercept = 0,linetype=2)
#
ggplot(torre.sub, aes(x=Estacao,
                      y=co2_flux))+
  geom_boxplot()+
#  geom_violin(alpha = 0.3, color='salmon')+
  geom_hline(yintercept = 0,linetype=2)


ggplot(torre.sub, aes(x=co2_flux,
                      color=Estacao,
                      fill=Estacao))+
  geom_density(alpha = .2)+
  geom_vline(xintercept = 0, linetype=2)
