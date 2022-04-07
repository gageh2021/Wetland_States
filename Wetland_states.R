pacman::p_load(reshape2,stats,dplyr,tidyr,broom,ggplot2,ggpubr,FSA,RColorBrewer,here,ggridges,plotly,lmtest,pscl,MASS,lattice,segmented,Hmisc,corrplot,nls2)

#Please check that the working directory displays correctly using 'here()'.
#It should show that the current directory ends with .../Wetland_States_Rpository

wetland_data<-read.table(here("Data","Full_RS_data.csv"),header=T,sep=",")
attach(wetland_data)
proportion_data<-read.table(here("Data","Proportion_data.csv"),header=T,sep=",")
attach(proportion_data)

RdNBR_data<-filter(wetland_data,OID!=c("37","70"))

Distance_data<-read.table(here("Data","Margin_Middle_RdNBR.csv"),header=T,sep=",")
attach(Distance_data)
Distance_data_positive<-filter(Distance_data,RdNBR>=0)

Correlation_data<-read.table(here("Data","Correlation_plot_data.csv"),header=T,sep=",")
attach(Correlation_data)

#Digitization Results (Wetland States) -------------------

#Summary Statistics

A<-filter(wetland_data,State=="A")
B<-filter(wetland_data,State=="B")
C<-filter(wetland_data,State=="C")
D<-filter(wetland_data,State=="D")
E<-filter(wetland_data,State=="E")

A_R<-filter(RdNBR_data,State=="A")
B_R<-filter(RdNBR_data,State=="B")
C_R<-filter(RdNBR_data,State=="C")
D_R<-filter(RdNBR_data,State=="D")
E_R<-filter(RdNBR_data,State=="E")

mean(A$Area_km)
mean(B$Area_km)
mean(C$Area_km)
mean(D$Area_km)
mean(E$Area_km)

sd(A$Area_km)
sd(B$Area_km)
sd(C$Area_km)
sd(D$Area_km)
sd(E$Area_km)

length(wetland_data$State)

sum(Area_km)
sum(A$Area_km)
sum(B$Area_km)
sum(C$Area_km)
sum(D$Area_km)
sum(E$Area_km)

count(A)
count(B)
count(C)
count(D)
count(E)

#Relative Frequency

barplot_1<-ggplot(data=proportion_data, aes(x=State, y=Relative_Proportion))+
  geom_bar(stat="identity", fill="#98022e")+labs(y="Relative Frequency (%)",x="Successional State")+
  theme_classic()+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
        axis.title.x=element_text(size=20,face='bold'),  
        axis.title.y=element_text(size=20,face='bold'),  
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
        axis.text.y=element_text(size=17,colour='black'))
barplot_1
ggsave(filename = here("Figures","Barplot1_Frequency_State.png"))

#Wetland Area

av1<-lm(Area_km~State)
anova(av1)
par(mfrow=c(1,1))
plot(av1)
shapiro.test(Area_km)
bartlett.test(wetland_data$Area_km~wetland_data$State)
kruskal.test(Area_km~State,data=wetland_data)
dunnTest(Area_km ~ State,data=wetland_data, method="bonferroni")

boxplot_1<-ggplot(wetland_data,aes(x=State,y=Area_km))+theme_classic()+geom_boxplot(notch=FALSE,outlier.shape = NA)+
  labs(y=expression(bold(paste('Wetland Area (km'^2*')'))), x="Successional State")+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
        axis.title.x=element_text(size=20,face='bold'),  
        axis.title.y=element_text(size=20,face='bold'),  
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
        axis.text.y=element_text(size=17,colour='black'))+
  geom_jitter(alpha=0.7,colour='#98022e')
boxplot_1
ggsave(filename = here("Figures","Boxplot1_Area_State.png"))

av2<-lm(Area_Proportion~State)
anova(av2)
par(mfrow=c(2,2))
plot(av2)
shapiro.test(Area_Proportion)
bartlett.test(wetland_data$Area_Proportion~wetland_data$State)
kruskal.test(Area_Proportion~State,data=wetland_data)
dunnTest(Area_Proportion ~ State,data=wetland_data, method="bonferroni")

boxplot_2<-ggplot(wetland_data,aes(x=State,y=Area_Proportion))+theme_classic()+geom_boxplot(notch=FALSE,outlier.shape = NA)+
  labs(y="Proportion of Total Area (%)", x="Successional State")+
  theme(plot.title=element_text(size=16, face="bold", hjust=0.5,lineheight=1.2),  
        axis.title.x=element_text(size=15,face='bold'),  
        axis.title.y=element_text(size=15,face='bold'),  
        axis.text.x=element_text(size=12, vjust=.5,colour='black'),  
        axis.text.y=element_text(size=12,colour='black'))+
  geom_jitter(alpha=0.7,colour='#98022e')
boxplot_2
ggsave(filename = here("Figures","Boxplot2_Proportion_State.png"))

barplot_2<-ggplot(data=proportion_data, aes(x=State, y=Area_Proportion_km))+
  geom_bar(stat="identity", fill="#98022e")+labs(y="Proportion of Total Area (%)",x="Successional State")+
  theme_classic()+
  theme(plot.title=element_text(size=16, face="bold", hjust=0.5,lineheight=1.2),  
        axis.title.x=element_text(size=20,face='bold'),  
        axis.title.y=element_text(size=20,face='bold'),  
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
        axis.text.y=element_text(size=17,colour='black'))
barplot_2
ggsave(filename = here("Figures","Barplot2_Proportion_State.png"))

#Burn Severity -------------------

mean(A_R$RdNBR_mean)
sd(A_R$RdNBR_SD)
mean(B_R$RdNBR_mean)
sd(B_R$RdNBR_SD)
mean(C_R$RdNBR_mean)
sd(C_R$RdNBR_SD)
mean(D_R$RdNBR_mean)
sd(D_R$RdNBR_SD)
mean(E_R$RdNBR_mean)
sd(E_R$RdNBR_SD)

av3<-lm(RdNBR_data$RdNBR_mean~RdNBR_data$State)
anova(av3)
par(mfrow=c(1,1))
plot(av3)
shapiro.test(RdNBR_data$RdNBR_mean)
bartlett.test(RdNBR_data$RdNBR_mean~RdNBR_data$State)
kruskal.test(RdNBR_mean~State,data=RdNBR_data)
dunnTest(RdNBR_mean ~ State,data=RdNBR_data, method="bonferroni")

boxplot_3<-ggplot(RdNBR_data,aes(x=State,y=RdNBR_mean))+theme_classic()+geom_boxplot(notch=FALSE,outlier.shape = NA)+
  labs(y='Mean RdNBR', x="Successional State")+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
        axis.title.x=element_text(size=20,face='bold'),  
        axis.title.y=element_text(size=20,face='bold'),  
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
        axis.text.y=element_text(size=17,colour='black'))+
  geom_jitter(alpha=0.7,colour='#98022e')
boxplot_3
ggsave(filename = here("Figures","Boxplot3_Mean_RdNBR_State.png"))

av4<-lm(RdNBR_SD~State,data=RdNBR_data)
anova(av4)
par(mfrow=c(2,2))
plot(av4)
shapiro.test(RdNBR_data$RdNBR_SD)
bartlett.test(RdNBR_data$RdNBR_SD~RdNBR_data$State)
kruskal.test(RdNBR_SD~State,data=RdNBR_data)
dunnTest(RdNBR_SD ~ State,data=RdNBR_data, method="bonferroni")

mean(A_R$RdNBR_SD)
sd(A_R$RdNBR_SD)
mean(B_R$RdNBR_SD)
sd(B_R$RdNBR_SD)
mean(C_R$RdNBR_SD)
sd(C_R$RdNBR_SD)
mean(D_R$RdNBR_SD)
sd(D_R$RdNBR_SD)
mean(E_R$RdNBR_SD)
sd(E_R$RdNBR_SD)

boxplot_4<-ggplot(RdNBR_data,aes(x=State,y=RdNBR_SD))+theme_classic()+geom_boxplot(notch=FALSE,outlier.shape = NA)+
  labs(y='Standard Deviation of RdNBR', x="Successional State")+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
        axis.title.x=element_text(size=20,face='bold'),  
        axis.title.y=element_text(size=20,face='bold'),  
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
        axis.text.y=element_text(size=17,colour='black'))+
  geom_jitter(alpha=0.7,colour='#98022e')
boxplot_4
ggsave(filename = here("Figures","Boxplot4_SD_RdNBR_State.png"))

cor1<-lm(RdNBR_mean~Area_km,data=RdNBR_data)
summary(cor1)
plot(cor1)
shapiro.test(RdNBR_data$Area_km)
shapiro.test(RdNBR_data$RdNBR_mean)
bartlett.test(RdNBR_data$RdNBR_SD~B_R$RdNBR_mean)
cor1_np<-cor.test(RdNBR_data$Area_km,RdNBR_data$RdNBR_mean,method="kendall")
cor1_np

Mean_plot<-ggplot(RdNBR_data,aes(x=Area_km,y=RdNBR_mean,colour=State))+theme_classic()+geom_point(aes(shape=State))+geom_smooth(method='lm',se=FALSE)+
  scale_colour_manual(values=c('#3C0A0A','#7F0D30','#C10048','red','#F98484'))+labs(y='Mean RdNBR', x=expression(bold(paste('Wetland Area (km'^2*')'))))+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
        axis.title.x=element_text(size=20,face='bold'),  
        axis.title.y=element_text(size=20,face='bold'),  
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
        axis.text.y=element_text(size=17,colour='black'), legend.title = element_text(size = 15.5,face='bold'),
        legend.text = element_text(size=14),legend.position = c(0.82,0.9),legend.key.size = unit(2,"line"),legend.box.background = element_rect(colour = "black"))+scale_shape_manual(values=c(15, 1, 17, 0, 19))+
  labs(colour="Successional State",shape='Successional State')+guides(color = guide_legend(nrow = 2))
Mean_plot
ggsave(filename = here("Figures","Scatter1_Mean_RdNBR_Area.png"))

cor2<-lm(RdNBR_SD~Area_km,data=RdNBR_data)
summary(cor2)
plot(cor2)
shapiro.test(RdNBR_data$RdNBR_SD)
cor2_np<-cor.test(RdNBR_data$Area_km,RdNBR_data$RdNBR_SD,method="kendall")
cor2_np

St_dev_plot<-ggplot(RdNBR_data,aes(x=Area_km,y=RdNBR_SD,colour=State))+theme_classic()+geom_point()+geom_smooth(method='lm',se=FALSE)+
  scale_colour_manual(name="Successional States",values=c('#3C0A0A','#7F0D30','#C10048','red','#F98484'))+
  labs(y='Standard Deviation of RdNBR', x=expression(bold(paste('Wetland Area (km'^2*')'))))+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),
        axis.title.x=element_text(size=20,face='bold'), 
        axis.title.y=element_text(size=20,face='bold'),
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),
        axis.text.y=element_text(size=17,colour='black'), legend.title = element_text(size = 13,face='bold'),
        legend.text = element_text(size=12),legend.position = c(0.85,0.8),legend.box.background = element_rect(colour = "black"))+xlim(0,0.3)+scale_shape_manual(name='Successional States',values=c(15, 1, 17, 0, 19))
St_dev_plot
ggsave(filename = here("Figures","Scatter2_SD_RdNBR_Area.png"))

cor3<-lm(RdNBR_SD~RdNBR_mean,data=RdNBR_data)
summary(cor4)
cor3_np<-cor.test(RdNBR_data$RdNBR_mean,RdNBR_data$RdNBR_SD,method="kendall")
cor3_np

cor3_np_A<-cor.test(A_R$RdNBR_mean,A_R$RdNBR_SD,method="kendall")
cor3_np_A
cor3_np_B<-cor.test(B_R$RdNBR_mean,B_R$RdNBR_SD,method="kendall")
cor3_np_B
cor3_np_C<-cor.test(C_R$RdNBR_mean,C_R$RdNBR_SD,method="kendall")
cor3_np_C
cor3_np_D<-cor.test(D_R$RdNBR_mean,D_R$RdNBR_SD,method="kendall")
cor3_np_D
cor3_np_E<-cor.test(E_R$RdNBR_mean,E_R$RdNBR_SD,method="kendall")
cor3_np_E

SD_Mean_plot<-ggplot(RdNBR_data,aes(x=RdNBR_SD,y=RdNBR_mean))+theme_classic()+geom_point(aes(shape=State,colour=State),size=5)+geom_smooth(method='lm',se=FALSE,colour='black')+
  scale_colour_manual(name="Successional State", values=c('#F98484','red','#C10048','#7F0D30','#3C0A0A'))+
  labs(y='Mean RdNBR', x="Standard Deviation of RdNBR")+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
        axis.title.x=element_text(size=20,face='bold'),  
        axis.title.y=element_text(size=20,face='bold'),  
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
        axis.text.y=element_text(size=17,colour='black'),legend.title = element_text(size = 17,face='bold'),
        legend.text = element_text(size=15),legend.position = c(0.8,0.85),
        legend.box.background = element_rect(colour = "black"))+
  scale_shape_manual(name="Successional State", values=c(15, 1, 17, 0, 19))
SD_Mean_plot
ggsave(filename = here("Figures","Scatter3_Mean_RdNBR_SD_RdNBR.png"))

#Density plot

Mean_density<-ggplot(data=RdNBR_data,aes(x=RdNBR_mean,colour=State,fill=State))+theme_classic()+
  geom_density(alpha=0.4)+scale_colour_manual(name="Successional State",values=c('#FFAD83','#FF5600','#C10048','#7F0D30','#3C0A0A'))+
scale_fill_manual(name="Successional State", values=c('#FFAD83','#FF5600','#C10048','#7F0D30','#3C0A0A'))+
  labs(x="Mean RdNBR",y="Relative Frequency")+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x=element_text(size=20,face='bold'),  
        axis.title.y=element_text(size=20,face='bold'),  
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
        legend.title = element_text(size = 17,face='bold'),
        legend.text = element_text(size=15),legend.position = c(0.85,0.85),
        legend.box.background = element_rect(colour = "black"))
Mean_density
ggsave(filename = here("Figures","Density1_Mean_RdNBR.png"))

Mean_density<-ggplot(data=RdNBR_data,aes(x=RdNBR_mean,y=State,fill=stat(x)))+theme_classic()+
  geom_density_ridges_gradient(scale = 2, size = 0.3, rel_min_height = 0.0001)+scale_colour_manual(name="Successional State",values=c('#FFAD83','#FF5600','#C10048','#7F0D30','#3C0A0A'))+
  scale_fill_viridis_c(name = "Mean RdNBR", option = "C")+
  labs(x="Mean RdNBR",y="State")+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x=element_text(size=20,face='bold'),  
        axis.title.y=element_text(size=20,face='bold'),  
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
        legend.title = element_text(size = 17,face='bold'),
        legend.text = element_text(size=15),legend.position = c(0.85,0.85),
        legend.box.background = element_rect(colour = "black"))
Mean_density
ggsave(filename = here("Figures","Density1_Mean_RdNBR.png"))

#NDWI -------------------

water_filtered<-filter(RdNBR_data,State!="E") #Filter out state E as it should have little to no SD in NDWI
water_filtered

mean(A$SD_NDWI_Pre)
mean(B$SD_NDWI_Pre)
mean(C$SD_NDWI_Pre)
mean(D$SD_NDWI_Pre)
mean(E$SD_NDWI_Pre)

sd(A$SD_NDWI_Pre)
sd(B$SD_NDWI_Pre)
sd(C$SD_NDWI_Pre)
sd(D$SD_NDWI_Pre)
sd(E$SD_NDWI_Pre)

av6_aov<-aov(Mean_NDWI_Pre~State,data=wetland_data)
anova(av6_aov)
par(mfrow=c(2,2))
plot(av6_aov)
shapiro.test(wetland_data$Mean_NDWI_Pre)
bartlett.test(wetland_data$Mean_NDWI_Pre~wetland_data$State)
TukeyHSD(av6_aov, conf.level=.95)

av6b<-aov(Mean_NDWI_Post~State,data=wetland_data)
anova(av6b)
par(mfrow=c(2,2))
plot(av6b)
shapiro.test(wetland_data$Mean_NDWI_Post)
bartlett.test(wetland_data$Mean_NDWI_Post~wetland_data$State)
kruskal.test(Mean_NDWI_Post~State,data=wetland_data)
dunnTest(Mean_NDWI_Post ~ State,data=wetland_data, method="bonferroni")

t_test_data <- data.frame(group = rep(c("Pre-Fire", "Post-Fire"), each = 114),
  NDWI = c(Mean_NDWI_Pre, Mean_NDWI_Post))
t.test(NDWI ~ group, data = t_test_data, paired = TRUE)

boxplot_7<-ggplot(wetland_data,aes(x=State,y=Mean_NDWI_Pre))+theme_classic()+geom_boxplot(notch=FALSE,outlier.shape = NA)+
  labs(y='Mean Pre-Fire NDWI', x="Successional State")+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
        axis.title.x=element_text(size=20,face='bold'),  
        axis.title.y=element_text(size=20,face='bold'),  
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
        axis.text.y=element_text(size=17,colour='black'))+
  geom_jitter(alpha=0.7,colour='#98022e')
boxplot_7
ggsave(filename = here("Figures","Boxplot7_Mean_NDWI_State.png"))

av7<-aov(SD_NDWI_Pre~State,data=wetland_data)
anova(av7)
par(mfrow=c(2,2))
plot(av7)
shapiro.test(wetland_data$SD_NDWI_Pre)
bartlett.test(wetland_data$SD_NDWI_Pre~wetland_data$State)
kruskal.test(SD_NDWI_Pre~State,data=wetland_data)
dunnTest(SD_NDWI_Pre ~ State,data=wetland_data, method="bonferroni")

NDWI.m<- melt(wetland_data,id.vars='State', measure.vars=c('Mean_NDWI_Pre','Mean_NDWI_Post'))
boxplot_7b <- ggplot(data=NDWI.m,aes(x=State, y=value, fill=variable))+
  geom_boxplot(alpha=0.7)+theme_classic()+
  scale_fill_manual(values=c('000000','#98022e'),name='NDWI',labels = c("Pre-Fire","Post-Fire"))+
  labs(y='Mean Pre-Fire NDWI', x="Successional State")+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
        axis.title.x=element_text(size=20,face='bold'),  
        axis.title.y=element_text(size=20,face='bold'),  
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
        axis.text.y=element_text(size=17,colour='black'),legend.title = element_text(size = 17,face='bold'),legend.text = element_text(size=15),
        legend.position = c(0.15,0.78),legend.box.background = element_rect(colour = "black"))
boxplot_7b
ggsave(filename = here("Figures","Boxplot7b_Mean_NDWI_State.png"))

boxplot_8<-ggplot(wetland_data,aes(x=State,y=SD_NDWI_Pre))+theme_classic()+geom_boxplot(notch=FALSE,outlier.shape = NA)+
  labs(y='Standard Deviation of Pre-Fire NDWI', x="Successional State")+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
        axis.title.x=element_text(size=20,face='bold'),  
        axis.title.y=element_text(size=20,face='bold'),  
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
        axis.text.y=element_text(size=17,colour='black'))+
  geom_jitter(alpha=0.7,colour='#98022e')
boxplot_8
ggsave(filename = here("Figures","Boxplot8_SD_NDWI_State.png"))

cor4<-lm(RdNBR_mean~Mean_NDWI_Pre,data=RdNBR_data)
summary(cor4)
cor4_np<-cor.test(RdNBR_mean,Mean_NDWI_Pre,data=RdNBR_data,method="kendall")
cor4_np

RdNBR_NDWI_plot<-ggplot(water_filtered,aes(x=Mean_NDWI_Pre,y=RdNBR_mean))+theme_classic()+geom_point(alpha=0.7,colour='#98022e')+geom_smooth(method='lm',se=FALSE,colour='black')+
  labs(y='Mean RdNBR', x="Mean Pre-Fire NDWI")+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
        axis.title.x=element_text(size=20,face='bold'),  
        axis.title.y=element_text(size=20,face='bold'),  
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
        axis.text.y=element_text(size=17,colour='black'))
RdNBR_NDWI_plot
ggsave(filename = here("Figures","Scatter4_Mean_RdNBR_Mean_NDWI.png"))

cor6<-lm(RdNBR_mean~SD_NDWI_Pre,data=water_filtered)
summary(cor6)
cor6_np<-cor.test(RdNBR_mean,SD_NDWI_Pre,data=water_filtered,method="kendall")
cor6_np

RdNBR_SD_NDWI_Pre_plot<-ggplot(water_filtered,aes(x=SD_NDWI_Pre,y=RdNBR_mean))+theme_classic()+geom_point(alpha=0.7,colour='#98022e')+geom_smooth(method='lm',se=FALSE,colour='black')+
  labs(y='Mean RdNBR', x="Standard Deviation of Pre-Fire NDWI")+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
        axis.title.x=element_text(size=20,face='bold'),  
        axis.title.y=element_text(size=20,face='bold'),  
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
        axis.text.y=element_text(size=17,colour='black'))
RdNBR_SD_NDWI_Pre_plot
ggsave(filename = here("Figures","Scatter5_Mean_RdNBR_SD_NDWI_Pre.png"))

#Density Plot

NDWI_density<-ggplot(data=wetland_data,aes(x=Mean_NDWI_Pre,colour=State,fill=State))+theme_classic()+
  geom_density(alpha=0.4)+scale_colour_manual(name="Successional State",values=c('#FFAD83','#FF5600','#C10048','#7F0D30','#3C0A0A'))+
  scale_fill_manual(name="Successional State", values=c('#FFAD83','#FF5600','#C10048','#7F0D30','#3C0A0A'))+
  labs(x="Mean NDWI",y="Relative Frequency")+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x=element_text(size=15,face='bold'),  
        axis.title.y=element_text(size=15,face='bold'),  
        axis.text.x=element_text(size=12, vjust=.5,colour='black'),  
        legend.title = element_text(size = 13,face='bold'),
        legend.text = element_text(size=12),legend.position = c(0.85,0.85),
        legend.box.background = element_rect(colour = "black"))
NDWI_density
ggsave(filename = here("Figures","Density2_Mean_NDWI.png"))

#NDMI ----------------

mean(A_R$NDMI_mean)
mean(B_R$NDMI_mean)
mean(C_R$NDMI_mean)
mean(D_R$NDMI_mean)
mean(E_R$NDMI_mean)

sd(A_R$NDMI_mean)
sd(B_R$NDMI_mean)
sd(C_R$NDMI_mean)
sd(D_R$NDMI_mean)
sd(E_R$NDMI_mean)

shapiro.test(RdNBR_data$NDMI_mean)
bartlett.test(RdNBR_data$NDMI_mean~RdNBR_data$State)
kruskal.test(NDMI_mean~State,data=RdNBR_data)
dunnTest(NDMI_mean ~ State,data=RdNBR_data, method="bonferroni")

boxplot_14<-ggplot(RdNBR_data,aes(x=State,y=NDMI_mean))+theme_classic()+geom_boxplot(notch=FALSE,outlier.shape = NA)+
  labs(y='Mean Pre-Fire NDMI', x="Successional State")+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
        axis.title.x=element_text(size=20,face='bold'),  
        axis.title.y=element_text(size=20,face='bold'),  
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
        axis.text.y=element_text(size=17,colour='black'))+
  geom_jitter(alpha=0.7,colour='#98022e')
boxplot_14
ggsave(filename = here("Figures","Boxplot7_Mean_NDWI_State.png"))

av7<-aov(SD_NDWI_Pre~State,data=wetland_data)
anova(av7)
par(mfrow=c(2,2))
plot(av7)
shapiro.test(wetland_data$SD_NDWI_Pre)
bartlett.test(wetland_data$SD_NDWI_Pre~wetland_data$State)
kruskal.test(SD_NDWI_Pre~State,data=wetland_data)
dunnTest(SD_NDWI_Pre ~ State,data=wetland_data, method="bonferroni")

NDWI.m<- melt(wetland_data,id.vars='State', measure.vars=c('Mean_NDWI_Pre','Mean_NDWI_Post'))
boxplot_7b <- ggplot(data=NDWI.m,aes(x=State, y=value, fill=variable))+
  geom_boxplot(alpha=0.7)+theme_classic()+
  scale_fill_manual(values=c('000000','#98022e'),name='NDWI',labels = c("Pre-Fire","Post-Fire"))+
  labs(y='Mean NDWI', x="Successional State")+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
        axis.title.x=element_text(size=20,face='bold'),  
        axis.title.y=element_text(size=20,face='bold'),  
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
        axis.text.y=element_text(size=17,colour='black'),legend.title = element_text(size = 17,face='bold'),legend.text = element_text(size=15),
        legend.position = c(0.15,0.78),legend.box.background = element_rect(colour = "black"))
boxplot_7b
ggsave(filename = here("Figures","Boxplot7b_Mean_NDWI_State.png"))

boxplot_8<-ggplot(wetland_data,aes(x=State,y=SD_NDWI_Pre))+theme_classic()+geom_boxplot(notch=FALSE,outlier.shape = NA)+
  labs(y='Standard Deviation of Pre-Fire NDWI', x="Successional State")+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
        axis.title.x=element_text(size=20,face='bold'),  
        axis.title.y=element_text(size=20,face='bold'),  
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
        axis.text.y=element_text(size=17,colour='black'))+
  geom_jitter(alpha=0.7,colour='#98022e')
boxplot_8
ggsave(filename = here("Figures","Boxplot8_SD_NDWI_State.png"))

cor4<-lm(RdNBR_mean~Mean_NDWI_Pre,data=RdNBR_data)
summary(cor4)
cor4_np<-cor.test(RdNBR_mean,Mean_NDWI_Pre,data=RdNBR_data,method="kendall")
cor4_np

RdNBR_NDWI_plot<-ggplot(water_filtered,aes(x=Mean_NDWI_Pre,y=RdNBR_mean))+theme_classic()+geom_point(alpha=0.7,colour='#98022e')+geom_smooth(method='lm',se=FALSE,colour='black')+
  labs(y='Mean RdNBR', x="Mean Pre-Fire NDWI")+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
        axis.title.x=element_text(size=20,face='bold'),  
        axis.title.y=element_text(size=20,face='bold'),  
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
        axis.text.y=element_text(size=17,colour='black'))
RdNBR_NDWI_plot
ggsave(filename = here("Figures","Scatter4_Mean_RdNBR_Mean_NDWI.png"))

cor6<-lm(RdNBR_mean~SD_NDWI_Pre,data=water_filtered)
summary(cor6)
cor6_np<-cor.test(RdNBR_mean,SD_NDWI_Pre,data=water_filtered,method="kendall")
cor6_np

RdNBR_SD_NDWI_Pre_plot<-ggplot(water_filtered,aes(x=SD_NDWI_Pre,y=RdNBR_mean))+theme_classic()+geom_point(alpha=0.7,colour='#98022e')+geom_smooth(method='lm',se=FALSE,colour='black')+
  labs(y='Mean RdNBR', x="Standard Deviation of Pre-Fire NDWI")+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
        axis.title.x=element_text(size=20,face='bold'),  
        axis.title.y=element_text(size=20,face='bold'),  
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
        axis.text.y=element_text(size=17,colour='black'))
RdNBR_SD_NDWI_Pre_plot
ggsave(filename = here("Figures","Scatter5_Mean_RdNBR_SD_NDWI_Pre.png"))

#Density Plot

NDWI_density<-ggplot(data=wetland_data,aes(x=Mean_NDWI_Pre,colour=State,fill=State))+theme_classic()+
  geom_density(alpha=0.4)+scale_colour_manual(name="Successional State",values=c('#FFAD83','#FF5600','#C10048','#7F0D30','#3C0A0A'))+
  scale_fill_manual(name="Successional State", values=c('#FFAD83','#FF5600','#C10048','#7F0D30','#3C0A0A'))+
  labs(x="Mean NDWI",y="Relative Frequency")+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x=element_text(size=15,face='bold'),  
        axis.title.y=element_text(size=15,face='bold'),  
        axis.text.x=element_text(size=12, vjust=.5,colour='black'),  
        legend.title = element_text(size = 13,face='bold'),
        legend.text = element_text(size=12),legend.position = c(0.85,0.85),
        legend.box.background = element_rect(colour = "black"))
NDWI_density
ggsave(filename = here("Figures","Density2_Mean_NDWI.png"))

#Beaver Dams ----------------------

beaver_data<-filter(RdNBR_data,State!="E")
wilcox.test(RdNBR_mean ~ Beaver_Dam, data=beaver_data) 

Dam_Y<-filter(beaver_data,Beaver_Dam=="Y")
Dam_N<-filter(beaver_data,Beaver_Dam=="N")

mean(Dam_Y$RdNBR_mean)
sd(Dam_Y$RdNBR_mean)
mean(Dam_N$RdNBR_mean)
sd(Dam_N$RdNBR_mean)

mean(Dam_Y$Mean_NDWI_Pre)
sd(Dam_Y$Mean_NDWI_Pre)
mean(Dam_N$Mean_NDWI_Pre)
sd(Dam_N$Mean_NDWI_Pre)

barplot_10<-ggplot(data=proportion_data, aes(x=State, y=Dam_Proportion))+
  geom_bar(stat="identity", fill="#98022e")+labs(y="Proportion of Wetlands with a Beaver dam (%)",x="Successional State")+
  theme_classic()+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
        axis.title.x=element_text(size=20,face='bold'),  
        axis.title.y=element_text(size=20,face='bold'),  
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
        axis.text.y=element_text(size=17,colour='black'))
barplot_10
ggsave(filename = here("Figures","Barplot10_Dam_Proportion.png"))

boxplot_9<-ggplot(data=beaver_data, aes(x=Beaver_Dam, y=RdNBR_mean))+
  geom_boxplot(outlier.shape = NA)+labs(y="Mean RdNBR",x="Beaver Dam")+
  theme_classic()+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
        axis.title.x=element_text(size=20,face='bold'),  
        axis.title.y=element_text(size=20,face='bold'),  
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
        axis.text.y=element_text(size=17,colour='black'))+geom_jitter(alpha=0.7,colour='#98022e')
boxplot_9
ggsave(filename = here("Figures","Boxplot9_Dam_RdNBR.png"))

wilcox.test(Mean_NDWI_Pre ~ Beaver_Dam, data=beaver_data)

boxplot_10<-ggplot(data=beaver_data, aes(x=Beaver_Dam, y=Mean_NDWI_Pre))+
  geom_boxplot(outlier.shape = NA)+labs(y="Mean Pre-Fire NDWI",x="Beaver Dam")+
  theme_classic()+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
        axis.title.x=element_text(size=20,face='bold'),  
        axis.title.y=element_text(size=20,face='bold'),  
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
        axis.text.y=element_text(size=17,colour='black'))+geom_jitter(alpha=0.7,colour='#98022e')
boxplot_10
ggsave(filename = here("Figures","Boxplot10_Dam_Mean_NDWI_Pre.png"))

boxplot_11<-ggplot(data=beaver_data, aes(x=Beaver_Dam, y=Mean_NDWI_Post))+
  geom_boxplot(outlier.shape = NA)+labs(y="Mean Post-Fire NDWI",x="Beaver Dam")+
  theme_classic()+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
        axis.title.x=element_text(size=20,face='bold'),  
        axis.title.y=element_text(size=20,face='bold'),  
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
        axis.text.y=element_text(size=17,colour='black'))+geom_jitter(alpha=0.7,colour='#98022e')
boxplot_11
ggsave(filename = here("Figures","Boxplot11_Dam_Mean_NDWI_Post.png"))

boxplot_13<-ggplot(data=beaver_data,aes(x=Beaver_Dam,y=Water_Proportion))+
                    geom_boxplot(outlier.shape = NA)+labs(y="Proportion of Open Water (%)",x="Beaver Dam")+
                    theme_classic()+
                    theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
                          axis.title.x=element_text(size=20,face='bold'),  
                          axis.title.y=element_text(size=20,face='bold'),  
                          axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
                          axis.text.y=element_text(size=17,colour='black'))+geom_jitter(alpha=0.7,colour='#98022e')
boxplot_13

#Margin versus Middle ----------------------

cor7<-lm(Distance_m~RdNBR,data=Distance_data_positive)
summary(cor7)
plot(cor7)
cor7_np<-cor.test(Distance_data_positive$Distance_m,Distance_data$RdNBR,method="kendall")
cor7_np

Margin_middle_plot<-ggplot(E_margin,aes(x=sqrt(Distance_m),y=RdNBR))+theme_classic()+geom_point(alpha=0.7,colour='#98022e')+
  labs(y='RdNBR', x="Square Root of Distance from wetland edge (m)")+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
        axis.title.x=element_text(size=20,face='bold'),  
        axis.title.y=element_text(size=20,face='bold'),  
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
        axis.text.y=element_text(size=17,colour='black'))
Margin_middle_plot
ggsave(filename = here("Figures","Scatter6_RdNBR_Distance.png"))

A_margin<-filter(Distance_data_positive,State=="A")
B_margin<-filter(Distance_data_positive,State=="B")
C_margin<-filter(Distance_data_positive,State=="C")
D_margin<-filter(Distance_data_positive,State=="D")
E_margin<-filter(Distance_data_positive,State=="E")

Margin_middle_plot<-ggplot(Distance_data_positive,aes(x=sqrt(Distance_m),y=RdNBR))+theme_classic()+geom_point(alpha=0.7,colour='#98022e')+
  labs(y='RdNBR', x="Square Root of Distance from edge (m)")+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
        axis.title.x=element_text(size=20,face='bold'),  
        axis.title.y=element_text(size=20,face='bold'),  
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
        axis.text.y=element_text(size=17,colour='black'))
Margin_middle_plot

Margin_data <- Distance_data_positive %>%
  filter(between(Distance_m, 0, 5))
Middle_data <- Distance_data_positive %>%
  filter(between(Distance_m, 5, 200))

Margin_middle_plot<-ggplot(Distance_data_positive,aes(x=Distance_m,y=RdNBR))+theme_classic()+geom_area(alpha=0.7,colour='#D3D3D3',fill='#D3D3D3')+
  stat_smooth(geom = 'area', method = 'loess', span = 1/8,alpha = 1/2, fill = '#98022e')+ 
  labs(title='State B',y='RdNBR', x="Distance from edge (m)")+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
        axis.title.x=element_text(size=20,face='bold'),  
        axis.title.y=element_text(size=20,face='bold'),  
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
        axis.text.y=element_text(size=17,colour='black'))
Margin_middle_plot

boxplot_12<-ggplot(data=Distance_data_positive, aes(x=Margin, y=RdNBR_mean))+
  geom_boxplot(outlier.shape = NA)+labs(title="Margin < 3m",y="Mean RdNBR",x="Margin")+
  theme_classic()+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
        axis.title.x=element_text(size=20,face='bold'),  
        axis.title.y=element_text(size=20,face='bold'),  
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
        axis.text.y=element_text(size=17,colour='black'))
boxplot_12
ggsave(filename = here("Figures","Boxplot11_Dam_Mean_NDWI_Post.png"))

#Model fitting

theta.0 <- min(Distance_data_positive$RdNBR) * 0.5 #Estimating appropriate initial values for exponential decay model
model.0 <- lm(log(RdNBR - theta.0) ~ Distance_m, data=Distance_data_positive)  
alpha.0 <- exp(coef(model.0)[1])
beta.0 <- coef(model.0)[2]
start <- list(alpha = alpha.0, beta = beta.0, theta = theta.0)
start
model <- nls(RdNBR ~ alpha * exp(beta * Distance_m) + theta , data = Distance_data_positive, start = start)

plot(sqrt(Distance_data_positive$Distance_m), (Distance_data_positive$RdNBR)) #Square root transformation works best
lines(Distance_data_positive$Distance_m, predict(model, list(x = Distance_data_positive$Distance_m)), col = 'skyblue', lwd = 3)

fit <- nls(RdNBR ~ SSasymp(Distance_m, yf, y0, log_alpha), data = Distance_data_positive) #Self-starting reciprocal function
fit
summary(fit)
plot(Distance_m,RdNBR)
lines(Distance_data_positive$Distance_m,predict(fit))

E_sqrt_Distance_m<-sqrt(E_margin$Distance_m) #Piecewise Linear Regression
fit <- lm(RdNBR ~ E_sqrt_Distance_m, data=E_margin)
segmented.fit <- segmented(fit, seg.Z = ~E_sqrt_Distance_m, psi=2)
summary(segmented.fit)

plot(E_sqrt_Distance_m, (E_margin$RdNBR), pch=16,col='steelblue')
plot(segmented.fit, add=T)

#Summary ------------------------

summary_cor<-rcorr(Correlation_data,type='spearman')

Full_correlation_data<-model.matrix(~0+., data=Correlation_data)
Full_corr<-cor(Full_correlation_data,method='spearman')
p<-corrplot(Full_corr,method='circle')

Numerical_correlation_data<-subset(Correlation_data,select=-c(State,Beaver_Dam))
Numerical_correlation_data

Num_corr<-cor(Numerical_correlation_data,method='spearman')
p<-corrplot(Num_corr,method='circle')

plot_ly()%>%add_trace(x=~NDWI_veg_pre_mean,y=~Mean_NDWI_Pre,z=~RdNBR_mean,type='scatter3d',colour=State,flatshading=TRUE)
