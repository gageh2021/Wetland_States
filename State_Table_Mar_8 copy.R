pacman::p_load(dplyr,ggplot2,ggpubr,FSA,RColorBrewer)

wetland_data<-read.table(file.choose(),header=T,sep=",")
attach(wetland_data)
as.factor(State)

proportion_data<-read.table(file.choose(),header=T,sep=",")
attach(proportion_data)

#Digitization Results (Wetland States) -------------------

A<-filter(wetland_data,State=="A")
B<-filter(wetland_data,State=="B")
C<-filter(wetland_data,State=="C")
D<-filter(wetland_data,State=="D")
E<-filter(wetland_data,State=="E")

mean(A$Area_km)
mean(B$Area_km)
mean(C$Area_km)
mean(D$Area_km)
mean(E$Area_km)

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
  geom_bar(stat="identity", fill="#98022e")+labs(y="Relative Frequency (%)",x="Successional Stage")+
  theme_classic()+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  # title
        axis.title.x=element_text(size=20,face='bold'),  # X axis title
        axis.title.y=element_text(size=20,face='bold'),  # Y axis title
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  # X axis text
        axis.text.y=element_text(size=17,colour='black'))
barplot_1

#Wetland Area versus State

av1<-lm(Area_km~State)
anova(av1)
par(mfrow=c(1,1))
shapiro.test(Area_km)
bartlett.test(State~Area_km)
kruskal.test(Area_km~State,data=wetland_data)
dunnTest(Area_km ~ State,data=wetland_data, method="bonferroni")

boxplot_1<-ggplot(wetland_data,aes(x=State,y=Area_km))+theme_classic()+geom_boxplot(notch=FALSE,outlier.shape = NA)+
  labs(y=expression(bold(paste('Wetland Area (km'^2*')'))), x="Successional Stage")+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  # title
        axis.title.x=element_text(size=20,face='bold'),  # X axis title
        axis.title.y=element_text(size=20,face='bold'),  # Y axis title
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  # X axis text
        axis.text.y=element_text(size=17,colour='black'))+
  geom_jitter(alpha=0.7,colour='#98022e')
boxplot_1

av2<-lm(Area_Proportion~State)
anova(av2)
par(mfrow=c(2,2))
plot(av2)
shapiro.test(Area_Proportion)
bartlett.test(State~Area_Proportion)
kruskal.test(Area_Proportion~State,data=wetland_data)
dunnTest(Area_Proportion ~ State,data=wetland_data, method="bonferroni")

boxplot_2<-ggplot(wetland_data,aes(x=State,y=Area_Proportion))+theme_classic()+geom_boxplot(notch=FALSE,outlier.shape = NA)+
  labs(y="Proportion of Total Area (%)", x="Successional Stage")+
  theme(plot.title=element_text(size=16, face="bold", hjust=0.5,lineheight=1.2),  # title
        axis.title.x=element_text(size=15,face='bold'),  # X axis title
        axis.title.y=element_text(size=15,face='bold'),  # Y axis title
        axis.text.x=element_text(size=12, vjust=.5,colour='black'),  # X axis text
        axis.text.y=element_text(size=12,colour='black'))+
  geom_jitter(alpha=0.7,colour='#98022e')
boxplot_2

barplot_2<-ggplot(data=proportion_data, aes(x=State, y=Area_Proportion_km))+
  geom_bar(stat="identity", fill="#98022e")+labs(y="Proportion of Total Area (%)",x="Successional Stage")+
  theme_classic()+
  theme(plot.title=element_text(size=16, face="bold", hjust=0.5,lineheight=1.2),  # title
        axis.title.x=element_text(size=20,face='bold'),  # X axis title
        axis.title.y=element_text(size=20,face='bold'),  # Y axis title
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  # X axis text
        axis.text.y=element_text(size=17,colour='black'))
barplot_2

#Burn Severity -------------------

av3<-lm(RdNBR_mean~State)
anova(av3)
par(mfrow=c(1,1))
plot(av3)
shapiro.test(wetland_data$RdNBR_mean)
bartlett.test(RdNBR_mean~State)
kruskal.test(RdNBR_mean~State,data=wetland_data)
dunnTest(RdNBR_mean ~ State,data=wetland_Data, method="bonferroni")

boxplot_3<-ggplot(wetland_data,aes(x=State,y=RdNBR_mean))+theme_classic()+geom_boxplot(notch=FALSE,outlier.shape = NA)+
  labs(y='Mean RdNBR', x="Successional Stage")+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  # title
        axis.title.x=element_text(size=20,face='bold'),  # X axis title
        axis.title.y=element_text(size=20,face='bold'),  # Y axis title
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  # X axis text
        axis.text.y=element_text(size=17,colour='black'))+
  geom_jitter(alpha=0.7,colour='#98022e')
boxplot_3

av4<-lm(RdNBR_SD~State,data=wetland_data)
anova(av4)
par(mfrow=c(1,1))
plot(av4)
shapiro.test(nbr_data$RdNBR_SD)
bartlett.test(RdNBR_SD~State)
kruskal.test(RdNBR_SD~State,data=wetland_data)
dunnTest(RdNBR_SD ~ State,data=wetland_data, method="bonferroni")

boxplot_4<-ggplot(wetland_data,aes(x=State,y=RdNBR_SD))+theme_classic()+geom_boxplot(notch=FALSE,outlier.shape = NA)+
  labs(y='Standard Deviation of RdNBR', x="Successional Stage")+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  # title
        axis.title.x=element_text(size=15,face='bold'),  # X axis title
        axis.title.y=element_text(size=15,face='bold'),  # Y axis title
        axis.text.x=element_text(size=12, vjust=.5,colour='black'),  # X axis text
        axis.text.y=element_text(size=12,colour='black'))+
  geom_jitter(alpha=0.7,colour='#98022e')
boxplot_4

av5<-lm(RdNBR_max~State)
anova(av5)
par(mfrow=c(1,1))
plot(av5)
shapiro.test(wetland_data$RdNBR_max)
bartlett.test(RdNBR_max~State)
kruskal.test(RdNBR_max~State,data=wetland_data)
dunnTest(RdNBR_max ~ State,data=wetland_data, method="bonferroni")

boxplot_5<-ggplot(wetland_data,aes(x=State,y=RdNBR_max))+theme_classic()+geom_boxplot(notch=FALSE,outlier.shape = NA)+
  labs(y='Maximum RdNBR', x="Successional Stage")+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  # title
        axis.title.x=element_text(size=15,face='bold'),  # X axis title
        axis.title.y=element_text(size=15,face='bold'),  # Y axis title
        axis.text.x=element_text(size=12, vjust=.5,colour='black'),  # X axis text
        axis.text.y=element_text(size=12,colour='black'))+
  geom_jitter(alpha=0.7,colour='#98022e')
boxplot_5

av6<-lm(RdNBR_min~State)
anova(av6)
par(mfrow=c(1,1))
plot(av6)
shapiro.test(wetland_data$RdNBR_min)
bartlett.test(RdNBR_min~State)
kruskal.test(RdNBR_min~State,data=wetland_data)
dunnTest(RdNBR_min ~ State,data=wetland_data, method="bonferroni")

boxplot_6<-ggplot(wetland_data,aes(x=State,y=RdNBR_min,colour=State))+geom_boxplot(notch=FALSE,outlier.shape = NA)+
  labs(title="Successional State versus Minimum RdNBR Value", y='Minimum RdNBR Value', x="Successional Stage")+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  # title
        axis.title.x=element_text(size=15,face='bold'),  # X axis title
        axis.title.y=element_text(size=15,face='bold'),  # Y axis title
        axis.text.x=element_text(size=12, vjust=.5,colour='black'),  # X axis text
        axis.text.y=element_text(size=12,colour='black'))+
  geom_jitter(alpha=0.7)
boxplot_6

cor1<-lm(RdNBR_mean~Area_km+State,data=wetland_data)
summary(cor1)

cor2<-lm(RdNBR_mean~Area_km,data=wetland_data)
summary(cor2)
plot(cor2)
shapiro.test(wetland_data$Area_km)
shapiro.test(wetland_data$RdNBR_mean)
cor2_np<-cor.test(Area_km,RdNBR_mean,method="kendall")
cor2_np

Mean_plot<-ggplot(wetland_data,aes(x=Area_km,y=RdNBR_mean,colour=State))+theme_classic()+geom_point(aes(shape=State))+geom_smooth(method='lm',se=FALSE)+
  scale_colour_manual(values=c('#3C0A0A','#7F0D30','#C10048','red','#F98484'))+labs(y='Mean RdNBR', x=expression(bold(paste('Wetland Area (km'^2*')'))))+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  # title
        axis.title.x=element_text(size=20,face='bold'),  # X axis title
        axis.title.y=element_text(size=20,face='bold'),  # Y axis title
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  # X axis text
        axis.text.y=element_text(size=17,colour='black'), legend.title = element_text(size = 13,face='bold'),
        legend.text = element_text(size=12),legend.position = c(0.85,0.9),legend.box.background = element_rect(colour = "black"))+scale_shape_manual(values=c(15, 1, 17, 0, 19))+
  labs(colour="Successional Stage",shape='Successional Stage')+guides(color = guide_legend(nrow = 2))
Mean_plot

cor3<-lm(RdNBR_SD~Area_km,data=wetland_data)
summary(cor3)
plot(cor3)
shapiro.test(wetland_data$RdNBR_SD)
cor3_np<-cor.test(Area_km,RdNBR_SD,method="kendall")
cor3_np

St_dev_plot<-ggplot(wetland_data,aes(x=Area_km,y=RdNBR_SD,colour=State))+geom_point()+geom_smooth(method='lm',se=FALSE)+
  labs(title="Wetland area versus Standard Deviation of RdNBR", y='Standard Deviation', x=expression(paste('Area (km'^2*')')))+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  # title
        axis.title.x=element_text(size=15,face='bold'),  # X axis title
        axis.title.y=element_text(size=15,face='bold'),  # Y axis title
        axis.text.x=element_text(size=12, vjust=.5,colour='black'),  # X axis text
        axis.text.y=element_text(size=12,colour='black'))+xlim(0,0.3)
St_dev_plot

cor4<-lm(RdNBR_mean~RdNBR_SD,data=wetland_data)
summary(cor4)
cor4_np<-cor.test(RdNBR_mean,RdNBR_SD,method="kendall")
cor4_np

SD_Mean_plot<-ggplot(wetland_data,aes(x=RdNBR_mean,y=RdNBR_SD))+theme_classic()+geom_point(aes(colour=State,shape=State))+geom_smooth(method='lm',se=FALSE,colour="black")+
  scale_colour_manual(name="Successional Stage", values=c('#F98484','red','#C10048','#7F0D30','#3C0A0A'))+
  labs(y='Standard Deviation of RdNBR', x="Mean RdNBR")+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  # title
        axis.title.x=element_text(size=20,face='bold'),  # X axis title
        axis.title.y=element_text(size=20,face='bold'),  # Y axis title
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  # X axis text
        axis.text.y=element_text(size=17,colour='black'),legend.title = element_text(size = 17,face='bold'),
        legend.text = element_text(size=15),legend.position = c(0.8,0.85),
        legend.box.background = element_rect(colour = "black"))+
  scale_shape_manual(name="Successional Stage", values=c(15, 1, 17, 0, 19))
SD_Mean_plot

#Density plots

plot_multi_histogram <- function(df, feature, label_column) {
  plt <- ggplot(df, aes(x=eval(parse(text=feature)), fill=eval(parse(text=label_column)))) +
    geom_histogram(alpha=0.7, position="identity", aes(y = ..density..), color="black") +
    geom_density(alpha=0.7) +
    geom_vline(aes(xintercept=mean(eval(parse(text=feature)))), color="black", linetype="dashed", size=1) +
    labs(x=feature, y = "Density")
  plt + guides(fill=guide_legend(title=label_column))
}

Mean_histogram<-plot_multi_histogram(wetland_data, 'RdNBR_mean', 'State')
Mean_histogram

Mean_density<-ggplot(data=wetland_data,aes(x=RdNBR_mean,colour=State,fill=State))+theme_classic()+
  geom_density(alpha=0.4)+scale_colour_manual(name="Successional Stage",values=c('#FFAD83','#FF5600','#C10048','#7F0D30','#3C0A0A'))+
scale_fill_manual(name="Successional Stage", values=c('#FFAD83','#FF5600','#C10048','#7F0D30','#3C0A0A'))+
  labs(x="Mean RdNBR",y="Relative Frequency")+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  # title
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x=element_text(size=20,face='bold'),  # X axis title
        axis.title.y=element_text(size=20,face='bold'),  # Y axis title
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  # X axis text
        legend.title = element_text(size = 17,face='bold'),
        legend.text = element_text(size=15),legend.position = c(0.85,0.85),
        legend.box.background = element_rect(colour = "black"))
Mean_density

Area_density<-ggplot(data=wetland_data,aes(x=Area_km,colour=State,fill=State))+theme_classic()+
  geom_density(alpha=0.4)+scale_colour_manual(name="Successional Stage",values=c('#FFAD83','#FF5600','#C10048','#7F0D30','#3C0A0A'))+
  scale_fill_manual(name="Successional Stage", values=c('#FFAD83','#FF5600','#C10048','#7F0D30','#3C0A0A'))+
  labs(x="Mean RdNBR",y=expression(bold(paste("Area (km"^2*")"))))+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  # title
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x=element_text(size=15,face='bold'),  # X axis title
        axis.title.y=element_text(size=15,face='bold'),  # Y axis title
        axis.text.x=element_text(size=12, vjust=.5,colour='black'),  # X axis text
        legend.title = element_text(size = 13,face='bold'),
        legend.text = element_text(size=12),legend.position = c(0.85,0.85),
        legend.box.background = element_rect(colour = "black"))+xlim(0,0.2)
Area_density

NDWI_density<-ggplot(data=wetland_data,aes(x=NDWI_mean,colour=State,fill=State))+theme_classic()+
  geom_density(alpha=0.4)+scale_colour_manual(name="Successional Stage",values=c('#FFAD83','#FF5600','#C10048','#7F0D30','#3C0A0A'))+
  scale_fill_manual(name="Successional Stage", values=c('#FFAD83','#FF5600','#C10048','#7F0D30','#3C0A0A'))+
  labs(x="Mean NDWI",y="Relative Frequency")+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  # title
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x=element_text(size=15,face='bold'),  # X axis title
        axis.title.y=element_text(size=15,face='bold'),  # Y axis title
        axis.text.x=element_text(size=12, vjust=.5,colour='black'),  # X axis text
        legend.title = element_text(size = 13,face='bold'),
        legend.text = element_text(size=12),legend.position = c(0.85,0.85),
        legend.box.background = element_rect(colour = "black"))
NDWI_density

#NDWI -------------------

water_filtered<-filter(wetland_data,State!="E")
water_filtered

av6<-lm(NDWI_mean~State)
anova(av6)
par(mfrow=c(2,2))
plot(av6)
shapiro.test(wetland_data$NDWI_mean)
bartlett.test(NDWI_mean~State)
av6_aov<-aov(NDWI_mean~State,data=wetland_data)
TukeyHSD(av6_aov, conf.level=.95)
#kruskal.test(NDWI_mean~State,data=wetland_data)
#dunnTest(NDWI_mean ~ State,data=wetland_data, method="bonferroni")

boxplot_7<-ggplot(wetland_data,aes(x=State,y=NDWI_mean))+theme_classic()+geom_boxplot(notch=FALSE,outlier.shape = NA)+
  labs(y='Mean Pre-Fire NDWI', x="Successional Stage")+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  # title
        axis.title.x=element_text(size=20,face='bold'),  # X axis title
        axis.title.y=element_text(size=20,face='bold'),  # Y axis title
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  # X axis text
        axis.text.y=element_text(size=17,colour='black'))+
  geom_jitter(alpha=0.7,colour='#98022e')
boxplot_7

av7<-lm(NDWI_SD~State,data=water_filtered)
anova(av7)
par(mfrow=c(2,2))
plot(av7)
shapiro.test(wetland_data$NDWI_SD)
bartlett.test(NDWI_SD~State)
av7_aov<-aov(NDWI_SD~State,data=wetland_data)
TukeyHSD(av7_aov, conf.level=.95)
#kruskal.test(NDWI_mean~State,data=wetland_data)
#dunnTest(NDWI_mean ~ State,data=wetland_data, method="bonferroni")

boxplot_8<-ggplot(wetland_data,aes(x=State,y=NDWI_SD))+theme_classic()+geom_boxplot(notch=FALSE,outlier.shape = NA)+
  labs(y='Standard Deviation of Pre-Fire NDWI', x="Successional Stage")+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  # title
        axis.title.x=element_text(size=15,face='bold'),  # X axis title
        axis.title.y=element_text(size=15,face='bold'),  # Y axis title
        axis.text.x=element_text(size=12, vjust=.5,colour='black'),  # X axis text
        axis.text.y=element_text(size=12,colour='black'))+
  geom_jitter(alpha=0.7,colour='#98022e')
boxplot_8

cor5<-lm(RdNBR_mean~NDWI_mean,data=water_filtered)
summary(cor5)
cor5_np<-cor.test(RdNBR_mean,NDWI_mean,data=water_filtered,method="kendall")
cor5_np

RdNBR_NDWI_plot<-ggplot(water_filtered,aes(x=NDWI_mean,y=RdNBR_mean))+theme_classic()+geom_point(alpha=0.7,colour='#98022e')+geom_smooth(method='lm',se=FALSE,colour='black')+
  labs(y='Mean RdNBR', x="Mean Pre-Fire NDWI")+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  # title
        axis.title.x=element_text(size=20,face='bold'),  # X axis title
        axis.title.y=element_text(size=20,face='bold'),  # Y axis title
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  # X axis text
        axis.text.y=element_text(size=17,colour='black'))
RdNBR_NDWI_plot

cor6<-lm(RdNBR_mean~NDWI_SD,data=water_filtered)
summary(cor6)
cor6_np<-cor.test(RdNBR_mean,NDWI_SD,data=water_filtered,method="kendall")
cor6_np

RdNBR_NDWI_SD_plot<-ggplot(water_filtered,aes(x=NDWI_SD,y=RdNBR_mean))+geom_point()+geom_smooth(method='lm',se=FALSE)+
  labs(title="Mean RdNBR versus Standard Deviation of Pre-Fire NDWI", y='Mean RdNBR', x="Standard Deviation of Pre-Fire NDWI")+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  # title
        axis.title.x=element_text(size=15,face='bold'),  # X axis title
        axis.title.y=element_text(size=15,face='bold'),  # Y axis title
        axis.text.x=element_text(size=12, vjust=.5,colour='black'),  # X axis text
        axis.text.y=element_text(size=12,colour='black'))
RdNBR_NDWI_SD_plot


