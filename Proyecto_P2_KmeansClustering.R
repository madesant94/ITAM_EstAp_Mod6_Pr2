# Script por Madeleine Santiago 17.05.2021
# Script en R para el Proyecto Parte 2. del Modulo 6. Análisis Multivariado del
# Dip en Estadistica Aplicada ITAM 2020-2021
# ----------
rm(list = ls())
library(tidyverse)
library(wesanderson)
library(reshape)
library(corrplot)
require(reshape2)
library(ggplot2)
library("ggsci")
library(GGally)
library(cluster)
library(factoextra)

# Análisis de Cumulos - k means
happy <- read.csv("Proyecto/Happiness_Index.csv",header=TRUE,
                  row.names=1,stringsAsFactors = TRUE)
#View(happy)
#colnames(happy)

cuant_happy<-data.matrix(happy[,2:7])

#Scale variable
happyStd<-scale(cuant_happy,center=TRUE,scale=TRUE)
pairs(happyStd) #Correlation plot

#Apply kmeans function to different n of clusters
#Save the Within Total Sum of Squares to find optimal n of clusters later
wss<-numeric(0)
for(i in 1:25) {
  W<-kmeans(happyStd,i)$tot.withinss
  wss<-c(wss,W)
  #print(W[1])
}

plot(wss) #Simple plot (Knee method)

#Plot with fviz_nbclust (GGplot based)
# Knee Method
p<-fviz_nbclust(happyStd,kmeans, k.max=25,
             method = "wss")
p+ geom_vline(xintercept=3, linetype="dashed", color = "indianred")
  
#Silhouette Method
p<-fviz_nbclust(happyStd, kmeans,k.max=25,
                method = "silhouette")
p+ geom_vline(xintercept=3, linetype="dashed", color = "indianred")

# Gap State (FYI)
fviz_nbclust(happyStd, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")


# I chose to continue with n=3
result<-kmeans(happyStd,3)
#Data regarding Clusters, their centers and size
result$cluster
result$centers
result$size
pairs(cuant_happy,col=result$cluster) #Plot correlation w. clusters

dev.off()
happy['Clusters']<-result$cluster
#Plot Data with 2 vars of interest
p<-ggplot(happy,aes(x=Log.GDP.per.capita,
                    y=Social.support,
                    colour=factor(Clusters)))+
  geom_point()+ 
  ggtitle('Clusters')
  #geom_hline(yintercept=0.7772, linetype="dashed", color = "dodgerblue4")+
  #geom_vline(xintercept=3, linetype="dashed", color = "dodgerblue4")
p+labs(x = "Log.GDP",y='Social.support')+theme_light()

#Plot Clusters with Fviz
p1 <- fviz_cluster(result, geom = "point", 
                   data = happyStd,palette = "jco") +
  ggtitle("k = 3")
p1

#Perform ACP analysis (to understand previous and future plots)
resultACP<-prcomp(cuant_happy, center = TRUE, scale = TRUE)
summary(resultACP)
plot(resultACP$x[,1],resultACP$x[,2], 
     xlab="PC1 (51.31%)", ylab = "PC2 (22.34%)", main = "PC1 / PC2 - plot")

#Plot clusters in 2D (ACP Dim 1 and Dim2)
p<-fviz_pca_ind(resultACP, geom.ind = "point",
                pointshape = 21,pointsize = 2, 
                fill.ind = factor(happy$Clusters), 
                col.ind = "black", 
                palette = "jco", 
                addEllipses = TRUE,
                label = "var",
                col.var = "black",
                repel = TRUE,
                legend.title = "Clusters") +
  ggtitle("Cluster Analysis") +
  theme(plot.title = element_text(hjust = 0.5))
p
resultACP
dev.off()
#Correlation between ACPs and variables of interest
#Reduce dimensions
fviz_pca_var(resultACP,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

dim(happyStd)
dim(result$cluster)

#Add dimensions and clusters to standarized data.frame (for plotting)
happyStd<-scale(cuant_happy,center=TRUE,scale=TRUE)
happyStd<-data.frame(happyStd)
happyStd$Clusters<-happy$Cluster

happyStd$Regional.indicator<-happy$Regional.indicator
happyStd$Country.name<-happy$Country.name
happyStd['Dim1']<-resultACP$x[,1]
happyStd['Dim2']<-resultACP$x[,2]
print(unique(happyStd$Regional.indicator))

#**** Plot individuals (contries) by clusters)
#*Color scheme
# 1. Azure 1DACE8 - South Asia
# 2. Dark Slate Blue #1C366B - Central Europe
# 3. Tomato #F24D29 - Middle East
# 4. Blush #F7B0AA - Latin America
# 5. Pale Peach #FDDDA4 Commonwealth
# 6. Grayish Teal #76A08A - North America
# 7. Faded Blue #7496D2 - Western Europe
# 8. Pine #2E604A - Sub Saharan Africa
# 9. Brick #B62A3D - Southeast Asia
# 10. Dull Orange #CC8B3C - East Asia
MyPalette <- c(Fair = "#5DD0B9", Good = "#E1E7E9", "Very Good" = "#1f78b4", Premium =  "#a6cee3", Ideal = "#de77ae")

C3<-c('South Asia'='#1DACE8','Central and Eastern Europe'='#1C366B',
      'Middle East and North Africa'='#F24D29','Latin America and Caribbean'='#F7B0AA',
      'Commonwealth of Independent States'='#FDDDA4','North America and ANZ'='#76A08A',
      'Western Europe'='#7496D2','Sub-Saharan Africa'='#2E604A',
      'Southeast Asia'='#B62A3D','East Asia'='#CC8B3C')
sub_df1<-subset(happyStd, happyStd$Clusters== 1) #sub_df for Cluster1
print(unique(sub_df1$Regional.indicator))
p<-ggplot(sub_df1, aes(x=Dim1, y=Dim2, aes(colour=Regional.indicator))) +
  geom_point(aes(colour=Regional.indicator,size=7.5,alpha=0.75))+
  geom_text(aes(label=Country.name),size=2,colour='black',hjust=0,vjust=0)+ 
  #geom_text(aes(label=Country.name,colour=Regional.indicator))+ 
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  geom_vline(xintercept=0, linetype="dashed", color = "red")+
  ggtitle('Individuals Cluster 1')+
  scale_color_manual(values=C3)
#p+theme_light()
p+xlim(-.25,4)+ylim(-3, 4)+theme_light()

sub_df2<-subset(happyStd, happyStd$Clusters== 2) #sub_df for Cluster2
p<-ggplot(sub_df2, aes(x=Dim1, y=Dim2, aes(colour=Regional.indicator))) +
  geom_point(aes(colour=Regional.indicator,size=7.5,alpha=0.75))+
  #geom_text(aes(label=Country.name),size=2,colour='black',hjust=0,vjust=0)+ 
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  geom_vline(xintercept=0, linetype="dashed", color = "red")+
  ggtitle('Individuals Cluster 2')+
  scale_color_manual(values=C3)
p+theme_light()
p+xlim(-1.75,2.25)+ylim(-2.5, 3.5)+theme_light()

sub_df3<-subset(happyStd, happyStd$Clusters== 3) #sub_df for Cluster3
p<-ggplot(sub_df3, aes(x=Dim1, y=Dim2, aes(colour=Regional.indicator))) +
  geom_point(aes(colour=Regional.indicator,size=7.5,alpha=0.75))+
  geom_text(aes(label=Country.name),size=2,colour='black',hjust=0,vjust=0)+ 
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  geom_vline(xintercept=0, linetype="dashed", color = "red")+
  ggtitle('Individuals Cluster 3')+
  scale_color_manual(values=C3)
p+theme_light()
p+xlim(-5,0)+ylim(-2.75, 3.75)+theme_light()


#Plot Values for each standarized variable according to Cluster 
p <- ggparcoord(data = happyStd, columns = c(1:6), groupColumn = "Clusters",
                showPoints=FALSE,alpha=0.25,
                mapping=aes(color=as.factor(Clusters))) +
  #scale_color_discrete("Clusters",labels=levels("Clusters"))+
  scale_color_jco("default")+
  labs(x = "happiness constituent", y = "value", title = "Clustering")
p+theme_light()+theme(axis.text.x = element_text(angle=90))


Regions3 <- table(sub_df3$Regional.indicator)
Regions3
for (i in Regions3 ){
  print(i)
}

dim(happyStd)
