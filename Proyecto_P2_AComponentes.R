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
library(ca)


# Análisis de Cumulos - k means
happy <- read.csv("Proyecto/Happiness_Index.csv",header=TRUE,
                  row.names=1,stringsAsFactors = TRUE)

#Converting some quant variables to Qual variables
labs <- cut(happy$Social.support, 
            breaks = c(0, 0.25, .5, .75, 1), 
            labels = c("VLow Support", "Low Support", 
                       "High Support", "VHigh Support"))
happy$Social.support.Qual<-labs

labs <- cut(happy$Generosity, 
            breaks = c(-10, 0, 10), 
            labels = c("No Generosity", "Generosity"))
happy$Generosity.Qual<-labs

labs <- cut(happy$Perceptions.of.corruption, 
            breaks = c(0, 0.25, .5, .75, 1), 
            labels = c("VLow Corruption", "Low Corruption", 
                       "High Corruption", "VHigh Corruption"))
happy$Corruption.Qual<-labs

max(happy$Log.GDP.per.capita)
min(happy$Log.GDP.per.capita)

labs <- cut(happy$Log.GDP.per.capita, 
            breaks = c(6.5, 8, 9.5, 11, 12.5), 
            labels = c("Tier 4", "Tier 3", 
                       "Tier 2", "Tier 1"))
happy$GDP.Qual<-labs #Qualitative GDP

happy.qual = subset(happy, select = c(Country.name,
                                Corruption.Qual,
                                Social.support.Qual,
                                Generosity.Qual,
                                Regional.indicator,
                                GDP.Qual
                                ) ) 
#Keep a sub_df with only Regional.indicator and GDP.Qual
happy.qual2<-subset(happy.qual, select = c(GDP.Qual,
                                      Regional.indicator) )

print('***************')
x=length(unique(happy.qual2$Regional.indicator))
y=length(unique(happy.qual2$GDP.Qual))
Matrix.C<-matrix(1:x*y, nrow = x, ncol = y) #Create Empty matrix
Matrix.C

# FOR Loop to save instances of every Var 2 on Var 1 
#(GDP.Qual repetitions on Regions)
ii<-1
for (i in unique(happy.qual2$Regional.indicator)){
  jj<-1
  for (j in unique(happy.qual2$GDP.Qual)){
    sub_df<-subset(happy.qual2, 
                   happy.qual2$Regional.indicator == i)
    sub_df<-subset(sub_df, 
                   sub_df$GDP.Qual== j)
    fl<-dim(sub_df)[1]
    Matrix.C[[ii,jj]]<-fl
    jj<-jj+1
  }
  ii<-ii+1
}

df<-data.frame(Matrix.C) #Convert Matrix to DataFrame
colnames(df) <- unique(happy.qual2$GDP.Qual)
rownames(df)<-unique(happy.qual2$Regional.indicator)
View(df)
chisq.test(df) #Pearson's Chi-Squared Test
?chisq.test
#p-value = 8.247e-13 -> No se rechaza H0 // We do not reject H0
#no hay independencia entre las dos variables // There is no independency between vars

#Component analysis
result_ca<-ca(df) 
result_ca

result_ca$rowcoord[,1:2]
plot(result_ca)
df4rows<-data.frame(result_ca$rowcoord[,1:2]) #for plotting Rows

df4columns<-data.frame(result_ca$colcoord[,1:2]) #for plotting columns

#Plot Rows and Columns for Visual Analysis
p<-ggplot(NULL)+
  geom_point(data=df4rows, aes(x=Dim1,y=Dim2),colour='steelblue') +
  geom_text(data=df4rows,label=rownames(df4rows),
            aes(x=Dim1,y=Dim2),hjust=-0.25,size=3.5)+ 
  ggtitle('Análisis de Correspondencia')+
  geom_hline(yintercept=0, linetype="dashed", color = "indianred")+
  geom_vline(xintercept=0, linetype="dashed", color = "indianred")+
  geom_point(data=df4columns,aes(x=Dim1,y=Dim2),colour='indianred',
             shape = 17,size=5)+
  geom_text(data=df4columns,label=rownames(df4columns),
            aes(x=Dim1,y=Dim2),hjust=-0.25, vjust=0.5)+
  xlab("Dim1 (67%") + 
  ylab("Dim2 (21%)") 
p+xlim(-2.5, 2.5) + ylim(-2.5, 2.5)+theme_light()


