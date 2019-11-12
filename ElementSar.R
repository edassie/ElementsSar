rm(list=ls(all=TRUE))
setwd("~/GitHub/ElementsSar")

#Load libraries
library(tidyr) 
library(dplyr)
library(plyr)
library(ggplot2)

# original data for all metals (except those with all readings below LOD)
ES= read.csv("ElementsSar.csv", sep = ",") 

# change from wide to long format 
ES2 <- gather(ES, Element, Value, Al:Zn)  

#Transformations
ES2$Index=as.factor(ES2$Index)
ES2$SiteN=as.factor (ES2$SiteN)
ES2$Element=as.factor(ES2$Element) 
ES2$Year=as.factor(ES2$Year) 

# Make new table with medians 
ESM<-ddply(ES2, .(Sample, SiteN, Month, Year, Element), 
           summarise, Median= median(Value, na.rm=TRUE))

write.table(ESM, "Medians.csv", quote=F, sep = ",", row.names=F)

# Summary by metal
Resume <- ddply(ESM, c("Element"), function(d) {c(Median = median (d$Median), 
                                                  min = min(d$Median), 
                                                   max = max(d$Median))}) 

write.table(Resume, "Resume.csv", quote=F, sep = ",", row.names=F)

# Summary by metal by Site
Resume2 <- ddply(ESM, c("Element", "SiteN"), function(d) {c(N = length (d$Median), 
                                                                Median = median (d$Median), min = min(d$Median), 
                                                                max = max(d$Median))}) 

write.table(Resume2, "ResumeSite.csv", quote=F, sep = ",", row.names=F)

# For building plot with morphotypes
Resume3<-ddply(ES2, .(Sample, SiteN, Spp, Month, Year, Element), 
           summarise, Median= median(Value, na.rm=TRUE))

write.table(ES2, "Resume3.csv", quote=F, sep = ",", row.names=F)

#Plot metals by site
Al<- Resume3[Resume3$Element=="Al",]
As<- Resume3[Resume3$Element=="As",]
Ca<- Resume3[Resume3$Element=="Ca",]
Cl<- Resume3[Resume3$Element=="Cl",]
Cu<- Resume3[Resume3$Element=="Cu",]
Fe<- Resume3[Resume3$Element=="Fe",]
K<- Resume3[Resume3$Element=="K",]
Mg<- Resume3[Resume3$Element=="Mg",]
Mn<- Resume3[Resume3$Element=="Mn",]
Mo<- Resume3[Resume3$Element=="Mo",]
P<- Resume3[Resume3$Element=="P",]
Pb<- Resume3[Resume3$Element=="Pb",]
Rb<- Resume3[Resume3$Element=="Rb",]
S<- Resume3[Resume3$Element=="S",]
Si<- Resume3[Resume3$Element=="Si",]
Sr<- Resume3[Resume3$Element=="Sr",]
Th<- Resume3[Resume3$Element=="Th",]
U<- Resume3[Resume3$Element=="U",]
V<- Resume3[Resume3$Element=="V",]
Zn<- Resume3[Resume3$Element=="Zn",]

# Make dotplot for each element 
ggplot(Al, aes(x=SiteN, y=Median, fill=Spp)) +
    geom_boxplot(fill="white")+
    geom_dotplot(binaxis='y', stackdir='center')+
    geom_hline(aes(yintercept = 140),
               linetype = 2,
               col="blue")+
    labs(x = "Site", y ="Concentration (ppm DW)")+
    scale_y_continuous(expand = c(0, 0), limits = c(0, 550))+
    theme(legend.position = "none", 
          axis.text=element_text(size=14),
          axis.title.x = element_text(size=14, face="bold"),
          axis.title.y = element_text (size=14, face="bold"))

ggplot(As, aes(x=SiteN, y=Median, fill=Spp)) +
    geom_boxplot(fill="white")+
    geom_dotplot(binaxis='y', stackdir='center')+
    geom_hline(aes(yintercept = 4),
               linetype = 2,
               col="blue")+
    labs(x = "Site", y ="Concentration (ppm DW)")+
    scale_y_continuous(expand = c(0, 0), limits = c(0, 180))+
    theme(legend.position = "none", 
          axis.text=element_text(size=14),
          axis.title.x = element_text(size=14, face="bold"),
          axis.title.y = element_text (size=14, face="bold"))

ggplot(Ca, aes(x=SiteN, y=Median, fill=Spp)) +
    geom_boxplot(fill="white")+
    geom_dotplot(binaxis='y', stackdir='center')+
    geom_hline(aes(yintercept = 394),
               linetype = 2,
               col="blue")+
    labs(x = "Site", y ="Concentration (ppm DW)")+
    scale_y_continuous(expand = c(0, 0), limits = c(0, 150000))+
    theme(legend.position = "none", 
          axis.text=element_text(size=14),
          axis.title.x = element_text(size=14, face="bold"),
          axis.title.y = element_text (size=14, face="bold"))

ggplot(Cl, aes(x=SiteN, y=Median, fill=Spp)) +
    geom_boxplot(fill="white")+
    geom_dotplot(binaxis='y', stackdir='center')+
    geom_hline(aes(yintercept = 266),
               linetype = 2,
               col="blue")+
    labs(x = "Site", y ="Concentration (ppm DW)")+
    scale_y_continuous(expand = c(0, 0), limits = c(0, 55000))+
    theme(legend.position = "none", 
          axis.text=element_text(size=14),
          axis.title.x = element_text(size=14, face="bold"),
          axis.title.y = element_text (size=14, face="bold"))

ggplot(Cu, aes(x=SiteN, y=Median, fill=Spp)) +
    geom_boxplot(fill="white")+
    geom_dotplot(binaxis='y', stackdir='center')+
    geom_hline(aes(yintercept = 6),
               linetype = 2,
               col="blue")+
    labs(x = "Site", y ="Concentration (ppm DW)")+
    scale_y_continuous(expand = c(0, 0), limits = c(0, 550))+
    theme(legend.position = "none", 
          axis.text=element_text(size=14),
          axis.title.x = element_text(size=14, face="bold"),
          axis.title.y = element_text (size=14, face="bold"))

ggplot(Fe, aes(x=SiteN, y=Median, fill=Spp)) +
    geom_boxplot(fill="white")+
    geom_dotplot(binaxis='y', stackdir='center')+
    geom_hline(aes(yintercept = 3),
               linetype = 2,
               col="blue")+
    labs(x = "Site", y ="Concentration (ppm DW)")+
    scale_y_continuous(expand = c(0, 0), limits = c(0, 15))+
    theme(legend.position = "none", 
          axis.text=element_text(size=14),
          axis.title.x = element_text(size=14, face="bold"),
          axis.title.y = element_text (size=14, face="bold"))

ggplot(K, aes(x=SiteN, y=Median, fill=Spp)) +
    geom_boxplot(fill="white")+
    geom_dotplot(binaxis='y', stackdir='center')+
    geom_hline(aes(yintercept = 333),
               linetype = 2,
               col="blue")+
    labs(x = "Site", y ="Concentration (ppm DW)")+
    scale_y_continuous(expand = c(0, 0), limits = c(0, 50000))+
    theme(legend.position = "none", 
          axis.text=element_text(size=14),
          axis.title.x = element_text(size=14, face="bold"),
          axis.title.y = element_text (size=14, face="bold"))

ggplot(Mg, aes(x=SiteN, y=Median, fill=Spp)) +
    geom_boxplot(fill="white")+
    geom_dotplot(binaxis='y', stackdir='center')+
    geom_hline(aes(yintercept = 2915),
               linetype = 2,
               col="blue")+
    labs(x = "Site", y ="Concentration (ppm DW)")+
    scale_y_continuous(expand = c(0, 0), limits = c(0, 15000))+
    theme(legend.position = "none", 
          axis.text=element_text(size=14),
          axis.title.x = element_text(size=14, face="bold"),
          axis.title.y = element_text (size=14, face="bold"))

ggplot(Mn, aes(x=SiteN, y=Median, fill=Spp)) +
    geom_boxplot(fill="white")+
    geom_dotplot(binaxis='y', stackdir='center')+
    geom_hline(aes(yintercept = 13),
               linetype = 2,
               col="blue")+
    labs(x = "Site", y ="Concentration (ppm DW)")+
    scale_y_continuous(expand = c(0, 0), limits = c(0, 150))+
    theme(legend.position = "none", 
          axis.text=element_text(size=14),
          axis.title.x = element_text(size=14, face="bold"),
          axis.title.y = element_text (size=14, face="bold"))

ggplot(Mo, aes(x=SiteN, y=Median, fill=Spp)) +
    geom_boxplot(fill="white")+
    geom_dotplot(binaxis='y', stackdir='center')+
    geom_hline(aes(yintercept = 1),
               linetype = 2,
               col="blue")+
    labs(x = "Site", y ="Concentration (ppm DW)")+
    scale_y_continuous(expand = c(0, 0), limits = c(0, 10))+
    theme(legend.position = "none", 
          axis.text=element_text(size=14),
          axis.title.x = element_text(size=14, face="bold"),
          axis.title.y = element_text (size=14, face="bold"))

ggplot(P, aes(x=SiteN, y=Median, fill=Spp)) +
    geom_boxplot(fill="white")+
    geom_dotplot(binaxis='y', stackdir='center')+
    geom_hline(aes(yintercept = 145),
               linetype = 2,
               col="blue")+
    labs(x = "Site", y ="Concentration (ppm DW)")+
    scale_y_continuous(expand = c(0, 0), limits = c(0, 450))+
    theme(legend.position = "none", 
          axis.text=element_text(size=14),
          axis.title.x = element_text(size=14, face="bold"),
          axis.title.y = element_text (size=14, face="bold"))

ggplot(Pb, aes(x=SiteN, y=Median, fill=Spp)) +
    geom_boxplot(fill="white")+
    geom_dotplot(binaxis='y', stackdir='center')+
    geom_hline(aes(yintercept = 2),
               linetype = 2,
               col="blue")+
    labs(x = "Site", y ="Concentration (ppm DW)")+
    scale_y_continuous(expand = c(0, 0), limits = c(0, 5))+
    theme(legend.position = "none", 
          axis.text=element_text(size=14),
          axis.title.x = element_text(size=14, face="bold"),
          axis.title.y = element_text (size=14, face="bold"))

ggplot(Rb, aes(x=SiteN, y=Median, fill=Spp)) +
    geom_boxplot(fill="white")+
    geom_dotplot(binaxis='y', stackdir='center')+
    geom_hline(aes(yintercept = 1),
               linetype = 2,
               col="blue")+
    labs(x = "Site", y ="Concentration (ppm DW)")+
    scale_y_continuous(expand = c(0, 0), limits = c(0, 150))+
    theme(legend.position = "none", 
          axis.text=element_text(size=14),
          axis.title.x = element_text(size=14, face="bold"),
          axis.title.y = element_text (size=14, face="bold"))

ggplot(S, aes(x=SiteN, y=Median, fill=Spp)) +
    geom_boxplot(fill="white")+
    geom_dotplot(binaxis='y', stackdir='center')+
    geom_hline(aes(yintercept = 199),
               linetype = 2,
               col="blue")+
    labs(x = "Site", y ="Concentration (ppm DW)")+
    scale_y_continuous(expand = c(0, 0), limits = c(0, 30000))+
    theme(legend.position = "none", 
          axis.text=element_text(size=14),
          axis.title.x = element_text(size=14, face="bold"),
          axis.title.y = element_text (size=14, face="bold"))

ggplot(Si, aes(x=SiteN, y=Median, fill=Spp)) +
    geom_boxplot(fill="white")+
    geom_dotplot(binaxis='y', stackdir='center')+
    geom_hline(aes(yintercept = 342),
               linetype = 2,
               col="blue")+
    labs(x = "Site", y ="Concentration (ppm DW)")+
    scale_y_continuous(expand = c(0, 0), limits = c(0, 3000))+
    theme(legend.position = "none", 
          axis.text=element_text(size=14),
          axis.title.x = element_text(size=14, face="bold"),
          axis.title.y = element_text (size=14, face="bold"))

ggplot(Sr, aes(x=SiteN, y=Median, fill=Spp)) +
    geom_boxplot(fill="white")+
    geom_dotplot(binaxis='y', stackdir='center')+
    geom_hline(aes(yintercept = 6),
               linetype = 2,
               col="blue")+
    labs(x = "Site", y ="Concentration (ppm DW)")+
    scale_y_continuous(expand = c(0, 0), limits = c(0, 3000))+
    theme(legend.position = "none", 
          axis.text=element_text(size=14),
          axis.title.x = element_text(size=14, face="bold"),
          axis.title.y = element_text (size=14, face="bold"))


ggplot(Th, aes(x=SiteN, y=Median, fill=Spp)) +
    geom_boxplot(fill="white")+
    geom_dotplot(binaxis='y', stackdir='center')+
    geom_hline(aes(yintercept = 1),
               linetype = 2,
               col="blue")+
    labs(x = "Site", y ="Concentration (ppm DW)")+
    scale_y_continuous(expand = c(0, 0), limits = c(0, 25))+
    theme(legend.position = "none", 
          axis.text=element_text(size=14),
          axis.title.x = element_text(size=14, face="bold"),
          axis.title.y = element_text (size=14, face="bold"))

ggplot(U, aes(x=SiteN, y=Median, fill=Spp)) +
    geom_boxplot(fill="white")+
    geom_dotplot(binaxis='y', stackdir='center')+
    geom_hline(aes(yintercept = 4),
               linetype = 2,
               col="blue")+
    labs(x = "Site", y ="Concentration (ppm DW)")+
    scale_y_continuous(expand = c(0, 0), limits = c(0, 50))+
    theme(legend.position = "none", 
          axis.text=element_text(size=14),
          axis.title.x = element_text(size=14, face="bold"),
          axis.title.y = element_text (size=14, face="bold"))

ggplot(V, aes(x=SiteN, y=Median, fill=Spp)) +
    geom_boxplot(fill="white")+
    geom_dotplot(binaxis='y', stackdir='center')+
    geom_hline(aes(yintercept = 3),
               linetype = 2,
               col="blue")+
    labs(x = "Site", y ="Concentration (ppm DW)")+
    scale_y_continuous(expand = c(0, 0), limits = c(0, 15))+
    theme(legend.position = "none", 
          axis.text=element_text(size=14),
          axis.title.x = element_text(size=14, face="bold"),
          axis.title.y = element_text (size=14, face="bold"))

ggplot(Zn, aes(x=SiteN, y=Median, fill=Spp)) +
    geom_boxplot(fill="white")+
    geom_dotplot(binaxis='y', stackdir='center')+
    geom_hline(aes(yintercept = 5),
               linetype = 2,
               col="blue")+
    labs(x = "Site", y ="Concentration (ppm DW)")+
    scale_y_continuous(expand = c(0, 0), limits = c(0, 20))+
    theme(legend.position = "none",
          axis.text=element_text(size=14),
          axis.title.x = element_text(size=14, face="bold"),
          axis.title.y = element_text (size=14, face="bold"))

#New Table for Sargassum forms (withouT Sargassum spp)
Spp<-ddply(ES2, .(SiteN, Sample, Spp, Element), 
              summarise, Median= median(Value, na.rm=TRUE))

Resume4 <- ddply(Spp, c("Spp", "Element"), function(d) {c(Median = median (d$Median), 
                                                             min = min(d$Median), 
                                                             max = max(d$Median))})
ElemSpp<-ddply(ES2, .(Sample, SiteN, Spp, Element), 
           summarise, Median= median(Value, na.rm=TRUE))

ElemSpp2=ElemSpp[ElemSpp$Spp=="Sflu III"| ElemSpp$Spp=="Snat I"|ElemSpp$Spp=="Snat VIII",]

write.table(ElemSpp2, "ElemSpp.csv", quote=F, sep = ",", row.names=F)

ElemSpp3= read.csv("ElemSpp.csv", sep = ",") 


Al2<- ElemSpp[ElemSpp$Element=="Al",]
As2<- ElemSpp[ElemSpp$Element=="As",]
Ca2<- ElemSpp[ElemSpp$Element=="Ca",]
Cl2<- ElemSpp[ElemSpp$Element=="Cl",]
Cu2<- ElemSpp[ElemSpp$Element=="Cu",]
Fe2<- ElemSpp[ElemSpp$Element=="Fe",]
K2<- ElemSpp[ElemSpp$Element=="K",]
Mg2<- ElemSpp[ElemSpp$Element=="Mg",]
Mn2<- ElemSpp[ElemSpp$Element=="Mn",]
Mo2<- ElemSpp[ElemSpp$Element=="Mo",]
P2<- ElemSpp[ElemSpp$Element=="P",]
Pb2<- ElemSpp[ElemSpp$Element=="Pb",]
Rb2<- ElemSpp[ElemSpp$Element=="Rb",]
S2<- ElemSpp[ElemSpp$Element=="S",]
Si2<- ElemSpp[ElemSpp$Element=="Si",]
Sr2<- ElemSpp[ElemSpp$Element=="Sr",]
Th2<- ElemSpp[ElemSpp$Element=="Th",]
U2<- ElemSpp[ElemSpp$Element=="U",]
V2<- ElemSpp[ElemSpp$Element=="V",]
Zn2<- ElemSpp[ElemSpp$Element=="Zn",]

# Kruskal-Wallis rank sum test (change element)
library (pgirmess) 
kruskal.test(Al2$Median~Al2$Spp)
kruskalmc(Al2$Median~ Al2$Spp) 

#Compare months for Puerto Morelos
PM<- ESM[ESM$SiteN=="3",]
Month <- ddply(PM, .(Element,Month), summarise, 
               Median = median (Median))

library(reshape) # Convert to wide format
MonthWide <- reshape(Month, 
                     timevar = "Month", # nombre de variable que quedemos quede separada
                     idvar = "Element", #nombre de todas las otras variables, menos la de interés (p.e. %)
                     direction = "wide")

write.table(MonthWide, "Month.csv", quote=F, sep = ",", row.names=F)

#Heatmap
HM= read.csv("MonthHM.csv", sep = ",")
str(HM)

HM2 <- HM %>% mutate_each_(funs(scale(.) %>% as.vector), 
                           vars=c("Al","As", "Ca", "Cl", "K", "Mg", "Mn", "P", "Rb", "S", "Si", "Sr", "Th", "U"))

write.table(HM2, "Ztrans.csv", quote=F, sep = ",", row.names=F)

# Load Libraries
library(gplots)  # for heatmap.2
library(RColorBrewer)

# Obtain matrix
row.names(HM2) <- HM2$Date
HM2 <- HM2[, -1]

# make the heatmap 
heatmap.2(as.matrix(HM2), Rowv = FALSE, Colv=FALSE,
          col = colorRampPalette(c("gold", "darkred")), margins = c(5, 8),
          trace = "none", density.info = "none", xlab = "Element", ylab = "Month+Year", 
          lhei = c(2, 6)) 