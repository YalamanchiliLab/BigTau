library("scales")
library("ggplot2")
library('gridExtra')
library('grid')
library('ggrepel')

#Alzheimr's Only OR Control Only Female v Male Plots
#targetSubject="Alzheimer Disease"
#name="Alzheimers_FemvMale"
targetSubject="control"
name="Control_Cerebellum"

#Cerebellum
output="."
data="./cerebellum_BTauPercentages.txt"
metadata="./cerebellum_metadata.txt"
dir.create(output)

data=read.table(data, header=TRUE, sep="\t")
metadata=read.table(metadata, header=TRUE, sep="\t")

fullData <-merge(data, metadata, "name")
fullData$"BTauExpress" <- fullData$BTau_norm/fullData$TTau_norm
##LOG2 SCALING
#fullData$BTauExpress <- log2(fullData$BTauExpress)
#fullData$BTau_norm <- log2(fullData$BTau_norm)
#####3

#filtering for Alzheimers and control Only
#dataSelect<-fullData[((fullData$sex=="female")|(fullData$diagnosis=="male")),]
dataSelect<-fullData[(fullData$diagnosis==targetSubject),]

#Pval
femData=dataSelect[dataSelect$sex=="female",]
maleData=dataSelect[dataSelect$sex=="male",]
pval <-round(ks.test(femData$BTauExpress,maleData$BTauExpress)$p.value[1], digits=8)

#Pval
pval_express <-round(ks.test(femData$BTau_norm,maleData$BTau_norm)$p.value[1], digits=8)


#number of samples
femN=table(dataSelect$sex)[["female"]]
maleN=table(dataSelect$sex)[["male"]]



# Plot 1.2 : Density Plot (Big Tau Express)
ggplot(dataSelect, aes(x=BTau_norm, fill=sex)) + geom_density(alpha=.3)+
  xlab("Big Tau Proportion") +
  ylab("Scaled Density") +
  #ggtitle("Density Plot")+
  theme(plot.title = element_text(hjust = 0.5)) +
  #scale_x_continuous(labels = label_comma(), limits = c(0, 500000)) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.justification = "left",
        legend.text = element_text(size=8),
        panel.grid = element_blank()) +
  scale_fill_discrete(labels = c(paste("Female (n=",femN,")"), paste0("Male (n= ",maleN," )"))) +
  ggtitle(paste0("Control - Cerebellum "))+
  annotate("text",label="Scatterplot Display")+
  labs(tag = paste0("p-value: ",pval_express)) +
  theme(plot.tag.position = c(0.85, 0.08))+
  theme(plot.tag  = element_text(size = 10, face='italic'))
#scale_color_manual(values = c("#eeaeee","#01bfff")) 
ggsave(paste0(output,name,"_BTExpress_density.png"), height = 5 , width=6)


# Temporal Cortex
output="."
data="./temporalCortex_BTauPercentages.txt"
metadata="./temporalCortex_metadata.txt"
dir.create(output)

data=read.table(data, header=TRUE, sep="\t")
metadata=read.table(metadata, header=TRUE, sep="\t")

fullData <-merge(data, metadata, "name")
fullData$"BTauExpress" <- fullData$BTau_norm/fullData$TTau_norm
#filtering for Alzheimers and control Only
#dataSelect<-fullData[((fullData$sex=="female")|(fullData$diagnosis=="male")),]
dataSelect<-fullData[(fullData$diagnosis==targetSubject),]



#Pval
femData=dataSelect[dataSelect$sex=="female",]
maleData=dataSelect[dataSelect$sex=="male",]
pval <-round(ks.test(femData$BTauExpress,maleData$BTauExpress)$p.value[1], digits=8)

#Pval
pval_express <-round(ks.test(femData$BTau_norm,maleData$BTau_norm)$p.value[1], digits=8)


#number of samples
femN=table(dataSelect$sex)[["female"]]
maleN=table(dataSelect$sex)[["male"]]


# Plot 1.2 : Density Plot (Big Tau Express)
ggplot(dataSelect, aes(x=BTau_norm, fill=sex)) + geom_density(alpha=.3)+
  xlab("Big Tau Expression") +
  ylab("Scaled Density") +
  #ggtitle("Density Plot")+
  theme(plot.title = element_text(hjust = 0.5)) +
  #scale_x_continuous(labels = label_comma(), limits = c(0, 500000)) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.justification = "left",
        legend.text = element_text(size=8),
        panel.grid = element_blank()) +
  scale_fill_discrete(labels = c(paste("Female (n=",femN,")"), paste0("Male (n= ",maleN," )"))) +
  ggtitle(paste0("Control - Temporal Cortex "))+
  annotate("text",label="Scatterplot Display")+
  labs(tag = paste0("p-value: ",pval_express)) +
  theme(plot.tag.position = c(0.85, 0.08))+
  theme(plot.tag  = element_text(size = 10, face='italic'))
#scale_color_manual(values = c("#eeaeee","#01bfff")) 
ggsave(paste0(output,name,"_BTExpress_density.png"), height = 5 , width=6)







#Alzheimr's Only OR Control Only Female v Male Plots
targetSubject="Alzheimer Disease"
name="AD_Cerebellum"
#targetSubject="control"
#name="Control_FemvMale"

#Cerebellum
output="/mnt/local_disk/michelle/data/Collaborators/HudaZoghbi/Chloe_Tau/Chloe_BigTau/BigTau_MalevFem/Publication_Plots/"
data="/mnt/local_disk/michelle/data/Collaborators/HudaZoghbi/Chloe_Tau/Chloe_BigTau/BigTau_MalevFem/cerebellum_BTauPercentages.txt"
metadata="/mnt/local_disk/michelle/data/Collaborators/HudaZoghbi/Chloe_Tau/Chloe_BigTau/BigTau_MalevFem/cerebellum_metadata.txt"
dir.create(output)

data=read.table(data, header=TRUE, sep="\t")
metadata=read.table(metadata, header=TRUE, sep="\t")

fullData <-merge(data, metadata, "name")
fullData$"BTauExpress" <- fullData$BTau_norm/fullData$TTau_norm
##LOG2 SCALING
#fullData$BTauExpress <- log2(fullData$BTauExpress)
#fullData$BTau_norm <- log2(fullData$BTau_norm)
#####3

#filtering for Alzheimers and control Only
#dataSelect<-fullData[((fullData$sex=="female")|(fullData$diagnosis=="male")),]
dataSelect<-fullData[(fullData$diagnosis==targetSubject),]

#Pval
femData=dataSelect[dataSelect$sex=="female",]
maleData=dataSelect[dataSelect$sex=="male",]
pval <-round(ks.test(femData$BTauExpress,maleData$BTauExpress)$p.value[1], digits=8)

#Pval
pval_express <-round(ks.test(femData$BTau_norm,maleData$BTau_norm)$p.value[1], digits=8)


#number of samples
femN=table(dataSelect$sex)[["female"]]
maleN=table(dataSelect$sex)[["male"]]



# Plot 1.2 : Density Plot (Big Tau Express)
ggplot(dataSelect, aes(x=BTau_norm, fill=sex)) + geom_density(alpha=.3)+
  xlab("Big Tau Proportion") +
  ylab("Scaled Density") +
  #ggtitle("Density Plot")+
  theme(plot.title = element_text(hjust = 0.5)) +
  #scale_x_continuous(labels = label_comma(), limits = c(0, 500000)) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.justification = "left",
        legend.text = element_text(size=8),
        panel.grid = element_blank()) +
  scale_fill_discrete(labels = c(paste("Female (n=",femN,")"), paste0("Male (n= ",maleN," )"))) +
  ggtitle(paste0("Alzheimer's - Cerebellum "))+
  annotate("text",label="Scatterplot Display")+
  labs(tag = paste0("p-value: ",pval_express)) +
  theme(plot.tag.position = c(0.85, 0.08))+
  theme(plot.tag  = element_text(size = 10, face='italic'))
#scale_color_manual(values = c("#eeaeee","#01bfff")) 
ggsave(paste0(output,name,"_BTExpress_density.png"), height = 5 , width=6)











#Temporal Cortex
name="AD_TemporalCortex"
output="/mnt/local_disk/michelle/data/Collaborators/HudaZoghbi/Chloe_Tau/Chloe_BigTau/BigTau_MalevFem/Publication_Plots/"
data="/mnt/local_disk/michelle/data/Collaborators/HudaZoghbi/Chloe_Tau/Chloe_BigTau/BigTau_MalevFem/temporalCortex_BTauPercentages.txt"
metadata="/mnt/local_disk/michelle/data/Collaborators/HudaZoghbi/Chloe_Tau/Chloe_BigTau/BigTau_MalevFem/temporalCortex_metadata.txt"
dir.create(output)

data=read.table(data, header=TRUE, sep="\t")
metadata=read.table(metadata, header=TRUE, sep="\t")

fullData <-merge(data, metadata, "name")
fullData$"BTauExpress" <- fullData$BTau_norm/fullData$TTau_norm
#filtering for Alzheimers and control Only
#dataSelect<-fullData[((fullData$sex=="female")|(fullData$diagnosis=="male")),]
dataSelect<-fullData[(fullData$diagnosis==targetSubject),]



#Pval
femData=dataSelect[dataSelect$sex=="female",]
maleData=dataSelect[dataSelect$sex=="male",]
pval <-round(ks.test(femData$BTauExpress,maleData$BTauExpress)$p.value[1], digits=8)

#Pval
pval_express <-round(ks.test(femData$BTau_norm,maleData$BTau_norm)$p.value[1], digits=8)


#number of samples
femN=table(dataSelect$sex)[["female"]]
maleN=table(dataSelect$sex)[["male"]]


# Plot 1.2 : Density Plot (Big Tau Express)
ggplot(dataSelect, aes(x=BTau_norm, fill=sex)) + geom_density(alpha=.3)+
  xlab("Big Tau Expression") +
  ylab("Scaled Density") +
  #ggtitle("Density Plot")+
  theme(plot.title = element_text(hjust = 0.5)) +
  #scale_x_continuous(labels = label_comma(), limits = c(0, 500000)) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.justification = "left",
        legend.text = element_text(size=8),
        panel.grid = element_blank()) +
  scale_fill_discrete(labels = c(paste("Female (n=",femN,")"), paste0("Male (n= ",maleN," )"))) +
  ggtitle(paste0("Alzheimer's - Temporal Cortex"))+
  annotate("text",label="Scatterplot Display")+
  labs(tag = paste0("p-value: ",pval_express)) +
  theme(plot.tag.position = c(0.85, 0.08))+
  theme(plot.tag  = element_text(size = 10, face='italic'))
#scale_color_manual(values = c("#eeaeee","#01bfff")) 
ggsave(paste0(output,name,"_BTExpress_density.png"), height = 5 , width=6)

