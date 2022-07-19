library(dplyr)
library(tidyr)
library(ggplot2)
library(poppr)
library(pegas)
library(phangorn)
library(ape)
library(adegenet)
library(usedist)
library(cowplot)

data<-read.csv("P.Microsats6.17.22.singles.csv", header=TRUE)
field<-read.csv("Field2017.csv", header=TRUE)
Density<-read.csv("2017Density.csv", header=TRUE)
data2017<-filter(data, year==2017)

###Get density stuff in order
Density<-select(Density, Lake, Julian, DentTot, PulTot, RetroTot, CerioTot, DubiaTot, ParvTot, AmbTot)
Density<-gather(Density, "Species","Density", 3:9)
Density$Species[Density$Species=="DentTot"]<-"D. dentifera"
Density$Species[Density$Species=="PulTot"]<-"D. pulicara"
Density$Species[Density$Species=="CerioTot"]<-"Ceriodaphnia"
Density$Species[Density$Species=="DubiaTot"]<-"D. dubia"
Density$Species[Density$Species=="ParvTot"]<-"D. parvula"
Density$Species[Density$Species=="AmbTot"]<-"D. ambigua"
#Correct densities
Density$Density.cor<-Density$Density/9


dentDensity<-filter(Density, Species=="D. dentifera")
dentDensity$Julian<-dentDensity$Julian-2017000

########################
#Little Appleton Field plots
LA<-filter(field, Lake=="LittleAppleton")
LA.1<-select(LA, Date, Julian, Lake, Total, UA, UJ, UninfectedMales, UninfectedEphip, APasteuria, JPasteuria, MalePasteuria, EphipPastueria, AMicGPast, JMicGPast, AMetschPast, JMetschPast,MPastSpider)
LA.1[is.na(LA.1)]<-0
LA.1$PastSum<-LA.1$APasteuria+LA.1$JPasteuria+LA.1$MalePasteuria+LA.1$EphipPastueria+LA.1$AMicGPast+LA.1$JMicGPast+LA.1$AMetschPast+LA.1$JMetschPast+LA.1$MPastSpider
LA.1$PastPrev<-LA.1$PastSum/LA.1$Total

LDens<-filter(dentDensity, Lake=="LittleAppleton")
LDens.1<-full_join(LA.1, LDens, by=c("Lake", "Julian"))
LDens.1$infDens<-LDens.1$PastPrev*LDens.1$Density.cor
LProp<-select(LDens.1, Date, Julian, PastPrev, infDens)
LProp$infDens.g<-LProp$infDens/100000
LProp<-select(LProp, Date, Julian, PastPrev, infDens.g)
LProp2<-gather(LProp, "measure", "amount", 3:4)

#Fig1A
LAprevdens<-ggplot(LProp2, aes(x=Julian, y=amount, group=measure, color=measure, linetype=measure)) +
  geom_line(size=1) +
  geom_point() +
  scale_color_manual(values=c("blue","black"))+
  scale_y_continuous(limits=c(0,0.42),name="Infection prevalence", 
                     sec.axis = sec_axis(~ 10*., name=bquote('Infected host density '~(animals/m^2~x~10^4))))+
  scale_linetype_manual(values=c("solid", "twodash"))+
  ggtitle("Little Appleton")+scale_x_continuous(limits=c(200, 320))+
  theme_classic()+
  theme(axis.text = element_text(color="black", size=14))+
  theme(axis.text.x = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(legend.position = "none")+
  theme(axis.title.y.right=element_text(color="blue", size=18))+
  theme(axis.text.y.right = element_text(color="blue", size=14))+
  theme(axis.title.y.left = element_text(size=18))+
  theme(title=element_text(size=20))

#Crooked P Field Plots
CP<-filter(field, Lake=="CrookedP")
CP.1<-select(CP, Date, Julian, Lake, Total, UA, UJ, UninfectedMales, UninfectedEphip, APasteuria, JPasteuria, MalePasteuria, EphipPastueria, AMicGPast, JMicGPast, AMetschPast, JMetschPast,MPastSpider)
CP.1[is.na(CP.1)]<-0
CP.1$PastSum<-CP.1$APasteuria+CP.1$JPasteuria+CP.1$MalePasteuria+CP.1$EphipPastueria+CP.1$AMicGPast+CP.1$JMicGPast+CP.1$AMetschPast+CP.1$JMetschPast+CP.1$MPastSpider
CP.1$PastPrev<-CP.1$PastSum/CP.1$Total
epidemicCP<-ggplot(CP.1, aes(Julian, PastPrev))+
  geom_point()+geom_line(size=1, lty="twodash")+
  scale_x_continuous(limits=c(200, 320))+
  theme(axis.title.y = element_text(size=16))+
  theme(axis.title.x = element_blank())+
  ylab("Infection prevalence")+
  scale_y_continuous(limits=c(0,0.4))+
  ggtitle("Crooked")

CPDens<-filter(dentDensity, Lake=="CrookedP")
CPDens.1<-full_join(CP.1, CPDens, by=c("Lake", "Julian"))
CPDens.1$infDens<-CPDens.1$PastPrev*CPDens.1$Density.cor
CPDens.1<-filter(CPDens.1, !is.na(PastPrev))
CPProp<-select(CPDens.1, Date, Julian, PastPrev, infDens)
CPProp$infDens<-CPProp$infDens/100000
CPProp2<-gather(CPProp, "measure", "amount", 3:4)

#Fig1B
CPprevdens<-ggplot(CPProp2, aes(x=Julian, y=amount, group=measure, color=measure, linetype=measure)) +
  theme_classic()+
  geom_line(size=1) +
  geom_point() +
  scale_color_manual(values=c("blue","black"))+
  scale_y_continuous(limits=c(0,0.42), name="Infection prevalence", 
                     sec.axis = sec_axis(~ 10*., name=bquote('Infected host density '~(animals/m^2~x~10^4))))+
  scale_linetype_manual(values=c("solid", "twodash"))+
  theme(legend.position = "none")+
  xlab("Ordinal day")+
  theme(axis.title.x= element_blank())+
  theme(axis.text.x = element_blank())+
  ggtitle("Crooked")+scale_x_continuous(limits=c(200, 320))+
  theme(axis.text.y.left = element_text(color="black", size=14))+
  theme(axis.text.y.right = element_text(color="blue", size=14))+
  theme(axis.title.y.right = element_text(color="blue",size=18))+
  theme(axis.title.y.left = element_text(size=18))+
  theme(title = element_text(size=20))

###########################################################################
#genetic analysis

data2017.1<-select(data2017, sample, Species, Lake, year, collection.date, Julian, p1, p2,p3, p4,p7, p11, p12, p16, p18, p19)
data2017.1<-filter(data2017.1, Species!="Cerio")
data2017.1$species_lake_date<- paste(data2017.1$Species,data2017.1$Lake,data2017.1$collection.date,sep="_")
dat<-select(data2017.1, sample, species_lake_date, p1, p2, p4,p11,p12, p16,p18,p19)
write.table(dat, "Past2017.txt", col.names = TRUE, row.names = FALSE, sep="\t", append=FALSE)
D<-read.loci("Past2017.txt", col.loci=3:10,row.names=1, col.pop=2, header=TRUE, allele.sep="-")
D1<-loci2genind(D, ploidy=1)
info_table(D1, plot=TRUE)

#Drop p3, 39.3% missing data
#and p7 has 28.6% missing data

#Little Appleton
dat<-select(data2017.1, sample, Lake, collection.date, Julian, species_lake_date, p1, p2, p4,p11,p12, p16,p18,p19)
L<-filter(dat, Lake=="L")
L.1<-L[rowSums(is.na(L)) < 3, ] #only use samples with at least 6/8 alleles
write.csv(L.1, "LittleAppletonGenotypes.csv")

CP<-filter(dat, Lake=="CP")
CP.1<-CP[rowSums(is.na(CP))<3,] #only use samples with at least 6/8 alleles
write.csv(CP.1, "CrookedGenotypes.csv")

L.1<-L.1%>%ungroup()
CP.1<-CP.1%>%ungroup()

#Prepare data for genetics packages
#For Little Appleton
datL<-select(L.1, sample, species_lake_date, p1, p2, p4, p11, p12, p16,p18,p19 )
#For Crooked P
datCP<-select(CP.1, sample, species_lake_date, p1, p2, p4, p11, p12,p16, p18,p19 )

write.table(datL, "LPast2017.txt", col.names = TRUE, row.names = FALSE, sep="\t", append=FALSE)
l17<-read.loci("LPast2017.txt", col.loci=3:10,row.names=1, col.pop=2, header=TRUE, allele.sep="-")
l17<-loci2genind(l17, ploidy=1)
L17<-genind2genpop(l17)

write.table(datCP, "CPPast2017.txt", col.names = TRUE, row.names = FALSE, sep="\t", append=FALSE)
cp17<-read.loci("CPPast2017.txt", col.loci=3:10,row.names=1, col.pop=2, header=TRUE, allele.sep="-")
cp17<-loci2genind(cp17, ploidy=1)
CP17<-genind2genpop(cp17)

genind2genalex(l17, filename = "LPast2017.csv", quiet = FALSE, pop = NULL,
               allstrata = TRUE, geo = FALSE, geodf = "xy", sep = ",",
               sequence = FALSE, overwrite=TRUE)
genind2genalex(cp17, filename = "CPPast2017.csv", quiet = FALSE, pop = NULL,
               allstrata = TRUE, geo = FALSE, geodf = "xy", sep = ",",
               sequence = FALSE, overwrite=TRUE)

L17<-read.genalex("LPast2017.csv")
CP17<-read.genalex("CPPast2017.csv")
splitStrata(L17)<-~species/lake/date
splitStrata(CP17)<-~species/lake/date

tab(L17)
nmll(L17)#16 MLGs if uncontracted
mll(L17)#show the multilocus genotype definitions

tab(CP17)
nmll(CP17)#42 samples condensed in 26 MLGS
mll(CP17)#show the multilocus genotype definitions

##################################
#locus based statistics
locus_table(L17)
locus_table(CP17)

##################################
#Nei's gene diversity analyses by date

#Little Appleton
setPop(L17)<-~species/lake/date
L17diversity<-poppr(L17)

infoL<-select(L.1, species_lake_date, collection.date, Julian)
infoL<-unique(infoL)
div.stats.L<-full_join(infoL, L17diversity, by=c("species_lake_date"="Pop"))
div.stats.L<-filter(div.stats.L, !is.na(Julian)) #This just gets rid of totals column

#generate confidence intervals for Little Appleton gene diversity
OUT<-NULL
for (j in c(1,2,3,4,5,6)){
  Lpop <- L17[pop = j]
  het <- function(i){x <- poppr(i);c(Hexp = mean(x$Hexp))}
  Lsamples <- replicate(1000, shufflepop(Lpop, method = 2))
  res <- sapply(Lsamples, het)
  res1<-as.matrix(res)
  CI<-apply(res1, 2, quantile, c(0.05, 0.95))
  output<-c(j, CI[1], CI[2])
  OUT<-rbind(OUT, output)
}

LCI<-OUT
colnames(LCI)<-c("num", "lower","upper")
rownames(LCI)<-NULL
LCI<-as.data.frame(LCI)
LCI$dif<-LCI$upper-LCI$lower
LCI$pop<-unique(L17$pop)
LCI$species_lake_date<-as.character(LCI$pop)

div.L<-full_join(LCI, div.stats.L, by=c("species_lake_date"))
div.L$er<-div.L$dif/2

#Figure 1C
Hexp.L<-ggplot(div.L, aes(Julian, Hexp))+
  scale_x_continuous(limits=c(200, 320), breaks=c(200, 225, 250, 275, 300), labels=c("19-Jul.","13-Aug.","7-Sept.", "2-Oct.", "27-Oct."))+
  xlab("Date")+
  ylab("Nei's gene diversity")+
  theme_classic()+
  theme(axis.title = element_text(size=18))+
  geom_text(aes(label=N),hjust=-0.5)+
  scale_y_continuous(limits=c(0,1))+
  geom_errorbar(aes(ymin=Hexp-er, ymax=Hexp+er))+
  geom_point(size=3)+
  theme(axis.text = element_text(color="black", size=14))

#Trend for diversity with time (Little Appleton)
m<-lm(Hexp~Julian, data=div.L)

#Crooked P
setPop(CP17)<-~species/lake/date
CP17d<-poppr(CP17)#I don't think I have enough in each category to really talk about diversity

info<-select(CP.1, species_lake_date, collection.date, Julian)
info<-unique(info)
div.stats.CP<-full_join(info, CP17d, by=c("species_lake_date"="Pop"))
div.stats.CP<-filter(div.stats.CP, !is.na(Julian))

#generate confidence intervals for Little Appleton gene diversity
OUT<-NULL
for (j in c(1,2,3,4,5,6)){
  CPpop <- CP17[pop = j]
  het <- function(i){x <- poppr(i);c(Hexp = mean(x$Hexp))}
  CPsamples <- replicate(1000, shufflepop(CPpop, method = 2))
  res <- sapply(CPsamples, het)
  res1<-as.matrix(res)
  CI<-apply(res1, 2, quantile, c(0.05, 0.95))
  output<-c(j, CI[1], CI[2])
  OUT<-rbind(OUT, output)
}

CPCI<-OUT
colnames(CPCI)<-c("num", "lower","upper")
rownames(CPCI)<-NULL
CPCI<-as.data.frame(CPCI)
CPCI$dif<-CPCI$upper-CPCI$lower
CPCI$pop<-unique(CP17$pop)
CPCI$species_lake_date<-as.character(CPCI$pop)

div.CP<-full_join(CPCI, div.stats.CP, by=c("species_lake_date"))
div.CP$er<-div.CP$dif/2

#Figure 1D
Hexp.CP<-ggplot(div.CP, aes(Julian, Hexp))+geom_point(size=3)+
  scale_x_continuous(limits=c(200, 320), breaks=c(200, 225, 250, 275, 300), labels=c("19-Jul.","13-Aug.","7-Sept.", "2-Oct.", "27-Oct."))+
  xlab("Date")+
  theme_classic()+
  ylab("Nei's gene diversity")+
  theme(axis.title = element_text(size=18))+
  geom_text(aes(label=N),hjust=-0.3, vjust=-0.3)+
  scale_y_continuous(limits=c(0,1))+
  geom_errorbar(aes(ymin=Hexp-er, ymax=Hexp+er))+
  theme(axis.text = element_text(color="black", size=14))

#Diversity trend with time in Crooked P
m<-lm(Hexp~Julian, data=div.CP)

#Difference in diversity between lakes?
t.test(div.L$Hexp, div.CP$Hexp)

########################################
#Build figure 1

epidemics<-plot_grid(LAprevdens, CPprevdens, Hexp.L, Hexp.CP, labels=c("A","B","C","D"), nrow=2, ncol=2, align=c("v","h"), axis=c("b","l"))
#export at 1000 and 1000

#######################################

#AMOVA
setPop(CP17)<-~date
CPAmova<-poppr.amova(CP17, ~date, within=FALSE, filter=TRUE,threshold = 0 , missing="ignore",
                     correction="lingoes", cutoff=0.5)
sig.CP<-randtest(CPAmova, nrepet=1000)

setPop(L17)<-~date
LAmova<-poppr.amova(L17, ~date, within=FALSE, filter=TRUE,threshold =0 , missing="ignore",
                    correction="lingoes", cutoff=0.5)
sig.L<-randtest(LAmova, nrepet=1000)
sig.L
############################################
#Make stacked frequency charts

#Little Appleton
mlgs<-c(mll(L17))
L.mlg<-cbind(L.1, mlgs)
L.mlg.count<-L.mlg%>%group_by(Julian, mlgs)%>%summarise(N=n())
L.mlg.totals<-L.mlg.count%>%group_by(Julian)%>%summarise(Total=sum(N))
L.mlg.freqs<-full_join(L.mlg.count, L.mlg.totals, by=c("Julian"))
L.mlg.freqs$freq<-L.mlg.freqs$N/L.mlg.freqs$Total
L.mlg.freqs$Julian<-as.factor(L.mlg.freqs$Julian)
L.mlg.freqs$mlgs<-as.factor(L.mlg.freqs$mlgs)
L.mlg.freqs$Julian<-as.numeric(as.character(L.mlg.freqs$Julian))
#Reassign order for figure
L.mlg.freqs$num.change[L.mlg.freqs$mlgs==1]<-1
L.mlg.freqs$num.change[L.mlg.freqs$mlgs==2]<-2
L.mlg.freqs$num.change[L.mlg.freqs$mlgs==3]<-11
L.mlg.freqs$num.change[L.mlg.freqs$mlgs==4]<-12
L.mlg.freqs$num.change[L.mlg.freqs$mlgs==5]<-9
L.mlg.freqs$num.change[L.mlg.freqs$mlgs==6]<-5
L.mlg.freqs$num.change[L.mlg.freqs$mlgs==7]<-6
L.mlg.freqs$num.change[L.mlg.freqs$mlgs==8]<-10
L.mlg.freqs$num.change[L.mlg.freqs$mlgs==9]<-7
L.mlg.freqs$num.change[L.mlg.freqs$mlgs==10]<-3
L.mlg.freqs$num.change[L.mlg.freqs$mlgs==11]<-15
L.mlg.freqs$num.change[L.mlg.freqs$mlgs==12]<-16
L.mlg.freqs$num.change[L.mlg.freqs$mlgs==13]<-8
L.mlg.freqs$num.change[L.mlg.freqs$mlgs==14]<-13
L.mlg.freqs$num.change[L.mlg.freqs$mlgs==15]<-14
L.mlg.freqs$num.change[L.mlg.freqs$mlgs==16]<-4
L.mlg.freqs$num.change<- as.factor(L.mlg.freqs$num.change)
#Generate colors for these plots
library(RColorBrewer)
n <- 42
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

Fig2A<-ggplot(L.mlg.freqs, aes(x = Julian, y = N, fill = num.change, label = num.change)) +  # Create stacked bar chart
  geom_bar(stat = "identity")+theme_bw()+
  scale_fill_manual(values=c(col_vector[1:11],col_vector[13:15] ,col_vector[40:41]))+
  theme(axis.text = element_text(color="black"))+
  ylab("Count")+xlab("")+
  scale_x_continuous(breaks=c(211,240, 251,260,276,286), labels=c("Jul. 30","Aug. 28","Sept. 8","Sept. 17","Oct. 3","Oct. 13"))+
  theme(legend.position = "none")+
  scale_y_continuous(breaks=c(0,2,4,6,8,10))+
  geom_text(size = 3, position = position_stack(vjust = 0.5))

#Filter MLGS for Crooked
mlgs<-c(mll(CP17))
CP.mlg<-cbind(CP.1, mlgs)
CP.mlg.count<-CP.mlg%>%group_by(Julian, mlgs)%>%summarise(N=n())
CP.mlg.totals<-CP.mlg.count%>%group_by(Julian)%>%summarise(Total=sum(N))
CP.mlg.freqs<-full_join(CP.mlg.count, CP.mlg.totals, by=c("Julian"))
CP.mlg.freqs$freq<-CP.mlg.freqs$N/CP.mlg.freqs$Total
CP.mlg.freqs$Julian<-as.factor(CP.mlg.freqs$Julian)
CP.mlg.freqs$mlgs<-as.factor(CP.mlg.freqs$mlgs)
CP.mlg.freqs$Julian<-as.numeric(as.character(CP.mlg.freqs$Julian))
#Reassign order for figure
CP.mlg.freqs$num.change[CP.mlg.freqs$mlgs==1]<-17
CP.mlg.freqs$num.change[CP.mlg.freqs$mlgs==2]<-31
CP.mlg.freqs$num.change[CP.mlg.freqs$mlgs==3]<-24
CP.mlg.freqs$num.change[CP.mlg.freqs$mlgs==4]<-37
CP.mlg.freqs$num.change[CP.mlg.freqs$mlgs==5]<-32
CP.mlg.freqs$num.change[CP.mlg.freqs$mlgs==6]<-33
CP.mlg.freqs$num.change[CP.mlg.freqs$mlgs==7]<-25
CP.mlg.freqs$num.change[CP.mlg.freqs$mlgs==8]<-18
CP.mlg.freqs$num.change[CP.mlg.freqs$mlgs==9]<-28
CP.mlg.freqs$num.change[CP.mlg.freqs$mlgs==10]<-19
CP.mlg.freqs$num.change[CP.mlg.freqs$mlgs==11]<-34
CP.mlg.freqs$num.change[CP.mlg.freqs$mlgs==12]<-35
CP.mlg.freqs$num.change[CP.mlg.freqs$mlgs==13]<-36
CP.mlg.freqs$num.change[CP.mlg.freqs$mlgs==14]<-20
CP.mlg.freqs$num.change[CP.mlg.freqs$mlgs==15]<-21
CP.mlg.freqs$num.change[CP.mlg.freqs$mlgs==16]<-29
CP.mlg.freqs$num.change[CP.mlg.freqs$mlgs==17]<-22
CP.mlg.freqs$num.change[CP.mlg.freqs$mlgs==18]<-23
CP.mlg.freqs$num.change[CP.mlg.freqs$mlgs==19]<-26
CP.mlg.freqs$num.change[CP.mlg.freqs$mlgs==20]<-9
CP.mlg.freqs$num.change[CP.mlg.freqs$mlgs==21]<-30
CP.mlg.freqs$num.change[CP.mlg.freqs$mlgs==22]<-11
CP.mlg.freqs$num.change[CP.mlg.freqs$mlgs==23]<-38
CP.mlg.freqs$num.change[CP.mlg.freqs$mlgs==24]<-4
CP.mlg.freqs$num.change[CP.mlg.freqs$mlgs==25]<-27
CP.mlg.freqs$num.change[CP.mlg.freqs$mlgs==26]<-39
CP.mlg.freqs$num.change<-as.character(CP.mlg.freqs$num.change)

Fig2B<-ggplot(CP.mlg.freqs, aes(x = Julian, y = N, fill = mlgs, label = num.change)) +  # Create stacked bar chart
  geom_bar(stat = "identity")+theme_bw()+
  scale_fill_manual(values=c(col_vector[17:35],col_vector[9],col_vector[36],col_vector[11],col_vector[37],col_vector[4], col_vector[38:39]))+
  theme(axis.text = element_text(color="black"))+
  ylab("")+xlab("")+
  scale_x_continuous(breaks=c(215,228, 243, 256,271,298), labels=c("Aug. 3","Aug. 16","Aug. 31","Sept. 13","Sept. 28","Oct. 25"))+
  theme(legend.position = "none")+
  geom_text(size = 3, position = position_stack(vjust = 0.5))

######################################################
#Population distances
l17<-read.loci("LPast2017.txt", col.loci=3:10,row.names=1, col.pop=2, header=TRUE, allele.sep="-")
l17<-loci2genind(l17, ploidy=1)
L17<-genind2genpop(l17)

cp17<-read.loci("CPPast2017.txt", col.loci=3:10,row.names=1, col.pop=2, header=TRUE, allele.sep="-")
cp17<-loci2genind(cp17, ploidy=1)
CP17<-genind2genpop(cp17)

#These genotypes haven't been condensed
popdistancesL<-dist.genpop(L17, method=5)#This makes a distance matrix for all populations using prevosti
popdistancesCP<-dist.genpop(CP17, method=5)#This makes a distance matrix for all populations using prevosti

#Look at the time between populations in a given lake and relate to distance.
lake.distL<-as.matrix(popdistancesL)
lake.distL<-as.data.frame(lake.distL)
species_lake_date<-rownames(lake.distL)
lakepopsL<-cbind(species_lake_date, lake.distL)
lake.pops<-gather(lakepopsL, "to_thispop", "distance", dentifera_L_10.13.2017, dentifera_L_10.3.2017, dentifera_L_7.30.2017, dentifera_L_8.28.2017, dentifera_L_9.17.2017, dentifera_L_9.8.2017)

species.data2L<-unique(L.2[,c("species_lake_date","Julian", "Species", "Lake")])
full.lakes.L<-inner_join(lake.pops, species.data2L, by="species_lake_date")
full.lakes.L<-select(full.lakes.L, species_lake_date, to_thispop, distance, Julian)
full.lakes2<-inner_join(full.lakes.L, species.data2L, by=c("to_thispop"="species_lake_date"))
full.lakes2$timedifference<-full.lakes2$Julian.y-full.lakes2$Julian.x
h.L<-select(full.lakes2, species_lake_date, to_thispop, timedifference, distance, Julian.y, Julian.x)
h.L$distance<-as.numeric(as.character(h.L$distance))
h.L$difference<-as.numeric(as.character(h.L$timedifference))
h.L$species_lake_date<-factor(h.L$species_lake_date, levels=c("dentifera_L_7.30.2017",
                                                              "dentifera_L_8.28.2017",
                                                              "dentifera_L_9.8.2017",
                                                              "dentifera_L_9.17.2017",
                                                              "dentifera_L_10.3.2017",
                                                              "dentifera_L_10.13.2017"))
h.L$to_thispop<-factor(h.L$to_thispop, levels=c("dentifera_L_7.30.2017",
                                                "dentifera_L_8.28.2017",
                                                "dentifera_L_9.8.2017",
                                                "dentifera_L_9.17.2017",
                                                "dentifera_L_10.3.2017",
                                                "dentifera_L_10.13.2017"))
Ltrend<-filter(h.L, timedifference>0)
L.m<-lm(distance~timedifference, data=Ltrend)

LATile<-ggplot(h.L, aes(species_lake_date, to_thispop)) + 
  geom_tile(aes(fill = distance), colour = "white")+theme_bw()+
  scale_fill_gradient(low = "white",high = "darkcyan", limits=c(0, 1))+
  theme(axis.text.x = element_text(angle=90, size=12, color="black"))+
  theme(axis.text.y = element_text(size=12, color="black"))+
  theme(axis.title = element_blank())+
  scale_x_discrete(labels=c("Jul. 30", "Aug. 28","Sept. 8","Sept. 17","Oct. 3","Oct. 13"))+
  scale_y_discrete(labels=c("Jul. 30", "Aug. 28","Sept. 8","Sept. 17","Oct. 3","Oct. 13"))+
  theme(legend.position = "none")

lake.distCP<-as.matrix(popdistancesCP)
lake.distCP<-as.data.frame(lake.distCP)
species_lake_date<-rownames(lake.distCP)
lakepopsCP<-cbind(species_lake_date, lake.distCP)
lake.popsCP<-gather(lakepopsCP, "to_thispop", "distance", dentifera_CP_10.25.2017, dentifera_CP_8.16.2017, dentifera_CP_8.3.2017, dentifera_CP_8.31.2017, dentifera_CP_9.13.2017, dentifera_CP_9.28.2017)

species.data2CP<-unique(CP.2[,c("species_lake_date","Julian", "Species", "Lake")])
full.lakes.CP<-inner_join(lake.popsCP, species.data2CP, by="species_lake_date")
full.lakes.CP<-select(full.lakes.CP, species_lake_date, to_thispop, distance, Julian)
full.lakes.CP<-inner_join(full.lakes.CP, species.data2CP, by=c("to_thispop"="species_lake_date"))
full.lakes.CP$timedifference<-full.lakes.CP$Julian.y-full.lakes.CP$Julian.x
h.CP<-select(full.lakes.CP, species_lake_date, to_thispop, timedifference, Species,Lake, distance, Julian.y, Julian.x)
h.CP$distance<-as.numeric(as.character(h.CP$distance))
h.CP$difference<-as.numeric(as.character(h.CP$timedifference))
h.CP$species_lake_date<-factor(h.CP$species_lake_date, levels=c("dentifera_CP_8.3.2017",
                                                                "dentifera_CP_8.16.2017",
                                                                "dentifera_CP_8.31.2017",
                                                                "dentifera_CP_9.13.2017",
                                                                "dentifera_CP_9.28.2017",
                                                                "dentifera_CP_10.25.2017"))
h.CP$to_thispop<-factor(h.CP$to_thispop, levels=c("dentifera_CP_8.3.2017",
                                                  "dentifera_CP_8.16.2017",
                                                  "dentifera_CP_8.31.2017",
                                                  "dentifera_CP_9.13.2017",
                                                  "dentifera_CP_9.28.2017",
                                                  "dentifera_CP_10.25.2017"))
CPtrend<-filter(h.CP, timedifference>0)
CP.m<-lm(distance~timedifference, data=CPtrend)

CPtile<-ggplot(h.CP, aes(species_lake_date, to_thispop)) + 
  geom_tile(aes(fill = distance), colour = "white")+theme_bw()+
  scale_fill_gradient(low = "white",high = "darkcyan", limits=c(0,1),breaks=c(0,0.25,0.5, 0.75, 1), name="Genetic \ndistance")+
  theme(axis.text.x = element_text(angle=90, size=12, color="black"))+
  theme(axis.text.y = element_text(size=12, color="black"))+
  theme(axis.title = element_blank())+
  scale_x_discrete(labels=c("Aug. 3","Aug. 16","Aug. 31","Sept. 13","Sept. 28","Oct. 25"))+
  scale_y_discrete(labels=c("Aug. 3","Aug. 16","Aug. 31","Sept. 13","Sept. 28","Oct. 25"))+
  theme(plot.title=element_text(face="plain"))+
  theme(legend.title = element_text(size=12))+
  theme(legend.text = element_text(size=8))

top<-plot_grid(Fig2A, Fig2B, align="h",axis="b", ncol=2, labels = c("A","B"))
bottom<-plot_grid(LATile, CPtile, ncol=2, labels=c("C","D"), align="h", axis="b")
Fig2<-plot_grid(top, bottom, ncol=1)
save_plot("PastPaperFig2.jpg", Fig2, base_height = 6, base_width = 10)
