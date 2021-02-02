###################
#Daten importieren und aufbereiten
##################

#top1_daten <-read.csv("import_veg.csv", header  = TRUE)

#top1_daten$id <- paste0(top1_daten$id, top1_daten$jahr-2000)
#top1_daten <- top1_daten[-c(seq(from = 2, to = length(top1_daten$id), by = 2)),]

#write.csv(top1_daten, "r_output.csv", row.names = F, na="")
#Spalten l?schen die keinen Inhalt haben
library(ggplot2)
library(tidyverse)

top1_daten <- read.csv("r_output.csv", header = T)
top1_daten$jahr <- top1_daten$jahr-2000

vec <- 1 #Vektor der sich die Spalten merkt welche nur aus NA bestehen

for (i in 1:length(top1_daten[1,])){
  if(sum(is.na(top1_daten[,i])) == length(top1_daten[,1])){
    vec <- c(vec, i)
  } 
}
vec <- vec[-1]

top1_daten<-top1_daten[,-vec] 

#Zeilen l?schen die keinen Inhalt haben

vec <- 1 # Alle Zeilen l?schen die nur aus NA bestehen (m?sste die 114 sein)

rl <- length(top1_daten[1,])-2
for (i in 1:length(top1_daten[,1])){
  if(sum(is.na(top1_daten[i,])) == rl){
    vec <- c(vec, i)
  } 
}
vec <- vec[-1]

top1_daten <- top1_daten[-vec,] #Alle Zeilen l?schen die nicht vorkommen
Arten_NA <- top1_daten #Tabelle f?r die ggplot Darstellung
top1_daten[is.na(top1_daten)] <- 0 #NA mit 0 ersetzen
arten <- top1_daten   #Tabelle f?r die weitere Analyse


#Matrix mit Namen f?r die Weiden, statt den Nummern-------------------------
nummerx <- matrix(0,length(top1_daten[,1]),1)
nummerx <- (Arten_NA$id - Arten_NA$jahr )/100
weide <- matrix(0,length(top1_daten[,1]),1)
weide <- as.data.frame(weide)
colnames(weide) <- c("Weiden")
Arten_NA<-cbind(nummerx, Arten_NA)
weide[which(Arten_NA$nummerx==5),] <- "Ernsbach SW"
weide[which(Arten_NA$nummerx==11),] <- "Merkenbach Ost "
weide[which(Arten_NA$nummerx==13),] <- "Moosbach SW "
weide[which(Arten_NA$nummerx==15),] <- " Moosbach NW "
weide[which(Arten_NA$nummerx==9),]  <- "Merkenbach West "
weide[which(Arten_NA$nummerx==18),] <- "Untertal"
weide[which(Arten_NA$nummerx==7),] <- " Ernsbach NW"
weide[which(Arten_NA$nummerx==1),] <- "Ernsbach NO"
weide[which(Arten_NA$nummerx==3),] <- "Ernsbach SO"
Arten_NA<-cbind(weide, Arten_NA)
#Nummern und Id spalte aus dem Dataframe l?schen
Arten_NA<-Arten_NA[,-c(2,4)]


#####################
#Darstellung aller Arten geben die Jahre,  eingeteilt in die Weiden
###################
#!Die indizierten Spalten ist die Einteilung der Gesellschaften, wenn diese nicht mehr stimmen m?ssen sie manuell angepasst werden!
#ggplot 
library(ggplot2)
library("reshape2")


test_data_long <- melt(Arten_NA[,-c(13:95)], id=c("jahr", "Weiden"))
g <- ggplot(test_data_long, aes(y=value, x=jahr)) + geom_histogram(aes(fill = Weiden), stat="identity")+coord_flip() + labs(title ="V Arrhenatherion und Polygono-Trisetion", x = "Jahre", y = "Deckungsgrad nach Londo ")
plot1 <- g + facet_grid(. ~ variable)

test_data_long <- melt(Arten_NA[,-c(3:12,23:95)], id=c("jahr", "Weiden"))
g <- ggplot(test_data_long, aes(y=value, x=jahr)) + geom_histogram(aes(fill=Weiden), stat="identity")+coord_flip() + labs(title ="V Cynosurion und andere Stör- und Nährstoffzeiger", x = "Jahre", y = "Deckungsgrad nach Londo ")
plot2 <- g + facet_grid(. ~ variable)

test_data_long <- melt(Arten_NA[,-c(3:22, 34:95)], id=c("jahr", "Weiden"))
g <- ggplot(test_data_long, aes(y=value, x=jahr)) + geom_histogram(aes(fill=Weiden), stat="identity")+coord_flip() + labs(title ="Nässezeiger", x = "Jahre", y = "Deckungsgrad nach Londo ")
plot3 <- g + facet_grid(. ~ variable)

test_data_long <- melt(Arten_NA[,-c(3:33, 48:95)], id=c("jahr", "Weiden"))
g <- ggplot(test_data_long, aes(y=value, x=jahr)) + geom_histogram(aes(fill=Weiden), stat="identity")+coord_flip() + labs(title ="Arten magerer, meist außerdem bodensaurer Standorte", x = "Jahre", y = "Deckungsgrad nach Londo ")
plot35 <- g + facet_grid(. ~ variable)

test_data_long <- melt(Arten_NA[,-c(3:47, 56:95)], id=c("jahr", "Weiden"))
g <- ggplot(test_data_long, aes(y=value, x=jahr)) + geom_histogram(aes(fill=Weiden), stat="identity")+coord_flip() + labs(title ="Arten magerer, meist außerdem bodensaurer Standorte", x = "Jahre", y = "Deckungsgrad nach Londo ")
plot375 <- g + facet_grid(. ~ variable)

test_data_long <- melt(Arten_NA[,-c(3:55, 70:95)], id=c("jahr", "Weiden"))
g <- ggplot(test_data_long, aes(y=value, x=jahr)) + geom_histogram(aes(fill=Weiden), stat="identity")+coord_flip() + labs(title ="O Arrhenatheretalia", x = "Jahre", y = "Deckungsgrad nach Londo ")
plot4 <- g + facet_grid(. ~ variable)

test_data_long <- melt(Arten_NA[,-c(3:69, 88:95)], id=c("jahr", "Weiden"))
g <- ggplot(test_data_long, aes(y=value, x=jahr)) + geom_histogram(aes(fill=Weiden), stat="identity")+coord_flip()+
  labs(title ="K Molinio-Arrhenatheretea und weitere typ. Grünlandarten", x = "Jahre", y = "Deckungsgrad nach Londo ")
plot5 <- g + facet_grid(. ~ variable)

test_data_long <- melt(Arten_NA[,-c(3:87)], id=c("jahr", "Weiden"))
g <- ggplot(test_data_long, aes(y=value, x=jahr)) + geom_histogram(aes(fill=Weiden), stat="identity")+coord_flip()+ labs(title ="Sonstige", x = "Jahre", y = "Deckungsgrad nach Londo ")
plot6 <- g + facet_grid(. ~ variable) 

#Alle plots gleichzeitig Darstellen
library("gridExtra")

pdf("/media/gabriel/INTENSO/Stand_18_01_18/Abbildungen/alle_arten.pdf", width=30, height=25)
grid.arrange(plot1,plot4,plot2, plot3, plot35,plot375, plot5, plot6, nrow=8, ncol=1, as.table=TRUE)
dev.off()

####################
#Zusammenf?gen von den Umweltdaten und der Vegetationstabelle
################
#library(devtools)
#install_version("BiodiversityR", version ="2.6-1") Load diversityresult and comp fun
#library(BiodiversityR)
#install.packages("BiodiversityR")
#install.packages("vegan")
library(vegan)
#library(BiodiversityR)
library(cluster)
library(psych)
library(MASS)
#Import von den Gro?vieheinheiten und anderen Umweltparametern-------------------------
GV <- read.csv("Grossvieheinheitenha.csv")


nummer1 <- paste0(GV[,1], GV[,4]) #ID herstellen durch die Kombination von Jahr und Nummer
nummer1 <- as.numeric(nummer1)
GV <- cbind(nummer1, GV)
l <- unique(GV$nummer1) #L?nge f?r einen eindeutigen ID Vektor
GV <- GV[,-c(3, 4,5)] #L?schen der ausgeschriebenden Namen und doppelter Inofrmation (Jahre)
entmatmean<-matrix(0, length(l), 7)

#Aus dem Datensatz GV wird der Mittelwert von allen Spalten auf Basis der 
#Nummer gebildet, weil pro Jahr und Probefl?che mehrere Eigenschaften Aufgenommen wurden.

for (i in 1:7){
  d <- with(GV, tapply(GV[,i], nummer1, mean))
  entmatmean[,i] <- d
}

#colnames(GV)


#colnames(entmatmean) <- colnames(GV) #Hinzuf?gen Spaltennamen
#env<-as.data.frame(entmatmean)  #als Dataframe

#env$nummer1
#Zusammen f?gen der Arten und Umweltfaktoren auf der Basis der ID (Jahr + Probefl?che)
newmerg <- merge(arten, GV, by.x = "id", by.y = "nummer1", all = F, all.x = T, all.y = F, sort = F, incomparables = NULL)

#L?ngenangaben f?r exportierenden env. Vector

L1<-length(arten[1,])+1
L2<-length(newmerg[1,])
arten.env <- newmerg[, c(1:2,L1:L2)] #Alle Umweltfaktoren Vektor als eigende Tabelle

artenm<-newmerg[,-c(1:2,L1:L2)]  #Alle Arten, Tabelle hat somit die gleiche L?nge wie die Eigenschaften

############################
#Korrelationen der Umweltdaten
#######################

pdf("/media/gabriel/INTENSO/Stand_18_01_18/Abbildungen/beweidungenv.pdf", width=6, height=6)
pairs.panels(arten.env[,-c(1, 3,7,8,9)])
dev.off()

#####################
#Verschiedene Biodiversit?tindexe
##################

#Berechung des Artenreichtums je nach Jahr und Probefl?che
#Darstellung der Artenanzahl (Alpha diversit?t)---------------
Diversity.6 <- diversitycomp(artenm, y=arten.env, factor1='id', factor2='jahr', index='richness', method='all')
Diversity.6 <- as.data.frame(Diversity.6) 
Diversity.6 <- Diversity.6[, -c(1:9)]
colnames(Diversity.6) <- c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017","2018")
Diversity.6 <- as.matrix(Diversity.6)

#Hinzuf?gen Artenanzahl von den Kennarten
new.df <- colnames(arten[-c(1,2)]) 
r01 <- data.frame(matrix(0, 1, length(new.df)))
colnames(r01) <- new.df
r01[1,] <- c(1:length(new.df))
name1 <- c(r01$Anemone.nemorosa, r01$Anthoxanthum.odoratum, r01$Briza.media, r01$Campanula.patula, r01$Cardamine.pratensis, r01$Carex.caryophyllea, r01$Carex.ovalis, r01$Carex.pilulifera, r01$Carum.carvi, r01$Centaurea.jacea, r01$Centaurea.nemoralis.ssp.nigra, r01$Crepis.mollis, r01$Danthonia.decumbens, r01$Festuca.rubra, r01$Helictotrichon.pubescens, r01$Hieracium.pilosella, r01$Leontodon.hispidus, r01$Leucanthemum.ircutianum, r01$Lychnis.flos.cuculi, r01$Myosotis.scorpioides, r01$Nardus.stricta, r01$Pimpinella.saxifraga, r01$Polygala.vulgaris, r01$Potentilla.erecta, r01$Potentilla.sterilis, r01$Primula.elatior, r01$Ranunculus.bulbosus, r01$Succisa.pratensis, r01$Thymus.pulegioides)

kennarten<-artenm[,name1]

Diversity.7 <- diversitycomp(kennarten, y=arten.env, factor1= "id", factor2 = "jahr", index = "richness", method = "all")

Diversity.7 <- as.data.frame(Diversity.7) 
Diversity.7 <- Diversity.7[, -c(1:9)]
colnames(Diversity.7) <- c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017","2018")
Diversity.7 <- as.matrix(Diversity.7)

#Darstellung
pdf("/media/gabriel/INTENSO/Stand_18_01_18/Abbildungen/artenanzahl.pdf", width=10, height=6)
boxplot(Diversity.6, use.cols = TRUE, xlab = "Jahre", ylab = c("Artenanzahl"), ylim = c(0,40))
boxplot(Diversity.7,  use.cols = TRUE, add = T, col = "grey" )
points(apply(Diversity.7, 2, mean, na.rm=TRUE), col ="white", pch=3)
points(apply(Diversity.6, 2, mean, na.rm=TRUE), col ="blue", pch=3)
abline(11,0, lty = 4, col = "greyCrepis.mollisCrepis.mollis")
abline(32,0, lty = 4, col = "grey")
legend("bottom", legend= c("Gesamtartenzahl", "Kennarten 6510 und 6520"), lwd=c(1, 10), col= c("black", "grey"), bty="n")
dev.off()

###############
#Renyi Index
###############

Renyi.4 <-renyicomp(artenm, y = arten.env, factor = 'jahr', evenvess = T, permutations = 100)
renyiplot(Renyi.4)

##################
#Distanzma?e (noch zu erweitern)
############

envir.distance <- vegdist(arten.env$Grossvieheinheiten.pro.Flaeche, method = "euclidean", na.rm=TRUE)
ecology.distance <- vegdist(artenm, method = "bray")

pdf("/media/gabriel/INTENSO/Stand_18_01_18/Abbildungen/distanzen.pdf", width=7, height=7)
plot(envir.distance, ecology.distance, 
     pch = 1, xlab = c("Distanzen der durchschnittlichen Großvieheinheit pro Fläche"),
     ylab = c("Distanzen der Artzusammensetzung")
     )

dev.off()
             

##############
#Darstellung von Shannon
#############


div.s <- diversitycomp(artenm, y = arten.env, factor1 = 'jahr', factor2 = "Laufende.Nummer", index = 'Shannon', method = 'all', sortit=TRUE, digits=3)

plotshannon<-as.data.frame(div.s)
test2 <- plotshannon[,-c(1:9)]
Jahr <- row.names(test2)
test2 <- cbind(Jahr, test2)

colnames(test2) <- c("Jahr", "Ernsbach NO", "Ernsbach SO", "Ernsbach SW", " Ernsbach NW", "Merkenbach West", "Merkenbach Ost", "Moosbach SW", " Moosbach NW","Untertal")
test3 <- melt(test2, id= "Jahr")
test3 <- subset(test3, (!is.na(test3$value)))


pdf("/media/gabriel/INTENSO/Stand_18_01_18/Abbildungen/shannon.pdf", width=7, height=7)
g <- ggplot(test3, aes(y=value, x=Jahr)) + geom_violin(scale = "area") 
  
g + geom_point(aes(col=variable, shape = variable), size=5) +
  scale_shape_manual(values=1:nlevels(test3$variable)) 
  #geom_line(aes(group = variable, col = variable))
dev.off()


######################
#Rankkurven f?r jedes Jahr erstellen
#####################

Alle_arten_Matrix <- top1_daten
artenm <- Alle_arten_Matrix[,-c(1,2)]
arten.env <- Alle_arten_Matrix[,-c(3:length(Alle_arten_Matrix))]

#Rang-Kurven f?r jedes Jahr berechnen
library(vegan)
rank_10 <- as.data.frame(rankabundance(artenm, y=arten.env, factor="jahr", level = 10))
rank_10 <- rank_10[,-c(3:7)]
rank_10$rankfreq <- row.names(rank_10)
colnames(rank_10) <- c("rank10", "abundanz10","name")
rank_11 <- as.data.frame(rankabundance(artenm, y=arten.env, factor="jahr", level = 11))
rank_11 <- rank_11[,-c(3:7)]
rank_11$rankfreq<-row.names(rank_11)
colnames(rank_11)<-c("rank11", "abundanz11","name")
rank_12 <- as.data.frame(rankabundance(artenm, y=arten.env, factor="jahr", level = 12))
rank_12 <- rank_12[,-c(3:7)]
rank_12$rankfreq<-row.names(rank_12)
colnames(rank_12)<-c("rank12", "abundanz12","name")
rank_13 <- as.data.frame(rankabundance(artenm, y=arten.env, factor="jahr", level = 13))
rank_13 <- rank_13[,-c(3:7)]
rank_13$rankfreq <- row.names(rank_13)
colnames(rank_13) <- c("rank13", "abundanz13","name")
rank_14 <- as.data.frame(rankabundance(artenm, y=arten.env, factor="jahr", level = 14))
rank_14 <- rank_14[,-c(3:7)]
rank_14$rankfreq <- row.names(rank_14)
colnames(rank_14) <- c("rank14", "abundanz14","name")
rank_15 <- as.data.frame(rankabundance(artenm, y=arten.env, factor="jahr", level = 15))
rank_15 <- rank_15[,-c(3:7)]
rank_15$rankfreq <- row.names(rank_15)
colnames(rank_15) <- c("rank15", "abundanz15","name")

rank_16 <- as.data.frame(rankabundance(artenm, y=arten.env, factor="jahr", level = 16))
rank_16 <- rank_16[,-c(3:7)]
rank_16$rankfreq <- row.names(rank_16)
colnames(rank_16) <- c("rank16", "abundanz16","name")

rank_17 <- as.data.frame(rankabundance(artenm, y=arten.env, factor="jahr", level = 17))
rank_17 <- rank_17[,-c(3:7)]
rank_17$rankfreq <- row.names(rank_17)
colnames(rank_17) <- c("rank17", "abundanz17","name")

rank_18 <- as.data.frame(rankabundance(artenm, y=arten.env, factor="jahr", level = 18))
rank_18 <- rank_18[,-c(3:7)]
rank_18$rankfreq <- row.names(rank_18)
colnames(rank_18) <- c("rank18", "abundanz18","name")


#Alle R?nge in eine Tabelle
MyMerge <- function(x, y){
  rank.allf <- merge(x, y, by= "name", all.x= TRUE, all.y= TRUE)
  return(rank.allf)
}

#Dublikate l?schen

new.df <- Reduce(MyMerge, list(rank_10, rank_11, rank_12, rank_13, rank_14, rank_15, rank_16, rank_17, rank_18))

#Beschriftung der Kennarten hinzuf?gen
r01 <- data.frame(matrix(0, 1, length(new.df$name)))
colnames(r01) <- new.df$name
r01[1,] <- c(1:length(new.df$name))
name1 <- c(r01$Anemone.nemorosa, r01$Anthoxanthum.odoratum, r01$Briza.media, r01$Campanula.patula, r01$Cardamine.pratensis, r01$Carex.caryophyllea, r01$Carex.ovalis, r01$Carex.pilulifera, r01$Carum.carvi, r01$Centaurea.jacea, r01$Centaurea.nemoralis.ssp.nigra, r01$Crepis.mollis, r01$Danthonia.decumbens, r01$Festuca.rubra, r01$Helictotrichon.pubescens, r01$Hieracium.pilosella, r01$Leontodon.hispidus, r01$Leucanthemum.ircutianum, r01$Lychnis.flos.cuculi, r01$Myosotis.scorpioides, r01$Nardus.stricta, r01$Pimpinella.saxifraga, r01$Polygala.vulgaris, r01$Potentilla.erecta, r01$Potentilla.sterilis, r01$Primula.elatior, r01$Ranunculus.bulbosus,r01$Succisa.pratensis, r01$Thymus.pulegioides)

name2 <- new.df$name
name1 <- name1[!is.na(name1)]
name2[-name1] <- NA

#ggplot Abbildungen

#2018
r18 <- new.df$rank10-new.df$rank18
g <- ggplot(new.df, aes(rank18, abundanz18, label = name2)) +
  geom_line(size = 1, alpha = 0.4)+
  geom_line(aes(rank10, abundanz10), size = 3, alpha = 0.1)+
  scale_y_log10()+
  coord_flip()+
  scale_x_reverse()+
  geom_text(hjust = -0.3, angle = 0, size = 3, aes(colour = r18 ))+
  scale_colour_gradient2(limit = c(-25, + 25), low = "darkred", mid = "orange", high = "darkgreen", name = "Veränderung der Ränge \nim Vergleich zum Jahr 2010")+
  expand_limits(y = c(1,70))+
  ggtitle("2018")+
  theme(legend.justification=c(1,0), legend.position=c(1,0))+
  labs(x = "Ränge", y = "Deckungsgrade nach Londo")
plot18 <- g+geom_point(aes(colour = r18 ))

#2017
r17 <- new.df$rank10-new.df$rank17
g <- ggplot(new.df, aes(rank17, abundanz17, label = name2)) +
  geom_line(size = 1, alpha = 0.4)+
  geom_line(aes(rank10, abundanz10), size = 3, alpha = 0.1)+
  scale_y_log10()+
  coord_flip()+
  scale_x_reverse()+
  geom_text(hjust = -0.3, angle = 0, size = 3, aes(colour = r17 ))+
  scale_colour_gradient2(limit = c(-25, + 25), low = "darkred", mid = "orange", high = "darkgreen", name = "Veränderung der Ränge \nim Vergleich zum Jahr 2010")+
  expand_limits(y = c(1,70))+
  ggtitle("2017")+
  theme(legend.justification=c(1,0), legend.position=c(1,0))+
  labs(x = "Ränge", y = "Deckungsgrade nach Londo")
plot17 <- g+geom_point(aes(colour = r17 ))

############2016
r16 <- new.df$rank10-new.df$rank16
g <- ggplot(new.df, aes(rank16, abundanz16, label = name2)) +
  geom_line(size = 1, alpha = 0.4)+
  geom_line(aes(rank10, abundanz10), size = 3, alpha = 0.1)+
  scale_y_log10()+
  coord_flip()+
  scale_x_reverse()+
  geom_text(hjust = -0.3, angle = 0, size = 3, aes(colour = r16 ))+
  scale_colour_gradient2(limit = c(-25, + 25), low = "darkred", mid = "orange", high = "darkgreen", name = "Veränderung der Ränge \nim Vergleich zum Jahr 2010")+
  expand_limits(y = c(1,70))+
  ggtitle("2016")
plot16 <- g +
  geom_point(aes(colour = r16 ))

pdf("/media/gabriel/INTENSO/Stand_18_01_18/Abbildungen/cum1716.pdf", width=10, height=10)
grid.arrange(plot17,plot16, nrow=1, ncol=2, as.table=TRUE)
dev.off()
#2010-2015-------

r15 <- new.df$rank10-new.df$rank15
g15 <- ggplot(new.df, aes(rank15, abundanz15, label= name2)) +
  geom_line(size=1, alpha=0.4)+
  geom_line(aes(rank10, abundanz10), size=3, alpha=0.1)+
  scale_y_log10()+
  coord_flip()+
  scale_x_reverse()+
  geom_text(hjust = -0.3, angle = 0, size=3, aes(colour = r15 ))+
  scale_colour_gradient2(limit= c(-25, + 25), low = "darkred", mid = "orange", high = "darkgreen", name= "Veränderung der Ränge \nim Vergleich zum Jahr 2010")+
  expand_limits(y = c(1,70))+
  ggtitle("2015")+
  theme(legend.justification=c(1,0), legend.position=c(1,0))+
  labs(x = "R?nge", y = "Deckungsgrade nach Londo")
g15 + geom_point(aes(colour = r15 )) -> plot15

#Jahr 2014

r14 <- new.df$rank10-new.df$rank14
g14 <- ggplot(new.df, aes(rank14, abundanz14, label= name2)) +
  geom_line(size=1, alpha=0.4)+
  geom_line(aes(rank10, abundanz10), size=3, alpha=0.1)+
  scale_y_log10()+
  coord_flip()+
  scale_x_reverse()+
  geom_text(hjust = -0.3, angle = 0, size=3, aes(colour = r14 ))+
  scale_colour_gradient2(limit= c(-25, + 25), low= "darkred", mid = "orange", high = "darkgreen", name= "Ver?nderung der R?nge \nim Vergleich zum Jahr 2010")+
  expand_limits(y = c(1,70))+
  ggtitle("2014")+
  theme(legend.position="none")+
  labs(x = "Ränge",
       y = "Deckungsgrade nach Londo")
g14+geom_point(aes(colour = r14 )) -> plot14

#Jahr2013------

r13 <- new.df$rank10-new.df$rank13
g13 <- ggplot(new.df, aes(rank13, abundanz13, label= name2)) +
  geom_line(size=1, alpha=0.4)+
  geom_line(aes(rank10, abundanz10), size=3, alpha=0.1)+
  scale_y_log10()+
  coord_flip()+
  scale_x_reverse()+
  geom_text(hjust = -0.3, angle = 0, size=3, aes(colour = r13 ))+
  scale_colour_gradient2(limit= c(-25, + 25), low= "darkred", mid = "orange", high = "darkgreen", name= "Veränderung der Ränge \nim Vergleich zum Jahr 2010")+
  expand_limits(y = c(1,70))+
  ggtitle("2013")+
  theme(legend.position="none")+
  labs(x = "Ränge",
       y = "Deckungsgrade nach Londo")
g13+geom_point(aes(colour = r13 )) -> plot13

#Jahr2012---------------

r12 <- new.df$rank10-new.df$rank12
g12 <- ggplot(new.df, aes(rank12, abundanz12, label= name2)) +
  geom_line(size=1, alpha=0.4)+
  geom_line(aes(rank10, abundanz10), size=3, alpha=0.1)+
  scale_y_log10()+
  coord_flip()+
  scale_x_reverse()+
  geom_text(hjust = -0.3, angle = 0, size=3, aes(colour = r12 ))+
  scale_colour_gradient2(limit= c(-25, + 25), low= "darkred", mid = "orange", high = "darkgreen", name= "Veränderung der Ränge \nim Vergleich zum Jahr 2010")+
  expand_limits(y = c(1,70))+
  ggtitle("2012")+
  theme(legend.justification=c(1,0), legend.position=c(1,0))+
  labs(x = "Ränge",
       y = "Deckungsgrade nach Londo")
g12+geom_point(aes(colour = r12)) -> plot12

#Jahr 2011 -----------

r11 <- new.df$rank10-new.df$rank11
g11 <- ggplot(new.df, aes(rank11, abundanz11, label= name2)) +
  geom_line(size=1, alpha=0.4)+
  geom_line(aes(rank10, abundanz10), size=3, alpha=0.1)+
  scale_y_log10()+
  coord_flip()+
  scale_x_reverse()+
  geom_text(hjust = -0.3, angle = 0, size=3, aes(colour = r11 ))+
  scale_colour_gradient2(limit= c(-25, + 25), low= "darkred", mid = "orange", high = "darkgreen")+
  expand_limits(y = c(1,70))+
  ggtitle("2011")+
  labs(x = "Ränge",
       y = "Deckungsgrade nach Londo")+
  theme(legend.position="none")
g11 + geom_point(aes(colour = r11 )) -> plot11

library("gridExtra")

pdf("/media/gabriel/INTENSO/Stand_18_01_18/Abbildungen/cum11.pdf", width=10, height=10)
grid.arrange(plot11, plot12, nrow=1, ncol=2, as.table=TRUE)
dev.off()

pdf("/media/gabriel/INTENSO/Stand_18_01_18/Abbildungen/cum13.pdf", width=15, height=10)
grid.arrange(plot13, plot14, plot15,  nrow=1, ncol=3, as.table=TRUE)
dev.off()

pdf("/media/gabriel/INTENSO/Stand_18_01_18/Abbildungen/cum1716.pdf", width=10, height=10)
grid.arrange(plot17, plot18,  nrow=1, ncol=2, as.table=TRUE)
dev.off()
###############
#Korrelation
##############

#Alle arten sollen gegen alle Arten geplottet werden, um m?gliche Zusammenh?nge fest zustellen
#Dies wird erst f?r alle Jahre gemacht und dann speziell f?r 2016 bzw. jedes beliebige Jahr
#Berrechnung der Korrelationskoefizienten mit einer Mindesanzahl von 20 Wertpaaren, 
#da es viele NA in dem Datensatz gibt (bzw NULL).Ist die Korrelation nur mit gen?gent Datenpunkten sinnvoll.

#Erstellen von einer Matrix mit Zeilen = Arten und Spalten = Arten, -------------
# in der in jede Zelle der Zusammenhang der Arten aufgetragen
entmat<-matrix(0,(length(top1_daten[1,])-2), length(top1_daten[1,])-2) 
#Erstellen von einer  Matrix um jede Art mit jeder zu vergleichen (siehe Forschleife)
art <- Arten_NA[,-c(1,2)]
matrix<-matrix(0, length(top1_daten[,1]), 2)

#L?nge f?r die Forschleife 
ji<-(length(top1_daten[1,])-3)
ii<-(length(top1_daten[1,])-2)

for(j in 1:ji){
  for(i in j:ii){
    matrix[,1]<-art[,j]
    matrix[,2]<-art[,i]
    cp12<-psych::count.pairwise(matrix[,1], matrix[,2])#z?hlt die Zeilen vollst?ndigen Werten 
    mat1<-if(cp12 >=40){cor(matrix[,1], matrix[,2], method = "ken", use = "pairwise.complete.obs")} else{NA} 
    entmat[j, i]<-mat1
  }
}
entmat <- as.data.frame(entmat)
entmat <-  sapply(entmat, function(x) ifelse(x == 0, NA, x)) #Ersetzt die Nuller durch NA

colnames(art) -> colnames(entmat) #Bennenung der Zeilen und Spalten  
colnames(art) -> row.names(entmat)

melted_cormatl <- melt(entmat) #Erstellung einer Longtable
melted_cormatl <- na.omit(melted_cormatl) #l?schen der NA Zeilen 
pdf("/media/gabriel/INTENSO/Stand_18_01_18/Abbildungen/cor_all.pdf", width=7, height=7)
ggplot(data = melted_cormatl, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "red", high = "yellow", name="Kendall\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
dev.off()
#Beispiel Plot f?r Korrelationen die in dem ?bersichtsblock dann genauer angescheut werden k?nnen
plot(art$Anthoxanthum, art$Arrhenatherum.elatius)
plot(art$Veronica.chamaedrys, art$Leucanthemum.ircutianum)
plot(art$Anthoxanthum.odoratum, art$Rumex.acetosa)



#Aufteilung in Jahren hier f?r das Jahr 2016 (gleicher Code wie oben)
Alle_arten_Matrix<-top1_daten
art<-Alle_arten_Matrix[,-c(1,2)]

entmat<-matrix(0,(length(top1_daten[1,])-2), length(top1_daten[1,])-2)
matrix<-matrix(0, length(top1_daten[,1]), 2)

ji<-(length(top1_daten[1,])-3)
ii<-(length(top1_daten[1,])-2)

for(j in 1:ji){
  for(i in j:ii){
    matrix<-matrix(0, length(art[which(Alle_arten_Matrix$jahr==16),j]), 2) #Indiziert alle Werte aus dem Jahr 2016
    matrix[,1]<-art[which(Alle_arten_Matrix$jahr==16),j]
    matrix[,2]<-art[which(Alle_arten_Matrix$jahr==16),i]
    cp12<-count.pairwise(matrix[,1], matrix[,2])
    mat1<-if(cp12 >=7){cor(matrix[,1], matrix[,2], method = "ken", use = "pairwise.complete.obs")} else{NA}
    entmat[j, i]<-mat1
  }
}

entmat <- as.data.frame(entmat)
entmat <-  sapply(entmat, function(x) ifelse(x == 0, NA, x))

colnames(art) -> colnames(entmat)
colnames(art) -> row.names(entmat)

melted_cormatl <- melt(entmat)
melted_cormatl <- na.omit(melted_cormatl)
ggplot(data = melted_cormatl, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "red", high = "yellow", name="Kendall\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1))+
  coord_fixed()
#Beispiel Plot
art<-as.data.frame(art)
plot(art$Anthoxanthum.odoratum[which(Alle_arten_Matrix$Jahre==14)], art$Arrhenatherum.elatius[which(Alle_arten_Matrix$Jahre==14)])

#################
#Cluster Analyse
##################

enf <- as.data.frame(weide)
enf$jahr <- arten.env$jahr
distmatrix <- vegdist(log(artenm+1), method="bray")
Cluster.4 <- agnes(distmatrix, method="average")
pdf("/media/gabriel/INTENSO/Stand_18_01_18/Abbildungen/cluster.pdf", width=20, height=7)
plot(Cluster.4, which.plots=2, hang=-1, label = paste(enf$Weiden, enf$jahr), las=2,main= "Dendrogramm geordnet nach der Distanzmatrix aus dem Bray-Curtis-Koeffizenten")
rect.hclust(Cluster.4, k=10)
dev.off()
##########
#Abbildung der Beweidungsdauer
##########

#Darstellung:
GV <- read.delim2("/media/gabriel/INTENSO/statistik_2017/Basis_daten/Grossvieheinheitenha.csv")
GVTp<-GV[which(GV$Probeflachenname=="Ernsbach NO Weide"|GV$Probeflachenname=="Moosbach N Weide"| GV$Probeflachenname=="Untertal Weide"| GV$Probeflachenname=="Merkenbach O Weide"),]
GVTp$Jahr[which(GVTp$Jahr==9)] <- 2009
GVTp$Jahr[which(GVTp$Jahr==10)] <- 2010
GVTp$Jahr[which(GVTp$Jahr==11)] <- 2011
GVTp$Jahr[which(GVTp$Jahr==12)] <- 2012
GVTp$Jahr[which(GVTp$Jahr==13)] <- 2013
GVTp$Jahr[which(GVTp$Jahr==14)] <- 2014
GVTp$Jahr[which(GVTp$Jahr==15)] <- 2015
GVTp$Jahr[which(GVTp$Jahr==16)] <- 2016
GVTp$Jahr[which(GVTp$Jahr==17)] <- 2017
GVTp$Jahr[which(GVTp$Jahr==18)] <- 2018
GVTp$Jahr <- as.Date(GVTp$Jahr, "%Y")

pdf("/media/gabriel/INTENSO/Stand_18_01_18/Abbildungen/beweidung.pdf", width=10, height=5)
gv <- ggplot(GVTp, aes(y=Dauer.der.Beweidung.in.Tagen, x=Jahr, fill = Grossvieheinheiten.pro.Flaeche)) + geom_bar(stat="identity", colour = "white") + ylab("Tage der Beweidung")+ scale_fill_gradient(low = "green", high = "red")
gv + facet_grid(. ~ nummer)
dev.off()

