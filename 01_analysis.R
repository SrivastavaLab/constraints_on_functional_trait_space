# species x traits PCA and related illustrations

# require(devtools)
# install_github("vanderleidebastiani/SYNCSA") # 21 Dec 2017

library(SYNCSA)
library(fwdata)
# library(ade4)
# library(NbClust)

fwdata::fw_versions(local = FALSE)
fwd <- fw_data("0.7.7")
str(fwd, max.level = 1)

names(fwd)

trait.full_original <- fwd$traits
remover <- c(4461, 6506, 6511,	6516, 7886,	6981,	6986,	6991,	7261,	6066,	6236,	6531,	6926,
             8026,	6231,	6786,	4021,	6186,	6226,	6921,	7316,	4736,	6806,	4076,	6916,	4681,
             7251,	6801,	6771,	6776,	6781,	6791,	6936,	6941,	6946,	6951,	5201,	5206,	5211,
             5421)
length(remover)
# remove animals which have more than 7 NAs, and test that in this roundabout way
sort(as.numeric(remover))==sort(as.numeric(trait.full_original$species_id[apply(is.na(trait.full_original[,-c(1:25)]), 1, sum)>7]))

head(trait.full_original)
trait.full_complete_spp <- trait.full_original[-which(trait.full_original$species_id %in% remover),]
dim(trait.full_complete_spp)
head(trait.full_complete_spp)

# convert traits into ranks
traits<-trait.full_complete_spp[,-c(1:25)]
traits<-traits[,]
traits <- sapply(1:ncol(traits), function(i) rank(traits[,i], na.last = "keep")) # transform each trait in ranks
colnames(traits) <- colnames(trait.full_complete_spp)[-c(1:25)]
rownames(traits) <- rownames(trait.full_complete_spp)

# View(traits)
head(traits)
dim(traits)

#Run PCA, get results to interpret
RES_pca<-SYNCSA::pca(traits)
RES_pca$eigenvalues
head(RES_pca$individuals)
RES_pca$variables

head(trait.full)
trait.full$ord

taxa.name<-trait.full$ord
taxa.name<-ifelse(is.na(taxa.name), trait.full$subclass, taxa.name)
taxa.name<-ifelse(is.na(taxa.name), trait.full$class, taxa.name)
taxa.name<-ifelse(is.na(taxa.name), trait.full$phylum, taxa.name)

taxa.name[which(trait.full$family=="Chironomidae")] <- "Chironomidae"
taxa.name[which(trait.full$family=="Culicidae")] <- "Culicidae"
taxa.name[which(taxa.name=="Diptera")] <- "Other_Diptera"

taxa.name <- ifelse(taxa.name == "Trombidiformes", "Acari", taxa.name)
taxa.name <- ifelse(taxa.name == "Rhynchobdellida", "Hirudinea", taxa.name)
taxa.name <- ifelse(taxa.name == "Turbellaria", "Platyhelminthes", taxa.name)
taxa.name <- ifelse(taxa.name == "Harpacticoida", "Copepoda", taxa.name)
taxa.name <- ifelse(taxa.name == "Cyclopoida", "Copepoda", taxa.name)
taxa.name <- ifelse(taxa.name == "Haplotaxida", "Oligochaeta", taxa.name)
taxa.name <- ifelse(taxa.name == "Nematomorpha", "Nematoda", taxa.name)

position.remove<-which(taxa.name=="Annelida") # remove Annelida
position.remove<-c(position.remove,which(is.na(taxa.name))) # remove unknown taxa 
position.remove

taxa.name<-taxa.name[-1*position.remove] # remove Annelida and unknown
taxa.name
table(taxa.name)

gra_options<-matrix(NA,length(table(taxa.name)),3)
rownames(gra_options)<-names(table(taxa.name))
colnames(gra_options)<-c("pch","col","freq")
gra_options[,3]<-table(taxa.name)

gra_options["Acari",1:2]<-c(17,"black") # ok
gra_options["Chironomidae",1:2]<-c(19,"black") #ok
gra_options["Coleoptera",1:2]<-c(19,"red") #ok
gra_options["Copepoda",1:2]<-c(17,"blue") # ok
gra_options["Culicidae",1:2]<-c(19,"grey45")  #ok
gra_options["Diplostraca",1:2]<-c(15,"black") # ok
#gra_options["Entomobryomorpha",1:2]<-c(15,"blue") #ok
#gra_options["Ephemeroptera",1:2]<-c(15,"gold3") #ok
gra_options["Hemiptera",1:2]<-c(21,"red") # ok
gra_options["Hirudinea",1:2]<-c(17,"gold3") #ok
gra_options["Lepidoptera",1:2]<-c(19,"blue") #ok
#gra_options["Megaloptera",1:2]<-c(19,"gold3") #ok
#gra_options["Nematoda",1:2]<-c(17,"red") # ok
gra_options["Odonata",1:2]<-c(24,"blue") #ok
gra_options["Oligochaeta",1:2]<-c(24,"green4") # ok
gra_options["Opisthopora",1:2]<-c(15,"green4") #ok
gra_options["Other_Diptera",1:2]<-c(21,"black") #ok
gra_options["Panpulmonata",1:2]<-c(15,"red") #ok
gra_options["Platyhelminthes",1:2]<-c(17,"green4") #ok
gra_options["Podocopida",1:2]<-c(24,"red") # ok
gra_options["Trichoptera",1:2]<-c(22,"red") # ok

gra_options 
gra_options[order(as.numeric(gra_options[,3] )),]

pch_vec<-as.numeric(gra_options[,1])
names(pch_vec)<-rownames(gra_options)
pch_col<-gra_options[,2]

setwd("./Current Results - 0.7.7")
pdf("plots_axes_0.7.7_ranks.pdf", 8,12)

par(mfrow=c(4,2), oma = c(1, 1, 2.5, 1),mgp = c(2, 0.5, 0),font.lab=1,cex.lab=1, cex.axis=1)
layout(matrix(c(1:8),4,2,byrow=T), widths=c(1,1,1,1),heights=c(1,1,1,0.4))

axes <- 1:2 # select axes 

ind.scores <-RES_pca$individuals[-1*position.remove,axes] # remove unknown taxa and Annelida
ind.scores
per.inertia <- RES_pca$eigenvalues[axes,2]
per.inertia <- per.inertia*100
x.lim<-c(min(ind.scores[, 1]), max(ind.scores[, 1]))
y.lim<-c(min(ind.scores[, 2]), max(ind.scores[, 2]))
xlim<-c(min(y.lim,x.lim), max(y.lim,x.lim))
ylim<-xlim

par(mar = c(4, 4, 0.5, 0.5))
plot(ind.scores[,1], ind.scores[,2], xlab = paste("PC ", axes[1], " (", round(per.inertia[1], 2), "%)", sep = ""), ylab = paste("PC ", axes[2], " (", round(per.inertia[2], 2), "%)", sep = ""), type = "n", xlim = x.lim, ylim = y.lim, asp = 1, bty = "n", xaxt = "n", yaxt = "n",  mgp = c(1, 1, 0))
points(ind.scores[,1], ind.scores[, 2], pch = pch_vec[taxa.name], col = pch_col[taxa.name])
abline(h = 0, v = 0)

var.scores <- RES_pca$variables[,axes]
var.scores
cor.min<-0.5
var.scores<-var.scores[apply(abs(var.scores)>=cor.min, 1, sum)>0,] # filter correlation
var.scores
circle <- seq(0, 2*pi, length = 200)
par(mar=c(1,1,1,1))
graphics::plot(cos(circle), sin(circle), type = 'l', col = "black", xlab = "", ylab = "", xlim = c(-1, 1), ylim = c(-1, 1), asp = 1, bty = "n", xaxt = "n", yaxt = "n")
graphics::abline(h = 0, v = 0, lty = 3, col = "black")
graphics::arrows(0 ,0, x1 = var.scores[,1]*1, y1 = var.scores[,2]*1, length = 0.05, col = "black", lwd =0.7)

rownames(var.scores)
arrow <- 1
rownames(var.scores)[arrow]
text((var.scores[arrow,1]*1.1)+0.04, (var.scores[arrow, 2]*1.1)+0.01, labels = paste(rownames(var.scores)[arrow]), cex = 0.8)
arrow <- 2
rownames(var.scores)[arrow]
text((var.scores[arrow,1]*1.1)-0.05, (var.scores[arrow, 2]*1.1)-0.01, labels = paste(rownames(var.scores)[arrow]), cex = 0.8)
arrow <- 3
rownames(var.scores)[arrow]
text((var.scores[arrow,1]*1.1), (var.scores[arrow, 2]*1.1), labels = paste(rownames(var.scores)[arrow]), cex = 0.8)
arrow <- 4
rownames(var.scores)[arrow]
text((var.scores[arrow,1]*1.1), (var.scores[arrow, 2]*1.1)-0.02, labels = paste(rownames(var.scores)[arrow]), cex = 0.8)
arrow <- 5
rownames(var.scores)[arrow]
text((var.scores[arrow,1]*1.1), (var.scores[arrow, 2]*1.1), labels = paste(rownames(var.scores)[arrow]), cex = 0.8)
arrow <- 6
rownames(var.scores)[arrow]
text((var.scores[arrow,1]*1.1), (var.scores[arrow, 2]*1.1), labels = paste(rownames(var.scores)[arrow]), cex = 0.8)
arrow <- 7
rownames(var.scores)[arrow]
text((var.scores[arrow,1]*1.1), (var.scores[arrow, 2]*1.1), labels = paste(rownames(var.scores)[arrow]), cex = 0.8)
arrow <- 8
rownames(var.scores)[arrow]
text((var.scores[arrow,1]*1.1), (var.scores[arrow, 2]*1.1)+0.02, labels = paste(rownames(var.scores)[arrow]), cex = 0.8)
arrow <- 9
rownames(var.scores)[arrow]
text((var.scores[arrow,1]*1.1)+0.04, (var.scores[arrow, 2]*1.1)-0.01, labels = paste(rownames(var.scores)[arrow]), cex = 0.8)
arrow <- 10
rownames(var.scores)[arrow]
text((var.scores[arrow,1]*1.1), (var.scores[arrow, 2]*1.1)-0.02, labels = paste(rownames(var.scores)[arrow]), cex = 0.8)
arrow <- 11
rownames(var.scores)[arrow]
text((var.scores[arrow,1]*1.1)+0.04, (var.scores[arrow, 2]*1.1), labels = paste(rownames(var.scores)[arrow]), cex = 0.8)
arrow <- 12
rownames(var.scores)[arrow]
text((var.scores[arrow,1]*1.1), (var.scores[arrow, 2]*1.1), labels = paste(rownames(var.scores)[arrow]), cex = 0.8)
arrow <- 13
rownames(var.scores)[arrow]
text((var.scores[arrow,1]*1.1), (var.scores[arrow, 2]*1.1), labels = paste(rownames(var.scores)[arrow]), cex = 0.8)
arrow <- 14
rownames(var.scores)[arrow]
text((var.scores[arrow,1]*1.1), (var.scores[arrow, 2]*1.1)-0.02, labels = paste(rownames(var.scores)[arrow]), cex = 0.8)
arrow <- 15
rownames(var.scores)[arrow]
text((var.scores[arrow,1]*1.1), (var.scores[arrow, 2]*1.1), labels = paste(rownames(var.scores)[arrow]), cex = 0.8)
arrow <- 16
rownames(var.scores)[arrow]
text((var.scores[arrow,1]*1.1)-0.03, (var.scores[arrow, 2]*1.1), labels = paste(rownames(var.scores)[arrow]), cex = 0.8)
arrow <- 17
rownames(var.scores)[arrow]
text((var.scores[arrow,1]*1.1), (var.scores[arrow, 2]*1.1), labels = paste(rownames(var.scores)[arrow]), cex = 0.8)
arrow <- 18
rownames(var.scores)[arrow]
text((var.scores[arrow,1]*1.1)-0.09, (var.scores[arrow, 2]*1.1)-0.04, labels = paste(rownames(var.scores)[arrow]), cex = 0.8)
arrow <- 19
rownames(var.scores)[arrow]
text((var.scores[arrow,1]*1.1)-0.07, (var.scores[arrow, 2]*1.1)-0.03, labels = paste(rownames(var.scores)[arrow]), cex = 0.8)
arrow <- 20
rownames(var.scores)[arrow]
text((var.scores[arrow,1]*1.1)-0.07, (var.scores[arrow, 2]*1.1)-0.04, labels = paste(rownames(var.scores)[arrow]), cex = 0.8)
arrow <- 21
rownames(var.scores)[arrow]
text((var.scores[arrow,1]*1.1), (var.scores[arrow, 2]*1.1), labels = paste(rownames(var.scores)[arrow]), cex = 0.8)
arrow <- 22
rownames(var.scores)[arrow]
text((var.scores[arrow,1]*1.1), (var.scores[arrow, 2]*1.1), labels = paste(rownames(var.scores)[arrow]), cex = 0.8)
arrow <- 23
rownames(var.scores)[arrow]
text((var.scores[arrow,1]*1.1), (var.scores[arrow, 2]*1.1)-0.05, labels = paste(rownames(var.scores)[arrow]), cex = 0.8)
arrow <- 24
rownames(var.scores)[arrow]
text((var.scores[arrow,1]*1.1)+0.04, (var.scores[arrow, 2]*1.1)-0.05, labels = paste(rownames(var.scores)[arrow]), cex = 0.8)
arrow <- 25
rownames(var.scores)[arrow]
text((var.scores[arrow,1]*1.1)+0.01, (var.scores[arrow, 2]*1.1)-0.03, labels = paste(rownames(var.scores)[arrow]), cex = 0.8)
arrow <- 26
rownames(var.scores)[arrow]
text((var.scores[arrow,1]*1.1), (var.scores[arrow, 2]*1.1), labels = paste(rownames(var.scores)[arrow]), cex = 0.8)
arrow <- 27
rownames(var.scores)[arrow]
text((var.scores[arrow,1]*1.1)-0.05, (var.scores[arrow, 2]*1.1), labels = paste(rownames(var.scores)[arrow]), cex = 0.8)
arrow <- 28
rownames(var.scores)[arrow]
text((var.scores[arrow,1]*1.1), (var.scores[arrow, 2]*1.1)-0.02, labels = paste(rownames(var.scores)[arrow]), cex = 0.8)

# text(var.scores[,1]*1.1, var.scores[, 2]*1.1, labels = rownames(var.scores)[], cex = 0.9)

# rownames(var.scores)
# rownames(var.scores)[which(rownames(var.scores)=="LO1")]<-paste("LO1","DM6")
# rownames(var.scores)[which(rownames(var.scores)=="BF2")]<-paste("BF2","CP2")
# rownames(var.scores)[which(rownames(var.scores)=="LO7")]<-paste("LO7","MD8","DM1")
# rownames(var.scores)[which(rownames(var.scores)=="FG1")]<-paste("FD1", "FG1")
# 
# rownames(var.scores)
# var.plot<-c(1,3,5:7,10:19,22,24,26)
# var.plot
# text(var.scores[var.plot,1]*1.1, var.scores[var.plot, 2]*1.1, labels = rownames(var.scores)[var.plot], cex = 0.8)
# text((var.scores[9,1]*1.1)+0, (var.scores[9, 2]*1.1)+0.02, labels = paste(rownames(var.scores)[9]), cex = 0.8)
# text((var.scores[8,1]*1.1)+0, (var.scores[8, 2]*1.1)-0.04, labels = paste(rownames(var.scores)[8]), cex = 0.8)
# text((var.scores[25,1]*1.1)+0.04, (var.scores[25, 2]*1.1)-0.0, labels = paste(rownames(var.scores)[25]), cex = 0.8)

axes <- c(1,3) # select axes 
ind.scores <-RES_pca$individuals[-1*position.remove,axes] # remove unknown taxa and Annelida
ind.scores
per.inertia <- RES_pca$eigenvalues[axes,2]
per.inertia <- per.inertia*100
x.lim<-c(min(ind.scores[, 1]), max(ind.scores[, 1]))
y.lim<-c(min(ind.scores[, 2]), max(ind.scores[, 2]))
xlim<-c(min(y.lim,x.lim), max(y.lim,x.lim))
ylim<-xlim

par(mar = c(4, 4, 0.5, 0.5))
plot(ind.scores[,1], ind.scores[,2], xlab = paste("PC ", axes[1], " (", round(per.inertia[1], 2), "%)", sep = ""), ylab = paste("PC ", axes[2], " (", round(per.inertia[2], 2), "%)", sep = ""), type = "n", xlim = x.lim, ylim = y.lim, asp = 1, bty = "n", xaxt = "n", yaxt = "n",  mgp = c(1, 1, 0))
points(ind.scores[,1], ind.scores[, 2], pch = pch_vec[taxa.name], col = pch_col[taxa.name])
abline(h = 0, v = 0)

var.scores <- RES_pca$variables[,axes]
var.scores
cor.min<-0.5
var.scores<-var.scores[apply(abs(var.scores)>=cor.min, 1, sum)>0,] # filter correlation
# var.scores<-var.scores[apply(abs(var.scores[,2,drop=FALSE])>=cor.min, 1, sum)>0,] # filter correlation
var.scores

circle <- seq(0, 2*pi, length = 200)
par(mar=c(1,1,1,1))
# graphics::plot(cos(circle), sin(circle), type = 'l', col = "gray", xlab = "", ylab = "", xlim = c(-1, 1), ylim = c(-1, 1), asp = 1)
graphics::plot(cos(circle), sin(circle), type = 'l', col = "black", xlab = "", ylab = "", xlim = c(-1, 1), ylim = c(-1, 1), asp = 1, bty = "n", xaxt = "n", yaxt = "n")
graphics::abline(h = 0, v = 0, lty = 3, col = "black")
graphics::arrows(0 ,0, x1 = var.scores[,1]*1, y1 = var.scores[,2]*1, length = 0.05, col = "black", lwd =0.7)
# text(var.scores[,1]*1.1, var.scores[, 2]*1.1, labels = rownames(var.scores), cex = 0.9)

rownames(var.scores)
var.plot<-c(1:5,7:9,11:13,15:18, 20:22) #1:23
# var.plot
text(var.scores[var.plot,1]*1.1, var.scores[var.plot, 2]*1.1, labels = rownames(var.scores)[var.plot], cex = 0.8)

arrow <- 14
rownames(var.scores)[arrow]
text((var.scores[arrow,1]*1.1)-0.02, (var.scores[arrow, 2]*1.1)+0.06, labels = paste(rownames(var.scores)[arrow]), cex = 0.8)
arrow <- 19
rownames(var.scores)[arrow]
text((var.scores[arrow,1]*1.1)-0.05, (var.scores[arrow, 2]*1.1)+0.04, labels = paste(rownames(var.scores)[arrow]), cex = 0.8)
arrow <- 6
rownames(var.scores)[arrow]
text((var.scores[arrow,1]*1.1)+0.05, (var.scores[arrow, 2]*1.1), labels = paste(rownames(var.scores)[arrow]), cex = 0.8)
arrow <- 10
rownames(var.scores)[arrow]
text((var.scores[arrow,1]*1.1)+0.02, (var.scores[arrow, 2]*1.1), labels = paste(rownames(var.scores)[arrow]), cex = 0.8)
arrow <- 23
rownames(var.scores)[arrow]
text((var.scores[arrow,1]*1.1), (var.scores[arrow, 2]*1.1)+0.02, labels = paste(rownames(var.scores)[arrow]), cex = 0.8)

# var.plot<-c(1:6,8:9,11:16,18:24) #16:24
# var.plot
# text(var.scores[var.plot,1]*1.1, var.scores[var.plot, 2]*1.1, labels = rownames(var.scores)[var.plot], cex = 0.8)
# text((var.scores[7,1]*1.1)+0.03, (var.scores[7, 2]*1.1)-0.02, labels = rownames(var.scores)[7], cex = 0.8)
# text((var.scores[10,1]*1.1)+0.00, (var.scores[10, 2]*1.1)+0.03, labels = rownames(var.scores)[10], cex = 0.8)
# text((var.scores[17,1]*1.1)+0.00, (var.scores[17, 2]*1.1)-0.04, labels = rownames(var.scores)[17], cex = 0.8)



axes <- c(1,4) # select axes 

ind.scores <-RES_pca$individuals[-1*position.remove,axes] # remove unknown taxa and Annelida
ind.scores
per.inertia <- RES_pca$eigenvalues[axes,2]
per.inertia <- per.inertia*100
x.lim<-c(min(ind.scores[, 1]), max(ind.scores[, 1]))
y.lim<-c(min(ind.scores[, 2]), max(ind.scores[, 2]))
xlim<-c(min(y.lim,x.lim), max(y.lim,x.lim))
ylim<-xlim

par(mar = c(4, 4, 0.5, 0.5))
plot(ind.scores[,1], ind.scores[,2], xlab = paste("PC ", axes[1], " (", round(per.inertia[1], 2), "%)", sep = ""), ylab = paste("PC ", axes[2], " (", round(per.inertia[2], 2), "%)", sep = ""), type = "n", xlim = x.lim, ylim = y.lim, asp = 1, bty = "n", xaxt = "n", yaxt = "n",  mgp = c(1, 1, 0))
points(ind.scores[,1], ind.scores[, 2], pch = pch_vec[taxa.name], col = pch_col[taxa.name])
abline(h = 0, v = 0)

var.scores <- RES_pca$variables[,axes]
var.scores
cor.min<-0.5

var.scores<-var.scores[apply(abs(var.scores)>=cor.min, 1, sum)>0,] # filter correlation
# var.scores<-var.scores[apply(abs(var.scores[,2,drop=FALSE])>=cor.min, 1, sum)>0,] # filter correlation
var.scores

par(mar=c(1,1,1,1))
circle <- seq(0, 2*pi, length = 200)
# graphics::plot(cos(circle), sin(circle), type = 'l', col = "gray", xlab = "", ylab = "", xlim = c(-1, 1), ylim = c(-1, 1), asp = 1)
graphics::plot(cos(circle), sin(circle), type = 'l', col = "black", xlab = "", ylab = "", xlim = c(-1, 1), ylim = c(-1, 1), asp = 1, bty = "n", xaxt = "n", yaxt = "n")
graphics::abline(h = 0, v = 0, lty = 3, col = "black")
graphics::arrows(0 ,0, x1 = var.scores[,1]*1, y1 = var.scores[,2]*1, length = 0.05, col = "black", lwd =0.7)
# text(var.scores[,1]*1.1, var.scores[, 2]*1.1, labels = rownames(var.scores), cex = 0.9)


rownames(var.scores)
var.plot<-c(1:3,6,13:16,20:24) # 1:24
var.plot
text(var.scores[var.plot,1]*1.1, var.scores[var.plot, 2]*1.1, labels = rownames(var.scores)[var.plot], cex = 0.8)

arrow <- 5
rownames(var.scores)[arrow]
text((var.scores[arrow,1]*1.1)-0.06, (var.scores[arrow, 2]*1.1)+0.03, labels = paste(rownames(var.scores)[arrow]), cex = 0.8)
arrow <- 8
rownames(var.scores)[arrow]
text((var.scores[arrow,1]*1.1)+0.05, (var.scores[arrow, 2]*1.1)-0.03, labels = paste(rownames(var.scores)[arrow]), cex = 0.8)
arrow <- 19
rownames(var.scores)[arrow]
text((var.scores[arrow,1]*1.1)-0.01, (var.scores[arrow, 2]*1.1), labels = paste(rownames(var.scores)[arrow]), cex = 0.8)
arrow <- 10
rownames(var.scores)[arrow]
text((var.scores[arrow,1]*1.1)-0.05, (var.scores[arrow, 2]*1.1)-0.05, labels = paste(rownames(var.scores)[arrow]), cex = 0.8)
arrow <- 17
rownames(var.scores)[arrow]
text((var.scores[arrow,1]*1.1)-0.05, (var.scores[arrow, 2]*1.1)-0.05, labels = paste(rownames(var.scores)[arrow]), cex = 0.8)
arrow <- 9
rownames(var.scores)[arrow]
text((var.scores[arrow,1]*1.1)+0.09, (var.scores[arrow, 2]*1.1)+0.05, labels = paste(rownames(var.scores)[arrow]), cex = 0.8)

arrow <- 18
rownames(var.scores)[arrow]
rownames(var.scores)[which(rownames(var.scores)=="FD1")]<-paste(rownames(var.scores)[arrow], "FD1")
arrow <- 4
rownames(var.scores)[arrow]
text((var.scores[arrow,1]*1.1)+0.07, (var.scores[arrow, 2]*1.1), labels = paste(rownames(var.scores)[arrow]), cex = 0.8)

arrow <- 11
rownames(var.scores)[arrow]
rownames(var.scores)[which(rownames(var.scores)=="RF3")]<-paste(rownames(var.scores)[arrow], "RF3")
arrow <- 12
rownames(var.scores)[arrow]
text((var.scores[arrow,1]*1.1)+0.01, (var.scores[arrow, 2]*1.1)-0.04, labels = paste(rownames(var.scores)[arrow]), cex = 0.8)

arrow <- 7
rownames(var.scores)[arrow]
text((var.scores[arrow,1]*1.1), (var.scores[arrow, 2]*1.1)-0.04, labels = paste(rownames(var.scores)[arrow]), cex = 0.8)



# rownames(var.scores)
# var.plot<-c(1,3:11,13:22)
# var.plot
# text(var.scores[var.plot,1]*1.1, var.scores[var.plot, 2]*1.1, labels = rownames(var.scores)[var.plot], cex = 0.8)
# text((var.scores[2,1]*1.1)-0.0, (var.scores[2, 2]*1.1)-0.04, labels = rownames(var.scores)[2], cex = 0.8)
# text((var.scores[12,1]*1.1)-0.0, (var.scores[12, 2]*1.1)-0.02, labels = rownames(var.scores)[12], cex = 0.8)

# text((var.scores[5,1]*1.1)-0.05, (var.scores[5, 2]*1.1)+0.01, labels = rownames(var.scores)[5], cex = 0.9)

par(mar=c(0,0,0,0))

legend_order<-order(as.numeric(gra_options[,3]),decreasing = TRUE)
gra_options[legend_order,]
legend_order<-c(legend_order[2:3],legend_order[1],legend_order[4:length(legend_order)]) # organize lengend for show Culicidae, Chironomidae and Other Diptera first all
legend_order

rownames(gra_options)[which(rownames(gra_options)=="Other_Diptera")] <- "Other Diptera"

plot(1,1, type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n")
legend("topright",legend = rownames(gra_options)[legend_order[1:8]],pch = pch_vec[legend_order[1:8]],col = pch_col[legend_order[1:8]], bty = "n", ncol = 2)

plot(1,1, type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n")
legend("topleft",legend = rownames(gra_options)[legend_order[9:16]],pch = pch_vec[legend_order[9:16]],col = pch_col[legend_order[9:16]], bty = "n", ncol = 2)

dev.off()

#Eigenvalues axes
# eigen<- RES_pca$eigenvalues
# barplot(eigen$Perc.Inertia, ylim=c(0,0.20))


#capture outputs in excel to sort traits and species then interpret
# options(max.print = 999999)
write.table(RES_pca$variables,sep="\t", file="RES_pca_variables_0.7.7_ranks.txt")
write.table(cbind(species_id=trait.full$species_id,RES_pca$individuals),sep="\t", file="RES_pca_individuals_0.7.7_ranks.txt", row.names=FALSE)
# write.table(trait.full,sep="\t", file="trait.full_0.7.7.txt")
write.table(RES_pca$eigenvalues,sep="\t", file="eigenvalues_0.7.7_ranks.txt")

# #Very basic plots
# plot.scores(RES_pca$individuals,axis=c(1,2),text=F,points=T)
# plot.correlations(RES_pca$variables,axis=c(1,2))
# 
# 
# ##plot traits: biplots
# plot.scores(RES_pca$variables,axis=c(1,2),xlab= "axis 1", ylab = "axis 2", cex= 0.8, text=T,points=F) #axes 1 : 2
# plot.scores(RES_pca$variables,axis=c(1,3),xlab= "axis 1", ylab = "axis 3", cex= 0.8, text=T,points=F) #axes 1 : 3
# plot.scores(RES_pca$variables,axis=c(1,4),xlab= "axis 1", ylab = "axis 4", cex= 0.8, text=T,points=F) #axes 1 : 4
# 
# #1-D plots of correlation between variables (traits) and axes
# sco.label(RES_pca$variables[,1], label = row.names(RES_pca$variables), pos.lab= 0.3, clabel = 0.8, boxes= FALSE, include.origin = TRUE, sub="axis 1", csub=1.5) #axis 1
# sco.label(RES_pca$variables[,2], label = row.names(RES_pca$variables), pos.lab= 0.3, clabel = 0.8, boxes= FALSE, include.origin = TRUE, sub="axis 2", csub=1.5) #axis 2
# sco.label(RES_pca$variables[,3], label = row.names(RES_pca$variables), pos.lab= 0.3, clabel = 0.8,  boxes= FALSE, include.origin = TRUE, sub="axis 3", csub=1.5) #axis 3
# sco.label(RES_pca$variables[,4], label = row.names(RES_pca$variables), pos.lab= 0.3, clabel = 0.8,  boxes= FALSE, include.origin = TRUE, sub="axis 4", csub=1.5) #axis 4
# 
# 
# ##plot inverts by order
# plot.scores(RES_pca$individuals,axis=c(1,2),xlab= "axis 1", ylab = "axis 2", cex= 0.8, pch=unclass(trait.full$ord), text=F,points=T) #axes 1 : 2
# plot.scores(RES_pca$individuals,axis=c(1,3),xlab= "axis 1", ylab = "axis 3", cex= 0.8, pch=unclass(trait.full$ord), text=F,points=T) #axes 1 : 3
# plot.scores(RES_pca$individuals,axis=c(1,4),xlab= "axis 1", ylab = "axis 4", cex= 0.8, pch=unclass(trait.full$ord), text=F,points=T) #axes 1 : 4
# 
# ####################
# ##Cluster species in trait PCA using kmeans
# #Find optimal number of clusters -> best partition?
# score<-RES_pca$individuals
# res<-NbClust(score, diss=NULL, distance = "euclidean", min.nc=2, max.nc=10, method = "kmeans", index = "all") 
# res$All.index
# res$Best.nc
# res$Best.partition#2 clusters !
# 
# #make K-Means Cluster Analysis with selected number of clusters
# fit <- kmeans(score, 2) # best clustering cluster solution
# # get cluster means 
# aggregate(score,by=list(fit$cluster),FUN=mean)
# # append cluster assignment
# clusters <- data.frame(score, fit$cluster)
# clusters
# ##plot K-means clusters on RLQ_traits
# par(mfrow = c(1, 1))
# syndromes<-factor(clusters$fit.cluster)
# coul <- c("red", "blue","yellow")
# s.chull(RES_pca$individuals,syndromes,cpoi=1,col=coul,clab=1, optchull = 1) 
# #s.class(RES_pca$individuals,syndromes,cpoi=1,col=coul,clab=1) 
# 
# 
