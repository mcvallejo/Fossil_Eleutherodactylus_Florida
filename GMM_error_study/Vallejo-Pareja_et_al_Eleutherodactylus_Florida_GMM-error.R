##############################################################################
###Eleutherodactylus in the Oligocene of Florida
###Erro Study
###Geometric Morphometric of the Humerus of Eleutherodactylus
###Vallejo-Pareja et al in prep
###############################################################################

rm(list=ls(all=T))
getwd()

install.packages("geomorph")
install.packages("geiger")
install.packages("phytools")
library(geomorph)
library(ape)
library(geiger)
library(phytools)


##Following the tutorial from Sheratt
#to read into R saved as CSV not .nts
filelist <- list.files(pattern = ".csv" ) # makes a list of all .txt files in working directory
names <- gsub (".csv", "", filelist) # extracts names of specimens from the file name
coords = NULL # make an empty object
for (i in 1:length(filelist)){
  tmp <- as.matrix(read.csv(filelist[i]))
  coords <- rbind(coords, tmp)
}
coords <- arrayspecs(coords, 14, 3, 16)
dimnames(coords)[[3]] <- names


#Generalized Procrustes analyses
genp<-gpagen(coords)

#looking at the results
print(genp)
summary(genp)
genp$coords
genp$Csize

#Importing classifier data

classifier<-read.table("Classifier_error.txt", header = TRUE)

#Procrustis ANOVA
AN2<-procD.lm (genp$coords ~ classifier$Catalog_number/classifier$Repetition )
AN2

summary (AN2)

#Repeatability and error (Following the blog from Emma Sherrat)
#MS(individual)-MS(individual/repetition)/number of repetitions



# PCA analysis
PCA<-gm.prcomp(genp$coords)
PCA

#Plot og PCA Analysis
names2<-as.factor(c("E. antillensis", "E. antillensis", "E. antillensis","E. antillensis", 
                    "E. ricordii", "E. ricordii", "E. ricordii","E. ricordii",
                    "Eleutherodactylus_sp_UF-H-494614", "Eleutherodactylus_sp_UF-H-494614", "Eleutherodactylus_sp_UF-H-494614","Eleutherodactylus_sp_UF-H-494614",
                    "Eleutherodactylus_sp_UF-UF-501310", "Eleutherodactylus_sp_UF-UF-501310", "Eleutherodactylus_sp_UF-UF-501310", "Eleutherodactylus_sp_UF-UF-501310"))

plot(PCA, axis1 = 1, axis2 = 2, label = TRUE, main = "Error Study", pch = 15, cex = 1, col = c("blue", "green", "purple",  "orange")[names2])
legend('bottomright', pch=15, col=c("blue", "green", "purple",  "orange"),
       paste('', levels(names2)),  cex=0.6)


#plotRefToTarget to explore vatiatio in space
#to explore the PCA space and see the variation with the global mean shape
picknplot.shape(plot(PCA), method = "vector")


PCA
summary(PCA)
print(PCA)
#plotTangentSpace won't work in this version of Morphosource
#instead use plot.gm.prcomp plot.gm.prcomp (PCA, axis =1, axis=2, label = TRUE)

#C size vs PC1
plot(genp$Csize, PCA$x[,1], col = c("blue", "green", "purple",  "orange")[names2])
legend('bottomright', pch=15, col=c("blue", "green", "purple",  "orange"),
       paste('', levels(names2)),  cex=0.6)

######



