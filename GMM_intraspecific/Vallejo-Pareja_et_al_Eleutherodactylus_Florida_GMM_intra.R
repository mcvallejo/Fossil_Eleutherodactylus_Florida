##############################################################################
###Eleutherodactylus in the Oligocene of Florida
###Intraspecific Variation
###Geometric Morphometric of the Humerus - Fossils not included
###Vallejo-Pareja et al in prep
###############################################################################

rm(list=ls(all=T))

install.packages("geomorph")
install.packages("geiger")
install.packages("phytools")
library(geomorph)
library(ape)
library(geiger)
library(phytools)


filelist <- list.files(pattern = ".csv" ) # makes a list of all .txt files in working directory
names <- gsub (".csv", "", filelist) # extracts names of specimens from the file name
coords = NULL # make an empty object

for (i in 1:length(filelist)){
  tmp <- as.matrix(read.csv(filelist[i]))
  coords <- rbind(coords, tmp)
}

coords <- arrayspecs(coords, 14, 3, 28) #here it goes: landmarks, dimensions, specimens
dimnames(coords)[[3]] <- names

#Generalized procrustes analyzes
Y.gpa_intra<-gpagen(coords) 
print(Y.gpa_intra)
summary(Y.gpa_intra)
Y.gpa_intra$coords
Y.gpa_intra$Csize

#plot to see the distribution of the landmarks
plot(Y.gpa_intra, label=TRUE) 

#Classifier file with all the information of the specimen - in the same order as data entered
classifier_intra<-read.table("Classifier_intraspecific.txt", header = TRUE)
sp <- as.factor(classifier_intra$Species)
maturity<-as.factor(classifier_intra$Distal_condyle)

#PCA of the procrustes distances
PCA_intra<-gm.prcomp(Y.gpa_intra$coords)
summary(PCA_intra) #this gives the eigenvalues 
PCA_intra$x #this gives the components for each specimen which I think are the PC scores

#Plot the PCA and color coding by species
levels(sp)
mycols_intra <- c("#44AA99", "#AA4499")
plot<-plot(PCA_intra, axis1 = 1, axis2 = 2, label = TRUE, main = "GMM Humerus", 
           pch =c(21,24)[maturity], cex = 0.6*Y.gpa_intra$Csize, col=mycols_intra[sp], 
           bg=adjustcolor(mycols_intra[sp],0.6))
legend('bottomleft', pch=21, col=mycols_intra,
       pt.bg=adjustcolor(mycols_intra, 0.8),
       paste('', levels(sp)),  cex=0.8, title='Species')
legend('topright', pch=c(21,24), col="black",
       paste('', levels(maturity)),  cex=0.8, title='Humerus condyle')
shapeHulls(plot, groups = sp)

#Testing the results with a non-parametric ANOVA
an_Y.gpa_intra<-procD.lm(Y.gpa_intra$coords ~ sp)
test_intra<-summary(an_Y.gpa_intra)
capture.output(test_intra, file="Intra_ANOVA_test.txt")

#Procrustes ANOVA to test for grouping based on distal condyle shape
maturity_test<-procD.lm(Y.gpa_intra$coords ~ maturity)
test_intra_maturity<-summary(maturity_test)
capture.output(test_intra_maturity, file="Intra_maturity_ANOVA_test.txt")

##### Effect of size - by centroid size - with the PC1
Size <- Y.gpa_intra$Csize

PC1_intra<-PCA_intra$x[,1]
plot(Size, PC1_intra, main = "PC1 vs Centroid Size", pch =c(21, 24)[maturity], col=mycols_intra[sp], 
     cex = 1.5, cex.lab = 1, font.lab = 1, bg=adjustcolor(mycols_intra[sp],0.6))
legend('bottomright', pch=21, col=mycols_intra,
       pt.bg=adjustcolor(mycols_intra, 0.8),
       paste('', levels(sp)),  cex=0.8, title='Species')
legend('right', pch=c(21,24), col="black",
      paste('', levels(maturity)),  cex=0.8, title='Humerus condyle')

####### Evaluating effect from size not from the C. Size but from the SVL

size_SUL <- classifier_intra$Snout.urostyle_length_.mm.
#PC1<-plot$PC.points[,1]
PC1_intra<-PCA_intra$x[,1]
plot(size_SUL, PC1_intra, main = "PC1 vs SUL", pch =c(21, 24)[maturity], col=mycols_intra[sp], 
     cex = 1.5, cex.lab = 1, font.lab = 1, bg=adjustcolor(mycols_intra[sp],0.6))
legend('bottomright', pch=21, col=mycols_intra,
       pt.bg=adjustcolor(mycols_intra, 0.8),
       paste('', levels(sp)),  cex=0.8, title='Species')
legend('right', pch=c(21,24), col="black",
       paste('', levels(maturity)),  cex=0.8, title='Humerus condyle')


#SVL vs CSize
summary(lm(Size_SVL~ Size))
plot(size_SUL, Size, main = "Centroid Size vs SUL", pch =c(21, 24)[maturity], col=mycols_intra[sp], 
     cex = 1.5, cex.lab = 1, font.lab = 1, bg=adjustcolor(mycols_intra[sp],0.6))
legend('topleft', pch=21, col=mycols_intra,
       pt.bg=adjustcolor(mycols_intra, 0.8),
       paste('', levels(sp)),  cex=0.8, title='Species')
legend('bottomright', pch=c(21,24), col="black",
       paste('', levels(maturity)),  cex=0.8, title='Humerus condyle')


### Evaluating shape differences between mature specimens in both species
##Excluding immature individuals

which (PC1_intra<=0)
PCA_mature<-PCA_intra[-which(PCA_intra$x[,1]>=0)]


?coords.subset #function to subset the dataset based on a factor
#there are now two lists with the grouped landmarks
new.coords<-coords.subset(coords, group=maturity) 
#the dataset with only the complete humeri complete=mature
coords2<-new.coords$complete

#Procrustes with mature individuals only
new_Y.gpa_intra<-gpagen(coords2)

#new classifier file with only the complete specimens
new_classifier<- classifier_intra[which (classifier_intra$Distal_condyle=="complete"),]
new_sp <- as.factor(new_classifier$Species)

#new PCA
new_PCA_intra<-gm.prcomp(new_Y.gpa_intra$coords)
summary(new_PCA_intra) #this gives the eigenvalues 

#New plot
levels(new_sp)
new_mycols_intra <- c("#44AA99", "#AA4499")
plot<-plot(new_PCA_intra, axis1 = 1, axis2 = 2, label = TRUE, main = "GMM Humerus of mature individuals", 
           pch = 21, cex = 0.6*new_Y.gpa_intra$Csize, col=new_mycols_intra[new_sp], 
           bg=adjustcolor(new_mycols_intra[new_sp],0.6))
legend('topleft', pch=21, col=new_mycols_intra,
       pt.bg=adjustcolor(new_mycols_intra, 0.8),
       paste('', levels(new_sp)),  cex=0.8, title='Species')

#new anova
an2_Y.gpa_intra<-procD.lm(new_Y.gpa_intra$coords ~ new_sp)
new_test_intra<-summary(an2_Y.gpa_intra)
capture.output(new_test_intra, file = "Intra_matureonly_Anova.txt")

##### Effect of size - by centroid size - with the PC1
new_Size <- new_Y.gpa_intra$Csize
#PC1<-plot$PC.points[,1]
new_PC1_intra<-new_PCA_intra$x[,1]
plot(new_Size, new_PC1_intra, main = "PC1 vs Centroid Size", pch = 21, 
     col=new_mycols_intra[new_sp], 
     cex = 1.5, cex.lab = 1, font.lab = 1, bg=adjustcolor(new_mycols_intra[new_sp],0.6))
legend('bottom', pch=21, col=new_mycols_intra,
       pt.bg=adjustcolor(new_mycols_intra, 0.8),
       paste('', levels(new_sp)),  cex=0.8, title='Species')

