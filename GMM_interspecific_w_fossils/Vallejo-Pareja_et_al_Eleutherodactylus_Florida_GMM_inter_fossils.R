##############################################################################
###Eleutherodactylus in the Oligocene of Florida
###Interspecific Variation
###Geometric Morphometric of the Humerus - Fossils included
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

coords <- arrayspecs(coords, 14, 3, 35) #here it goes: landmarks, dimensions, specimens
dimnames(coords)[[3]] <- names

#Generalized procrustes analyzes
Y.gpa<-gpagen(coords)
print(Y.gpa)
summary(Y.gpa)
Y.gpa$coords
Y.gpa$Csize

plot(Y.gpa, label=TRUE)

#Classifier file with all the infomration of the specimen - in the same order as data entered
classifier<-read.table("Classifier_interspecific_2.txt", header = TRUE)
subgenus <- as.factor(classifier$Subgenus)	# store subgenus as a factor

#PCA of the procrustes distances
PCA<-gm.prcomp(Y.gpa$coords)
PCA$x


#Plot the PCA and color coding by species
levels(subgenus)

mycols<-c("#332288","#88CCEE","black","#44AA99","#117733","#999933")
typeof(subgenus)
subgenus2<-as.factor(subgenus)
maturity<-as.factor(classifier$Distal_condyle)
maturity2<-as.factor(classifier$Distal_condyle2)

plot<-plot(PCA, axis1 = 1, axis2 = 2, label = TRUE, main = "GMM Humerus", 
           pch =c(21,0,24)[maturity2], 
           cex = 0.6*Y.gpa$Csize, col=mycols[subgenus], 
           bg = adjustcolor(mycols[subgenus], 0.6))
legend('bottomright', pch=21, col=mycols[subgenus],
       pt.bg=adjustcolor(mycols, 0.8),
       paste('', levels(subgenus)),  cex=0.8, title='Subgenus')
legend('topright', pch=c(21,24), col="black",
       paste('', levels(maturity)),  cex=0.8, title='Humerus condyle')

shapeHulls(plot, groups = subgenus)

picknplot.shape(plot, methd = "point")
picknplot.shape(plot(PCA), method = "vector")

#Procrustes ANOVA to test for grouping based on Subgenus
an_Y.gpa<-procD.lm(Y.gpa$coords ~ subgenus)
(test_inter<-summary(an_Y.gpa))
capture.output(test_inter, file = "Inter_fossils_Anova.txt")

#Procrustes ANOVA to test for grouping based on distal condyle shape
maturity_test<-procD.lm(Y.gpa$coords ~ maturity)
(test_inter_maturiry<-summary(maturity_test))
capture.output(test_inter_maturiry, file = "Inter_fossils_maturity_Anova.txt")

##### Effect of size - by centroid size - with the PC1 
Size <- Y.gpa$Csize
#PC1<-plot$PC.points[,1]
PC1<-PCA$x[,1]
plot(Size, PC1, main = "PC1 vs Centroid Size", pch = c(21,24)[maturity], 
     col=mycols[subgenus], 
     bg=adjustcolor(mycols[subgenus], 0.6), cex = 1.5, cex.lab = 1, font.lab = 1)
legend('topright', pch=21, col=mycols,
       pt.bg=adjustcolor(mycols, 0.8),
       paste('', levels(subgenus)),  cex=0.8, title='Subgenus')
legend('topright', pch=c(21,24), col="black",
       paste('', levels(maturity)),  cex=0.8, title='Humerus condyle')

abline(lm(PC1 ~ Size))

### Evaluating shape differences between mature specimens
##Excluding immature individuals

#Which specimens have incomplete humerus according to PC1
which (PC1<=-0.08)


#there are now two lists with the grouped landmarks
new.coords<-coords.subset(coords, group=maturity) 
#the dataset with only the complete humery complete=mature
coords2<-new.coords$complete

#Procrustes with mature individuals only
new_Y.gpa_inter2<-gpagen(coords2)


#new classifier file with only the complete specimens
new_classifier<- classifier[which (classifier$Distal_condyle=="complete"),]
new_subgenus <- as.factor(new_classifier$Subgenus)
new_maturity<-as.factor(new_classifier$Distal_condyle2)

#new PCA
new_PCA_inter<-gm.prcomp(new_Y.gpa_inter2$coords)
summary(new_PCA_inter) #this gives the eigenvalues 
new_PCA_inter$x[,1]
new_PCA_inter$x

#New plot
levels(new_subgenus)
new_mycols_inter <- c("#332288","#88CCEE","black","#44AA99","#117733","#999933") 
#c("#6699CC", "#CC6677", "Black", "#DDCC77", "#117733", "#332288")
plot<-plot(new_PCA_inter, axis1 = 1, axis2 = 2, label = TRUE, main = "GMM Humerus of mature individuals", 
           pch = c(21,0)[new_maturity], cex = 0.6*new_Y.gpa_inter2$Csize, col=new_mycols_inter[new_subgenus], 
           bg=adjustcolor(new_mycols_inter[new_subgenus],0.6))
legend('bottomright', pch=21, col=new_mycols_inter,
       pt.bg=adjustcolor(new_mycols_inter, 0.8),
       paste('', levels(new_subgenus)),  cex=0.8, title='Subgenus')

picknplot.shape(plot, methd = "point")

#new anova
new_test_inter<-procD.lm(new_Y.gpa_inter2$coords ~ new_subgenus)
(summary_new_test<-summary(new_test_inter))
capture.output(summary_new_test, file = "Inter_fossils_matureonly_Anova.txt")

##### Effect of size - by centroid size - with the PC1
new_Size <- new_Y.gpa_inter2$Csize
#PC1<-plot$PC.points[,1]
new_PC1_inter<-new_PCA_inter$x[,1]
plot(new_Size, new_PC1_inter, main = "PC1 vs Centroid Size", pch = 21, 
     col=new_mycols_inter[new_subgenus], 
     cex = 1.5, cex.lab = 1, font.lab = 1, bg=adjustcolor(new_mycols_inter[new_subgenus],0.6))
legend('topright', pch=21, col=new_mycols_inter,
       pt.bg=adjustcolor(new_mycols_inter, 0.8),
       paste('', levels(new_subgenus)),  cex=0.8, title='Subgenus')
