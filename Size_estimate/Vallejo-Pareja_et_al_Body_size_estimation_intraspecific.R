###################################################################
## Body Size estimation in Eleutherodactylus and Coefficient of Variation (CV) 
## Measurements taken form CTscans             
## 2 Species of Eleutherodactylus - intraspecific variation
## Eleutherodactylus glandulifer and E. planirostris
## Vallejo-Pareja et al_ in prep Eleutherodactylus Oligocene Florida
## ################################################################

rm(list=ls(all=T))
getwd()

#### Using compiled dataset of Eleutherodactylus measurements for
#### intraspecific variation

#read file
intra_sp<-read.csv("Intraspecific_variation_measurements2.csv", na.strin =".")
intra_sp <- na.omit (intra_sp) 

#Save variables as factors
subgenus <- as.factor(intra_sp$Subgenus)
species <- as.factor(intra_sp$Species)	
maturity <-as.factor(intra_sp$Distal.condyle)
immature<-intra_sp[which(intra_sp$Distal.condyle=="incomplete"),]
mature<-intra_sp[-which(intra_sp$Distal.condyle=="incomplete"),]
immatureSUL<-log(immature$Snout.urostyle.length..mm.)

#dataset with only mature specimens
#Don't do this part if I want to have mature and immature specimens
intra_sp<-intra_sp[which(intra_sp$Distal.condyle=="complete"),]
species <- as.factor(intra_sp$Species)	# store subgenus as a factor
maturity <-as.factor(intra_sp$Distal.condyle)

#Variables: 
#SUL: Snout Urostyle length
#var 1: (ah) acetabulum height 
#var 2: (paiw) pre-acetabulum ilium width
#var 3: (uaw) urostyle anterior width.
#var 4: (hhw) humerus head width
#var 5: (ow) olecranon width 
#var 6: (rpw) radioulna proximal width
#var 7: (spcw) sacrum posterior condyle width 
#var 8: (scl) sacrum centrum length
#var 9:(sacw) sacrum anterior cotyle width
#var 10: (mhhw) maximum width of distal humerus


#Logarithmic Transformed the data for complete dataset

var1<-log(intra_sp$Acetabulum.ht.) #Log transform data
var2<-log(intra_sp$Ilium.w..post.acetabulum) #Log transform data
var3<-log(intra_sp$Urostyle.ant..w.) #Log transform data
var4<-log(intra_sp$Humerus.head.w.) #Log transform data
var5<-log(intra_sp$Olecranon.w.) #Log transform data
var6<-log(intra_sp$Radioulna.prox..w.) #Log transform data
var7<-log(intra_sp$Sacrum.post..condyle.w.) #Log transform data
var8<-log(intra_sp$Sacrum.centrum.w.) #Log transform data
var9<-log(intra_sp$Sacrum.ant..condyle.w.)
var10<-log(intra_sp$Max.w..distal.humerus)
SUL<-log(intra_sp$Snout.urostyle.length..mm.) #Log transform data

###Subseting the dataset for species
small_sp<-subset(intra_sp, species=="Eleutherodactylus glandulifer")
small_sp<-subset(intra_sp, species== 'Eleutherodactylus planirostris')

#Regression 
#Linear Regression Model
#Ordinary Least Squares [OLS] regression: SUL as a function of HHW
#Use function "lm" to compute simple linear regression in lm SUL as a function of the different variables

#all
(OLS_1 <- (lm(SUL~var1)))
(OLS_2 <- (lm(SUL~var2)))		
(OLS_3 <- (lm(SUL~var3)))
(OLS_4 <- (lm(SUL~var4)))
(OLS_5 <- (lm(SUL~var5)))
(OLS_6 <- (lm(SUL~var6)))
(OLS_7 <- (lm(SUL~var7)))
(OLS_8 <- (lm(SUL~var8)))
(OLS_9 <- (lm(SUL~var9)))
(OLS_10 <- (lm(SUL~var10)))

####Saving the summaries
(sumvar1<-summary(OLS_1))
(sumvar2<-summary(OLS_2))
(sumvar3<-summary(OLS_3))
(sumvar4<-summary(OLS_4))
(sumvar5<-summary(OLS_5))
(sumvar6<-summary(OLS_6))
(sumvar7<-summary(OLS_7))
(sumvar8<-summary(OLS_8))
(sumvar9<-summary(OLS_9))
(sumvar10<-summary(OLS_10))

#Confidence and prediction intervals for each species and each variable
new1 <- seq (min(var1), max(var1), by=0.05) 
CI1<-(predict(OLS_1, newdata=data.frame(var1=new1), interval="confidence", level = 0.95))
PI1<-(predict(OLS_1, newdata=data.frame(var1=new1), interval="prediction", level = 0.95))

new2 <- seq (min(var2), max(var2), by=0.05) 
CI2<-(predict(OLS_2, newdata=data.frame(var2=new2), interval="confidence", level = 0.95))
PI2<-(predict(OLS_2, newdata=data.frame(var2=new2), interval="prediction", level = 0.95))

new3 <- seq (min(var3), max(var3), by=0.05) 
CI3<-(predict(OLS_3, newdata=data.frame(var3=new3), interval="confidence", level = 0.95))
PI3<-(predict(OLS_3, newdata=data.frame(var3=new3), interval="prediction", level = 0.95))

new4 <- seq (min(var4), max(var4), by=0.05) 
CI4<-(predict(OLS_4, newdata=data.frame(var4=new4), interval="confidence", level = 0.95))
PI4<-(predict(OLS_4, newdata=data.frame(var4=new4), interval="prediction", level = 0.95))

new5 <- seq (min(var5), max(var5), by=0.05) 
CI5<-(predict(OLS_5, newdata=data.frame(var5=new5), interval="confidence", level = 0.95))
PI5<-(predict(OLS_5, newdata=data.frame(var5=new5), interval="prediction", level = 0.95))

new6 <- seq (min(var6), max(var6), by=0.05) 
CI6<-(predict(OLS_6, newdata=data.frame(var6=new6), interval="confidence", level = 0.95))
PI6<-(predict(OLS_6, newdata=data.frame(var6=new6), interval="prediction", level = 0.95))

new7 <- seq (min(var7), max(var7), by=0.05) 
CI7<-(predict(OLS_7, newdata=data.frame(var7=new7), interval="confidence", level = 0.95))
PI7<-(predict(OLS_7, newdata=data.frame(var7=new7), interval="prediction", level = 0.95))

new8 <- seq (min(var8), max(var8), by=0.05) 
CI8<-(predict(OLS_8, newdata=data.frame(var8=new8), interval="confidence", level = 0.95))
PI8<-(predict(OLS_8, newdata=data.frame(var8=new8), interval="prediction", level = 0.95))

new9 <- seq (min(var9), max(var9), by=0.05) 
CI9<-(predict(OLS_9, newdata=data.frame(var9=new9), interval="confidence", level = 0.95))
PI9<-(predict(OLS_9, newdata=data.frame(var9=new9), interval="prediction", level = 0.95))

new10 <- seq (min(var10), max(var10), by=0.05) 
CI10<-(predict(OLS_10, newdata=data.frame(var10=new10), interval="confidence", level = 0.95))
PI10<-(predict(OLS_10, newdata=data.frame(var10=new10), interval="prediction", level = 0.95))

#Obtain regression formula

coeffs1= coefficients(OLS_1)
coeffs2= coefficients(OLS_2)
coeffs3= coefficients(OLS_3)
coeffs4= coefficients(OLS_4)
coeffs5= coefficients(OLS_5)
coeffs6= coefficients(OLS_6)
coeffs7= coefficients(OLS_7)
coeffs8= coefficients(OLS_8)
coeffs9= coefficients(OLS_9)
coeffs10= coefficients(OLS_10)

###Adding the Oligocene fossils to the regressions
###Adding the Oligocene fossils to the regressions
#SUL_est=coeffsn[1]+coeffsn[2]*fossil_measurement
#Est_Size<-round(exp(SUL_est), 2)

#Estimating for the fossil humerus humerus head and max. humerus head:
UF494598=coeffs4[1]+coeffs4[2]*log(0.67)
Foss_hum_1<-round(exp(UF494598),2)
UF494598_2=coeffs10[1]+coeffs10[2]*log(0.88)
Foss_hummax_1<-round(exp(UF494598_2),2)

UF494614=coeffs4[1]+coeffs4[2]*log(0.78)
Foss_hum_2<-round(exp(UF494614),2)
UF494614_2=coeffs10[1]+coeffs10[2]*log(1.17)
Foss_hummax_2<-round(exp(UF494614_2),2)

UF494613=coeffs4[1]+coeffs4[2]*log(0.65)
Foss_hum_3<-round(exp(UF494613),2)
UF494613_2=coeffs10[1]+coeffs10[2]*log(0.89)
Foss_hummax_3<-round(exp(UF494613_2),2)

UF501310=coeffs4[1]+coeffs4[2]*log(0.67)
Foss_hum_4<-round(exp(UF501310),2)
UF501310_2=coeffs10[1]+coeffs10[2]*log(0.96)
Foss_hummax_4<-round(exp(UF501310_2),2)

UF501312=coeffs4[1]+coeffs4[2]*log(0.66)
Foss_hum_5<-round(exp(UF501312),2)
UF501312_2=coeffs10[1]+coeffs10[2]*log(0.97)
Foss_hummax_5<-round(exp(UF501312_2),2)

UF501314=coeffs4[1]+coeffs4[2]*log(0.81)
Foss_hum_6<-round(exp(UF501314),2)
UF501314_2=coeffs10[1]+coeffs10[2]*log(1.06)
Foss_hummax_6<-round(exp(UF501314_2),2)

UF501328=coeffs4[1]+coeffs4[2]*log(0.64)
Foss_hum_7<-round(exp(UF501328),2)
UF501328_2=coeffs10[1]+coeffs10[2]*log(0.9)
Foss_hummax_7<-round(exp(UF501328_2),2)

#Estimating for the fossil ilia: Acetabulum hight and ant acetabulum width
UF494602=coeffs1[1]+coeffs1[2]*log(1.19)
Foss_ili_1<-round(exp(UF494602),2)
UF494602_2=coeffs2[1]+coeffs2[2]*log(0.83)
Foss_iliwid_1<-round(exp(UF494602_2),2)

UF494596=coeffs1[1]+coeffs1[2]*log(1.59)
Foss_ili_2<-round(exp(UF494596),2)
UF494596_2=coeffs2[1]+coeffs2[2]*log(1.02)
Foss_iliwid_2<-round(exp(UF494596_2),2)

UF494597=coeffs1[1]+coeffs1[2]*log(1.46)
Foss_ili_3<-round(exp(UF494597),2)
UF494597_2=coeffs2[1]+coeffs2[2]*log(1.24)
Foss_iliwid_3<-round(exp(UF494597_2),2)

UF494606=coeffs1[1]+coeffs1[2]*log(1.04)
Foss_ili_4<-round(exp(UF494606),2)
UF494606_2=coeffs2[1]+coeffs2[2]*log(0.77)
Foss_iliwid_4<-round(exp(UF494606_2),2)

UF499723=coeffs1[1]+coeffs1[2]*log(1.33)
Foss_ili_5<-round(exp(UF499723),2)
UF499723_2=coeffs2[1]+coeffs2[2]*log(0.86)
Foss_iliwid_5<-round(exp(UF499723_2),2)

UF499737=coeffs1[1]+coeffs1[2]*log(0.92)
Foss_ili_6<-round(exp(UF499737),2)
UF499737_2=coeffs2[1]+coeffs2[2]*log(0.78)
Foss_iliwid_6<-round(exp(UF499737_2),2)

UF499755=coeffs1[1]+coeffs1[2]*log(1.14)
Foss_ili_7<-round(exp(UF499755),2)
UF499755_2=coeffs2[1]+coeffs2[2]*log(0.87)
Foss_iliwid_7<-round(exp(UF499755_2),2)

UF501355=coeffs1[1]+coeffs1[2]*log(1.15)
Foss_ili_8<-round(exp(UF501355),2)
UF501355_2=coeffs2[1]+coeffs2[2]*log(0.84)
Foss_iliwid_8<-round(exp(UF501355_2),2)

UF501352=coeffs1[1]+coeffs1[2]*log(1.1)
Foss_ili_9<-round(exp(UF501352),2)
UF501352_2=coeffs2[1]+coeffs2[2]*log(0.87)
Foss_iliwid_9<-round(exp(UF501352_2),2)

UF499732=coeffs1[1]+coeffs1[2]*log(0.99)
Foss_ili_10<-round(exp(UF499732),2)
UF499732_2=coeffs2[1]+coeffs2[2]*log(0.75)
Foss_iliwid_10<-round(exp(UF499732_2),2)

#Estimating for the fossil urostyle
UF501321=coeffs3[1]+coeffs3[2]*log(0.78)
Foss_uros_1<-round(exp(UF501321),2)

#Estimating for the fossil Radioulnae: oleocranon and proximal width
UF501323=coeffs5[1]+coeffs5[2]*log(0.77)
Foss_oleoc_1<-round(exp(UF501323),2)
UF501323_2=coeffs6[1]+coeffs6[2]*log(0.61)
Foss_radwid_1<-round(exp(UF501323_2),2)

UF501324=coeffs5[1]+coeffs5[2]*log(0.87)
Foss_oleoc_2<-round(exp(UF501324),2)
UF501324_2=coeffs6[1]+coeffs6[2]*log(0.64)
Foss_radwid_2<-round(exp(UF501324_2),2)

#Estimating for the fossil Sacrum: posterior condyle width, centrum length and anteriot centrum width
UF497977=coeffs7[1]+coeffs7[2]*log(0.88)
Foss_sacrcond_1<-round(exp(UF497977),2)
UF497977_2=coeffs8[1]+coeffs8[2]*log(0.46)
Foss_sacrcent_1<-round(exp(UF497977_2),2)
UF497977_3=coeffs9[1]+coeffs9[2]*log(0.7)
Foss_sacrant_1<-round(exp(UF497977_3),2)


UF497981=coeffs7[1]+coeffs7[2]*log(0.77)
Foss_sacrcond_2<-round(exp(UF497981),2)
UF497981_2=coeffs8[1]+coeffs8[2]*log(0.37)
Foss_sacrcent_2<-round(exp(UF497981_2),2)
UF497981_3=coeffs9[1]+coeffs9[2]*log(0.64)
Foss_sacrant_2<-round(exp(UF497981_3),2)


##PLOTS!
mycols <- c("#44AA99", "#AA4499")

#1 VARIABLE
plot(x = intra_sp$Acetabulum.ht.,y = intra_sp$Snout.urostyle.length..mm., pch = "",
     cex="", axis(side=3))
par(new=TRUE, mar=c(5,5,5,5))
plot(intra_sp$Acetabulum.ht.,intra_sp$Snout.urostyle.length..mm., axis(side=4))
par(new=TRUE, mar=c(5,5,5,5))
plot(var1,SUL, main=NULL, ylab="", xlab="",
     col=mycols[species], bg=adjustcolor(mycols[species], 0.6),pch =c(21, 24)[maturity], cex=1.3, axes=T)
abline(lm(SUL~var1), lty=1, col = "black", lwd=1.5)
abline(h=mean(SUL), v=mean(var1), lty=2, col="black")
matlines(new1, CI1[,2:3], col = "#888888", lty=2) #confidence interval
matlines(new1, PI1[,2:3], col = "#555555", lty=4) #precision interval
points(log(1.19),UF494602, pch=0, cex=1.3, col="black")
points(log(1.59),UF494596, pch=0, cex=1.3, col="black")
points(log(1.46),UF494597, pch=0, cex=1.3, col="black")
points(log(1.04),UF494606, pch=0, cex=1.3, col="black")
points(log(1.33),UF499723, pch=0, cex=1.3, col="black")
points(log(0.92),UF499737, pch=0, cex=1.3, col="black")
points(log(1.14),UF499755, pch=0, cex=1.3, col="black")
points(log(1.15),UF501355, pch=0, cex=1.3, col="black")
points(log(1.1),UF501352, pch=0, cex=1.3, col="black")
points(log(0.99),UF499732, pch=0, cex=1.3, col="black")
legend('bottomright', pch=21, col=mycols,
       pt.bg=adjustcolor(mycols, 0.6),
       paste('', levels(species)),  cex=0.5, title='Species')
legend('right', pch=c(21,24), col="black",
       paste('', levels(maturity)),  cex=0.5, title='Distal condyle')
mtext(side=3, line=-1.2, cex=0.6, adj=0.05, las= 1, col = "black",
      bquote(italic(r)^2 == .(round(sumvar1$r.squared,3)))) #r^2 value
mtext(side = 2, line = 2, "Log of SUL", cex=0.6)
mtext(side = 1, line = 2, "Log of acetabulum height", cex=0.6) #make line 1.8 for compound figure
mtext(side = 3, line = 2, "Acetabulum height [mm]", cex=0.6) #make line 1.8 for compound figure
mtext(side = 4, line = 2, "SUL [mm]", cex=0.6)


#2 VARIABLE
plot(x = intra_sp$Ilium.w..post.acetabulum,y = intra_sp$Ilium.w..post.acetabulum, pch = "",
     cex="", axis(side=3))
par(new=TRUE, mar=c(5,5,5,5))
plot(intra_sp$Ilium.w..post.acetabulum,intra_sp$Snout.urostyle.length..mm., axis(side=4))
par(new=TRUE, mar=c(5,5,5,5))
plot(var2,SUL, main=NULL, ylab="", xlab="",
     col=mycols[species], bg=adjustcolor(mycols[species], 0.6),pch =c(21, 24)[maturity], cex=1.3, axes=T)
abline(lm(SUL~var2), lty=1, col = "black", lwd=1.5)
abline(h=mean(SUL), v=mean(var2), lty=2, col="black")
matlines(new2, CI2[,2:3], col = "#888888", lty=2) #confidence interval
matlines(new2, PI2[,2:3], col = "#555555", lty=4) #precision interval
#abline(a=coeffs2l[1], b=coeffs2l[2], lty=1, col = "#44AA99", lwd=1.5) #regression line
#matlines(newl2, CIl2[,2:3], col = "#44AA99", lty=2) #confidence interval
#abline(a=coeffs2s[1], b=coeffs2s[2], lty=1, col = "#AA4499", lwd=1.5) #regression line
#matlines(newsm2, CIS2[,2:3], col = "#AA4499", lty=2) #confidence interval
points(log(0.83),UF494602_2, pch=0, cex=1.3, col="black")
points(log(1.02),UF494596_2, pch=0, cex=1.3, col="black")
points(log(1.24),UF494597_2, pch=0, cex=1.3, col="black")
points(log(0.77),UF494606_2, pch=0, cex=1.3, col="black")
points(log(0.86),UF499723_2, pch=0, cex=1.3, col="black")
points(log(0.78),UF499737_2, pch=0, cex=1.3, col="black")
points(log(0.87),UF499755_2, pch=0, cex=1.3, col="black")
points(log(0.84),UF501355_2, pch=0, cex=1.3, col="black")
points(log(0.87),UF501352_2, pch=0, cex=1.3, col="black")
points(log(0.75),UF499732_2, pch=0, cex=1.3, col="black")
legend('bottomright', pch=21, col=mycols,
       pt.bg=adjustcolor(mycols, 0.6),
       paste('', levels(species)),  cex=0.5, title='Species')
legend('right', pch=c(21,24), col="black",
       paste('', levels(maturity)),  cex=0.5, title='Distal condyle')
#mtext(side=3, line=-2.5, cex=0.6, adj=0.05, las= 1, col = "#44AA99",
#      bquote(italic(r)^2 == .(round(lsumvar2$r.squared,3))))
#mtext(side=3, line=-4.5, cex=0.6, adj=0.05, col = "#AA4499",
#      bquote(italic(r)^2 == .(round(ssumvar2$r.squared,3))))
mtext(side=3, line=-1.2, cex=0.6, adj=0.05, las= 1, col = "black",
      bquote(italic(r)^2 == .(round(sumvar2$r.squared,3))))
#mtext("(A)", side=3, line=1, cex=0.5, adj=0.05)
mtext(side = 2, line = 2, "Log of SUL", cex=0.6)
mtext(side = 1, line = 2, "Log of Pre-acetabulum ilium width", cex=0.6) #make line 1.8 for compound figure
mtext(side = 3, line = 2, "Pre-acetabulum ilium width [mm]", cex=0.6) #make line 1.8 for compound figure
mtext(side = 4, line = 2, "SUL [mm]", cex=0.6)

#3 VARIABLE
plot(x = intra_sp$Urostyle.ant..w.,y = intra_sp$Urostyle.ant..w., pch = "",
     cex="", axis(side=3))
par(new=TRUE, mar=c(5,5,5,5))
plot(intra_sp$Urostyle.ant..w.,intra_sp$Snout.urostyle.length..mm., axis(side=4))
par(new=TRUE, mar=c(5,5,5,5))
plot(var3,SUL, main=NULL, ylab="", xlab="",
     col=mycols[species], bg=adjustcolor(mycols[species], 0.6),pch =c(21, 24)[maturity], cex=1.3, axes=T)
abline(lm(SUL~var3), lty=1, col = "black", lwd=1.5)
abline(h=mean(SUL), v=mean(var3), lty=2, col="black")
matlines(new3, CI3[,2:3], col = "#888888", lty=2) #confidence interval
matlines(new3, PI3[,2:3], col = "#555555", lty=4) #precision interval
#abline(a=coeffs3l[1], b=coeffs3l[2], lty=1, col = "#44AA99", lwd=1.5) #regression line
#matlines(newl3, CIl3[,2:3], col = "#44AA99", lty=2) #confidence interval
#abline(a=coeffs3s[1], b=coeffs3s[2], lty=1, col = "#AA4499", lwd=1.5) #regression line
#matlines(newsm3, CIS3[,2:3], col = "#AA4499", lty=2) #confidence interval
points(log(0.78),UF501321, pch=0, cex=1.3, col="black")
legend('bottomright', pch=21, col=mycols,
       pt.bg=adjustcolor(mycols, 0.6),
       paste('', levels(species)),  cex=0.5, title='Species')
legend('right', pch=c(21,24), col="black",
       paste('', levels(maturity)),  cex=0.5, title='Distal condyle')
#mtext(side=3, line=-2.5, cex=0.6, adj=0.05, las= 1, col = "#44AA99",
#      bquote(italic(r)^2 == .(round(lsumvar3$r.squared,3))))
#mtext(side=3, line=-4.5, cex=0.6, adj=0.05, col = "#AA4499",
#      bquote(italic(r)^2 == .(round(ssumvar3$r.squared,3))))
mtext(side=3, line=-1.2, cex=0.6, adj=0.05, las= 1, col = "black",
      bquote(italic(r)^2 == .(round(sumvar3$r.squared,3))))
#mtext("(A)", side=3, line=1, cex=0.5, adj=0.05)
mtext(side = 2, line = 2, "Log of SUL", cex=0.6)
mtext(side = 1, line = 2, "Log of urostyle anterior width", cex=0.6) #make line 1.8 for compound figure
mtext(side = 3, line = 2, "Urostyle anterior width [mm]", cex=0.6) #make line 1.8 for compound figure
mtext(side = 4, line = 2, "SUL [mm]", cex=0.6)

#4 VARIABLE
plot(x = intra_sp$Humerus.head.w.,y = intra_sp$Humerus.head.w., pch = "",
     cex="", axis(side=3))
par(new=TRUE, mar=c(5,5,5,5))
plot(intra_sp$Humerus.head.w.,intra_sp$Snout.urostyle.length..mm., axis(side=4))
par(new=TRUE, mar=c(5,5,5,5))
plot(var4,SUL, main=NULL, ylab="", xlab="",
     col=mycols[species], bg=adjustcolor(mycols[species], 0.6),pch =c(21, 24)[maturity], cex=1.3, axes=T)
abline(lm(SUL~var4), lty=1, col = "black", lwd=1.5)
abline(h=mean(SUL), v=mean(var4), lty=2, col="black")
matlines(new4, CI4[,2:3], col = "#888888", lty=2) #confidence interval
matlines(new4, PI4[,2:3], col = "#555555", lty=4) #precision interval
#abline(a=coeffs4l[1], b=coeffs4l[2], lty=1, col = "#44AA99", lwd=1.5) #regression line
#matlines(newl4, CIl4[,2:3], col = "#44AA99", lty=2) #confidence interval
#abline(a=coeffs4s[1], b=coeffs4s[2], lty=1, col = "#AA4499", lwd=1.5) #regression line
#matlines(newsm4, CIS4[,2:3], col = "#AA4499", lty=2) #confidence interval
points(log(0.67),UF494598, pch=0, cex=1.3, col="black")
points(log(0.78),UF494614, pch=0, cex=1.3, col="black")
points(log(0.65),UF494613, pch=0, cex=1.3, col="black")
points(log(0.67),UF501310, pch=0, cex=1.3, col="black")
points(log(0.66),UF501312, pch=0, cex=1.3, col="black")
points(log(0.81),UF501314, pch=0, cex=1.3, col="black")
points(log(0.64),UF501328, pch=0, cex=1.3, col="black")
legend('bottomright', pch=21, col=mycols,
       pt.bg=adjustcolor(mycols, 0.6),
       paste('', levels(species)),  cex=0.5, title='Species')
legend('right', pch=c(21,24), col="black",
       paste('', levels(maturity)),  cex=0.5, title='Distal condyle')
#mtext(side=3, line=-2.5, cex=0.6, adj=0.05, las= 1, col = "#44AA99",
#      bquote(italic(r)^2 == .(round(lsumvar4$r.squared,3))))
#mtext(side=3, line=-4.5, cex=0.6, adj=0.05, col = "#AA4499",
#      bquote(italic(r)^2 == .(round(ssumvar4$r.squared,3))))
mtext(side=3, line=-1.2, cex=0.6, adj=0.05, las= 1, col = "black",
      bquote(italic(r)^2 == .(round(sumvar4$r.squared,3))))
#mtext("(A)", side=3, line=1, cex=0.5, adj=0.05)
mtext(side = 2, line = 2, "Log of SUL", cex=0.6)
mtext(side = 1, line = 2, "Log of humerus head width", cex=0.6) #make line 1.8 for compound figure
mtext(side = 3, line = 2, "Humerus head width [mm]", cex=0.6) #make line 1.8 for compound figure
mtext(side = 4, line = 2, "SUL [mm]", cex=0.6)


#5 VARIABLE
plot(x = intra_sp$Olecranon.w.,y = intra_sp$Olecranon.w., pch = "",
     cex="", axis(side=3))
par(new=TRUE, mar=c(5,5,5,5))
plot(intra_sp$Olecranon.w.,intra_sp$Snout.urostyle.length..mm., axis(side=4))
par(new=TRUE, mar=c(5,5,5,5))
plot(var5,SUL, main=NULL, ylab="", xlab="",
     col=mycols[species], bg=adjustcolor(mycols[species], 0.6),pch =c(21, 24)[maturity], cex=1.3, axes=T)
abline(lm(SUL~var5), lty=1, col = "black", lwd=1.5)
abline(h=mean(SUL), v=mean(var5), lty=2, col="black")
matlines(new5, CI5[,2:3], col = "#888888", lty=2) #confidence interval
matlines(new5, PI5[,2:3], col = "#555555", lty=4) #precision interval
#abline(a=coeffs5l[1], b=coeffs5l[2], lty=1, col = "#44AA99", lwd=1.5) #regression line
#matlines(newl5, CIl5[,2:3], col = "#44AA99", lty=2) #confidence interval
#abline(a=coeffs5s[1], b=coeffs5s[2], lty=1, col = "#AA4499", lwd=1.5) #regression line
#matlines(newsm5, CIS5[,2:3], col = "#AA4499", lty=2) #confidence interval
points(log(0.77),UF501323, pch=0, cex=1.3, col="black")
points(log(0.87),UF501324, pch=0, cex=1.3, col="black")
legend('bottomright', pch=21, col=mycols,
       pt.bg=adjustcolor(mycols, 0.6),
       paste('', levels(species)),  cex=0.5, title='Species')
legend('right', pch=c(21,24), col="black",
       paste('', levels(maturity)),  cex=0.5, title='Distal condyle')
#mtext(side=3, line=-2.5, cex=0.6, adj=0.05, las= 1, col = "#44AA99",
#      bquote(italic(r)^2 == .(round(lsumvar5$r.squared,3))))
#mtext(side=3, line=-4.5, cex=0.6, adj=0.05, col = "#AA4499",
#      bquote(italic(r)^2 == .(round(ssumvar5$r.squared,3))))
mtext(side=3, line=-1.2, cex=0.6, adj=0.05, las= 1, col = "black",
      bquote(italic(r)^2 == .(round(sumvar5$r.squared,3))))
#mtext("(A)", side=3, line=1, cex=0.5, adj=0.05)
mtext(side = 2, line = 2, "Log of SUL", cex=0.6)
mtext(side = 1, line = 2, "Log of olecranon width", cex=0.6) #make line 1.8 for compound figure
mtext(side = 3, line = 2, "Olecranon width [mm]", cex=0.6) #make line 1.8 for compound figure
mtext(side = 4, line = 2, "SUL [mm]", cex=0.6)

#6 VARIABLE
plot(x = intra_sp$Radioulna.prox..w.,y = intra_sp$Radioulna.prox..w., pch = "",
     cex="", axis(side=3))
par(new=TRUE, mar=c(5,5,5,5))
plot(intra_sp$Radioulna.prox..w.,intra_sp$Snout.urostyle.length..mm., axis(side=4))
par(new=TRUE, mar=c(5,5,5,5))
plot(var6,SUL, main=NULL, ylab="", xlab="",
     col=mycols[species], bg=adjustcolor(mycols[species], 0.6),pch =c(21, 24)[maturity], cex=1.3, axes=T)
abline(lm(SUL~var6), lty=1, col = "black", lwd=1.5)
abline(h=mean(SUL), v=mean(var6), lty=2, col="black")
matlines(new6, CI6[,2:3], col = "#888888", lty=2) #confidence interval
matlines(new6, PI6[,2:3], col = "#555555", lty=4) #precision interval
#abline(a=coeffs6l[1], b=coeffs6l[2], lty=1, col = "#44AA99", lwd=1.5) #regression line
#matlines(newl6, CIl6[,2:3], col = "#44AA99", lty=2) #confidence interval
#abline(a=coeffs6s[1], b=coeffs6s[2], lty=1, col = "#AA4499", lwd=1.5) #regression line
#matlines(newsm6, CIS6[,2:3], col = "#AA4499", lty=2) #confidence interval
points(log(0.61),UF501323_2, pch=0, cex=1.3, col="black")
points(log(0.64),UF501324_2, pch=0, cex=1.3, col="black")
legend('bottomright', pch=21, col=mycols,
       pt.bg=adjustcolor(mycols, 0.6),
       paste('', levels(species)),  cex=0.5, title='Species')
legend('right', pch=c(21,24), col="black",
       paste('', levels(maturity)),  cex=0.5, title='Distal condyle')
#mtext(side=3, line=-2.5, cex=0.6, adj=0.05, las= 1, col = "#44AA99",
#      bquote(italic(r)^2 == .(round(lsumvar6$r.squared,3))))
#mtext(side=3, line=-4.5, cex=0.6, adj=0.05, col = "#AA4499",
#      bquote(italic(r)^2 == .(round(ssumvar6$r.squared,3))))
mtext(side=3, line=-1.2, cex=0.6, adj=0.05, las= 1, col = "black",
      bquote(italic(r)^2 == .(round(sumvar6$r.squared,3))))
#mtext("(A)", side=3, line=1, cex=0.5, adj=0.05)
mtext(side = 2, line = 2, "Log of SUL", cex=0.6)
mtext(side = 1, line = 2, "Log of radioulna proximal width", cex=0.6) #make line 1.8 for compound figure
mtext(side = 3, line = 2, "Radioulna proximal width [mm]", cex=0.6) #make line 1.8 for compound figure
mtext(side = 4, line = 2, "SUL [mm]", cex=0.6)

#7 VARIABLE
plot(x = intra_sp$Sacrum.post..condyle.w.,y = intra_sp$Sacrum.post..condyle.w., pch = "",
     cex="", axis(side=3))
par(new=TRUE, mar=c(5,5,5,5))
plot(intra_sp$Sacrum.post..condyle.w.,intra_sp$Snout.urostyle.length..mm., axis(side=4))
par(new=TRUE, mar=c(5,5,5,5))
plot(var7,SUL, main=NULL, ylab="", xlab="",
     col=mycols[species], bg=adjustcolor(mycols[species], 0.6),pch =c(21, 24)[maturity], cex=1.3, axes=T)
abline(lm(SUL~var7), lty=1, col = "black", lwd=1.5)
abline(h=mean(SUL), v=mean(var7), lty=2, col="black")
matlines(new7, CI7[,2:3], col = "#888888", lty=2) #confidence interval
matlines(new7, PI7[,2:3], col = "#555555", lty=4) #precision interval
#abline(a=coeffs7l[1], b=coeffs7l[2], lty=1, col = "#44AA99", lwd=1.5) #regression line
#matlines(newl7, CIl7[,2:3], col = "#44AA99", lty=2) #confidence interval
#abline(a=coeffs7s[1], b=coeffs7s[2], lty=1, col = "#AA4499", lwd=1.5) #regression line
#matlines(newsm7, CIS7[,2:3], col = "#AA4499", lty=2) #confidence interval
points(log(0.88),UF497977, pch=0, cex=1.3, col="black")
points(log(0.77),UF497981, pch=0, cex=1.3, col="black")
legend('bottomright', pch=21, col=mycols,
       pt.bg=adjustcolor(mycols, 0.6),
       paste('', levels(species)),  cex=0.5, title='Species')
legend('right', pch=c(21,24), col="black",
       paste('', levels(maturity)),  cex=0.5, title='Distal condyle')
#mtext(side=3, line=-2.5, cex=0.6, adj=0.05, las= 1, col = "#44AA99",
#      bquote(italic(r)^2 == .(round(lsumvar7$r.squared,3))))
#mtext(side=3, line=-4.5, cex=0.6, adj=0.05, col = "#AA4499",
#      bquote(italic(r)^2 == .(round(ssumvar7$r.squared,3))))
mtext(side=3, line=-1.2, cex=0.6, adj=0.05, las= 1, col = "black",
      bquote(italic(r)^2 == .(round(sumvar7$r.squared,3))))
#mtext("(A)", side=3, line=1, cex=0.5, adj=0.05)
mtext(side = 2, line = 2, "Log of SUL", cex=0.6)
mtext(side = 1, line = 2, "Log of sacrum posterior condyle width", cex=0.6) #make line 1.8 for compound figure
mtext(side = 3, line = 2, "Sacrum posterior condyle [mm]", cex=0.6) #make line 1.8 for compound figure
mtext(side = 4, line = 2, "SUL [mm]", cex=0.6)

#8 VARIABLE
plot(x = intra_sp$Sacrum.centrum.w.,y = intra_sp$Sacrum.centrum.w., pch = "",
     cex="", axis(side=3))
par(new=TRUE, mar=c(5,5,5,5))
plot(intra_sp$Sacrum.centrum.w.,intra_sp$Snout.urostyle.length..mm., axis(side=4))
par(new=TRUE, mar=c(5,5,5,5))
plot(var8,SUL, main=NULL, ylab="", xlab="",
     col=mycols[species], bg=adjustcolor(mycols[species], 0.6),pch =c(21, 24)[maturity], cex=1.3, axes=T)
abline(lm(SUL~var8), lty=1, col = "black", lwd=1.5)
abline(h=mean(SUL), v=mean(var8), lty=2, col="black")
matlines(new8, CI8[,2:3], col = "#888888", lty=2) #confidence interval
matlines(new8, PI8[,2:3], col = "#555555", lty=4) #precision interval
#abline(a=coeffs8l[1], b=coeffs8l[2], lty=1, col = "#44AA99", lwd=1.5) #regression line
#matlines(newl8, CIl8[,2:3], col = "#44AA99", lty=2) #confidence interval
#abline(a=coeffs8s[1], b=coeffs8s[2], lty=1, col = "#AA4499", lwd=1.5) #regression line
#matlines(newsm8, CIS8[,2:3], col = "#AA4499", lty=2) #confidence interval
points(log(0.46),UF497977_2, pch=0, cex=1.3, col="black")
points(log(0.37),UF497981_2, pch=0, cex=1.3, col="black")
legend('bottomright', pch=21, col=mycols,
       pt.bg=adjustcolor(mycols, 0.6),
       paste('', levels(species)),  cex=0.5, title='Species')
legend('right', pch=c(21,24), col="black",
       paste('', levels(maturity)),  cex=0.5, title='Distal condyle')
#mtext(side=3, line=-2.5, cex=0.6, adj=0.05, las= 1, col = "#44AA99",
#     bquote(italic(r)^2 == .(round(lsumvar8$r.squared,3))))
#mtext(side=3, line=-4.5, cex=0.6, adj=0.05, col = "#AA4499",
#      bquote(italic(r)^2 == .(round(ssumvar8$r.squared,3))))
mtext(side=3, line=-1.2, cex=0.6, adj=0.05, las= 1, col = "black",
      bquote(italic(r)^2 == .(round(sumvar8$r.squared,3))))
#mtext("(A)", side=3, line=1, cex=0.5, adj=0.05)
mtext(side = 2, line = 2, "Log of SUL", cex=0.6)
mtext(side = 1, line = 2, "Log of sacrum centrum length", cex=0.6) #make line 1.8 for compound figure
mtext(side = 3, line = 2, "Sacrum centrum length [mm]", cex=0.6) #make line 1.8 for compound figure
mtext(side = 4, line = 2, "SUL [mm]", cex=0.6)

#9 VARIABLE
plot(x = intra_sp$Sacrum.ant..condyle.w.,y = intra_sp$Sacrum.ant..condyle.w., pch = "",
     cex="", axis(side=3))
par(new=TRUE, mar=c(5,5,5,5))
plot(intra_sp$Sacrum.ant..condyle.w.,intra_sp$Snout.urostyle.length..mm., axis(side=4))
par(new=TRUE, mar=c(5,5,5,5))
plot(var9,SUL, main=NULL, ylab="", xlab="",
     col=mycols[species], bg=adjustcolor(mycols[species], 0.6),pch =c(21, 24)[maturity], cex=1.3, axes=T)
abline(lm(SUL~var9), lty=1, col = "black", lwd=1.5)
abline(h=mean(SUL), v=mean(var9), lty=2, col="black")
matlines(new9, CI9[,2:3], col = "#888888", lty=2) #confidence interval
matlines(new9, PI9[,2:3], col = "#555555", lty=4) #precision interval
#abline(a=coeffs9l[1], b=coeffs9l[2], lty=1, col = "#44AA99", lwd=1.5) #regression line
#matlines(newl9, CIl9[,2:3], col = "#44AA99", lty=2) #confidence interval
#abline(a=coeffs9s[1], b=coeffs9s[2], lty=1, col = "#AA4499", lwd=1.5) #regression line
#matlines(newsm9, CIS9[,2:3], col = "#AA4499", lty=2) #confidence interval
points(log(0.7),UF497977_3, pch=0, cex=1.3, col="black")
points(log(0.64),UF497981_3, pch=0, cex=1.3, col="black")
legend('bottomright', pch=21, col=mycols,
       pt.bg=adjustcolor(mycols, 0.6),
       paste('', levels(species)),  cex=0.5, title='Species')
legend('right', pch=c(21,24), col="black",
       paste('', levels(maturity)),  cex=0.5, title='Distal condyle')
#mtext(side=3, line=-2.5, cex=0.6, adj=0.05, las= 1, col = "#44AA99",
#      bquote(italic(r)^2 == .(round(lsumvar9$r.squared,3))))
#mtext(side=3, line=-4.5, cex=0.6, adj=0.05, col = "#AA4499",
#      bquote(italic(r)^2 == .(round(ssumvar9$r.squared,3))))
mtext(side=3, line=-1.2, cex=0.6, adj=0.05, las= 1, col = "black",
      bquote(italic(r)^2 == .(round(sumvar9$r.squared,3))))
#mtext("(A)", side=3, line=1, cex=0.5, adj=0.05)
mtext(side = 2, line = 2, "Log of SUL", cex=0.6)
mtext(side = 1, line = 2, "Log of Sacrum anterior condyle width", cex=0.6) #make line 1.8 for compound figure
mtext(side = 3, line = 2, "Sacrum anterior condyle width [mm]", cex=0.6) #make line 1.8 for compound figure
mtext(side = 4, line = 2, "SUL [mm]", cex=0.6)

#10 VARIABLE
plot(x = intra_sp$Max.w..distal.humerus,y = intra_sp$Max.w..distal.humerus, pch = "",
     cex="", axis(side=3))
par(new=TRUE, mar=c(5,5,5,5))
plot(intra_sp$Max.w..distal.humerus,intra_sp$Snout.urostyle.length..mm., axis(side=4))
par(new=TRUE, mar=c(5,5,5,5))
plot(var10,SUL, main=NULL, ylab="", xlab="",
     col=mycols[species], bg=adjustcolor(mycols[species], 0.6),pch =c(21, 24)[maturity], cex=1.3, axes=T)
abline(lm(SUL~var10), lty=1, col = "black", lwd=1.5)
abline(h=mean(SUL), v=mean(var10), lty=2, col="black")
matlines(new10, CI10[,2:3], col = "#888888", lty=2) #confidence interval
matlines(new10, PI10[,2:3], col = "#555555", lty=4) #precision interval
#abline(a=coeffs10l[1], b=coeffs10l[2], lty=1, col = "#44AA99", lwd=1.5) #regression line
#matlines(newl10, CIl10[,2:3], col = "#44AA99", lty=2) #confidence interval
#abline(a=coeffs10s[1], b=coeffs10s[2], lty=1, col = "#AA4499", lwd=1.5) #regression line
#matlines(newsm10, CIS10[,2:3], col = "#AA4499", lty=2) #confidence interval
#points(mean(var10), mean(SUL), pch=16, cex=0.5, col="black")
points(log(0.88),UF494598_2, pch=0, cex=1.3, col="black")
points(log(1.17),UF494614_2, pch=0, cex=1.3, col="black")
points(log(0.89),UF494613_2, pch=0, cex=1.3, col="black")
points(log(0.96),UF501310_2, pch=0, cex=1.3, col="black")
points(log(0.97),UF501312_2, pch=0, cex=1.3, col="black")
points(log(1.06),UF501314_2, pch=0, cex=1.3, col="black")
points(log(0.9),UF501328_2, pch=0, cex=1.3, col="black")
legend('bottomright', pch=21, col=mycols,
       pt.bg=adjustcolor(mycols, 0.6),
       paste('', levels(species)),  cex=0.5, title='Species')
legend('right', pch=c(21,24), col="black",
       paste('', levels(maturity)),  cex=0.5, title='Distal condyle')
#mtext(side=3, line=-2.5, cex=0.6, adj=0.05, las= 1, col = "#44AA99",
#      bquote(italic(r)^2 == .(round(lsumvar10$r.squared,3))))
#mtext(side=3, line=-4.5, cex=0.6, adj=0.05, col = "#AA4499",
#      bquote(italic(r)^2 == .(round(ssumvar10$r.squared,3))))
mtext(side=3, line=-1.2, cex=0.6, adj=0.05, las= 1, col = "black",
      bquote(italic(r)^2 == .(round(sumvar10$r.squared,3))))
#mtext("(A)", side=3, line=1, cex=0.5, adj=0.05)
mtext(side = 2, line = 2, "Log of SUL", cex=0.6)
mtext(side = 1, line = 2, "Log of humerus head maximum width", cex=0.6) #make line 1.8 for compound figure
mtext(side = 3, line = 2, "Humerus head maximum width [mm]", cex=0.6) #make line 1.8 for compound figure
mtext(side = 4, line = 2, "SUL [mm]", cex=0.6)




##################

#Calculating the coefficient of Variation of the measurements
# CV=sd/mean

#For the intraspecific
cv1_ah<-sd(intra_sp$Acetabulum.ht./mean(intra_sp$Acetabulum.ht.))
cv2_paiw<-sd(intra_sp$Ilium.w..post.acetabulum/mean(intra_sp$Ilium.w..post.acetabulum))
cv3_uaw<-sd(intra_sp$Urostyle.ant..w./mean(intra_sp$Urostyle.ant..w.))
cv4_hhw<-sd(intra_sp$Humerus.head.w./mean(intra_sp$Humerus.head.w.))
cv5_ow<-sd(intra_sp$Olecranon.w./mean(intra_sp$Olecranon.w.))
cv6_rpw<-sd(intra_sp$Radioulna.prox..w./mean(intra_sp$Radioulna.prox..w.))
cv7_spcw<-sd(intra_sp$Sacrum.post..condyle.w./mean(intra_sp$Sacrum.post..condyle.w.))
cv8_scw<-sd(intra_sp$Sacrum.centrum.w./mean(intra_sp$Sacrum.centrum.w.))
cv9_sacw<-sd(intra_sp$Sacrum.ant..condyle.w./mean(intra_sp$Sacrum.ant..condyle.w.))
cv10_mhhw<-sd(intra_sp$Max.w..distal.humerus/mean(intra_sp$Max.w..distal.humerus))

#For mature vs immature
mcv1_ah<-sd(mature$Acetabulum.ht./mean(mature$Acetabulum.ht.))
mcv2_paiw<-sd(mature$Ilium.w..post.acetabulum/mean(mature$Ilium.w..post.acetabulum))
mcv3_uaw<-sd(mature$Urostyle.ant..w./mean(mature$Urostyle.ant..w.))
mcv4_hhw<-sd(mature$Humerus.head.w./mean(mature$Humerus.head.w.))
mcv5_ow<-sd(mature$Olecranon.w./mean(mature$Olecranon.w.))
mcv6_rpw<-sd(mature$Radioulna.prox..w./mean(mature$Radioulna.prox..w.))
mcv7_spcw<-sd(mature$Sacrum.post..condyle.w./mean(mature$Sacrum.post..condyle.w.))
mcv8_scw<-sd(mature$Sacrum.centrum.w./mean(mature$Sacrum.centrum.w.))
mcv9_sacw<-sd(mature$Sacrum.ant..condyle.w./mean(mature$Sacrum.ant..condyle.w.))
mcv10_mhhw<-sd(mature$Max.w..distal.humerus/mean(mature$Max.w..distal.humerus))

imcv1_ah<-sd(immature$Acetabulum.ht./mean(immature$Acetabulum.ht.))
imcv2_paiw<-sd(immature$Ilium.w..post.acetabulum/mean(immature$Ilium.w..post.acetabulum))
imcv3_uaw<-sd(immature$Urostyle.ant..w./mean(immature$Urostyle.ant..w.))
imcv4_hhw<-sd(immature$Humerus.head.w./mean(immature$Humerus.head.w.))
imcv5_ow<-sd(immature$Olecranon.w./mean(immature$Olecranon.w.))
imcv6_rpw<-sd(immature$Radioulna.prox..w./mean(immature$Radioulna.prox..w.))
imcv7_spcw<-sd(immature$Sacrum.post..condyle.w./mean(immature$Sacrum.post..condyle.w.))
imcv8_scw<-sd(immature$Sacrum.centrum.w./mean(immature$Sacrum.centrum.w.))
imcv9_sacw<-sd(immature$Sacrum.ant..condyle.w./mean(immature$Sacrum.ant..condyle.w.))
imcv10_mhhw<-sd(immature$Max.w..distal.humerus/mean(immature$Max.w..distal.humerus))

#For each of the species
#E.glandulifer large
lcv1_ah<-sd(large_sp$Acetabulum.ht./mean(large_sp$Acetabulum.ht.))
lcv2_paiw<-sd(large_sp$Ilium.w..post.acetabulum/mean(large_sp$Ilium.w..post.acetabulum))
lcv3_uaw<-sd(large_sp$Urostyle.ant..w./mean(large_sp$Urostyle.ant..w.))
lcv4_hhw<-sd(large_sp$Humerus.head.w./mean(large_sp$Humerus.head.w.))
lcv5_ow<-sd(large_sp$Olecranon.w./mean(large_sp$Olecranon.w.))
lcv6_rpw<-sd(large_sp$Radioulna.prox..w./mean(large_sp$Radioulna.prox..w.))
lcv7_spcw<-sd(large_sp$Sacrum.post..condyle.w./mean(large_sp$Sacrum.post..condyle.w.))
lcv8_scw<-sd(large_sp$Sacrum.centrum.w./mean(large_sp$Sacrum.centrum.w.))
lcv9_sacw<-sd(large_sp$Sacrum.ant..condyle.w./mean(large_sp$Sacrum.ant..condyle.w.))
lcv10_mhhw<-sd(large_sp$Max.w..distal.humerus/mean(large_sp$Max.w..distal.humerus))

#E.planirostris
scv1_ah<-sd(small_sp$Acetabulum.ht./mean(small_sp$Acetabulum.ht.))
scv2_paiw<-sd(small_sp$Ilium.w..post.acetabulum/mean(small_sp$Ilium.w..post.acetabulum))
scv3_uaw<-sd(small_sp$Urostyle.ant..w./mean(small_sp$Urostyle.ant..w.))
scv4_hhw<-sd(small_sp$Humerus.head.w./mean(small_sp$Humerus.head.w.))
scv5_ow<-sd(small_sp$Olecranon.w./mean(small_sp$Olecranon.w.))
scv6_rpw<-sd(small_sp$Radioulna.prox..w./mean(small_sp$Radioulna.prox..w.))
scv7_spcw<-sd(small_sp$Sacrum.post..condyle.w./mean(small_sp$Sacrum.post..condyle.w.))
scv8_scw<-sd(small_sp$Sacrum.centrum.w./mean(small_sp$Sacrum.centrum.w.))
scv9_sacw<-sd(small_sp$Sacrum.ant..condyle.w./mean(small_sp$Sacrum.ant..condyle.w.))
scv10_mhhw<-sd(small_sp$Max.w..distal.humerus/mean(small_sp$Max.w..distal.humerus))

#E.glandulifer mature only
mlarge_sp<-large_sp[which(large_sp$Distal.condyle=="complete"),]

mlcv1_ah<-sd(mlarge_sp$Acetabulum.ht./mean(mlarge_sp$Acetabulum.ht.))
mlcv2_paiw<-sd(mlarge_sp$Ilium.w..post.acetabulum/mean(mlarge_sp$Ilium.w..post.acetabulum))
mlcv3_uaw<-sd(mlarge_sp$Urostyle.ant..w./mean(mlarge_sp$Urostyle.ant..w.))
mlcv4_hhw<-sd(mlarge_sp$Humerus.head.w./mean(mlarge_sp$Humerus.head.w.))
mlcv5_ow<-sd(mlarge_sp$Olecranon.w./mean(mlarge_sp$Olecranon.w.))
mlcv6_rpw<-sd(mlarge_sp$Radioulna.prox..w./mean(mlarge_sp$Radioulna.prox..w.))
mlcv7_spcw<-sd(mlarge_sp$Sacrum.post..condyle.w./mean(mlarge_sp$Sacrum.post..condyle.w.))
mlcv8_scw<-sd(mlarge_sp$Sacrum.centrum.w./mean(mlarge_sp$Sacrum.centrum.w.))
mlcv9_sacw<-sd(mlarge_sp$Sacrum.ant..condyle.w./mean(mlarge_sp$Sacrum.ant..condyle.w.))
mlcv10_mhhw<-sd(mlarge_sp$Max.w..distal.humerus/mean(mlarge_sp$Max.w..distal.humerus))

#Compiling data into a CSV
CV_intra<-c(cv1_ah,cv2_paiw,cv3_uaw,cv4_hhw,cv5_ow,cv6_rpw,cv7_spcw,cv8_scw,cv9_sacw,cv10_mhhw)
CV_mat<-c(mcv1_ah,mcv2_paiw,mcv3_uaw,mcv4_hhw,mcv5_ow,mcv6_rpw,mcv7_spcw,mcv8_scw,mcv9_sacw,mcv10_mhhw)
CV_immat<-c(imcv1_ah,imcv2_paiw,imcv3_uaw,imcv4_hhw,imcv5_ow,imcv6_rpw,imcv7_spcw,imcv8_scw,imcv9_sacw,imcv10_mhhw)
CV_glandulifer<-c(lcv1_ah,lcv2_paiw,lcv3_uaw,lcv4_hhw,lcv5_ow,lcv6_rpw,lcv7_spcw,lcv8_scw,lcv9_sacw,lcv10_mhhw)
CV_planirostris<-c(scv1_ah,scv2_paiw,scv3_uaw,scv4_hhw,scv5_ow,scv6_rpw,scv7_spcw,scv8_scw,scv9_sacw,scv10_mhhw)
CV_mature_gl<-c(mlcv1_ah,mlcv2_paiw,mlcv3_uaw,mlcv4_hhw,lcv5_ow,mlcv6_rpw,mlcv7_spcw,mlcv8_scw,mlcv9_sacw,mlcv10_mhhw)
measurements<-c("ah","paiw","uaw","hhw","ow","rpw","spcw","scw","sacw","mhhw")


CV<-cbind(measurements,CV_intra, CV_mat, CV_immat,CV_glandulifer,CV_mature_gl, CV_planirostris)
write.csv(CV, file="CV_intra.csv")
