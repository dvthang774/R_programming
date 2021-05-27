setwd('C:\\Users\\Admin\\R_programming')
Owls <- read.table(file = "Owls.txt", header= TRUE)
names(Owls)
str(Owls)
boxplot(Owls$NegPerChick)
par(mfrow= c(2,2), mar= c(3,3,2,1))
boxplot(NegPerChick ~ SexParent, data = Owls)
boxplot(NegPerChick ~ FoodTreatment, data = Owls)
boxplot(NegPerChick ~ SexParent*FoodTreatment, data = Owls)
boxplot(NegPerChick ~ SexParent*FoodTreatment,
        names= c("F/Dep","M/Dep","F/Sat","M/Sat"), 
        data = Owls)
boxplot(NegPerChick ~ï..Nest, data = Owls)


par(mar=c(2,2,3,3))
boxplot(NegPerChick ~ ï..Nest, 
        data = Owls,
        axes=FALSE, 
        ylim=c(-3,8.5))
axis(2, at=c(0,2,4,6,8))
text(x=1:27, y=-2, 
     labels= levels(Owls$Nest),
     cex=0.75, srt=65)


R2 <- read.table(file = "RIKZ2.txt", header= TRUE)
names(R2)
str(R2)
par(mfrow= c(2,2), mar= c(3,3,2,1))
boxplot(Richness ~ Beach, 
        data = R2)

boxplot(Richness ~ Beach, 
        data = R2,
        col="grey",xlab="Richness",ylab="Beach")
veg <- read.table(file = "vegetation2.txt", header= TRUE)
names(veg)
str(veg)
boxplot(R ~ Transect, 
        data = veg,
        col="grey",xlab="R",ylab="Transect")
