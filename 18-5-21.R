setwd('C:\\Users\\Admin\\R_programming')
#Chuong 5 => plot => bi???u d??? d???ng x, y
#Thêm m???t s??? tool cho v??? bi???u d???
#Phân tích d??? li???u
#7.1. Pie chart
#H5N1 Avian Infuenza A => WHO World Health Organization
BFCases <- read.table(file = "BirdFluCases.txt", sep="\t", header = TRUE)
#Phân cách gi???a các c???t d??? li???u chúng ta là tab tham s???: sep="\t"
names(BFCases)
str(BFCases)
#Thao tác th???ng kê s??? case the nam
Cases <- rowSums(BFCases[,2:16])
names(Cases)<-BFCases[,1]
Cases
#V??? bi???u d??? pie chart
#. Bi???u d??? chu???n - standard pie chart
#Dùng d??? chia màn hình
par(mfrow= c(2,2), mar= c(3,3,2,1))
#mfrow = c(2,2)=> Chia màn hình ra làm 4 góc
#c=(1,4); c=(4,1)
par(mfrow= c(1,4), mar= c(3,3,2,1))
par(mfrow= c(4,1), mar= c(3,3,2,1))
#mar=c(3,3,2,1): kho???ng tr???ng gi???a các bi???u d???
#margin: bottom, left, top, right


pie(Cases, main = "Standard pie chart")
pie(Cases, main = "Grey color",
    col = gray(seq(0.4, 1.0, length=6)),
    clockwise = TRUE
)
pie(Cases, main = "Rainbow color",
    col = rainbow(6),
    clockwise = TRUE
)
#Cài gói libary plotrix
install.packages("plotrix")
#G???i thu vi???n plotrix
library(plotrix)




pie3D(Cases, main = "3D Pie Chart",
      explode=0.1,
      labels = names(Cases),
      labelcex=0.6,
)




rm()
op <- par(mfrow= c(2,2), mar= c(3,3,2,1))
pie(Cases, main = "Standard pie chart")
pie(Cases, main = "Grey color",
    col = gray(seq(0.4, 1.0, length=6)),
    clockwise = TRUE
)
pie(Cases, main = "Rainbow color",
    col = rainbow(6),
    clockwise = TRUE
)
pie3D(Cases, main = "3D Pie Chart",
      explode=0.1,
      labels = names(Cases),
      labelcex=0.6,
)
par(op)




#7.2 Bar Chart and Strip Chart
BFDeaths <- read.table(file="BirdFluDeaths.txt",
                       sep="\t",
                       header = TRUE,
)
names(BFDeaths)
str(BFDeaths)
Deaths <-rowSums(BFDeaths[,2:16])
names(Deaths)<-BFDeaths[,1]
Deaths


#Chia màn hình là 4 ph???n 2x2
par(mfrow=c(2,2), mar= c(3,3,2,1))


barplot(Cases, main="Bird Flu Cases")
Counts<-cbind(Cases, Deaths)
barplot(Counts)
barplot(t(Counts), col=gray(c(0.5,1)))
barplot(t(Counts), beside=TRUE)


Counts
t(Counts)


#S??? d???ng hàm merge d??? n???i 2 data.frame l???i v???i nhau
#S??? d???ng hàm write.table(dataframe, file="tên m???i.txt",...)
#S??? d???ng 2 hàm trên ???ng d???ng cho dataset RIKZ.txt
# Bi???n thành m???t dataset RIKZ_A.txt có ch???a các c???t
#Sample, Richness, Shannon
Benthic <-read.table(file="RIKZ2.txt", header=TRUE)
Bent.M <-tapply(Benthic$Richness, 
                INDEX=Benthic$Beach, 
                FUN=mean)
Bent.sd <-tapply(Benthic$Richness, 
                 INDEX=Benthic$Beach, 
                 FUN=sd)
MSD <- cbind(Bent.M, Bent.sd)
MSD
barplot(Bent.M)


bp <- barplot(Bent.M, xlab="Beach",
              ylim = c(0,20),
              ylab="Richness",
              col=rainbow(9)
)
arrows(bp, Bent.M, 
       bp, Bent.M+Bent.sd,
       lwd=1.5,
       angle=90,
       length=0.1
)
box()


Benth.le <- tapply(Benthic$Richness,
                   INDEX=Benthic$Beach,
                   FUN=length)
#se= standard error= 
#standard deviation/square root of the sample size
#
Bent.se <- Bent.sd/sqrt(Benth.le)
Bent.se
#STRIP CHART


stripchart(Benthic$Richness~Benthic$Beach,
           vert=TRUE,
           pch=1,
           method="jitter",
           jit=0.05,
           xlab="Beach",
           ylab="Richness")
points(1:9, Bent.M, pch=16,
       cex=1.5)
arrows(1:9, Bent.M,
       1:9, Bent.M+Bent.se,
       lwd=1.5,
       angle=90,
       length = 0.1)
arrows(1:9, Bent.M,
       1:9, Bent.M-Bent.se,
       lwd=1.5,
       angle=90,
       length = 0.1)