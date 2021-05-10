setwd("C:\\Users\\Admin\\random")
Veg <- read.table(file="Vegetation2.txt", header=TRUE)
str(Veg)
plot(Veg$BARESOIL,Veg$R)
plot(Veg$R, Veg$BARESOIL)
plot(x=Veg$BARESOIL, y=Veg$R)
plot(R~BARESOIL, data = Veg)
plot(x=Veg$BARESOIL, #biến x
     y=Veg$R, #biến y
     xlab="Exposed soil",  #tiêu đề của x
     ylab = "Species richness", #tiêu đề của y
     main="Scatter plot", #tiêu đề của đồ thị
     xlim=c(0,45), #khoảng giá trị của x 0-45
     ylim=c(4,19))
plot(x=Veg$BARESOIL, #biến x
     y=Veg$R, #biến y
     xlab="Exposed soil",  #tiêu đề của x
     ylab = "Species richness", #tiêu đề của y
     main="Scatter plot", #tiêu đề của đồ thị
     xlim=c(min(Veg$BARESOIL), max(Veg$BARESOIL)), #khoảng giá trị của x 0-45
     ylim=c(min(Veg$R), max(Veg$R))) #Khoảng giá trị của y
plot(x=Veg$BARESOIL, #biến x
     y=Veg$R, #biến y
     xlab="Exposed soil",  #tiêu đề của x
     ylab = "Species richness", #tiêu đề của y
     main="Scatter plot", #tiêu đề của đồ thị
     xlim=c(min(Veg$BARESOIL), max(Veg$BARESOIL)), #khoảng giá trị của x 0-45
     ylim=c(min(Veg$R), max(Veg$R)), pch=8) #Khoảng giá trị của y
#mặc định: biểu tượng - pch=1 - hình tròn
#pch=2: hình tam giác


Veg$Transect
plot(x=Veg$BARESOIL, #biến x
     y=Veg$R, #biến y
     xlab="Exposed soil",  #tiêu đề của x
     ylab = "Species richness", #tiêu đề của y
     main="Scatter plot", #tiêu đề của đồ thị
     xlim=c(min(Veg$BARESOIL), max(Veg$BARESOIL)), #khoảng giá trị của x 0-45
     ylim=c(min(Veg$R), max(Veg$R)), pch=Veg$Transect) #Khoảng giá trị của y


Veg$Time
#Nếu <=1980 thì cho biểu tượng pch=1
#Nếu >1980 thì cho biểu tượng pch =10
TimeGroup <-Veg$Time
TimeGroup[Veg$Time<=1980] <- 1
TimeGroup[Veg$Time>1980] <- 10
plot(x=Veg$BARESOIL, #biến x
     y=Veg$R, #biến y
     xlab="Exposed soil",  #tiêu đề của x
     ylab = "Species richness", #tiêu đề của y
     main="Scatter plot", #tiêu đề của đồ thị
     xlim=c(min(Veg$BARESOIL), max(Veg$BARESOIL)), #khoảng giá trị của x 0-45
     ylim=c(min(Veg$R), max(Veg$R)), pch=TimeGroup) #Khoảng giá trị của y




plot(x=Veg$BARESOIL, #biến x
     y=Veg$R, #biến y
     xlab="Exposed soil",  #tiêu đề của x
     ylab = "Species richness", #tiêu đề của y
     main="Scatter plot", #tiêu đề của đồ thị
     xlim=c(min(Veg$BARESOIL), max(Veg$BARESOIL)), #khoảng giá trị của x 0-45
     ylim=c(min(Veg$R), max(Veg$R)), pch=TimeGroup, col=3) #Khoảng giá trị của y


#tham số: col: color: 1- black
#2 - red
#3 - green


x<- 1:8
plot(x, col=x)


#điều chỉnh kích thước của symbols
CexSize <-Veg$Time
CexSize[Veg$Time<=1980] <- 1
CexSize[Veg$Time>1980] <- 2
plot(x=Veg$BARESOIL, #biến x
     y=Veg$R, #biến y
     xlab="Exposed soil",  #tiêu đề của x
     ylab = "Species richness", #tiêu đề của y
     main="Scatter plot", #tiêu đề của đồ thị
     xlim=c(min(Veg$BARESOIL), max(Veg$BARESOIL)), #khoảng giá trị của x 0-45
     ylim=c(min(Veg$R), 
            max(Veg$R)), 
     pch=TimeGroup, 
     col=1,
     cex=CexSize) #Khoảng giá trị của y
veg1 <- loess(R~BARESOIL, data=Veg)
fit <- fitted(veg1)
lines(Veg$BARESOIL, fit)

# Create the data for the chart.
v <- c(7,12,28,3,41)
t <- c(14,7,6,19,3)

# Give the chart file a name.
png(file = "line_chart_2_lines.jpg")

# Plot the bar chart.
plot(v,type = "o",col = "red", xlab = "Month", ylab = "Rain fall", 
     main = "Rain fall chart")

lines(t, type = "o", col = "blue")

# Save the file.
dev.off()


install.packages("ggplot2")                             # Install and load ggplot2
library("ggplot2")
data <- data.frame(x = rep(1:10, 3),                    # Create data frame 
                   y = c(y1, y2, y3),
                   line = c(rep("y1", 10),
                            rep("y2", 10),
                            rep("y3", 10)))
head(data)        
ggplot(data, aes(x = x, y = y, col = line)) +           # Draw line plot with ggplot2
        geom_line()

v <- c(7,12,28,3,41)
t <- c(14,7,6,19,3)

# Give the chart file a name.
png(file = "line_chart_2_lines.jpg")

# Plot the bar chart.
plot(v,type = "o",col = "red", xlab = "Month", ylab = "Rain fall", 
     main = "Rain fall chart")

lines(t, type = "o", col = "blue")

# Save the file.
dev.off()

#loess
load(url('https://www.dropbox.com/s/ud32tbptyvjsnp4/data.R?dl=1'))
lw1 <- loess(y ~ x,data=data)
plot(y ~ x, data=data,pch=19,cex=0.1)
j <- order(data$x)
lines(data$x[j],lw1$fitted[j],col="red",lwd=3)


#fitted
library(ggplot2)
library(MASS)

# Generate gamma rvs

x <- rgamma(100000, shape = 2, rate = 0.2)

den <- density(x)

dat <- data.frame(x = den$x, y = den$y)

# Plot density as points

ggplot(data = dat, aes(x = x, y = y)) + 
        geom_point(size = 3) +
        theme_classic()



# Create dummy data
variety <- rep( c("soldur", "silur", "lloyd", "pescadou", "X4582", "Dudur", "Classic"), each=40)
treatment <- rep(c(rep("high" , 20) , rep("low" , 20)) , 7)
note <- c( rep(c(sample(0:4, 20 , replace=T) , sample(1:6, 20 , replace=T)),2), 
           rep(c(sample(5:7, 20 , replace=T), sample(5:9, 20 , replace=T)),2), 
           c(sample(0:4, 20 , replace=T) , sample(2:5, 20 , replace=T), 
             rep(c(sample(6:8, 20 , replace=T) , sample(7:10, 20 , replace=T)),2) ))
data=data.frame(variety, treatment ,  note)

# Reorder varieties (group) (mixing low and high treatments for the calculations)
new_order <- with(data, reorder(variety , note, mean , na.rm=T))

# Then I make the boxplot, asking to use the 2 factors : variety (in the good order) AND treatment :
par(mar=c(3,4,3,1))
myplot <- boxplot(note ~ treatment*new_order , data=data  , 
                  boxwex=0.4 , ylab="sickness",
                  main="sickness of several wheat lines" , 
                  col=c("slateblue1" , "tomato") ,  
                  xaxt="n")

# To add the label of x axis
my_names <- sapply(strsplit(myplot$names , '\\.') , function(x) x[[2]] )
my_names <- my_names[seq(1 , length(my_names) , 2)]
axis(1, 
     at = seq(1.5 , 14 , 2), 
     labels = my_names , 
     tick=FALSE , cex=0.3)

# Add the grey vertical lines
for(i in seq(0.5 , 20 , 2)){ 
        abline(v=i,lty=1, col="grey")
}

# Add a legend
legend("bottomright", legend = c("High treatment", "Low treatment"), 
       col=c("slateblue1" , "tomato"),
       pch = 15, bty = "n", pt.cex = 3, cex = 1.2,  horiz = F, inset = c(0.1, 0.1))

