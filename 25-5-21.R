#Ngày 25/05/2021
#7.6 PairPlot
setwd('C:\\Users\\Admin\\R_programming')
Benthic <-read.table(file="RIKZ2.txt", header=TRUE)
pairs(Benthic[,2:9])


pairs(Benthic[,2:9], 
      diag.panel = panel.hist,
      upper.panel = panel.smooth,
      lower.panel = panel.cor
)
help(pairs)


example(pairs) 


pairs> pairs(USJudgeRatings, 
             lower.panel = panel.smooth, 
             upper.panel = panel.cor,
             pairs+       gap=0, 
             row1attop=FALSE)






#Hàm Coplot


coplot(Richness ~ NAP | as.factor(Beach),
       pch=19, data=Benthic)
coplot(Richness ~ NAP | grainsize,
       pch=19, data=Benthic)






panel.lm= function(x,y,...){
  tmp <- lm(y~x, na.action = na.omit)
  abline(tmp)
  points(x, y,...)
}
coplot(Richness ~ NAP | as.factor(Beach),
       pch=19, panel=panel.lm, data=Benthic)


#Richness vs NAP --> Beach
#Vẽ biểu đồ coplot sử dụng data set vegetation
#richness vs biến bất kỳ dựa trên đk là transect


MyLayout <- matrix(c(2,0,1,3), 
                   nrow=2, ncol=2, 
                   byrow=TRUE)
MyLayout
nf<- layout(mat=MyLayout, widths=c(3,1),
            heights = c(1,3),
            respect = TRUE)
layout.show(nf)
setwd('C:\\Users\\Admin\\R_programming')
Benthic <-read.table(file="RIKZ2.txt", header=TRUE)
xrange <-c(min(Benthic$NAP), 
           max(Benthic$NAP))
yrange <- c(min(Benthic$Richness), 
            max(Benthic$Richness))
#First graph
par(mar=c(4,4,2,2))
plot(Benthic$NAP, Benthic$Richness,
     xlim=xrange, ylim=yrange,
     xlab="NAP", ylab="Richness")
#Second graph
par(mar=c(0,3,1,1))
boxplot(Benthic$NAP, horizontal=TRUE,
        axes = FALSE,
        frame.plot = FALSE,
        ylim= xrange,
        space=0)
#Second graph
par(mar=c(3,0,1,1))
boxplot(Benthic$Richness, horiz=TRUE,
        axes = FALSE,
        frame.plot = FALSE,
        ylim= yrange,
        space=0)