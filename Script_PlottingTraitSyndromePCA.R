require(calibrate)
ScatterData <- read.table("ScatterData_1x2.txt", nrows=64, header=TRUE) #read full scatter data
ScatterData #show scatter data (optional)
plot(ScatterData$Axis1, ScatterData$Axis2, xlab='PCA axis 1 (14.8%)', ylab='PCA axis 2 (12.2%)', frame.plot=TRUE, pch =1,cex=0.5) #show correlation plot
textxy(ScatterData$Axis1, ScatterData$Axis2, ScatterData$Labels, cex = 0.6) #add labels. cx specifies the font size

ScatterData <- read.table("ScatterData_1x3.txt", nrows=64, header=TRUE) #read full scatter data
ScatterData #show scatter data (optional)
plot(ScatterData$Axis1, ScatterData$Axis3, xlab='PCA axis 1 (14.8%)', ylab='PCA axis 3 (8.1%)', frame.plot=TRUE, pch =1,cex=0.5) #show correlation plot
textxy(ScatterData$Axis1, ScatterData$Axis3, ScatterData$Labels, cex = 0.6) #add labels. cx specifies the font size

ScatterData <- read.table("ScatterData_1x4.txt", nrows=64, header=TRUE) #read full scatter data
ScatterData #show scatter data (optional)
plot(ScatterData$Axis1, ScatterData$Axis4, xlab='PCA axis 1 (14.8%)', ylab='PCA axis 4 (7.3%)', frame.plot=TRUE, pch =1,cex=0.5) #show correlation plot
textxy(ScatterData$Axis1, ScatterData$Axis4, ScatterData$Labels, cex = 0.6) #add labels. cx specifies the font size

#With the orders identifying the species
ScatterData <- read.table("ScatterData_1x2_Orders.txt", nrows=767, header=TRUE) #read full scatter data
ScatterData #show scatter data (optional)
plot(ScatterData$Axis1, ScatterData$Axis1, xlab='PCA axis 1 (14.8%)', ylab='PCA axis 2 (12.2%)', frame.plot=TRUE, pch =1,cex=0.5) #show correlation plot
textxy(ScatterData$Axis1, ScatterData$Axis2, ScatterData$Labels, cex = 0.6) #add labels. cx specifies the font size

ScatterData <- read.table("ScatterData_1x3_Orders.txt", nrows=767, header=TRUE) #read full scatter data
ScatterData #show scatter data (optional)
plot(ScatterData$Axis1, ScatterData$Axis3, xlab='PCA axis 1 (14.8%)', ylab='PCA axis 3 (8.1%)', frame.plot=TRUE, pch =1,cex=0.5) #show correlation plot
textxy(ScatterData$Axis1, ScatterData$Axis3, ScatterData$Labels, cex = 0.6) #add labels. cx specifies the font size

ScatterData <- read.table("ScatterData_1x4_Orders.txt", nrows=767, header=TRUE) #read full scatter data
ScatterData #show scatter data (optional)
plot(ScatterData$Axis1, ScatterData$Axis4, xlab='PCA axis 1 (14.8%)', ylab='PCA axis 4 (7.3%)', frame.plot=TRUE, pch =1,cex=0.5) #show correlation plot
textxy(ScatterData$Axis1, ScatterData$Axis4, ScatterData$Labels, cex = 0.6) #add labels. cx specifies the font size


#Only the correlation circle based on the correlations between trait modalities and axes
ScatterData <- read.table("ScatterData_1x2_Correl.txt", nrows=64, header=TRUE) #read full scatter data
ScatterData #show scatter data (optional)
s.corcircle(ScatterData, xax =2, yax=3, label = ScatterData$Labels, clabel = 1, grid = TRUE, sub = "", csub = 1, possub = "bottomleft", cgrid = 0, fullcircle = TRUE, box = FALSE, add.plot = FALSE)

plot(ScatterData$Axis1, ScatterData$Axis2, xlab='PCA axis 1 (14.8%)', ylab='PCA axis 2 (12.2%)', frame.plot=TRUE, pch =1,cex=0.5) #show correlation plot
textxy(ScatterData$Axis1, ScatterData$Axis2, ScatterData$Labels, cex = 0.6) #add labels. cx specifies the font size

ScatterData <- read.table("ScatterData_1x3_Correl.txt", nrows=64, header=TRUE) #read full scatter data
ScatterData #show scatter data (optional)
plot(ScatterData$Axis1, ScatterData$Axis3, xlab='PCA axis 1 (14.8%)', ylab='PCA axis 3 (8.1%)', frame.plot=TRUE, pch =1,cex=0.5) #show correlation plot
textxy(ScatterData$Axis1, ScatterData$Axis3, ScatterData$Labels, cex = 0.6) #add labels. cx specifies the font size

ScatterData <- read.table("ScatterData_1x4_Correl.txt", nrows=64, header=TRUE) #read full scatter data
ScatterData #show scatter data (optional)
plot(ScatterData$Axis1, ScatterData$Axis4, xlab='PCA axis 1 (14.8%)', ylab='PCA axis 4 (7.3%)', frame.plot=TRUE, pch =1,cex=0.5) #show correlation plot
textxy(ScatterData$Axis1, ScatterData$Axis4, ScatterData$Labels, cex = 0.6) #add labels. cx specifies the font size


s.corcircle(dfxy, label = row.names(dfxy), grid = TRUE,
            box = FALSE)

