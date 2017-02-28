library(patternize)

###############################################
# Analysis for Figure 1
###############################################

###
# patternize landmark/RGB analysis of ten Heliconius erato hydara
###

# List with samples
IDlist <- c('BC0004','BC0049','BC0050','BC0071','BC0077','BC0079','BC0082','BC0125','BC0129','BC0366')

# make list with landmarks
prepath <- 'landmarks'
extension <- '_landmarks_LFW.txt'
landmarkList <- makeList(IDlist, 'landmark', prepath, extension)

# make list with images
prepath <- 'images/heliconius'
extension <- '-D.JPG'
imageList <- makeList(IDlist, 'image', prepath, extension)

# run alignment of color patterns
RGB <- c(114,17,0) # red
rasterList_lanRGB <- patLanRGB(imageList, landmarkList, RGB, transformRef = 'BC0004', resampleFactor = 3, colOffset = 0.15, crop = TRUE, res = 200, adjustCoords = TRUE, plot = TRUE)

# sum the colorpatterns
summedRaster_lanRGB <- sumRaster(rasterList_lanRGB, IDlist, type = 'RGB')

# plot heatmap
outline_BC0004 <- read.table('cartoon/BC0004_outline.txt', h= F)
lines_BC0004 <- list.files(path='cartoon', pattern='vein', full.names = T)

plotHeat(summedRaster_lanRGB, IDlist, plotCartoon = TRUE, refShape = 'target', outline = outline_BC0004, lines = lines_BC0004, landList = landmarkList, adjustCoords = TRUE, flipRaster = 'y', imageList = imageList, cartoonID = 'BC0004')


###
# patternize registration/RGB analysis of ten Heliconius erato hydara
###

# List with samples
IDlist <- c('BC0004','BC0049','BC0050','BC0071','BC0077','BC0079','BC0082','BC0125','BC0129','BC0366')

# make list with images
prepath <- 'images/Heliconius'
extension <- '-D.JPG'
imageList <- makeList(IDlist, 'image', prepath, extension)

# choose target image
target <- imageList[['BC0004']]

# run alignment of color patterns
RGB <- c(114,17,0) # red
rasterList_regRGB <- patRegRGB(imageList, target, RGB, resampleFactor = 5, colOffset= 0.15, removebg = TRUE, plot = TRUE)

# sum the colorpatterns
summedRaster_regRGB <- sumRaster(rasterList_regRGB, IDlist, type = 'RGB')

# plot heatmap
outline_BC0004 <- read.table('cartoon/BC0004_outline.txt', h= F)
lines_BC0004 <- list.files(path='cartoon', pattern='BC0004_vein', full.names = T)

plotHeat(summedRaster_regRGB, IDlist, plotCartoon = TRUE, refShape = 'target', outline = outline_BC0004, lines = lines_BC0004, landList = landmarkList, flipRaster = 'xy', imageList = imageList, cartoonID = 'BC0004')

##
# Compare landmark and registration
##

# give rasters same extent and resolution
rasterEx <- raster::extent(min(outline_BC0004[,1]),max(outline_BC0004[,1]),min(outline_BC0004[,2]),max(outline_BC0004[,2]))
rRe <- raster::raster(nrow=200,ncol=200)
extent(rRe) <- rasterEx

summedRaster_lanRGB2 <- raster::resample(summedRaster_lanRGB,rRe,datatype="INT1U", method='ngb')
summedRaster_regRGB2 <- raster::resample(summedRaster_regRGB,rRe,datatype="INT1U", method='ngb')

# orient rasters
summedRaster_lanRGB2 <- raster::flip(summedRaster_lanRGB2, 'y')
summedRaster_regRGB2 <- raster::flip(summedRaster_regRGB2, 'y')
summedRaster_regRGB2 <- raster::flip(summedRaster_regRGB2, 'x')

# subtract rasters
subtracted <- summedRaster_lanRGB2 - summedRaster_regRGB2

# plot heatmap
colfunc <- c("blue","lightblue","white","pink","red")
plotHeat(subtracted, IDlist, plotCartoon = TRUE, refShape = 'target', outline = outline_BC0004, lines = lines_BC0004, landList = landmarkList, adjustCoords = TRUE, imageList = imageList, cartoonID = 'BC0004', zlim=c(-1,1), colpalette= colfunc)

###############################################
# Analysis for Figure 2
###############################################

###
# patternize registration/RGB analysis of Heliconius and hybrids
###

# Lists with samples
IDListEra <- c('BC0057','BC0064','BC0085','BC0088','BC0149','BC0154','BC0163','BC0200','BC0340','BC2127')
IDListHyb <- c('BC0213','BC0360','BC0391','BC0565','BC2090','BC2207','BC2208','BC2321','BC2359','BC2550')

# make list with images
prepath <- 'images/heliconius'
extension <- '-D.JPG'
imageListEra <- makeList(IDListEra, 'image', prepath, extension)
imageListHyb <- makeList(IDListHyb, 'image', prepath, extension)

# choose target image
target <- imageListEra[['BC0057']]

# run alignment of color patterns

RGB <- c(143,138,49) # yellow
rasterList_regRGBEra <- patRegRGB(imageListEra, target, RGB, resampleFactor = 5, colOffset= 0.12, crop = c(1000,3800,500,2800), removebg = TRUE, plot = TRUE, focal = TRUE, sigma = 5, iterations = 5)

RGB <- c(114,17,0) # red
rasterList_regRGBHyb <- patRegRGB(imageListHyb, target, RGB, resampleFactor = 5, colOffset= 0.12, crop = c(1000,3800,500,2800), removebg = TRUE, plot = TRUE, focal = TRUE, sigma = 5, iterations = 5)

# sum the colorpatterns
summedRaster_regRGBEra <- sumRaster(rasterList_regRGBEra, IDListEra, type = 'RGB')
summedRaster_regRGBHyb <- sumRaster(rasterList_regRGBHyb, IDListHyb, type = 'RGB')

# plot heatmap
outline_BC0057 <- read.table('cartoon/BC0057_outline.txt', h= F)
lines_BC0057 <- list.files(path='cartoon', pattern='BC0057_vein', full.names = T)

plotHeat(summedRaster_regRGBEra, IDListEra, plotCartoon = TRUE, refShape = 'target', outline = outline_BC0057, lines = lines_BC0057, landList = landmarkList, crop = c(1000,3800,500,2800), flipRaster = 'xy', imageList = imageListEra, cartoonID = 'BC0057')
plotHeat(summedRaster_regRGBHyb, IDListHyb, plotCartoon = TRUE, refShape = 'target', outline = outline_BC0057, lines = lines_BC0057, landList = landmarkList, crop = c(1000,3800,500,2800), flipRaster = 'xy', imageList = imageListHyb, cartoonID = 'BC0057')

# subtract rasters
subtracted <- summedRaster_regRGBEra/length(IDListEra) - summedRaster_regRGBHyb/length(IDListHyb)

# plot subtracted heatmap
colfunc <- c("blue","lightblue","white","pink","red")
plotHeat(subtracted, IDlist, plotCartoon = TRUE, refShape = 'target', outline = outline_BC0057, lines = lines_BC0057, landList = landmarkList, adjustCoords = TRUE, crop = c(1000,3800,500,2800), flipRaster = 'xy', imageList = imageList, cartoonID = 'BC0057', zlim=c(-1,1), colpalette= colfunc, normalized = TRUE)

###
# Plot PCA
###

# Make population and color list
popList <- list(IDListEra, IDListHyb)
colList <- c("black", "pink")

TotalList <- c(rasterList_regRGBEra, rasterList_regRGBHyb)

pcaOut <- patPCA(TotalList, popList, colList)
comp <- prcomp(pcaOut[[1]])

pcaOut[[2]]
plot(comp$rotation[,1:2], col=as.vector(pcaOut[[2]]$col), pch=19)



# library(logisticPCA)
# 
# logi<-logisticPCA((pcaOut[[1]]), k=20)
# logi$U
# 
# 
# 
# 
# 
# pcdata <- comp$x
# pcdata <-logi$PCs
# rotation <- comp$rotation
# k <- dim(pcaOut[[1]])[1] ; p <- dim(pcaOut[[1]])[2]
# 
# PC1min <- min(pcdata[,2])
# PC1max <- max(pcdata[,2])
# 
# pc.vecMi <- rep(0, dim(logi$U)[2])
# pc.vecMi[2] <- PC1min 
# 
# pc.vecMa <- rep(0, dim(logi$U)[2])
# pc.vecMa[2] <- PC1max 
# 
# 
# xMi <- pcdata %*% pc.vecMi
# xMa <- pcdata %*% pc.vecMa
# 
# x2Mi <-matrix(xMi,ncol = dim(TotalList[[1]])[1], nrow = dim(TotalList[[1]])[2])
# x2Ma <-matrix(xMa,ncol = dim(TotalList[[1]])[1], nrow = dim(TotalList[[1]])[2])
# 
# mapMi <-raster::raster(x2Mi)
# mapMi[mapMi == 0] <- NA
# 
# mapMa <-raster::raster(x2Ma)
# mapMa[mapMa == 0] <- NA
# 
# raster::extent(mapMi) <- raster::extent(TotalList[[1]])
# raster::extent(mapMa) <- raster::extent(TotalList[[1]])
# 
# raster::plot(mapMi)
# raster::plot(mapMa)
# 
# raster::plot(mapMa-mapMi)
###
# Calculate colored areas
###

AreaEra <- patArea(rasterList_regRGBEra, IDListEra, refShape = 'target', type = 'RGB', outline = outline_BC0057, crop = c(1000,3800,500,2800), adjustCoords = TRUE, imageList = imageList, cartoonID = 'BC0057', flipRaster = 'xy')
AreaHyb <- patArea(rasterList_regRGBHyb, IDListHyb, refShape = 'target', type = 'RGB', outline = outline_BC0057, crop = c(1000,3800,500,2800), adjustCoords = TRUE, imageList = imageList, cartoonID = 'BC0057', flipRaster = 'xy')
hist(AreaEra$Area)
hist(AreaHyb$Area)

hist(AreaEra$Area, xlim=c(0.05,0.2), col="yellow", breaks=5)
hist(AreaHyb$Area, add=T, col=rgb(1, 0, 0, 0.5), breaks=10)

dEra <- density(AreaEra$Area)
dHyb <- density(AreaHyb$Area)
plot(dEra, xlim=c(0.05,0.25), ylim=c(0,40), main="")
polygon(dEra, col=rgb(1, 1, 0, 0.5), border="yellow")
par(new=T)
plot(dHyb, xlim=c(0.05,0.25), ylim=c(0,40), main="")
polygon(dHyb, col=rgb(1, 0, 0, 0.5), border="red")

###############################################
# Analysis for Figure X - Guppies
###############################################

###
# patternize registration/K analysis
###

# Lists with samples
IDListWT <- c('cross20_F1fBC1_wtm_9472','cross20_F1fBC1_wtm_9471','cross20_F1fBC1_wtm_9470','cross20_F1fBC1_wtm_9469','cross20_F1fBC1_wtm_9131','cross20_F1fBC1_wtm_9130','cross20_F1fBC1_wtm_0319','cross20_F1fBC1_wtm_0318','cross20_F1fBC1_wtm_0317','cross20_F1fBC1_wtm_0316')
IDListG <- c('cross20_F1fBC2_whitem_7819','cross20_F1fBC2_whitem_7818','cross20_F1fBC5_whitem_1284','cross20_F1fBC2_whitem_7816','cross20_F1fBC2_whitem_7815','cross20_F1fBC3_whitem_1277','cross20_F1fBC1_whitem_1292','cross20_F1fBC1_whitem_1291','cross20_F1fBC1_whitem_1279','cross20_F1fBC1_whitem_1278')

# make lists with images
prepath <- 'images/guppies'
extension <- '.jpg'
imageListWT <- makeList(IDListWT, 'image', prepath, extension)
imageListG <- makeList(IDListG, 'image', prepath, extension)

# choose target image
target <- imageListWT[['cross20_F1fBC1_wtm_9472']]

# run alignment of color patterns
rasterList_regKWT <- patRegK(imageListWT, target, k = 5, resampleFactor = 6, crop = c(300,2800,300,1800), plot = TRUE, useBlockPercentage = 90)
rasterList_regKG <- patRegK(imageListG, target, k = 5, resampleFactor = 6, crop = c(300,2800,300,1800), plot = TRUE, useBlockPercentage = 90)

# sum the colorpatterns
summedRaster_KWT <- sumRaster(rasterList_regKWT, IDListWT, type = 'k')
summedRaster_KG <- sumRaster(rasterList_regKG, IDListG, type = 'k')

# plot heatmaps using the list of summed rasters
outline_9472 <- read.table('cartoon/cross20_F1fBC1_wtm_9472_outline.txt', h= F)

plotHeat(summedRaster_KWT, IDListWT, plotCartoon = TRUE, refShape = 'target', outline = outline_9472, crop = c(300,2800,300,1800), flipRaster = 'y', flipOutline = 'x', imageList = imageListWT, cartoonOrder = 'under', cartoonFill = 'black')
plotHeat(summedRaster_KG, IDListWT, plotCartoon = TRUE, refShape = 'target', outline = outline_9472, crop = c(300,2800,300,1800), flipRaster = 'y', flipOutline = 'x', imageList = imageListWT, cartoonOrder = 'under', cartoonFill = 'black')

# plot selected heatmap
plotHeat(summedRaster_KWT[[3]], IDListWT, plotCartoon = TRUE, refShape = 'target', outline = outline_9472, crop = c(300,2800,300,1800), flipRaster = 'y', flipOutline = 'x', imageList = imageListWT, cartoonOrder = 'above')
plotHeat(summedRaster_KG[[2]], IDListWT, plotCartoon = TRUE, refShape = 'target', outline = outline_9472, crop = c(300,2800,300,1800), flipRaster = 'y', flipOutline = 'x', imageList = imageListWT, cartoonOrder = 'above')


###
# Plot PCA
###

# Make population and color list
popList <- list(IDListWT, IDListG)
colList <- c("black", "pink")

# extract rasters for k-means cluster of interest and combine rasters
rasterList_regKWT_S <- lapply(rasterList_regKWT, function (x) x[[3]])
rasterList_regKG_S <- lapply(rasterList_regKG, function (x) x[[2]])

TotalList <- c(rasterList_regKWT_S, rasterList_regKG_S)

pcaOut <- patPCA(TotalList, popList, colList)
comp <- prcomp(pcaOut[[1]])
pcaOut[[2]]
plot(comp$rotation, col=as.vector(pcaOut[[2]]$col), pch=19)


###############################################
# Analysis for Figure X - Wolfspiders from the Galapagos
###############################################

###
# patternize registration/K analysis
###

# Galapagos: Santa Cruz - Top
IDListGala <- c('IMG_2627','IMG_2645','IMG_2602','IMG_0710','IMG_2637','IMG_2591','IMG_2599','IMG_2600','IMG_2608','IMG_2630')

# Galapagos: Santa Cruz - Coast
IDListHend <-c('IMG_1739','IMG_1745','IMG_1697','IMG_1711','IMG_1733','IMG_1737','IMG_1742','IMG_1746','IMG_1755','IMG_1763')

# Galapagos: San Cristobal - Top
IDListJunc <- c('IMG_1238','IMG_1242','IMG_1244','IMG_1262','IMG_1266','IMG_1268','IMG_1271','IMG_1276','IMG_1281','IMG_1285')

# Galapagos: San Cristobal - Coast
IDListSnod <- c('IMG_1389','IMG_1373','IMG_1380','IMG_1392','IMG_1540','IMG_1393','IMG_1394','IMG_1365','IMG_1364','IMG_1341')

# Galapagos: Espanola - Coast
IDListEspa <- c('IMG_1130','IMG_1150','IMG_0957','IMG_0930','IMG_1039','IMG_1107','IMG_1115','IMG_1116','IMG_1122','IMG_1126')

# make lists with images
extension <- '.jpg'
prepath <- 'images/Hogna/Hgalapagoensis'
imageListGala <- makeList(IDListGala, 'image', prepath, extension)
prepath <- 'images/Hogna/Hhendrickxi'
imageListHend <- makeList(IDListHend, 'image', prepath, extension)
prepath <- 'images/Hogna/Hjunco'
imageListJunc <- makeList(IDListJunc, 'image', prepath, extension)
prepath <- 'images/Hogna/Hsnodgrassi'
imageListSnod <- makeList(IDListSnod, 'image', prepath, extension)
prepath <- 'images/Hogna/Hespanola'
imageListEspa <- makeList(IDListEspa, 'image', prepath, extension)

# choose target image
target <- imageListGala[['IMG_2600']]

# run alignment of color patterns

outline_IMG_2600 <- read.table('cartoon/IMG_2600_outline.txt', h= F)

# color extraction using k-means
rasterList_regGalaK <- patRegK(imageListGala, target, k = 3, resampleFactor = 10, plot = T, removebgR = 130, removebgK = 150)
rasterList_regHendK <- patRegK(imageListHend, target, k = 3, resampleFactor = 10, plot = T, removebgR = 130, removebgK = 180)
rasterList_regJuncK <- patRegK(imageListJunc, target, k = 3, resampleFactor = 10, plot = T, removebgR = 130, removebgK = 130)
rasterList_regSnodK <- patRegK(imageListSnod, target, k = 3, resampleFactor = 10, plot = T, removebgR = 130, removebgK = 200)
rasterList_regEspaK <- patRegK(imageListEspa, target, k = 3, resampleFactor = 10, plot = T, removebgR = 130, removebgK = 200)

summedRaster_GalaK <- sumRaster(rasterList_regGalaK, IDListGala, type = 'k')
summedRaster_HendK <- sumRaster(rasterList_regHendK, IDListHend, type = 'k')
summedRaster_JuncK <- sumRaster(rasterList_regJuncK, IDListJunc, type = 'k')
summedRaster_SnodK <- sumRaster(rasterList_regSnodK, IDListSnod, type = 'k')
summedRaster_EspaK <- sumRaster(rasterList_regEspaK, IDListEspa, type = 'k')

plotHeat(summedRaster_GalaK, IDListGal)
plotHeat(summedRaster_HendK, IDListHend)
plotHeat(summedRaster_JuncK, IDListJunc)
plotHeat(summedRaster_SnodK, IDListSnod)
plotHeat(summedRaster_SnodK, IDListEspa)

summedRaster_GalaKM <- maskOutline(summedRaster_GalaK[[1]], outline_IMG_2600, refShape = 'target', flipOutline = 'y')
summedRaster_HendKM <- maskOutline(summedRaster_HendK[[1]], outline_IMG_2600, refShape = 'target', flipOutline = 'y')
summedRaster_JuncKM <- maskOutline(summedRaster_JuncK[[1]], outline_IMG_2600, refShape = 'target', flipOutline = 'y')
summedRaster_SnodKM <- maskOutline(summedRaster_SnodK[[3]], outline_IMG_2600, refShape = 'target', flipOutline = 'y')

plotHeat(summedRaster_GalaKM, IDListGal)
plotHeat(summedRaster_HendKM, IDListHend)
plotHeat(summedRaster_JuncKM, IDListJunc)
plotHeat(summedRaster_SnodKM, IDListSnod)
