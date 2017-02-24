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
rasterList_regRGBEra <- patRegRGB(imageListEra, target, RGB, resampleFactor = 5, colOffset= 0.11, crop = c(1000,3800,500,2800), removebg = TRUE, plot = TRUE, focal = TRUE, sigma = 5, iterations = 5)

RGB <- c(114,17,0) # red
rasterList_regRGBHyb <- patRegRGB(imageListHyb, target, RGB, resampleFactor = 5, colOffset= 0.11, crop = c(1000,3800,500,2800), removebg = TRUE, plot = TRUE, focal = TRUE, sigma = 5, iterations = 5)

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

# plot heatmap
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
plot(comp$rotation, col=as.vector(pcaOut[[2]]$col), pch=19)

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
# Analysis for Figure 3
###############################################

###
# patternize registration/K analysis of guppies
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
