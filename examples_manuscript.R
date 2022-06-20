"
This file contains code to run the examples presented in the manuscript. 

Figure 1: Comparison landmark and registration alignement
Figure 2: Heliconius erato erato and hybrids
Figure 3: Guppies 
Figure 4: Wolf spiders
Figure 5: Salamanders


Guppie images were obtained from:
Kottler, V.A., Fadeev, A., Weigel, D. & Dreyer, C. (2013). 
Pigment pattern formation in the guppy, Poecilia reticulata, involves the Kita and 
Csf1ra receptor tyrosine kinases. Genetics, 194, 631-646.

Wolfspider images were obtained from:
De Busschere, C., Baert, L., Van Belleghem, S.M., Dekoninck, W. & Hendrickx, F. (2012). 
Parallel phenotypic evolution in a wolf spider radiation on Gal?pagos. 

Salamander images were obtained from:
Balogová M & Uhrin M (2015) Sex-biased dorsal spotted patterns in the fire salamander 
(Salamandra salamandra) Salamandra 51(1): 12-18.

Landmarks and outlines for the analysis and plots were obtained in Fiji: 
Schindelin, J., Arganda-carreras, I., Frise, E., Kaynig, V., Longair, M., Pietzsch, T., 
Preibisch, S., Rueden, C., Saalfeld, S., Schmid, B., Tinevez, J., White, D.J., Hartenstein, V., 
Eliceiri, K., Tomancak, P. & Cardona, A. (2012). Fiji: an open-source platform for 
biological-image analysis. Nature Methods, 9, 676-682.

Note that when running k-means clustering of colors, the rasterlayers containing the correct
cluster has to be set manually each time and this may be different compared to the layers defined 
in the current example.
"
###
# load patternize library
###
library(patternize)

###
# make sure these dependencies are installed:
###
#install.packages("rgdal","abind","raster","sp","RniftyReg")
#library(devtools)
#install_github("zarquon42b/Morpho", local=FALSE)

###
# The viridis package provides colour blind friendly colour schemes
###
# install.packages("viridis")
library(viridis)

###############################################
# Analysis for Figure 1 - Comparison landmark and registration alignement
###############################################

###
# patternize landmark/RGB analysis of ten Heliconius erato hydara
###

# List with samples
IDlist <- c('BC0004',
            'BC0049',
            'BC0050',
            'BC0071',
            'BC0077',
            'BC0079',
            'BC0082',
            'BC0125',
            'BC0129',
            'BC0366')

# make list with landmarks
prepath <- 'landmarks/Heliconius'
extension <- '_landmarks_LFW.txt'
landmarkList <- makeList(IDlist, 'landmark', prepath, extension)

# make list with images
prepath <- 'images/Heliconius'
extension <- '-D.jpg'
imageList <- makeList(IDlist, 'image', prepath, extension)

# run alignment of color patterns
RGB <- c(114,17,0) # red
rasterList_lanRGB <- patLanRGB(imageList, landmarkList, RGB, transformRef = 'BC0004', resampleFactor = 3, 
                               colOffset = 0.15, crop = TRUE, res = 200, adjustCoords = TRUE, plot = 'stack')

# If you don't want to run the function, you can load the saved output rasterList
# save(rasterList_lanRGB, file = 'output/Fig1_rasterList_lanRGB.rda')
load('output/Fig1_rasterList_lanRGB.rda')

# sum the colorpatterns
summedRaster_lanRGB <- sumRaster(rasterList_lanRGB, IDlist, type = 'RGB')

# plot heatmap
outline_BC0004 <- read.table('cartoon/BC0004_outline.txt', h= F)
lines_BC0004 <- list.files(path='cartoon', pattern='BC0004_vein', full.names = T)

# colfunc <- c("black","lightblue","blue","green", "yellow","red")
colfunc <- inferno(100)
plotHeat(summedRaster_lanRGB, IDlist, plotCartoon = TRUE, refShape = 'target', outline = outline_BC0004, 
         lines = lines_BC0004, landList = landmarkList, adjustCoords = TRUE, flipRaster = 'y', 
         imageList = imageList, cartoonID = 'BC0004', cartoonFill = 'black', cartoonOrder = 'under', 
         colpalette = colfunc)


###
# patternize registration/RGB analysis of ten Heliconius erato hydara
###

# List with samples
IDlist <- c('BC0004',
            'BC0049',
            'BC0050',
            'BC0071',
            'BC0077',
            'BC0079',
            'BC0082',
            'BC0125',
            'BC0129',
            'BC0366')

# make list with images
prepath <- 'images/Heliconius'
extension <- '-D.jpg'
imageList <- makeList(IDlist, 'image', prepath, extension)

# choose target image
target <- imageList[['BC0004']]

# run alignment of color patterns
RGB <- c(114,17,0) # red
rasterList_regRGB <- patRegRGB(imageList, target, RGB, resampleFactor = 5, colOffset= 0.15, 
                               removebg = 100, plot = 'stack')

# If you don't want to run the function, you can load the saved output rasterList
# save(rasterList_regRGB, file = 'output/Fig1_rasterList_regRGB.rda')
load('output/Fig1_rasterList_regRGB.rda')

# sum the colorpatterns
summedRaster_regRGB <- sumRaster(rasterList_regRGB, IDlist, type = 'RGB')

# plot heatmap
outline_BC0004 <- read.table('cartoon/BC0004_outline.txt', h= F)
lines_BC0004 <- list.files(path='cartoon', pattern='BC0004_vein', full.names = T)

# colfunc <- c("black","lightblue","blue","green", "yellow","red")
colfunc <- inferno(100)
plotHeat(summedRaster_regRGB, IDlist, plotCartoon = TRUE, refShape = 'target', outline = outline_BC0004, 
         lines = lines_BC0004, flipRaster = 'xy', imageList = imageList, cartoonID = 'BC0004', 
         cartoonFill = 'black', cartoonOrder = 'under', colpalette = colfunc)

##
# Compare landmark and registration
##

# give rasters same extent and resolution
rasterEx <- raster::extent(min(outline_BC0004[,1]),
                           max(outline_BC0004[,1]),
                           min(outline_BC0004[,2]),
                           max(outline_BC0004[,2]))

rRe <- raster::raster(nrow=200,ncol=200)
raster::extent(rRe) <- rasterEx

summedRaster_lanRGB2 <- raster::resample(summedRaster_lanRGB,rRe,datatype="INT1U", method='ngb')
summedRaster_regRGB2 <- raster::resample(summedRaster_regRGB,rRe,datatype="INT1U", method='ngb')

# orient rasters
summedRaster_lanRGB2 <- raster::flip(summedRaster_lanRGB2, 'y')
summedRaster_regRGB2 <- raster::flip(summedRaster_regRGB2, 'y')
summedRaster_regRGB2 <- raster::flip(summedRaster_regRGB2, 'x')

# subtract rasters
subtracted <- summedRaster_lanRGB2/length(IDlist) - summedRaster_regRGB2/length(IDlist)

# plot heatmap
colfunc <- c("blue","lightblue","black","pink","red")
plotHeat(subtracted, IDlist, plotCartoon = TRUE, refShape = 'target', outline = outline_BC0004, 
         lines = lines_BC0004, landList = landmarkList, adjustCoords = TRUE, imageList = imageList, 
         normalized = TRUE,cartoonID = 'BC0004', zlim=c(-1,1), colpalette= colfunc, cartoonFill = 'black', 
         cartoonOrder = 'under', legendTitle = 'Difference')


###############################################
# Analysis for Figure 2 - Heliconius erato erato verus hybrid
###############################################

###
# patternize registration/RGB analysis of Heliconius and hybrids
###

# Lists with samples
IDListEra <- c('BC0057','BC0327','BC0148','BC0064','BC0085','BC0088','BC0149','BC0154','BC0163','BC0340')
IDListHyb <- c('BC0213','BC0360','BC0391','BC2407','BC2321','BC2207','BC2208','BC0637','BC2359','BC2550')

# make list with images
prepath <- 'images/Heliconius'
extension <- '-D.jpg'
imageListEra <- makeList(IDListEra, 'image', prepath, extension)
imageListHyb <- makeList(IDListHyb, 'image', prepath, extension)

# choose target image
target <- imageListEra[['BC0057']]

# run alignment of color patterns

RGB <- c(143,138,49) # yellow
rasterList_regRGBEra <- patRegRGB(imageListEra, target, RGB, resampleFactor = 5, colOffset= 0.12, 
                                  crop = c(1000,3800,500,2800), removebgR = 100, plot = 'stack', focal = TRUE, 
                                  sigma = 5, iterations = 3)

RGB <- c(114,17,0) # red
rasterList_regRGBHyb <- patRegRGB(imageListHyb, target, RGB, resampleFactor = 5, colOffset= 0.12, 
                                  crop = c(1000,3800,500,2800), removebgR = 100, plot = 'stack', focal = TRUE, 
                                  sigma = 5, iterations = 3)

# If you don't want to run the function, you can load the saved output rasterList
# save(rasterList_regRGBEra, file = 'output/Fig2_rasterList_regRGBEra.rda')
load('output/Fig2_rasterList_regRGBEra.rda')
# save(rasterList_regRGBHyb, file = 'output/Fig2_rasterList_regRGBHyb.rda')
load('output/Fig2_rasterList_regRGBHyb.rda')

# sum the colorpatterns
summedRaster_regRGBEra <- sumRaster(rasterList_regRGBEra, IDListEra, type = 'RGB')
summedRaster_regRGBHyb <- sumRaster(rasterList_regRGBHyb, IDListHyb, type = 'RGB')

# plot heatmap
outline_BC0057 <- read.table('cartoon/BC0057_outline.txt', h= F)
lines_BC0057 <- list.files(path='cartoon', pattern='BC0057_vein', full.names = T)

# colfunc <- c("black","lightblue","blue","green", "yellow","red")
colfunc <- inferno(100)
plotHeat(summedRaster_regRGBEra, IDListEra, plotCartoon = TRUE, refShape = 'target', outline = outline_BC0057, 
         lines = lines_BC0057, flipRaster = 'xy', 
         imageList = imageListEra, cartoonID = 'BC0057', cartoonFill = 'black', cartoonOrder = 'under', 
         colpalette = colfunc, refImage = target)

plotHeat(summedRaster_regRGBHyb, IDListHyb, plotCartoon = TRUE, refShape = 'target', outline = outline_BC0057, 
         lines = lines_BC0057, flipRaster = 'xy',
         imageList = imageListHyb, cartoonID = 'BC0057', cartoonFill = 'black', cartoonOrder = 'under', 
         colpalette = colfunc, refImage = target)

# subtract rasters
subtracted <- summedRaster_regRGBHyb/length(IDListHyb) - summedRaster_regRGBEra/length(IDListEra)

# plot subtracted heatmap
colfunc <- c("blue","lightblue","black","pink","red")
plotHeat(subtracted, IDListEra, plotCartoon = TRUE, refShape = 'target', outline = outline_BC0057, 
         lines = lines_BC0057, landList = landmarkList, adjustCoords = TRUE, crop = c(1000,3800,500,2800), 
         flipRaster = 'xy', imageList = imageListEra, cartoonID = 'BC0057', zlim=c(-1,1), colpalette= colfunc, 
         normalized = TRUE, cartoonFill = 'black', cartoonOrder = 'under', legendTitle = 'Difference')

###
# Calculate colored areas
###

AreaEra <- patArea(rasterList_regRGBEra, IDListEra, refShape = 'target', type = 'RGB', 
                   outline = outline_BC0057, crop = c(1000,3800,500,2800), adjustCoords = TRUE, 
                   imageList = imageListEra, cartoonID = 'BC0057', flipRaster = 'xy')

AreaHyb <- patArea(rasterList_regRGBHyb, IDListHyb, refShape = 'target', type = 'RGB', 
                   outline = outline_BC0057, crop = c(1000,3800,500,2800), adjustCoords = TRUE, 
                   imageList = imageListHyb, cartoonID = 'BC0057', flipRaster = 'xy')

hist(AreaEra$Area)
hist(AreaHyb$Area)

hist(AreaEra$Area, xlim=c(0.05,0.2), col="yellow", breaks=5)
hist(AreaHyb$Area, add=T, col=rgb(1, 0, 0, 0.5), breaks=10)

dEra <- density(AreaEra$Area)
dHyb <- density(AreaHyb$Area)

plot(dEra, xlim=c(0.05,0.25), ylim=c(0,60), main="")
polygon(dEra, col=rgb(1, 0.84, 0, 0.5), border="gold")

par(new=T)
plot(dHyb, xlim=c(0.05,0.25), ylim=c(0,60), main="")
polygon(dHyb, col=rgb(1, 0, 0, 0.5), border="red")


###
# Plot PCA
###

# Make population and color list
popList <- list(IDListEra, IDListHyb)
colList <- c("gold", "red")
symbolList <- c(16,17)

TotalList <- c(rasterList_regRGBEra, rasterList_regRGBHyb)

pcaOut <- patPCA(TotalList, popList, colList, symbolList = symbolList, plot = TRUE, plotType = 'points', 
                 plotChanges = TRUE, PCx = 1, PCy = 2, plotCartoon = TRUE, refShape = 'target', 
                 outline = outline_BC0057, crop = c(1000,3800,500,2800), flipRaster = 'xy', 
                 imageList = imageListEra, cartoonID = 'BC0057', normalized = TRUE, 
                 cartoonFill = 'black', cartoonOrder = 'under', legendTitle = 'Predicted')


###############################################
# Analysis for Figure 3 - Guppies
###############################################

###
# patternize registration/K analysis
###

# Lists with samples
IDListWT <- c('cross20_F1fBC1_wtm_9472',
              'cross20_F1fBC1_wtm_9471',
              'cross20_F1fBC1_wtm_9470',
              'cross20_F1fBC1_wtm_9469',
              'cross20_F1fBC1_wtm_9131',
              'cross20_F1fBC1_wtm_9130',
              'cross20_F1fBC1_wtm_0319',
              'cross20_F1fBC1_wtm_0318',
              'cross20_F1fBC1_wtm_0317',
              'cross20_F1fBC1_wtm_0316')

IDListG <- c('cross20_F1fBC1_whitem_1278',
             'cross20_F1fBC5_whitem_1284',
             'cross20_F1fBC2_whitem_7819',
             'cross20_F1fBC2_whitem_7818',
             'cross20_F1fBC2_whitem_7816',
             'cross20_F1fBC2_whitem_7815',
             'cross20_F1fBC3_whitem_1277',
             'cross20_F1fBC1_whitem_1292',
             'cross20_F1fBC1_whitem_1291',
             'cross20_F1fBC1_whitem_1279')

# make lists with images
prepath <- 'images/Guppies'
extension <- '.jpg'
imageListWT <- makeList(IDListWT, 'image', prepath, extension)
imageListG <- makeList(IDListG, 'image', prepath, extension)

# choose target image
target <- imageListWT[['cross20_F1fBC1_wtm_9472']]

# read in the outline
outline_9472 <- read.table('cartoon/cross20_F1fBC1_wtm_9472_outline.txt', h= F)

# run alignment of color patterns
rasterList_regKWT <- patRegK(imageListWT, target, k = 7, resampleFactor = 5, crop = c(200,2800,300,1800), 
                             plot = TRUE, useBlockPercentage = 90, maskOutline = outline_9472, maskColor = 255)

rasterList_regKG <- patRegK(imageListG, target, k = 7, resampleFactor = 5, crop = c(200,2800,300,1800), 
                            plot = TRUE, useBlockPercentage = 90, maskOutline = outline_9472, maskColor = 255)

# If you don't want to run the function, you can load the saved output rasterList
# save(rasterList_regKWT, file = 'output/Fig3_rasterList_regKWT.rda')
load('output/Fig3_rasterList_regKWT.rda')
# save(rasterList_regKG, file = 'output/Fig3_rasterList_regKG.rda')
load('output/Fig3_rasterList_regKG.rda')

# sum the colorpatterns
summedRaster_KWT <- sumRaster(rasterList_regKWT, IDListWT, type = 'k')
summedRaster_KG <- sumRaster(rasterList_regKG, IDListG, type = 'k')

# plot heatmaps using the list of summed rasters
plotHeat(summedRaster_KWT, IDListWT, plotCartoon = TRUE, refShape = 'target', outline = outline_9472, 
         crop = c(200,2800,300,1800), flipRaster = 'y', imageList = imageListWT, 
         cartoonOrder = 'under', cartoonFill = 'black')

plotHeat(summedRaster_KG, IDListWT, plotCartoon = TRUE, refShape = 'target', outline = outline_9472, 
         crop = c(200,2800,300,1800), flipRaster = 'y', imageList = imageListWT, 
         cartoonOrder = 'under', cartoonFill = 'black')

# plot selected heatmap (note that you will have to pick the correct color cluster manually (number between [[...]]))
# colfunc <- c("gray","lightblue","blue","green", "yellow","red")
colfunc <- inferno(100)
plotHeat(summedRaster_KWT[[4]], IDListWT, plotCartoon = TRUE, refShape = 'target', outline = outline_9472, 
         crop = c(300,2800,300,1800), flipRaster = 'y', imageList = imageListWT, cartoonOrder = 'under', 
         cartoonFill = 'black', colpalette = colfunc)

plotHeat(summedRaster_KG[[2]], IDListWT, plotCartoon = TRUE, refShape = 'target', outline = outline_9472, 
         crop = c(300,2800,300,1800), flipRaster = 'y', imageList = imageListWT, cartoonOrder = 'under',
         cartoonFill = 'black', colpalette = colfunc)

# subtract rasters
subtracted <- summedRaster_KWT[[4]] - summedRaster_KG[[2]]

# plot heatmap
colfunc <- c("blue","lightblue","black","pink","red")
plotHeat(subtracted, IDListWT, plotCartoon = TRUE, refShape = 'target', outline = outline_9472, 
         crop = c(200,2800,300,1800),landList = landmarkList, adjustCoords = TRUE, imageList = imageListWT, 
         flipRaster = 'y', cartoonID = 'cross20_F1fBC1_wtm_9472', zlim=c(-1,1), colpalette= colfunc, 
         cartoonFill = 'black', cartoonOrder = 'under', legendTitle = 'Difference')


###
# Plot PCA
###

# Make population and color list
popList <- list(IDListWT, IDListG)
colList <- c("black", "gold")
symbolList <- c(16,17)

# extract rasters for k-means cluster of interest and combine rasters 
#(note that you will have to pick the correct color cluster manually (number between [[...]]))
rasterList_regKWT_S <- lapply(rasterList_regKWT, function (x) x[[4]])
rasterList_regKG_S <- lapply(rasterList_regKG, function (x) x[[2]])

TotalList <- c(rasterList_regKWT_S, rasterList_regKG_S)

# Run and plot PCA
colfunc <- c("blue","lightblue","black","pink","red")
pcaOut <- patPCA(TotalList, popList, colList, symbolList = symbolList, plot = TRUE, plotType = 'points', plotChanges = TRUE, PCx = 1, PCy = 2, 
                 plotCartoon = TRUE, refShape = 'target', outline = outline_9472, colpalette = colfunc, 
                 crop = c(300,2800,300,1800),flipRaster = 'y', imageList = imageListWT, cartoonID = 'cross20_F1fBC1_wtm_9472', 
                 normalized = TRUE, cartoonFill = 'black', cartoonOrder = 'under', legendTitle = 'Predicted')



###############################################
# Analysis for Figure 4 - Wolf spiders from the Galapagos
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

# choose target image
target <- imageListGala[['IMG_2600']]

# run alignment of color patterns using k-means
rasterList_regGalaK <- patRegK(imageListGala, target, k = 3, resampleFactor = 10, plot = T, removebgR = 130, removebgK = 150)
rasterList_regHendK <- patRegK(imageListHend, target, k = 3, resampleFactor = 10, plot = T, removebgR = 130, removebgK = 180)
rasterList_regJuncK <- patRegK(imageListJunc, target, k = 3, resampleFactor = 10, plot = T, removebgR = 130, removebgK = 130)
rasterList_regSnodK <- patRegK(imageListSnod, target, k = 3, resampleFactor = 10, plot = T, removebgR = 130, removebgK = 200)

# If you don't want to run the function, you can load the saved output rasterList
# save(rasterList_regGalaK, file = 'output/Fig4_rasterList_regGalaK.rda')
load('output/Fig4_rasterList_regGalaK.rda')
# save(rasterList_regHendK, file = 'output/Fig4_rasterList_regHendK.rda')
load('output/Fig4_rasterList_regHendK.rda')
# save(rasterList_regJuncK, file = 'output/Fig4_rasterList_regJuncK.rda')
load('output/Fig4_rasterList_regJuncK.rda')
# save(rasterList_regSnodK, file = 'output/Fig4_rasterList_regSnodK.rda')
load('output/Fig4_rasterList_regSnodK.rda')

# sum rasters
summedRaster_GalaK <- sumRaster(rasterList_regGalaK, IDListGala, type = 'k')
summedRaster_HendK <- sumRaster(rasterList_regHendK, IDListHend, type = 'k')
summedRaster_JuncK <- sumRaster(rasterList_regJuncK, IDListJunc, type = 'k')
summedRaster_SnodK <- sumRaster(rasterList_regSnodK, IDListSnod, type = 'k')

# plot all k
colfunc <- inferno(100)
plotHeat(summedRaster_GalaK, IDListGala, colpalette = colfunc)
plotHeat(summedRaster_HendK, IDListHend, colpalette = colfunc)
plotHeat(summedRaster_JuncK, IDListJunc, colpalette = colfunc)
plotHeat(summedRaster_SnodK, IDListSnod, colpalette = colfunc)

# plot selected k
plotHeat(summedRaster_GalaK[[1]], IDListGala, colpalette = colfunc)
plotHeat(summedRaster_HendK[[2]], IDListHend, colpalette = colfunc)
plotHeat(summedRaster_JuncK[[2]], IDListJunc, colpalette = colfunc)
plotHeat(summedRaster_SnodK[[1]], IDListSnod, colpalette = colfunc)

# mask outline
outline_IMG_2600 <- read.table('cartoon/IMG_2600_outline.txt', h= F)

summedRaster_GalaKM <- maskOutline(summedRaster_GalaK[[1]], outline_IMG_2600, refShape = 'target', flipOutline = 'y', imageList = imageListGala)
summedRaster_HendKM <- maskOutline(summedRaster_HendK[[2]], outline_IMG_2600, refShape = 'target', flipOutline = 'y', imageList = imageListHend)
summedRaster_JuncKM <- maskOutline(summedRaster_JuncK[[2]], outline_IMG_2600, refShape = 'target', flipOutline = 'y', imageList = imageListJunc)
summedRaster_SnodKM <- maskOutline(summedRaster_SnodK[[1]], outline_IMG_2600, refShape = 'target', flipOutline = 'y', imageList = imageListSnod)

# plot selected k with masked outline
colfunc <- inferno(100)
plotHeat(summedRaster_GalaKM, IDListGala, plotCartoon = TRUE, refShape = 'target', outline = outline_IMG_2600, 
         flipOutline = 'y', imageList = imageListGala, cartoonOrder = 'under', colpalette = colfunc, cartoonFill = 'black')
plotHeat(summedRaster_HendKM, IDListHend, plotCartoon = TRUE, refShape = 'target', outline = outline_IMG_2600, 
         flipOutline = 'y', imageList = imageListHend, cartoonOrder = 'under', colpalette = colfunc, cartoonFill = 'black')
plotHeat(summedRaster_JuncKM, IDListJunc, plotCartoon = TRUE, refShape = 'target', outline = outline_IMG_2600, 
         flipOutline = 'y', imageList = imageListJunc, cartoonOrder = 'under', colpalette = colfunc, cartoonFill = 'black')
plotHeat(summedRaster_SnodKM, IDListSnod, plotCartoon = TRUE, refShape = 'target', outline = outline_IMG_2600, 
         flipOutline = 'y', imageList = imageListSnod, cartoonOrder = 'under', colpalette = colfunc, cartoonFill = 'black')

# # code for high quality image
# rasterList_regHendK_HQ <- patRegK(imageListHend, target, k = 3, resampleFactor = 2, plot = T, removebgR = 130, removebgK = 180)
# 
# # save(rasterList_regHendK_HQ, file = 'output/Fig4_rasterList_regHendK_HQ.rda')
# load('output/Fig4_rasterList_regHendK_HQ.rda')
# 
# summedRaster_HendK_HQ <- sumRaster(rasterList_regHendK_HQ, IDListHend, type = 'k')
# plotHeat(summedRaster_HendK_HQ[[1]], IDListHend, colpalette = colfunc)

# # some funky 3D plot if you like
# install.packages("rasterVis")
# library(rasterVis)
# plot3D(summedRaster_HendK_HQ[[1]], col = colorRampPalette(c("white","lightblue","blue","green", "yellow","red")))

###
# Plot PCA
###

# Make population and color list
popList <- list(IDListGala, IDListHend, IDListJunc, IDListSnod)
colList <- c("green", "red", "darkgreen","orange")
symbolList <- c(15,16,17,18)

# extract rasters for k-means cluster of interest and combine rasters
rasterList_regGalaK_S <- lapply(rasterList_regGalaK, function (x) x[[1]])
rasterList_regHendK_S <- lapply(rasterList_regHendK, function (x) x[[2]])
rasterList_regJuncK_S <- lapply(rasterList_regJuncK, function (x) x[[2]])
rasterList_regSnodK_S <- lapply(rasterList_regSnodK, function (x) x[[1]])

# mask the selected rasters with outline
rasterList_regGalaK_SM <-list()
for(e in 1:length(rasterList_regGalaK_S)){
  rasterList_regGalaK_SM[[e]] <- maskOutline(rasterList_regGalaK_S[[e]], outline_IMG_2600, refShape = 'target', 
                                             flipOutline = 'y', imageList = imageListGala)
}
rasterList_regHendK_SM <-list()
for(e in 1:length(rasterList_regGalaK_S)){
  rasterList_regHendK_SM[[e]] <- maskOutline(rasterList_regHendK_S[[e]], outline_IMG_2600, refShape = 'target', 
                                             flipOutline = 'y', imageList = imageListGala)
}
rasterList_regJuncK_SM <-list()
for(e in 1:length(rasterList_regGalaK_S)){
  rasterList_regJuncK_SM[[e]] <- maskOutline(rasterList_regJuncK_S[[e]], outline_IMG_2600, refShape = 'target', 
                                             flipOutline = 'y', imageList = imageListGala)
}
rasterList_regSnodK_SM <-list()
for(e in 1:length(rasterList_regGalaK_S)){
  rasterList_regSnodK_SM[[e]] <- maskOutline(rasterList_regSnodK_S[[e]], outline_IMG_2600, refShape = 'target', 
                                             flipOutline = 'y', imageList = imageListGala)
}


TotalList <- c(rasterList_regGalaK_SM, rasterList_regHendK_SM, rasterList_regJuncK_SM, rasterList_regSnodK_SM)#, rasterList_regEspaK_SM)

# Run and plot PCA
pcaOut <- patPCA(TotalList, popList, colList, symbolList = symbolList, plot = TRUE, plotType = 'points', plotChanges = TRUE, 
                 PCx = 1, PCy = 2, plotCartoon = TRUE, refShape = 'target', outline = outline_IMG_2600, flipOutline = 'y', 
                 imageList = imageListGala, cartoonID = 'IMG_2600', normalized = TRUE, cartoonFill = 'black', 
                 cartoonOrder = 'under', legendTitle = 'Predicted')


###############################################
# Analysis for Figure 5 - Salamanders
###############################################

###
# patternize landmarks/watershed analysis
###

# Lists with samples
IDListSal <- c('4031','3864','3875','3959','4118','4706','4758','5574','5580','5596')

# make list with landmarks
prepath <- 'landmarks/Salamanders'
extension <- '_landmarks.txt'
landmarkListSal <- makeList(IDListSal, 'landmark', prepath, extension)

# make lists with images
prepath <- 'images/Salamanders'
extension <- '.JPG'
imageListSal <- makeList(IDListSal, 'image', prepath, extension)

# make square reference to transform images to
REF = matrix(c(0,0,100,0,100,100,0,100), nrow=4, ncol=2, byrow = TRUE)

# extract the patterns using watershed
rasterList_SalW <- patLanW(imageListSal, landmarkListSal, transformRef = REF, resampleFactor = 5, plotTransformed = TRUE, 
                        correct = TRUE, plotCorrect = FALSE, blur = FALSE, sigma = 2, bucketfill = FALSE, cleanP = 3, adjustCoords = TRUE,
                        splitC = 10, plotPriority = TRUE, plotWS = TRUE, plotBF = TRUE, plotFinal = TRUE, maskOutline = REF)

# save(rasterList_SalW, file = 'output/Fig4_rasterList_SalW.rda')
load('output/Fig4_rasterList_SalW.rda')

# sum the colorpatterns
summedRaster_SalW <- sumRaster(rasterList_SalW, IDListSal, type = 'RGB')

# plot heatmap
outline_4031 <- read.table('cartoon/4031_outline.txt', h= F)

colfunc <- inferno(100)
plotHeat(summedRaster_SalW, IDListSal, plotCartoon = TRUE, refShape = REF, outline = outline_4031, landList = landmarkListSal, 
         adjustCoords = TRUE, flipOutline = 'y', imageList = imageListSal, cartoonID = '4031', cartoonFill = 'black', 
         cartoonOrder = 'under', colpalette = colfunc)

rect(0,0,100,100, border= 'red')

