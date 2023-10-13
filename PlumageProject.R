{library(ggbiplot)
library(ggplot2)
library(lmerTest)
library(lme4)
library(lubridate)
library(stringr)
library(reshape2)
library(readxl)
library(maditr)
library(Rmisc)
library(dplyr)
library(tidyr)
library(data.table)
library(pca3d)
library(rgl)}

# Map Packages
{library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggmap)
library(maps)
library(Hmisc)
library(ggsn)}

rm(list = ls())  #clear variables/environment
if(!is.null(dev.list())) dev.off() #clear plots
setwd("/Users/katherinefeldmann/Desktop/CU_Boulder/Taylor_Lab/Honors Thesis/Revised_Resubmit/")

##### CAMERA DATA #####
"
EXPLANATORY (categorical; 2 levels each): Species, Sex
COVARIATE (numeric): Julian Date
RANDOM (categorical): Subspecies, Year.Collected, Location.Collected, Museum
"

## Process Cluster Results ##
{cluster.results <- read.csv("Cluster_Results.csv")
#cluster.results <- cluster.results[,c("Image","ClusterID","lw_mean","lw_sd","mw_mean","mw_sd","sw_mean","sw_sd","uv_mean","uv_sd","dbl_mean","dbl_sd","Area")] # If including AREA
cluster.results <- cluster.results[,c("Image","ClusterID","lw_mean","lw_sd","mw_mean","mw_sd","sw_mean","sw_sd","uv_mean","uv_sd","dbl_mean","dbl_sd")]
split.cluster <- split(cluster.results, cluster.results$Image) # create a list of lists where each individual list contains data for one individual-patch
split.cluster <- lapply(split.cluster, function(x) x[order(x$dbl_mean),]) # order each sub-list by dbl_mean (luminance)
dark.color <- lapply(split.cluster, function(x) lapply(x[,c("lw_mean","lw_sd","mw_mean","mw_sd","sw_mean","sw_sd","uv_mean","uv_sd","dbl_mean","dbl_sd")], function(y) mean(head(y, n = length(y)/2)))) # calculate the mean colors for the 50% smallest dbl_mean value
light.color <- lapply(split.cluster, function(x) lapply(x[,c("lw_mean","lw_sd","mw_mean","mw_sd","sw_mean","sw_sd","uv_mean","uv_sd","dbl_mean","dbl_sd")], function(y) mean(tail(y, n = length(y)/2)))) # calculate the mean for the 50% largest dbl_mean value
#light.area <- lapply(split.cluster, function(x) sum(tail(x[,"Area"], n = length(x[,"Area"])/2))/sum(x[,"Area"])) # If including AREA - calculate the mean area for the 50% smallest dbl_mean value
dark.color <- as.data.frame(do.call(rbind, lapply(dark.color, as.vector)))
dark.color <- cbind(Image = rownames(dark.color), dark.color)
colnames(dark.color) <- c("Image", paste(colnames(dark.color)[-1], "dark", sep = "."))
light.color <- as.data.frame(do.call(rbind, lapply(light.color, as.vector)))
#light.area <- as.data.frame(do.call(rbind, lapply(light.area, as.vector))) # If including AREA
light.color <- cbind(Image = rownames(light.color), light.color)
#light.area <- cbind(Image = rownames(light.area), light.area) # If including AREA
colnames(light.color) <- c("Image", paste(colnames(light.color)[-1], "light", sep = "."))
#colnames(light.area) <- c("Image","Area.light") # If including AREA
#light.half <- merge(light.color, light.area, by = "Image") # If including AREA
#combined.data <- merge(dark.color, light.half, by = "Image") # If including AREA
combined.data <- merge(dark.color, light.color, by = "Image")
combined.data$Individual_Side <- paste(str_match(combined.data$Image, "[BM][CO]CH[0-9]+"), str_match(combined.data$Image, "[B]*[aeS][cli][kld][ye]*"), sep = ".")
combined.data$Image <- NULL
combined.data <- cbind(data.frame(lapply(combined.data[,c(1:ncol(combined.data)-1)], function(x) unlist(x))),combined.data$Individual_Side)
colnames(combined.data)[colnames(combined.data) == "combined.data$Individual_Side"] <- "Individual_Side"
combined.data <- aggregate(.~Individual_Side, data = combined.data, mean)

## Process Summary Results ##
summary.results <- read.csv("Summary_Results.csv")
#summary.results <- summary.results[,c("Image","CAA.PT","CAA.Asp","VCA.ML","VCA.sL","VCA.CVL","VCA.MSsat","VCA.sSsat","VCA.CVSsat","BSA.BML","BSA.BsL","BSA.BCVL","BSA.BMSsat","BSA.BsSsat","BSA.BCVSsat")] # If including AREA
summary.results <- summary.results[,c("Image","VCA.ML","VCA.sL","VCA.CVL","VCA.MSsat","VCA.sSsat","VCA.CVSsat","BSA.BML","BSA.BsL","BSA.BCVL","BSA.BMSsat","BSA.BsSsat","BSA.BCVSsat")]
summary.results$Individual_Side <- paste(str_match(summary.results$Image, "[BM][CO]CH[0-9]+"), str_match(summary.results$Image, "[B]*[aeS][cli][kld][ye]*"), sep = ".")
summary.results$Image <- NULL
summary.results <- aggregate(.~Individual_Side, data = summary.results, mean)
combined.data <- merge(combined.data, summary.results, by = "Individual_Side")

## Add Location and Individual Data ##
locations <- read.csv("Locations.csv")
camera.individuals <- read.csv("Camera_Individuals.csv")

location.data <- locations[,c("Specimen_ID","Quadrant","Elevation","Latitude","Longitude")]
camera.individuals <- merge(camera.individuals, location.data, by = "Specimen_ID")
camera.individuals$Specimen_ID <- str_remove(camera.individuals$Specimen_ID, "[CD][UM][N]*[S]*_")
camera.individuals$SpeciesSex <- paste(camera.individuals$Species, camera.individuals$Sex, sep = ".")
camera.individuals$Year.Collected <- as.factor(str_match(camera.individuals$Date.Collected, "[0-9][0-9][0-9][0-9]"))  #extract Year and save to separate column
camera.individuals$Julian.Date <- yday(as.Date(camera.individuals$Date.Collected, "%d-%b-%Y"))  #convert date (day-month-year) to Julian dates
camera.individuals$Quadrant <- as.factor(camera.individuals$Quadrant) #convert Quadrant to factor to make it a random variable
combined.data <- separate(combined.data, col = "Individual_Side", into = c("Specimen_ID","Side"), sep = "[.]")
combined.data <- dcast(setDT(combined.data), Specimen_ID ~ Side, value.var = colnames(combined.data)[3:ncol(combined.data)])
combined.data <- merge(camera.individuals, combined.data, by = "Specimen_ID")

## Add Area Data
camera.area <- read.csv("/Users/katherinefeldmann/Desktop/CU_Boulder/Taylor_Lab/Honors Thesis/R Code/Camera_Data_Area.csv")
temp.area <- aggregate(camera.area[,c("Specimen_ID","Patch","adjusted_area")], by = list(camera.area$Specimen_ID, camera.area$Patch), FUN = mean)
temp.area$Specimen_ID <- NULL
temp.area$Patch <- NULL
temp.area <- separate(data = temp.area, col = "Group.1", into = c("Museum","Specimen_ID"), sep = "_")
temp.area <- dcast(temp.area, Specimen_ID + Museum ~ Group.2, value.var = "adjusted_area")
colnames(temp.area) <- c("Specimen_ID","Museum","adjusted_area.cheek","adjusted_area.head","adjusted_area.throat")
combined.data <- merge(combined.data, temp.area, by = c("Specimen_ID","Museum"))}

## Run PCA ##
species.camera.data <- combined.data[,c(19:(ncol(combined.data)-3))] # SPECIES MODEL
BCCH.camera.data <- combined.data[which(combined.data$Species == "BCCH"),c(19:(ncol(combined.data)-3))] # BCCH MODEL
MOCH.camera.data <- combined.data[which(combined.data$Species == "MOCH"),c(19:(ncol(combined.data)-3))] # MOCH MODEL

# Change between species.camera.data, BCCH.camera.data and MOCH.camera.data
prcomp.camera <- prcomp(species.camera.data, scale = T, center = T)
plot(prcomp.camera)
prcomp.camera$rotation
summary(prcomp.camera)
PC2 <- prcomp.camera$rotation[order(prcomp.camera$rotation[,"PC2"]),"PC2"] # Order PC1 values to determine what variables are driving the difference in either species: large values (BCCH) and small values (MOCH)

# Prepare for publication
'{row.names(prcomp.camera$rotation)[which(str_match(row.names(prcomp.camera$rotation),"lw_mean.dark") == "lw_mean.dark")] <- "LW.Dark.mean"
row.names(prcomp.camera$rotation)[which(str_match(row.names(prcomp.camera$rotation),"lw_sd.dark") == "lw_sd.dark")] <- "LW.Dark.sd"
row.names(prcomp.camera$rotation)[which(str_match(row.names(prcomp.camera$rotation),"lw_mean.light") == "lw_mean.light")] <- "LW.Bright.mean"
row.names(prcomp.camera$rotation)[which(str_match(row.names(prcomp.camera$rotation),"lw_sd.light") == "lw_sd.light")] <- "LW.Bright.sd"
row.names(prcomp.camera$rotation)[which(str_match(row.names(prcomp.camera$rotation),"mw_mean.dark") == "mw_mean.dark")] <- "MW.Dark.mean"
row.names(prcomp.camera$rotation)[which(str_match(row.names(prcomp.camera$rotation),"mw_sd.dark") == "mw_sd.dark")] <- "MW.Dark.sd"
row.names(prcomp.camera$rotation)[which(str_match(row.names(prcomp.camera$rotation),"mw_mean.light") == "mw_mean.light")] <- "MW.Bright.mean"
row.names(prcomp.camera$rotation)[which(str_match(row.names(prcomp.camera$rotation),"mw_sd.light") == "mw_sd.light")] <- "MW.Bright.sd"
row.names(prcomp.camera$rotation)[which(str_match(row.names(prcomp.camera$rotation),"sw_mean.dark") == "sw_mean.dark")] <- "SW.Dark.mean"
row.names(prcomp.camera$rotation)[which(str_match(row.names(prcomp.camera$rotation),"sw_sd.dark") == "sw_sd.dark")] <- "SW.Dark.sd"
row.names(prcomp.camera$rotation)[which(str_match(row.names(prcomp.camera$rotation),"sw_mean.light") == "sw_mean.light")] <- "SW.Bright.mean"
row.names(prcomp.camera$rotation)[which(str_match(row.names(prcomp.camera$rotation),"sw_sd.light") == "sw_sd.light")] <- "SW.Bright.sd"
row.names(prcomp.camera$rotation)[which(str_match(row.names(prcomp.camera$rotation),"uv_mean.dark") == "uv_mean.dark")] <- "UV.Dark.mean"
row.names(prcomp.camera$rotation)[which(str_match(row.names(prcomp.camera$rotation),"uv_sd.dark") == "uv_sd.dark")] <- "UV.Dark.sd"
row.names(prcomp.camera$rotation)[which(str_match(row.names(prcomp.camera$rotation),"uv_mean.light") == "uv_mean.light")] <- "UV.Bright.mean"
row.names(prcomp.camera$rotation)[which(str_match(row.names(prcomp.camera$rotation),"uv_sd.light") == "uv_sd.light")] <- "UV.Bright.sd"
row.names(prcomp.camera$rotation)[which(str_match(row.names(prcomp.camera$rotation),"dbl_mean.dark") == "dbl_mean.dark")] <- "DBL.Dark.mean"
row.names(prcomp.camera$rotation)[which(str_match(row.names(prcomp.camera$rotation),"dbl_sd.dark") == "dbl_sd.dark")] <- "DBL.Dark.sd"
row.names(prcomp.camera$rotation)[which(str_match(row.names(prcomp.camera$rotation),"dbl_mean.light") == "dbl_mean.light")] <- "DBL.Bright.mean"
row.names(prcomp.camera$rotation)[which(str_match(row.names(prcomp.camera$rotation),"dbl_sd.light") == "dbl_sd.light")] <- "DBL.Bright.sd"
row.names(prcomp.camera$rotation)[which(str_match(row.names(prcomp.camera$rotation),"VCA.ML") == "VCA.ML")] <- "Luminance.mean"
row.names(prcomp.camera$rotation)[which(str_match(row.names(prcomp.camera$rotation),"VCA.sL") == "VCA.sL")] <- "Luminance.sd"
row.names(prcomp.camera$rotation)[which(str_match(row.names(prcomp.camera$rotation),"VCA.CVL") == "VCA.CVL")] <- "Luminance.cov"
row.names(prcomp.camera$rotation)[which(str_match(row.names(prcomp.camera$rotation),"VCA.MSsat") == "VCA.MSsat")] <- "RNL Saturation.mean"
row.names(prcomp.camera$rotation)[which(str_match(row.names(prcomp.camera$rotation),"VCA.sSsat") == "VCA.sSsat")] <- "RNL Saturation.sd"
row.names(prcomp.camera$rotation)[which(str_match(row.names(prcomp.camera$rotation),"VCA.CVSsat") == "VCA.CVSsat")] <- "RNL Saturation.cov"
row.names(prcomp.camera$rotation)[which(str_match(row.names(prcomp.camera$rotation),"BSA.BML") == "BSA.BML")] <- "Luminance.BS.mean"
row.names(prcomp.camera$rotation)[which(str_match(row.names(prcomp.camera$rotation),"BSA.BsL") == "BSA.BsL")] <- "Luminance.BS.sd"
row.names(prcomp.camera$rotation)[which(str_match(row.names(prcomp.camera$rotation),"BSA.BCVL") == "BSA.BCVL")] <- "Luminance.BS.cov"
row.names(prcomp.camera$rotation)[which(str_match(row.names(prcomp.camera$rotation),"BSA.BMSsat") == "BSA.BMSsat")] <- "RNL Saturation.BS.mean"
row.names(prcomp.camera$rotation)[which(str_match(row.names(prcomp.camera$rotation),"BSA.BsSsat") == "BSA.BsSsat")] <- "RNL Saturation.BS.sd"
row.names(prcomp.camera$rotation)[which(str_match(row.names(prcomp.camera$rotation),"BSA.BCVSsat") == "BSA.BCVSsat")] <- "RNL Saturation.BS.cov"}'

rowNames <- c("LW.Dark.mean.Dorsal","LW.Dark.mean.Ventral","LW.Dark.mean.Lateral","LW.Dark.sd.Dorsal","LW.Dark.sd.Ventral","LW.Dark.sd.Lateral","MW.Dark.mean.Dorsal","MW.Dark.mean.Ventral","MW.Dark.mean.Lateral","MW.Dark.sd.Dorsal","MW.Dark.sd.Ventral","MW.Dark.sd.Lateral","SW.Dark.mean.Dorsal","SW.Dark.mean.Ventral","SW.Dark.mean.Lateral","SW.Dark.sd.Dorsal","SW.Dark.sd.Ventral","SW.Dark.sd.Lateral","UV.Dark.mean.Dorsal","UV.Dark.mean.Ventral","UV.Dark.mean.Lateral","UV.Dark.sd.Dorsal","UV.Dark.sd.Ventral","UV.Dark.sd.Lateral","DBL.Dark.mean.Dorsal","DBL.Dark.mean.Ventral","DBL.Dark.mean.Lateral","DBL.Dark.sd.Dorsal","DBL.Dark.sd.Ventral","DBL.Dark.sd.Lateral","LW.Bright.mean.Dorsal","LW.Bright.mean.Ventral","LW.Bright.mean.Lateral","LW.Bright.sd.Dorsal","LW.Bright.sd.Ventral","LW.Bright.sd.Lateral","MW.Bright.mean.Dorsal","MW.Bright.mean.Ventral","MW.Bright.mean.Lateral","MW.Bright.sd.Dorsal","MW.Bright.sd.Ventral","MW.Bright.sd.Lateral","SW.Bright.mean.Dorsal","SW.Bright.mean.Ventral","SW.Bright.mean.Lateral","SW.Bright.sd.Dorsal","SW.Bright.sd.Ventral","SW.Bright.sd.Lateral","UV.Bright.mean.Dorsal","UV.Bright.mean.Ventral","UV.Bright.mean.Lateral","UV.Bright.sd.Dorsal","UV.Bright.sd.Ventral","UV.Bright.sd.Lateral","DBL.Bright.mean.Dorsal","DBL.Bright.mean.Ventral","DBL.Bright.mean.Lateral","DBL.Bright.sd.Dorsal","DBL.Bright.sd.Ventral","DBL.Bright.sd.Lateral","Luminance.mean.Dorsal","Luminance.mean.Ventral","Luminance.mean.Lateral","Luminance.sd.Dorsal","Luminance.sd.Ventral","Luminance.sd.Lateral","Luminance.cov.Dorsal","Luminance.cov.Ventral","Luminance.cov.Lateral","RNL.Saturation.mean.Dorsal","RNL.Saturation.mean.Ventral","RNL.Saturation.mean.Lateral","RNL.Saturation.sd.Dorsal","RNL.Saturation.sd.Ventral","RNL.Saturation.sd.Lateral","RNL.Saturation.cov.Dorsal","RNL.Saturation.cov.Ventral","RNL.Saturation.cov.Lateral","Luminance.BS.mean.Dorsal","Luminance.BS.mean.Ventral","Luminance.BS.mean.Lateral","Luminance.BS.sd.Dorsal","Luminance.BS.sd.Ventral","Luminance.BS.sd.Lateral","Luminance.BS.cov.Dorsal","Luminance.BS.cov.Ventral","Luminance.BS.cov.Lateral","RNL.Saturation.BS.mean.Dorsal","RNL.Saturation.BS.mean.Ventral","RNL.Saturation.BS.mean.Lateral","RNL.Saturation.BS.sd.Dorsal","RNL.Saturation.BS.sd.Ventral","RNL.Saturation.BS.sd.Lateral","RNL.Saturation.BS.cov.Dorsal","RNL.Saturation.BS.cov.Ventral","RNL.Saturation.BS.cov.Lateral")
rownames(prcomp.camera$rotation) <- rowNames

pca.camera <- combined.data[,c(1:18)]
pca.camera <- combined.data[which(combined.data$Species == "BCCH"),c(1:18)]
pca.camera <- combined.data[which(combined.data$Species == "MOCH"),c(1:18)]

pca.camera$PCA1 <- prcomp.camera$x[,1]
pca.camera$PCA2 <- prcomp.camera$x[,2]
pca.camera$PCA3 <- prcomp.camera$x[,3]

## Plot PCA and Create Data Frame for Modeling ##
biplot <- ggbiplot(prcomp.camera, ellipse = T, groups = pca.camera$Species, choices = c(1,2), var.axes = T) +
  geom_point(aes(color = pca.camera$Species), size = 4) +
  scale_color_manual(values = c("coral1","navy")) +
  theme_bw() +
  theme(legend.title = element_blank(), axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), plot.margin = margin(t = 4, b = 0, r = 0, l = 0)) +
  lims(x = c(-3,5), y = c(-3,3))
biplot$layers <- c(biplot$layers[[3]],biplot$layers[[5]],biplot$layers[[1]],biplot$layers[[4]])
biplot$layers[[3]]$aes_params$colour <- c(rep("darkorange4",30),rep("darkorange2",30),rep("dodgerblue2",18),rep("dodgerblue4",18))
biplot$layers[[4]]$aes_params$colour <- c(rep("darkorange4",30),rep("darkorange2",30),rep("dodgerblue2",18),rep("dodgerblue4",18))
#biplot$layers[[3]]$aes_params$alpha <- c(rep(0,30),rep(0,30),rep(0,18),rep(0,18))
biplot$layers[[4]]$aes_params$alpha <- c(rep(0,30),rep(0,30),rep(0,18),rep(0,18))
biplot$layers[[4]]$aes_params$size <- 4

# Subset biplot
options(ggrepel.max.overlaps = Inf)
biplot <- autoplot(prcomp.camera, groups = pca.camera$Species, loadings = T, loadings.label = F, loadings.label.repel = T) +
  geom_point(aes(color = pca.camera$Species), size = 4) +
  scale_color_manual(values = c("coral1","navy")) +
  theme_bw() +
  theme(legend.title = element_blank(), axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), plot.margin = margin(t = 4, b = 0, r = 0, l = 0))
biplot$layers <- c(biplot$layers[[1]],biplot$layers[[4]],biplot$layers[[2]],biplot$layers[[3]])
biplot$layers[[3]]$aes_params$colour <- c(rep("darkorange4",30),rep("darkorange2",30),rep("dodgerblue2",18),rep("dodgerblue4",18))
biplot$layers[[3]]$aes_params$alpha <- rep(0.5,96)
biplot$layers[[4]]$aes_params$colour <- c(rep("darkorange4",30),rep("darkorange2",30),rep("dodgerblue2",18),rep("dodgerblue4",18))
g <- ggplot_build(biplot)
#g$data[[3]] <- g$data[[3]][0:30,]
#g$data[[4]] <- g$data[[4]][0:30,]
#g$data[[3]] <- g$data[[3]][31:60,]
#g$data[[4]] <- g$data[[4]][31:60,]
#g$data[[3]] <- g$data[[3]][61:78,]
#g$data[[4]] <- g$data[[4]][61:78,]
g$data[[3]] <- g$data[[3]][79:96,]
g$data[[4]] <- g$data[[4]][79:96,]
plot(ggplot_gtable(g))

pca.camera$Color <- NA
pca.camera[which(pca.camera$Species == "BCCH"),"Color"] <- "coral1"
pca.camera[which(pca.camera$Species == "MOCH"),"Color"] <- "navy"
#pca.camera[which(pca.camera$SpeciesSex == "MOCH.M"),"Color"] <- "darkblue"
#pca.camera[which(pca.camera$SpeciesSex == "BCCH.M"),"Color"] <- "darkred"
#pca.camera[which(pca.camera$SpeciesSex == "MOCH.F"),"Color"] <- "deepskyblue"
#pca.camera[which(pca.camera$SpeciesSex == "BCCH.F"),"Color"] <- "orangered"
pca3d(prcomp.camera, group = pca.camera$Species, bg= "white", axes.color= "black", shape = "sphere", col = pca.camera$Color, show.ellipses = F, radius = 3, show.axe.titles = F, show.plane = F)
rgl.viewpoint(theta = 130, phi = 5, fov = 60, zoom = 0.6)
rglwidget()
rgl.postscript("/Users/katherinefeldmann/Desktop/camera.pdf","pdf")

## PCA Model ##
camera.Model <- lm(PCA3 ~ Species * Sex + Julian.Date + Museum + Year.Collected + Quadrant, data = pca.camera) # SPECIES MODEL
camera.Model <- lm(PCA3 ~ Sex + Julian.Date + Museum + Year.Collected + Quadrant, data = pca.camera) # BCCH / MOCH MODEL

summary(camera.Model)
hist(camera.Model$PCA1)
plot(camera.Model)

## Area Model ##
camera.Area <- lm(adjusted_area.throat ~ Species * Sex + Julian.Date + Museum + Year.Collected + Quadrant, data = combined.data)
camera.Area <- lm(adjusted_area.cheek ~ Sex + Julian.Date + Museum + Year.Collected + Quadrant, data = combined.data[which(combined.data$Species == "MOCH"),])

summary(camera.Area)
hist(camera.Area$PCA1)
plot(camera.Area)

##### CAMERA PLOTS ######
plot.combined.data <- combined.data[,c(1,19:ncol(combined.data))]
plot.combined.data <- reshape2::melt(plot.combined.data, by = "Specimen_ID")
plot.combined.data$variable <- str_remove(plot.combined.data$variable,"_[BS][aei][cld][kle][y]*")
plot.combined.data <- aggregate(value~Specimen_ID+variable, data = plot.combined.data, mean)
plot.combined.data <- dcast(plot.combined.data, Specimen_ID~variable)
plot.combined.data <- merge(combined.data[,c("Specimen_ID","Species","Sex","SpeciesSex")], plot.combined.data, by = "Specimen_ID")
lw.summary <- summarySE(plot.combined.data, measurevar=c("lw_mean.light"), groupvars=c("Species"))
mw.summary <- summarySE(plot.combined.data, measurevar=c("mw_mean.light"), groupvars=c("Species"))
sw.summary <- summarySE(plot.combined.data, measurevar=c("sw_mean.light"), groupvars=c("Species"))
uv.summary <- summarySE(plot.combined.data, measurevar=c("uv_mean.light"), groupvars=c("Species"))

## Area Plot ##
combined.data %>% ggplot(aes(x = Species, y = Area.light_Side, col = Species))+
  geom_jitter(cex = 5)+
  stat_summary(fun.data = mean_sdl, fun.args = list(mult=1), colour = "white", cex = 1.5, geom = "errorbar", width = 0.1)+
  stat_summary(fun.data = mean_sdl, fun.args = list(mult=1), colour = "white", cex = 1.2)+
  stat_summary(fun.data = mean_sdl, fun.args = list(mult=1), colour = "black", cex = 1, geom = "errorbar", width = 0.1)+
  stat_summary(fun.data = mean_sdl, fun.args = list(mult=1), colour = "black", cex = 1)+
  scale_color_manual(values = c("coral1","navy"))+
  theme_bw()+
  theme(legend.title = element_blank(), axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), strip.background = element_rect(color = "grey80", fill = "grey80"), strip.text = element_text(size = 16))

##### SPECTROMETER DATA #####
"
EXPLANATORY (categorical; 2 levels each): Species, Sex
COVARIATE (numeric): Julian Date
RANDOM (categorical): Box.Number, Location, Number.of.Head.Feathers, Number.of.Throat.Feathers, Number.of.Cheek.Feathers, Number.of.Side.Feathers, number_patches (Model 1)
"

## Create and Write Data Frame ##
#setwd("/Volumes/Feldmann_Research/Taylor_Lab_Honors_Thesis/Spectrometer_Data/")
#folders <- list.dirs("/Volumes/Feldmann_Research/Taylor_Lab_Honors_Thesis/Spectrometer_Data/", full.names = F)
#indivSpecData <- read.csv("/Users/katherinefeldmann/Desktop/CU_Boulder/Taylor_Lab/Honors Thesis/Revised_Resubmit/Spectrometer_Individuals.csv", header = T)
#colnames(indivSpecData)[1] <- "Individual"

#specData <- data.frame()
#for (i in folders[-1]){
  #data <- read_excel(paste(i, "/", i, ".xls", sep = ""))
  #data$Individual <- i
  #specData <- bind_rows(specData, data)
#}
#specData <- merge(specData, indivSpecData[,c("Individual","Species","Sex","Date.Sampled","Box.Number","Location","Number.of.Head.Feathers","Number.of.Throat.Feathers","Number.of.Cheek.Feathers","Number.of.Side.Feathers")], by = "Individual")
#specData$SpeciesSex <- paste(specData$Species, specData$Sex, sep = ".")
#specData$number_patches <- 4-rowSums(cbind(is.na(specData[,"Head"]), is.na(specData[,"Throat"]), is.na(specData[,"Cheek"]), is.na(specData[,"Side"])))
#write.csv(specData, file = "/Users/katherinefeldmann/Desktop/CU_Boulder/Taylor_Lab/Honors Thesis/Revised_Resubmit/Spectrometer_Data.csv")

## Process Spectrometer Data ##
{spec.data <- read.csv("Spectrometer_Data.csv", header = T)
spec.data[which(spec.data$Location == "CU"),"Location"] <- "BLD"
spec.data[which(is.na(spec.data$Box.Number)),"Box.Number"] <- "Feeder"
spec.data$Julian.Date <- yday(as.Date(spec.data$Date.Sampled, "%d-%b-%Y")) # Calculate Julian Date
spec.data$Wavelength_Range <- paste(substr(spec.data$Wavelength, 0, 2), "0.", as.numeric(substr(spec.data$Wavelength, 0, 2)) + 1, "0", sep = "") # Set wavelength ranges - Every 10 nm
drop.wavelength <- c("180.190","190.200","200.210","210.220","220.230","230.240","240.250","250.260","260.270","270.280"
                    ,"280.290","290.300","700.710","710.720","720.730","730.740","740.750","750.760","760.770","770.780"
                    ,"780.790","790.800","800.810","810.820","820.830","830.840","840.850","850.860","860.870") # Remove wavelengths that are outside the avian visual spectrum: 300-700 nm
'%notin%' <- Negate('%in%')
spec.data.dropped <- spec.data[which(spec.data$Wavelength_Range %notin% drop.wavelength),]
pca.spec.data <- aggregate(.~Wavelength_Range + Individual, spec.data.dropped[,c("Wavelength_Range","Individual","Head","Throat","Cheek","Side")], mean)
prcomp.spec.data <- dcast(setDT(pca.spec.data), Individual ~ Wavelength_Range, value.var = c("Head","Throat","Cheek","Side"))

## Calculate Brightness: average of reflectance across 300-700 nm for each patch-individual ##
brightness <- aggregate(.~Individual, spec.data.dropped[,c("Individual","Head","Throat","Cheek","Side")], mean)
colnames(brightness) <- c("Individual","brightness.Head","brightness.Throat","brightness.Cheek","brightness.Side")
prcomp.spec.data <- merge(prcomp.spec.data, brightness[,c("Individual","brightness.Head")], by = "Individual")
prcomp.spec.data <- merge(prcomp.spec.data, brightness[,c("Individual","brightness.Throat")], by = "Individual")
prcomp.spec.data <- merge(prcomp.spec.data, brightness[,c("Individual","brightness.Cheek")], by = "Individual")
prcomp.spec.data <- merge(prcomp.spec.data, brightness[,c("Individual","brightness.Side")], by = "Individual")}

## Run PCA ##
cheekcolumns <- c("Individual", colnames(prcomp.spec.data)[grep("Cheek",colnames(prcomp.spec.data))]) # BCCH CHEEK COLUMNS
BCCH.spec.data <- prcomp.spec.data[which(substr(prcomp.spec.data$Individual,1,4) == "BCCH"),..cheekcolumns] ## BCCH CHEEK MODEL

species.spec.data <- prcomp.spec.data ## SPECIES MODEL
BCCH.spec.data <- prcomp.spec.data[which(substr(prcomp.spec.data$Individual,1,4) == "BCCH"),] ## BCCH MODEL
MOCH.spec.data <- prcomp.spec.data[which(substr(prcomp.spec.data$Individual,1,4) == "MOCH"),] ## MOCH MODEL

prcomp.spec <- prcomp(MOCH.spec.data[,-1], scale = T, center = T)
plot(prcomp.spec)
prcomp.spec$rotation
summary(prcomp.spec)
View(prcomp.spec$rotation[order(prcomp.spec$rotation[,"PC3"]),"PC3"]) # Order PC1 values to determine what variables are driving the difference in either species: large values (BCCH) and small values (MOCH)

# Plot PCA and Create Data Frame for Modeling
pca.spec <- merge(species.spec.data[,"Individual"], unique(spec.data.dropped[,c("Individual","Species","Sex","SpeciesSex","Julian.Date","Location","Box.Number","Number.of.Head.Feathers","Number.of.Throat.Feathers","Number.of.Cheek.Feathers","Number.of.Side.Feathers","number_patches")]), by = "Individual")
pca.spec <- merge(BCCH.spec.data[,"Individual"], unique(spec.data.dropped[which(spec.data.dropped$Species == "BCCH"),c("Individual","Species","Sex","SpeciesSex","Julian.Date","Location","Box.Number","Number.of.Head.Feathers","Number.of.Throat.Feathers","Number.of.Cheek.Feathers","Number.of.Side.Feathers","number_patches")]), by = "Individual") # BCCH MODEL
pca.spec <- merge(MOCH.spec.data[,"Individual"], unique(spec.data.dropped[which(spec.data.dropped$Species == "MOCH"),c("Individual","Species","Sex","SpeciesSex","Julian.Date","Location","Box.Number","Number.of.Head.Feathers","Number.of.Throat.Feathers","Number.of.Cheek.Feathers","Number.of.Side.Feathers","number_patches")]), by = "Individual") # MOCH MODEL

ggbiplot(prcomp.spec, ellipse = T, groups = pca.spec$Sex, choices = c(1,3), var.axes = T) +
  geom_point(aes(color = pca.spec$Sex), size = 6) +
  scale_color_manual(values = c("skyblue","navy")) +
  theme_bw() +
  theme(legend.title = element_blank(), axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
pca.spec$PCA1 <- prcomp.spec$x[,1]
pca.spec$PCA2 <- prcomp.spec$x[,2]
pca.spec$PCA3 <- prcomp.spec$x[,3]

pca.spec$Color <- "NA"
pca.spec[which(pca.spec$Species == "BCCH"),"Color"] <- "coral1"
pca.spec[which(pca.spec$Species == "MOCH"),"Color"] <- "navy"
#pca.spec[which(pca.spec$SpeciesSex == "BCCH.M"),"Color"] <- "darkred"
#pca.spec[which(pca.spec$SpeciesSex == "MOCH.M"),"Color"] <- "darkblue"
#pca.spec[which(pca.spec$SpeciesSex == "BCCH.F"),"Color"] <- "orangered"
#pca.spec[which(pca.spec$SpeciesSex == "MOCH.F"),"Color"] <- "deepskyblue"
pca3d(prcomp.spec, group = pca.spec$Species, bg= "white", axes.color= "black", shape = "sphere", col = pca.spec$Color, show.ellipses = F, radius = 3, show.axe.titles = F, show.plane = F)
rgl.viewpoint(theta = 30, phi = 3, fov = 60, zoom = 0.7)
rglwidget()
rgl.postscript("/Users/katherinefeldmann/Desktop/spec.pdf","pdf")

## PCA Model ##
spec.Model <- lmer(PCA3 ~ Species * Sex + Julian.Date + Location + (1 | Box.Number) + (1 | Number.of.Head.Feathers) + (1 | Number.of.Throat.Feathers) + (1 | Number.of.Cheek.Feathers) + (1 | Number.of.Side.Feathers), data = pca.spec) # SPECIES MODEL
spec.Model <- lmer(PCA3 ~ Sex + Julian.Date + Location + (1 | Box.Number) + (1 | Number.of.Head.Feathers) + (1 | Number.of.Throat.Feathers) + (1 | Number.of.Cheek.Feathers) + (1 | Number.of.Side.Feathers), data = pca.spec) # BCCH MODEL
spec.Model <- lmer(PCA3 ~ Sex + Julian.Date + Location + (1 | Box.Number) + (1 | Number.of.Head.Feathers) + (1 | Number.of.Throat.Feathers) + (1 | Number.of.Side.Feathers), data = pca.spec) # MOCH MODEL - dropped Cheek because there is no variance
spec.Model <- lmer(PCA3 ~ Sex + Julian.Date + Location + (1 | Box.Number) + (1 | Number.of.Cheek.Feathers), data = pca.spec) # BCCH CHEEK MODEL

summary(spec.Model)
hist(pca.spec$PCA1)
plot(spec.Model)
{qqnorm(resid(spec.Model))
  qqline(resid(spec.Model))}

##### SPECTROMETER PLOTS #####

## Reflectance Curves ##
{headSummary <- summarySE(spec.data, measurevar = "Head", groupvars = c("SpeciesSex","Wavelength"), na.rm = T)
throatSummary <- summarySE(spec.data, measurevar = "Throat", groupvars = c("SpeciesSex","Wavelength"), na.rm = T)
cheekSummary <- summarySE(spec.data, measurevar = "Cheek", groupvars = c("SpeciesSex","Wavelength"), na.rm = T)
sideSummary <- summarySE(spec.data, measurevar = "Side", groupvars = c("SpeciesSex","Wavelength"), na.rm = T)}
ggplot(data = sideSummary, aes(x = Wavelength, y = Side, col = SpeciesSex, fill = SpeciesSex))+
  geom_ribbon(aes(ymin = Side-sd, ymax = Side+sd), alpha = 0.1, col = NA)+
  geom_line(aes(linetype = SpeciesSex), cex = 1)+
  scale_x_continuous(limits = c(290,710), expand = c(0,0))+
  scale_y_continuous(limits = c(0,75), expand = c(0,0))+
  scale_linetype_manual(values = c("longdash","solid","longdash","solid"))+
  scale_color_manual(values = c("coral1","coral1","navy","navy"))+
  scale_fill_manual(values = c("coral1","coral1","navy","navy"))+
  theme_bw()+
  theme(legend.title = element_blank(), axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), strip.background = element_rect(color = "grey80", fill = "grey80"), strip.text = element_text(size = 16))

## Cheek and Side Reflectance in Mountain Chickadees ##
MOCH.data <- spec.data[which(spec.data$Species == "MOCH"),]
cheekMOCH <- summarySE(MOCH.data, measurevar = "Cheek", groupvars = c("Sex","Wavelength"), na.rm = T)
sideMOCH <- summarySE(MOCH.data, measurevar = "Side", groupvars = c("Sex","Wavelength"), na.rm = T)
ggplot(data = sideMOCH, aes(x = Wavelength, y = Side, col = Sex, fill = Sex))+
  geom_ribbon(aes(ymin = Side-sd, ymax = Side+sd), alpha = 0.1, col = NA)+
  geom_line(aes(linetype = Sex), cex = 1)+
  scale_x_continuous(limits = c(290,710), expand = c(0,0))+
  scale_y_continuous(limits = c(0,60), expand = c(0,0))+
  scale_linetype_manual(values = c("longdash","solid"))+
  scale_color_manual(values = c("skyblue","navy"))+
  scale_fill_manual(values = c("skyblue","navy"))+
  theme_bw()+
  theme(legend.title = element_blank(), axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), strip.background = element_rect(color = "grey80", fill = "grey80"), strip.text = element_text(size = 16))

##### MAPS #####
setwd("/Users/katherinefeldmann/Desktop/CU_Boulder/Taylor_Lab/Honors Thesis/R Code/")

locations <- read.csv("Locations_Revised.csv")
theme_set(theme_bw())
world <- ne_countries(scale = "medium", returnclass = "sf")
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

## PHOTOGRAPHY ##

# Code to Generate Terrain Layer: Need Powerful Computer
"dem.raster1 <- getData('SRTM', lat = 36.5, lon = -109.5, download = TRUE)
dem.raster2 <- getData('SRTM', lat = 36.5, lon = -101.6, download = TRUE)
dem.raster3 <- getData('SRTM', lat = 41.5, lon = -109.5, download = TRUE)
dem.raster4 <- getData('SRTM', lat = 41.5, lon = -101.6, download = TRUE)

boundary <- c(xmin = -109.5, xmax = -101.6, ymin = 36.5, ymax = 41.5)
boundary.m <- matrix(c(boundary['xmin'], boundary['ymin'], boundary['xmax'], boundary['ymax']), ncol = 2)
boundary.extent <- extent(boundary.m)

raster <- list(dem.raster1, dem.raster2, dem.raster3, dem.raster4)
dem.raster <- do.call(merge, raster)
dem.raster <- crop(dem.raster, boundary.extent, snap='out')
dem.raster <- dem.raster * 20
slope.raster <- terrain(dem.raster, opt='slope')
aspect.raster <- terrain(dem.raster, opt='aspect')
hill.shade <- hillShade(slope.raster, aspect.raster, 40, 270)
hill.points <- rasterToPoints(hill.shade)
hill.df <- data.frame(hill.points)
colnames(hill.df) <- c('lon', 'lat, 'hill')"

# Modified Geom Jitter
#install.packages("remotes")
#remotes::install_version("SDMTools", "1.1-221")
require(SDMTools)
bounded_jitter <- function(mapping, data, bounds, width, height){
  # data2 is the jittered data
  data <- na.omit(data)
  data2 <- data
  data2[, "Longitude"] <- rnorm(nrow(data), data[, "Longitude"], width/1.96)
  data2[, "Latitude"] <- rnorm(nrow(data), data[, "Latitude"], height/1.96)
  # is it inside the polygon?
  idx <- as.logical(pnt.in.poly(pnts = data2[, c("Longitude", "Latitude")],  
                                poly.pnts = bounds[, c("long", "lat")])[, 'pip'])
  while(!all(idx)) { # redo for points outside polygon
    data2[!idx, "Longitude"] <- rnorm(sum(!idx), data[!idx, "Longitude"], width/1.96)
    data2[!idx, "Latitude"] <- rnorm(sum(!idx), data[!idx, "Latitude"], height/1.96)
    idx <- as.logical(pnt.in.poly(pnts = data2[, c("Longitude", "Latitude")],  
                                  poly.pnts = bounds[, c("long", "lat")])[, 'pip'])
  }
  # the point
  geom_point(data = data2, aes(x = Longitude, y = Latitude, col = Species), size = 3)
}

# Load Terrain Layer
load(file = "terrain_layer.Rda")
all_states <- map_data("state")
st.map <- subset(all_states, region == "colorado")

# Points Not Within Lines
ggplot(data = world) +
  geom_raster(data = hill.df, aes(lon, lat, fill = hill), alpha = .45) +
  scale_fill_gradientn(colours = grey.colors(100)) +
  geom_sf(data = states, fill = NA) +
  coord_sf(xlim = c(-109.5, -101.6), ylim = c(36.5, 41.5), expand = FALSE) +
  geom_point(data = locations, aes(x = Longitude, y = Latitude, col = Species), size = 3, position = position_jitter(width = 0.3, height = 0.3)) +
  scale_color_manual(values = c("coral1","navy"))

# Points Within Lines
ggplot(data = world) +
  geom_raster(data = hill.df, aes(lon, lat, fill = hill), alpha = .45) +
  scale_fill_gradientn(colours = grey.colors(100)) +
  geom_sf(data = states, fill = NA) +
  coord_sf(xlim = c(-109.5, -101.6), ylim = c(36.5, 41.5), expand = FALSE) +
  bounded_jitter(data = locations, aes(x = Longitude, y = Latitude), bounds = st.map, width = 0.4, height = 0.4) +
  scale_color_manual(values = c("coral1","navy"))

## SPECTROMETRY ##
#devtools::install_github('oswaldosantos/ggsn')
boxes <- read.csv("Boxes_Revised.csv")
myLocation <- c(-105.6, 39.95, -105.2, 40.1)

myMap <- get_map(location = myLocation, source = "google", maptype ="terrain", zoom = 12, color = "bw")
ggmap(myMap) +
  geom_point(data = boxes, aes(x = LON, y = LAT, col = Species), size = 2.5, position = position_jitter(width = 0.015, height = 0.015)) +
  scale_color_manual(values = c("coral1","navy")) +
  scalebar(x.min = -105.6, x.max = -105.2, y.min = 39.95,  y.max = 40.1, dist = 5, transform = T, model = 'WGS84', box.fill = c("black", "white"), dist_unit = "km", border.size = 0.5, st.bottom = F)

##### Corrections #####
cameraSpecies <- c(0.0679,0.00479,0.5438,0.174,0.000101,0.000883,0.000000000000000665,0.64901,0.00001,0.0000000000000002,0.825,0.847,0.0000865,0.000152,0.133)
specSpecies <- c(0.000000776,0.000000341,0.0669,0.00000463,0.00271,0.883)
cameraSex <- c(0.888,0.153,0.97805,0.4,0.2042,0.415,0.833044,0.288,0.69244,0.4184,0.1154,0.827,0.954106,0.563163,0.1951,0.2486,0.414,0.6716,0.35,0.324,0.04137,0.2278,0.571898,0.8537,0.646,0.127,0.1106,0.2762)
specSex <- c(0.00657,0.628,0.888,0.0531,0.0462,0.0288,0.58,0.363,0.0222,0.0127,0.119,0.991,0.725,0.374)

cameraSpecies.fdr <- p.adjust(cameraSpecies, method = "fdr")
specSpecies.fdr <- p.adjust(specSpecies, method = "fdr")
cameraSex.fdr <- p.adjust(cameraSex, method = "fdr")
specSex.fdr <- p.adjust(specSex, method = "fdr")

