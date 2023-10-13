{library(ggbiplot)
library(ggplot2)
library(lmerTest)
library(lme4)
library(lubridate)
library(stringr)
library(tidyr)
library(tidyverse)
library(dplyr)
library(reshape2)}

#Additional Packages for Plotting
{library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggmap)
library(maps)
library(Hmisc)}

cat("\014") #clear console
rm(list = ls())  #clear variables/environment
if(!is.null(dev.list())) dev.off() #clear plots
setwd("/Users/katherinefeldmann/Desktop/CU_Boulder/Taylor_Lab/Honors Thesis/R Code")

##### PCA PLOT FUNCTION ##### 
pca_plot <- function(patch_type, pca_type, pcaData, method, point_fill, PCA){
  if(pca_type == "Species" & method == "Camera"){
    pcaData <- pcaData[,-grep("_eyebrow", colnames(pcaData))]
  } else if(pca_type == "BCCH"){
    if(method == "Camera"){
      pcaData <- pcaData[,-grep("_eyebrow", colnames(pcaData))]
    }
    pcaData <- pcaData[which(pcaData$Species == "BCCH"),]
  } else if(pca_type == "MOCH"){
    pcaData <- pcaData[which(pcaData$Species == "MOCH"),]
  } else {
    pcaData <- pcaData
  }
  if(!is.na(patch_type)){
    #if(patch_type == "Custom"){customCol <- c("uv_head","uv_throat","uv_cheek","uv_side","uv_eyebrow")}  ## INCLUDE CUSTOMCOL
    #if(patch_type == "Custom"){customCol <- c("Bird_ID","Species","Sex","hue_disparity","max_hue_disparity","hue_disparity_variance")}  ## EXCLUDE CUSTOMCOL
    if(patch_type == "Custom"){customCol <- colnames(pcaData)[grep("Cheek", colnames(pcaData))]}  ## GREP CUSTOMCOL
  }
  if(!is.na(patch_type)){
    if(method == "Camera"){tempData <- pcaData[,c("Specimen_ID","Species","Sex")]}
    if(method == "Spectrometer"){tempData <- pcaData[,c("Bird_ID","Species","Sex")]}
    if(patch_type == "Custom"){
      pcaData <- cbind(tempData, pcaData[,customCol]) ## INCLUDE AND GREP CUSTOMCOL
      #pcaData <- cbind(tempData, pcaData[,!names(pcaData) %in% customCol]) ## EXCLUDE CUSTOMCOL
    } else if(patch_type == "Contrast"){
      if(method == "Camera"){pcaData <- cbind(tempData, pcaData[,c("average_span","span_variance","max_span","volume","hue_disparity","hue_disparity_variance","max_hue_disparity","average_chroma")])}
      if(method == "Spectrometer"){pcaData <- cbind(tempData, pcaData[,c("average_span","span_variance","max_span","hue_disparity","hue_disparity_variance","max_hue_disparity")])}
    } else {
      pcaData <- cbind(tempData, pcaData[,grep(patch_type, colnames(pcaData))])
    }
    
  }
  print(pcaData[,grep("_ID",colnames(pcaData))])
  pcaPatchData <- pcaData %>% select(4:ncol(pcaData))
  print(paste("Color variable:",colnames(pcaPatchData)))
  if(method == "Camera"){
    pcaPatch <- prcomp(pcaPatchData, scale = T, center = T)
    if(pca_type == "Species"){
      cameraSpecies <- pcaData[,"Species"]
      plot1 <- ggbiplot(pcaPatch, ellipse = T, groups = cameraSpecies, choices = 1:2, var.axes = T) +
        geom_point(aes(color = cameraSpecies), size = 4) +
        scale_color_manual(values = c("coral1","navy")) +
        theme_bw() +
        theme(legend.title = element_blank(), axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
      finalPatch <- pcaData %>% select("Specimen_ID","Species","Sex")
      } else {
      cameraSex <- pcaData[,"Sex"]
      plot1 <- ggbiplot(pcaPatch, ellipse = T, groups = cameraSex, choices = 1:2, var.axes = T) +
        geom_point(aes(color = cameraSex), size = 4) +
        scale_color_manual(values = c("lightblue","darkblue")) +
        theme_bw() +
        theme(legend.title = element_blank(), axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
      finalPatch <- pcaData[which(pcaData$Species == pca_type),c("Specimen_ID","Species","Sex")]
      }
    finalPatch$PCA1 <- pcaPatch$x[,1]
    finalPatch$PCA2 <- pcaPatch$x[,2]
    finalPatch$PCA3 <- pcaPatch$x[,3]
    randomEffects <- cameraData[which(cameraData$Patch == "head"), c("Specimen_ID","Julian.Date","Breeding.Season","Subspecies","Year.Collected","Museum","Quadrant","latitude_quadrant","Elevation","Latitude")]
    finalPatch <- merge(finalPatch, randomEffects, by = "Specimen_ID")
    finalPatch$SpeciesSex <- paste(finalPatch$Species, finalPatch$Sex, sep = ".")
    finalPatch$Year.Collected <- as.numeric(as.character(finalPatch$Year.Collected))
  } else if(method == "Spectrometer"){
    pcaPatch <- prcomp(pcaPatchData, scale =  T, center = T)
    if(pca_type == "Species"){
      specSpecies <- pcaData[,"Species"]
      plot1 <- ggbiplot(pcaPatch, ellipse = T, groups = specSpecies, choices = 1:2, var.axes = T) +
        geom_point(aes(color = specSpecies), size = 4) +
        scale_color_manual(values = c("coral1","navy")) +
        theme_bw() +
        theme(legend.title = element_blank(), axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
      finalPatch <- pcaData %>% select("Bird_ID","Species","Sex")
      } else {
      specSex <- pcaData[,"Sex"]
      plot1 <- ggbiplot(pcaPatch, ellipse = T, groups = specSex, choices = 2:3, var.axes = T) +
        geom_point(aes(color = specSex), size = 4) +
        scale_color_manual(values = c("lightblue","darkblue")) +
        theme_bw() +
        theme(legend.title = element_blank(), axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
      finalPatch <- pcaData[which(pcaData$Species == pca_type),c("Bird_ID","Species","Sex")]
      }
    finalPatch$PCA1 <- pcaPatch$x[,1]
    finalPatch$PCA2 <- pcaPatch$x[,2]
    finalPatch$PCA3 <- pcaPatch$x[,3]
    randomEffects <- specData[specData$patch == "Head", c("Bird_ID","Julian.Date","Box.Number","Location","Number.of.Head.Feathers","Number.of.Throat.Feathers","Number.of.Cheek.Feathers","Number.of.Side.Feathers","number_patches")]
    finalPatch <- merge(finalPatch, randomEffects, by = "Bird_ID")
    nestlingData <- chickadee.dropHOWR[,c("Box.Number","Prop.Nestlings")]
    finalPatch <- merge(finalPatch, nestlingData, by = "Box.Number", all.x = T)
    finalPatch$SpeciesSex <- paste(finalPatch$Species, finalPatch$Sex, sep = ".")
  } else {
    print("Must include method: Camera or Spectrometer.")
  }
  if (!is.na(PCA)){
    if(PCA == "pcaPatch"){
      return(pcaPatch)
    } else {
      return(finalPatch)
    }
  }
  ### PLOTTING ###
  if(!is.na(patch_type)){
    if(patch_type == "Contrast"){
      if(method == "Camera"){pcaPlotData <- pcaData[,c("average_span","span_variance","max_span","volume","hue_disparity","hue_disparity_variance","max_hue_disparity","average_chroma")]}
      if(method == "Spectrometer"){pcaPlotData <- pcaData[,c("average_span","span_variance","max_span","hue_disparity","hue_disparity_variance","max_hue_disparity")]}
    } else if(patch_type == "Custom"){
      pcaPlotData <- pcaData[,customCol]  ## INCLUDE AND GREP CUSTOMCOL
      #pcaPlotData <- pcaData[,!names(pcaData) %in% customCol]  ## EXCLUDE CUSTOMCOL
    } else {
      pcaPlotData <- pcaData[,grep(patch_type, colnames(pcaData))]
    }
  } else {
    pcaPlotData <- pcaData %>% select(4:ncol(pcaData))
  }
  pcaPatch <- prcomp(pcaPlotData, scale =  T, center = T)
  if(method == "Camera"){
    plotPatch <- pcaData %>% select("Specimen_ID","Species","Sex","average_span","span_variance","max_span","volume","hue_disparity","hue_disparity_variance","max_hue_disparity","average_chroma")
    tempPatch <- finalPatch[,c("Specimen_ID","SpeciesSex","Julian.Date","Breeding.Season","Subspecies","Year.Collected","Museum","Quadrant","latitude_quadrant","Elevation","Latitude")]
    plotPatch <- merge(plotPatch, tempPatch, by = "Specimen_ID")
  } else {
    plotPatch <- pcaData %>% select("Bird_ID","Species","Sex","average_span","span_variance","max_span","hue_disparity","hue_disparity_variance","max_hue_disparity","normalized_brilliance_Cheek","normalized_brilliance_Side")
    tempPatch <- finalPatch[,c("Bird_ID","SpeciesSex","Julian.Date","Box.Number","Location","Number.of.Head.Feathers","Number.of.Throat.Feathers","Number.of.Cheek.Feathers","Number.of.Side.Feathers","number_patches","Prop.Nestlings")]
    plotPatch <- merge(plotPatch, tempPatch, by = "Bird_ID")
  }
  plotPatch$PCA1 <- pcaPatch$x[,1]
  plotPatch$PCA2 <- pcaPatch$x[,2]
  plotPatch$PCA3 <- pcaPatch$x[,3]
  
  hullPatch <- plotPatch %>% 
    group_by(SpeciesSex) %>%
    slice(chull(PCA1, PCA2))
  if(point_fill == "SpeciesSex"){
    # Plot PCA
    plot2 <- plotPatch %>% ggplot(aes(x = PCA1, y = PCA2, col = SpeciesSex, fill = SpeciesSex)) +
      geom_polygon(data = hullPatch, alpha = 0.1) +
      geom_point(cex = 5) +
      scale_fill_discrete(labels = c(" BCCH Female"," BCCH Male"," MOCH Female"," MOCH Male")) +
      scale_color_discrete(labels = c(" BCCH Female"," BCCH Male"," MOCH Female"," MOCH Male")) +
      labs(title = paste(patch_type,point_fill)) +
      theme_bw() +
      theme(legend.title = element_blank(), axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank()) +
      geom_text(aes(label = plotPatch[,grep("_ID",colnames(plotPatch))]), size = 2)
  } else if (is.discrete(plotPatch[,point_fill])){
    # Color Points by Discrete Variable
    plotPatch <- plotPatch[rev(order(plotPatch[,point_fill])),]
    plot2 <- plotPatch %>% ggplot(aes(x = PCA1, y = PCA2, fill = SpeciesSex)) +
      geom_polygon(data = hullPatch, alpha = 0.3) +
      geom_point(aes(col = plotPatch[,point_fill])) +
      scale_fill_discrete(labels = c(" BCCH Female"," BCCH Male"," MOCH Female"," MOCH Male")) +
      labs(title = paste(patch_type,point_fill)) +
      theme_bw() +
      theme(legend.title = element_blank(), axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank()) #+
      #geom_text(aes(label = plotPatch[,1]), size = 2)
  } else {
    # Color Points by Continuous Variable
    plotPatch <- plotPatch[rev(order(plotPatch[,point_fill])),]
    plot2 <- plotPatch %>% ggplot(aes(x = PCA1, y = PCA2, fill = SpeciesSex)) +
      geom_polygon(data = hullPatch, alpha = 0.3) +
      geom_point(aes(col = plotPatch[,point_fill]), cex = 2) +
      scale_color_gradient(low = "yellow", high = "black", na.value = "white") +
      scale_fill_discrete(labels = c(" BCCH Female"," BCCH Male"," MOCH Female"," MOCH Male")) +
      labs(title = paste(patch_type,point_fill)) +
      theme_bw() +
      theme(legend.title = element_blank(), axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank()) #+
      #geom_text(aes(label = plotPatch[,1]), size = 2)
  }
  list(plot(plot1), plot(plot2))
}

##### ASSEMBLE CAMERA DATA #####

## Read Data From CSV Files ##
{cameraIndividuals <- read.csv("Camera_Individuals.csv")
cameraPatch <- read.csv("Camera_Data_Patch.csv")
cameraDataIndiv <- read.csv("Camera_Data_Individual.csv")
cameraArea <- read.csv("Camera_Data_Area.csv")
eyebrowArea <- read.csv("Camera_Eyebrow_Area.csv")
eyebrowPatch <- read.csv("Camera_Eyebrow_Patch.csv")

## Merge Data To Create Master Data Frame ##
cameraPatch <- rbind(cameraPatch, eyebrowPatch)
cameraArea <- rbind(cameraArea, eyebrowArea)
cameraData <- merge(cameraIndividuals, cameraPatch, by = "Specimen_ID")
cameraData <- merge(cameraData, cameraDataIndiv, by = "Specimen_ID")
cameraData <- merge(cameraData, cameraArea, by = c("Specimen_ID","Patch","Side"), all = T)

locations <- read.csv("Locations.csv")
locationData <- locations[,c("Specimen_ID","Location.Collected","Quadrant","latitude_quadrant","Elevation","Latitude")]
cameraData <- merge(cameraData, locationData, by = "Specimen_ID")

cameraData$Year.Collected <- as.factor(str_match(cameraData$Date.Collected, "[0-9][0-9][0-9][0-9]"))  #extract Year and save to separate column
cameraData$Julian.Date <- yday(as.Date(cameraData$Date.Collected, "%d-%b-%Y"))  #convert date (day-month-year) to Julian dates
cameraData$Quadrant <- as.factor(cameraData$Quadrant) #convert Quadrant (1, 2, 3 and 4) to factor

cameraData$Breeding.Season <- "No"
cameraData$Breeding.Season[which(cameraData$Julian.Date >= 91 & cameraData$Julian.Date <= 181)] <- "Yes"
cameraData$Breeding.Season[which(cameraData$Julian.Date > 189  & cameraData$Julian.Date < 304)] <- "Molt"

sideCol <- grep("terior",cameraData$Patch)
eyebrowRows <- which(cameraData$Patch == "eyebrow_L" | cameraData$Patch == "eyebrow_R")
levels(cameraData$Patch) <- c(levels(cameraData$Patch),"side","eyebrow")
cameraData$Patch[sideCol] <- "side"
cameraData$Patch[eyebrowRows] <- "eyebrow"
meanPatch <- c("theta","phi","achieved_r","lw","mw","sw","uv","relative_lw","relative_mw","relative_sw","relative_uv","adjusted_area")
tempCamData <- cameraData %>% group_by(Specimen_ID, Patch) %>% summarise_at(vars(meanPatch), list(mean = mean))
subsetData <- cameraData[,c("Specimen_ID","Patch","Species","Subspecies","Museum","Sex","Julian.Date","Breeding.Season","Quadrant","latitude_quadrant","Latitude","Elevation","average_span","span_variance","max_span","volume","hue_disparity","hue_disparity_variance","max_hue_disparity","average_chroma","Year.Collected")]
cameraData <- merge(subsetData, tempCamData, by = c("Specimen_ID","Patch"))
cameraData <- unique(cameraData)
cameraData <- cameraData %>% rename(theta = theta_mean, phi = phi_mean, achieved_r = achieved_r_mean, lw = lw_mean, mw = mw_mean, sw = sw_mean, uv = uv_mean, relative_lw = relative_lw_mean, relative_mw = relative_mw_mean, relative_sw = relative_sw_mean, relative_uv = relative_uv_mean, adjusted_area = adjusted_area_mean)

## Populate NA Julian Date Column ##
levels(cameraData$Julian.Date) <- c(levels(cameraData$Julian.Date), 0)
cameraData[which(is.na(cameraData$Julian.Date)),"Julian.Date"] <- 0}

##### ASSEMBLE SPECTROMETER DATA #####

## Read Data From CSV Files ##
{specIndividuals <- read.csv("Spectrometer_Individuals.csv")

specPatch <- read.csv("Spectrometer_Data_Patch.csv")
specDataIndiv <- read.csv("Spectrometer_Data_Individual.csv")

## Merge Data To Create Master Data Frame ##
specData <- merge(specIndividuals, specPatch, by = "Bird_ID")
specData <- merge(specData, specDataIndiv, by = "Bird_ID")

specData$Julian.Date <- yday(as.Date(specData$Date.Sampled, "%d-%b-%Y"))  #convert date (day-month-year) to Julian dates
specData$max_wavelength <- as.numeric(as.character(specData$max_wavelength))  #convert wavelength from string to numeric (will turn "Multiple Occurances" to NA)

## Convert Random Effect Variables to Factor ##
to_factor <- c("Number.of.Head.Feathers","Number.of.Throat.Feathers","Number.of.Cheek.Feathers","Number.of.Side.Feathers","number_patches")
specData[to_factor] <- lapply(specData[to_factor], as.factor)

## Drop Individuals Without Four Patches and Columns That Have NA ##
specData <- specData[which(specData$number_patches == 4),]
specData$volume <- NULL
specData$max_wavelength <- NULL

## Populate NA Box Number Columns ##
levels(specData$Box.Number) <- c(levels(specData$Box.Number), as.character(specData[which(is.na(specData$Box.Number)),"Bird_ID"]))
specData[which(is.na(specData$Box.Number)),"Box.Number"] <- specData[which(is.na(specData$Box.Number)),"Bird_ID"]

## Proportion Nestling Data ##
chickadee <- read.csv("Chickadee Breeding Season 2019 .csv")
chickadee.dropHOWR <-subset(chickadee, chickadee$Species =="BCCH" | chickadee$Species=="MOCH")
str(chickadee.dropHOWR)
chickadee.dropHOWR <- na.omit(chickadee.dropHOWR)
chickadee.dropHOWR <- subset(chickadee.dropHOWR, chickadee.dropHOWR$Hatch.Date !="U" & chickadee.dropHOWR$Hatch.Date !="N/A")
chickadee.dropHOWR$Clutch.Size <- as.numeric(chickadee.dropHOWR$Clutch.Size)
chickadee.dropHOWR$Nestlings <- as.numeric(chickadee.dropHOWR$Nestlings)
chickadee.dropHOWR$Prop.Nestlings <- chickadee.dropHOWR$Nestlings/chickadee.dropHOWR$Clutch.Size
chickadee.dropHOWR$Box.Number <- paste(substring(chickadee.dropHOWR$Nest.Box.ID, 5), "-2019", sep = "")

specData$SpeciesSex <- paste(specData$Species, specData$Sex, sep = "")}

##### CAMERA MODELS #####
"
EXPLANATORY (categorical; 2 levels each): Species, Sex
COVARIATE (numeric): Julian Date
RANDOM (categorical): Subspecies, Year.Collected, Location.Collected, Museum

RESPONSE: INDIVIDUAL (numeric; 8 variables): average_span, span_variance, max_span, volume, hue_disparity, hue_disparity_variance, max_hue_disparity, average_chroma
RESPONSE: PATCH (numeric; 11 variables): theta, phi, achieved_r, lw, mw, sw, uv, relative_lw, relative_mw, relative_sw, relative_uv
RESPONSE: AREA (numeric; 1 variable): adjusted_area
"

## Create Data Frame With Patch_Response As Columns (Each Individual Has One Row) ##
# 170 rows (individuals); 144 columns (response variables)
{modCameraData <- cameraData[which(cameraData$Patch == "head"),c("Specimen_ID","Species","Sex")]
responseCamera <- c("theta","phi","achieved_r","lw","mw","sw","uv","relative_lw","relative_mw","relative_sw","relative_uv")
  for (i in responseCamera){
    tempData <- cameraData %>% select("Specimen_ID","Patch",i)
    tempData <- tempData %>% 
      group_by_at(c("Specimen_ID","Patch")) %>%
      mutate(row_id=1:n()) %>% ungroup() %>%
      spread(key=Patch, value=i) %>%
      select(-row_id)  # drop the index
    colnames(tempData)[2:ncol(tempData)] <- paste(i, colnames(tempData)[2:ncol(tempData)], sep = "_")
    if(i == "theta"){
      modCameraData <- merge(modCameraData, tempData, by = "Specimen_ID")
    } else{
      modCameraData <- merge(modCameraData, tempData, by = "Specimen_ID")
    }
  }
indivCameraData <- cameraData[cameraData$Patch == "head",c("Specimen_ID","average_span","span_variance","max_span","volume","hue_disparity","hue_disparity_variance","max_hue_disparity","average_chroma")]
pcaCameraData <- merge(modCameraData, indivCameraData, by = "Specimen_ID")}

## Subset By Museum ##
{CUmodCameraData <- modCameraData[grep("CU", modCameraData$Specimen_ID),]
CUpcaCameraData <- CUmodCameraData %>% select(2:ncol(CUmodCameraData))

DMNSmodCameraData <- modCameraData[grep("DMNS", modCameraData$Specimen_ID),]
DMNSpcaCameraData <- DMNSmodCameraData %>% select(2:ncol(DMNSmodCameraData))}

## PCA Model ##
# Species #
finalCamera <- pca_plot(NA, "Species", pcaCameraData, "Camera", "SpeciesSex", "finalPatch")
camera.Model <- lmer(PCA1 ~ Species + Sex + Julian.Date + Museum + (1 | Year.Collected) + (1 | Quadrant), data = finalCamera)
# Sex #
finalCamera <- pca_plot("Custom", "MOCH", pcaCameraData, "Camera", "SpeciesSex", "finalPatch")
camera.Model <- lmer(PCA3 ~ Sex + Julian.Date + Museum + (1 | Year.Collected) + (1 | Quadrant), data = finalCamera)

summary(camera.Model)
hist(finalCamera$PCA1)
plot(camera.Model)
{qqnorm(resid(camera.Model))
  qqline(resid(camera.Model))}

pcaPATCH <- pca_plot("Custom", "MOCH", pcaCameraData, "Camera", "SpeciesSex", "pcaPatch")
plot(pcaPATCH)
pcaPATCH$rotation
summary(pcaPATCH)

## Breeding Season Model ##
finalCamera <- pca_plot("head", pcaCameraData, modCameraData, "Camera", "SpeciesSex", "finalPatch", NA)
breedingModel <- lm(PCA1 ~ Breeding.Season, data = finalCamera)
summary(breedingModel)

## Area Model ##
areaHead <- cameraData[which(cameraData$Patch == "head"),]
areaThroat <- cameraData[which(cameraData$Patch == "throat"),]
areaCheek <- cameraData[which(cameraData$Patch == "cheek"),]
areaEyebrow <- cameraData[which(cameraData$Patch == "eyebrow"),]

# Species #
camera.Area <- lmer(adjusted_area ~ Species + Sex + Julian.Date + Museum + (1 | Year.Collected) + (1 | Quadrant), data = areaEyebrow)
# Sex #
camera.Area <- lmer(adjusted_area ~ Sex + Julian.Date + Museum + (1 | Year.Collected) + (1 | Quadrant), data = areaEyebrow)
summary(camera.Area)
hist(areaHead$adjusted_area)
plot(camera.Area)
{qqnorm(resid(camera.Area))
  qqline(resid(camera.Area))}

##### CAMERA PLOTS ######

areaData <- subset(cameraData, cameraData$Patch == "head" | cameraData$Patch == "throat" | cameraData$Patch == "cheek")

## AREA: Patch and Species ##
ggplot(areaData, aes(x = Species, y = adjusted_area)) +
  geom_jitter(aes(col = Species), cex = 5, alpha = 0.5) +
  scale_color_manual(values = c("coral1","navy")) +
  stat_summary(fun.data = mean_sdl, geom = "pointrange", col = "black", size = 1) +
  stat_summary(fun.data = mean_sdl, geom = "point", size = 11, col = "white", shape = 18) +
  stat_summary(fun.data = mean_sdl, geom = "point", size = 8, col = "black", shape = 5) +
  stat_summary(fun.data = mean_sdl, geom = "point", size = 7, col = "black", shape = 18) +
  labs(x = "Species", y = "Adjusted Area") +
  facet_wrap(areaData$Patch) +
  theme_bw() +
  theme(legend.title = element_blank(), axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), strip.background = element_rect(color = "grey80", fill = "grey80"), strip.text = element_text(size = 16))

## AREA: Patch and Species-Sex ##
ggplot(areaData, aes(x = Sex, y = adjusted_area)) +
  geom_jitter(aes(col = Sex), cex = 5, alpha = 0.5) +
  scale_color_manual(values = c("darkblue","lightblue")) +
  stat_summary(fun.data = mean_sdl, geom = "pointrange", col = "black", size = 1) +
  stat_summary(fun.data = mean_sdl, geom = "point", size = 11, col = "white", shape = 18) +
  stat_summary(fun.data = mean_sdl, geom = "point", size = 8, col = "black", shape = 5) +
  stat_summary(fun.data = mean_sdl, geom = "point", size = 7, col = "black", shape = 18) +
  labs(x = "", y = "Adjusted Area") +
  facet_grid(areaData$Patch ~ areaData$Species) +
  theme_bw() +
  theme(legend.title = element_blank(), axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), strip.background = element_rect(color = "grey80", fill = "grey80"), strip.text = element_text(size = 16))

## Julian Date by PCA: Change "patch" (head, throat, cheek or side) ##
finalPatch <- pca_plot("head", pcaCameraData, modCameraData, "Camera", "SpeciesSex", "finalPatch", NA)
finalPatch %>% ggplot(aes(Julian.Date, PCA1)) +
  geom_point(aes(col = Species)) +
  labs(x = "Julian Date", y = "PCA1", title = "DMNS Side")

## Longitude, Latitude and Elevation by Museum
museumLocations.MOCH <- subset(locations, Species == "MOCH")
museumLocations.BCCH <- subset(locations, Species == "BCCH")
museumLocations.MOCH <- merge(museumLocations.MOCH, tempfinalPatch, by = "Specimen_ID")

museumLocations.MOCH %>% ggplot(aes(Longitude, Latitude)) +
  geom_jitter(aes(col = Museum), cex = 5)

museumLocations.MOCH %>% ggplot(aes(Museum, Elevation)) +
  geom_jitter(aes(col = Museum))

## PCA by Breeding Season ##
cameraData %>% ggplot(aes(Species, theta)) +
  geom_jitter(aes(col = Patch))

finalPatch <- pca_plot("head", pcaCameraData, modCameraData, "Camera", "SpeciesSex", "finalPatch", NA)
finalPatch %>% ggplot(aes(Species, PCA1)) +
  geom_jitter(aes(col = Breeding.Season))

## Contrast Variables ##
contrastCamData <- cameraData[which(cameraData$Patch == "head"),c("Specimen_ID","Species","average_span","span_variance","max_span","hue_disparity","hue_disparity_variance","max_hue_disparity")]
contrastCamData <- melt(contrastCamData, id.vars = c("Specimen_ID","Species"), variable.name = "Variable", value.name = "Value")
contrastCamData$Type <- "Color Span"
contrastCamData$Type[which(contrastCamData$Variable == "hue_disparity" | contrastCamData$Variable == "max_hue_disparity" | contrastCamData$Variable == "hue_disparity_variance")] <- "Hue Disparity"
levels(contrastCamData$Variable) <- c(levels(contrastCamData$Variable), "Average","Maximum","Variance")
contrastCamData$Variable[which(contrastCamData$Variable == "average_span" | contrastCamData$Variable == "hue_disparity")] <- "Average"
contrastCamData$Variable[which(contrastCamData$Variable == "max_span" | contrastCamData$Variable == "max_hue_disparity")] <- "Maximum"
contrastCamData$Variable[which(contrastCamData$Variable == "span_variance" | contrastCamData$Variable == "hue_disparity_variance")] <- "Variance"
contrastCamData %>% ggplot(aes(x = Species, y = Value, fill = Species)) +
  geom_boxplot() +
  scale_fill_manual(values = c("blue","brown1")) +
  facet_wrap(~ Type + Variable, scales = "free") +
  theme_bw() +
  theme(legend.title = element_blank(), axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), strip.background = element_rect(color = "grey80", fill = "grey80"), strip.text = element_text(size = 16))

## Overall Reflectance ##
camPlotData <- cameraData[which(cameraData$Patch == "cheek" | cameraData$Patch == "side"),c("Specimen_ID","Patch","Species","lw","mw","sw","uv")]
camPlotData <- melt(camPlotData, id.vars = c("Specimen_ID","Patch","Species"), variable.name = "Variable", value.name = "Value")
camPlotData %>% ggplot(aes(x = Variable, y = Value)) +
  geom_jitter(aes(col = Species), cex = 5, alpha = 0.5) +
  scale_color_manual(values = c("coral1","navy")) +
  stat_summary(data = camPlotData[which(camPlotData$Species == "BCCH"),], fun.y = "mean", geom = "point", col = "white", size = 11, shape = 18) +
  stat_summary(data = camPlotData[which(camPlotData$Species == "BCCH"),], fun.y = "mean", geom = "point", col = "black", size = 8, shape = 5) +
  stat_summary(data = camPlotData[which(camPlotData$Species == "BCCH"),], fun.y = "mean", geom = "point", col = "coral1", size = 7, shape = 18) +
  stat_summary(data = camPlotData[which(camPlotData$Species == "MOCH"),], fun.y = "mean", geom = "point", col = "white", size = 11, shape = 18) +
  stat_summary(data = camPlotData[which(camPlotData$Species == "MOCH"),], fun.y = "mean", geom = "point", col = "black", size = 8, shape = 5) +
  stat_summary(data = camPlotData[which(camPlotData$Species == "MOCH"),], fun.y = "mean", geom = "point", col = "navy", size = 7, shape = 18) +
  facet_wrap(~ Patch, scales = "free") +
  theme_bw() +
  theme(legend.title = element_blank(), axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), strip.background = element_rect(color = "grey80", fill = "grey80"), strip.text = element_text(size = 16))

## Relative Reflectance ##
camPlotData <- cameraData[which(cameraData$Patch == "cheek" | cameraData$Patch == "side"),c("Specimen_ID","Patch","Species","relative_lw")]
camPlotData <- melt(camPlotData, id.vars = c("Specimen_ID","Patch","Species"), variable.name = "Variable", value.name = "Value")
camPlotData %>% ggplot(aes(x = Variable, y = Value)) +
  geom_jitter(aes(col = Species), cex = 5, alpha = 0.5) +
  scale_color_manual(values = c("coral1","navy")) +
  stat_summary(data = camPlotData[which(camPlotData$Species == "BCCH"),], fun.y = "mean", geom = "point", col = "white", size = 11, shape = 18) +
  stat_summary(data = camPlotData[which(camPlotData$Species == "BCCH"),], fun.y = "mean", geom = "point", col = "black", size = 8, shape = 5) +
  stat_summary(data = camPlotData[which(camPlotData$Species == "BCCH"),], fun.y = "mean", geom = "point", col = "coral1", size = 7, shape = 18) +
  stat_summary(data = camPlotData[which(camPlotData$Species == "MOCH"),], fun.y = "mean", geom = "point", col = "white", size = 11, shape = 18) +
  stat_summary(data = camPlotData[which(camPlotData$Species == "MOCH"),], fun.y = "mean", geom = "point", col = "black", size = 8, shape = 5) +
  stat_summary(data = camPlotData[which(camPlotData$Species == "MOCH"),], fun.y = "mean", geom = "point", col = "navy", size = 7, shape = 18) +
  facet_wrap(~ Patch, scales = "free") +
  theme_bw() +
  theme(legend.title = element_blank(), axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), strip.background = element_rect(color = "grey80", fill = "grey80"), strip.text = element_text(size = 16))

## Theta ##
camPlotData <- cameraData[which(cameraData$Patch == "cheek" | cameraData$Patch == "side"),c("Specimen_ID","Patch","Species","theta")]
camPlotData <- melt(camPlotData, id.vars = c("Specimen_ID","Patch","Species"), variable.name = "Variable", value.name = "Value")
camPlotData %>% ggplot(aes(x = Variable, y = Value, fill = Species)) +
  geom_boxplot() +
  scale_fill_manual(values = c("blue","brown1")) +
  facet_wrap(~ Patch, scales = "free") +
  theme_bw() +
  theme(legend.title = element_blank(), axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), strip.background = element_rect(color = "grey80", fill = "grey80"), strip.text = element_text(size = 16))

## Chroma ##
camPlotData <- cameraData[,c("Specimen_ID","Patch","Species","achieved_r")]
camPlotData <- melt(camPlotData, id.vars = c("Specimen_ID","Patch","Species"), variable.name = "Variable", value.name = "Value")
camPlotData %>% ggplot(aes(x = Variable, y = Value, fill = Species)) +
  geom_boxplot() +
  facet_wrap(~ Patch, scales = "free")

## Average Chroma ##
camPlotData <- cameraData[,c("Specimen_ID","Patch","Species","average_chroma")]
camPlotData <- melt(camPlotData, id.vars = c("Specimen_ID","Patch","Species"), variable.name = "Variable", value.name = "Value")
camPlotData %>% ggplot(aes(x = Variable, y = Value, fill = Species)) +
  geom_boxplot()

## Volume ##
camPlotData <- cameraData[,c("Specimen_ID","Patch","Species","volume")]
camPlotData <- melt(camPlotData, id.vars = c("Specimen_ID","Patch","Species"), variable.name = "Variable", value.name = "Value")
camPlotData %>% ggplot(aes(x = Variable, y = Value, fill = Species)) +
  geom_boxplot()

## COLOR: PCA ## (Point Fills: Breeding.Season, Quadrant, Year.Collected, Latitude, Elevation, Museum)
pca_plot(NA, "Species", pcaCameraData, "Camera", "SpeciesSex", NA)

##### SPECTROMETER MODELS #####
"
EXPLANATORY (categorical; 2 levels each): Species, Sex
COVARIATE (numeric): Julian Date
RANDOM (categorical): Box.Number, Location, Number.of.Head.Feathers, Number.of.Throat.Feathers, Number.of.Cheek.Feathers, Number.of.Side.Feathers, number_patches (Model 1)

RESPONSE: INDIVIDUAL (numeric; 7 variables): average_span, span_variance, max_span, hue_disparity, hue_disparity_variance, max_hue_disparity
RESPONSE: PATCH (numeric; 10 variables): theta, phi, achieved_r, relative_lw, relative_mw, relative_sw, relative_uv, normalized_brilliance, max_wavelength, reflectance_at_max
"

## Create Data Frame With Patch_Response As Columns (Each Individual Has One Row) ##
# 50 rows (individuals); 47 columns (response variables)
{modSpecData <- specData[which(specData$patch == "Head"),c("Bird_ID","Species","Sex")]
responseSpec <- c("theta","phi","achieved_r","relative_lw","relative_mw","relative_sw","relative_uv","normalized_brilliance")
  for (i in responseSpec){
    tempData <- specData %>% select("Bird_ID","patch",i)
    tempData <- tempData %>% 
      group_by_at(c("Bird_ID","patch")) %>%
      mutate(row_id=1:n()) %>% ungroup() %>%
      spread(key=patch, value=i) %>%
      select(-row_id)
    colnames(tempData)[2:ncol(tempData)] <- paste(i, colnames(tempData)[2:ncol(tempData)], sep = "_")
    if(i == "theta"){
      modSpecData <- merge(modSpecData, tempData, by = "Bird_ID")
    } else{
      modSpecData <- merge(modSpecData, tempData, by = "Bird_ID")
    }
  }
indivSpecData <- specData[specData$patch == "Head",c("Bird_ID","average_span","span_variance","max_span","hue_disparity","hue_disparity_variance","max_hue_disparity")]
pcaSpecData <- merge(modSpecData, indivSpecData, by = "Bird_ID")
### DROP HEAD AND THROAT BECAUSE 0'S WERE GENERATED BY PATCHES TOO DARK TO ACCURATELY MEASURE ###
pcaSpecData[,grep("Head",colnames(pcaSpecData))] <- NULL
pcaSpecData[,grep("Throat", colnames(pcaSpecData))] <- NULL}

## PCA Model ##
# Species #
finalSpec <- pca_plot(NA, "Species", pcaSpecData, "Spectrometer", "SpeciesSex", "finalPatch")
spec.Model <- lmer(PCA3 ~ Species + Sex + Julian.Date + (1 | Location) + (1 | Box.Number) + (1 | Number.of.Cheek.Feathers) + (1 | Number.of.Side.Feathers), data = finalSpec)
# Sex #
finalSpec <- pca_plot("Custom", "BCCH", pcaSpecData, "Spectrometer", "SpeciesSex", "finalPatch")
rspec.Model <- lmer(PCA3 ~ Sex + Julian.Date + (1 | Location) + (1 | Box.Number) + (1 | Number.of.Cheek.Feathers) + (1 | Number.of.Side.Feathers), data = finalSpec)

summary(spec.Model)
hist(finalSpec$PCA1)
plot(spec.Model)
{qqnorm(resid(spec.Model))
  qqline(resid(spec.Model))}

pcaPATCH <- pca_plot("Custom", "BCCH", pcaSpecData, "Spectrometer", "SpeciesSex", "pcaPatch")
plot(pcaPATCH)
pcaPATCH$rotation
summary(pcaPATCH)

##### SPECTROMETER PLOTS #####

## Phi by Theta ##
specData  %>% ggplot(aes(theta, phi, col = SpeciesSex)) +
  geom_point()

## Contrast Variables ##
contrastSpecData <- specData[which(specData$patch == "Head"),c("Bird_ID","Species","average_span","span_variance","max_span","hue_disparity","hue_disparity_variance","max_hue_disparity")]
contrastSpecData <- melt(contrastSpecData, id.vars = c("Bird_ID","Species"), variable.name = "Variable", value.name = "Value")
contrastSpecData$Type <- "Color Span"
contrastSpecData$Type[which(contrastSpecData$Variable == "hue_disparity" | contrastSpecData$Variable == "max_hue_disparity" | contrastSpecData$Variable == "hue_disparity_variance")] <- "Hue Disparity"
levels(contrastSpecData$Variable) <- c(levels(contrastSpecData$Variable), "Average","Maximum","Variance")
contrastSpecData$Variable[which(contrastSpecData$Variable == "average_span" | contrastSpecData$Variable == "hue_disparity")] <- "Average"
contrastSpecData$Variable[which(contrastSpecData$Variable == "max_span" | contrastSpecData$Variable == "max_hue_disparity")] <- "Maximum"
contrastSpecData$Variable[which(contrastSpecData$Variable == "span_variance" | contrastSpecData$Variable == "hue_disparity_variance")] <- "Variance"
contrastSpecData %>% ggplot(aes(x = Species, y = Value, fill = Species)) +
  geom_boxplot() +
  scale_fill_manual(values = c("blue","brown1")) +
  facet_wrap(contrastSpecData$Type ~ contrastSpecData$Variable, scales = "free") +
  theme_bw() +
  theme(legend.title = element_blank(), axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), strip.background = element_rect(color = "grey80", fill = "grey80"), strip.text = element_text(size = 16))

finalSpec <- pca_plot("Contrast", pcaSpecData, modSpecData, "Spectrometer", "SpeciesSex", "finalPatch", NA)
finalSpec %>% ggplot(aes(Species, PCA1, col = Species)) +
  geom_jitter(cex = 3) +
  stat_summary(fun.data = mean_sdl, geom = "pointrange", color = "black") +
  scale_color_manual(values = c("coral1","navy")) +
  theme_bw() +
  theme(legend.title = element_blank(), axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), strip.background = element_rect(color = "grey80", fill = "grey80"), strip.text = element_text(size = 16))

## Normalized Brilliance ##
specPlotData <- specData[which(specData$patch == "Cheek" | specData$patch == "Side"),c("Bird_ID","patch","Species","normalized_brilliance")]
specPlotData <- melt(specPlotData, id.vars = c("Bird_ID","patch","Species"), variable.name = "Variable", value.name = "Value")
specPlotData %>% ggplot(aes(x = Variable, y = Value)) +
  geom_jitter(aes(col = Species), cex = 5, alpha = 0.5) +
  scale_color_manual(values = c("coral1","navy")) +
  stat_summary(data = specPlotData[which(specPlotData$Species == "BCCH"),], fun.y = "mean", geom = "point", col = "white", size = 11, shape = 18) +
  stat_summary(data = specPlotData[which(specPlotData$Species == "BCCH"),], fun.y = "mean", geom = "point", col = "black", size = 8, shape = 5) +
  stat_summary(data = specPlotData[which(specPlotData$Species == "BCCH"),], fun.y = "mean", geom = "point", col = "coral1", size = 7, shape = 18) +
  stat_summary(data = specPlotData[which(specPlotData$Species == "MOCH"),], fun.y = "mean", geom = "point", col = "white", size = 11, shape = 18) +
  stat_summary(data = specPlotData[which(specPlotData$Species == "MOCH"),], fun.y = "mean", geom = "point", col = "black", size = 8, shape = 5) +
  stat_summary(data = specPlotData[which(specPlotData$Species == "MOCH"),], fun.y = "mean", geom = "point", col = "navy", size = 7, shape = 18) +
  facet_wrap(~ patch, scales = "free", labeller = labeller(c("cheek","side"))) +
  theme_bw() +
  theme(legend.title = element_blank(), axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), strip.background = element_rect(color = "grey80", fill = "grey80"), strip.text = element_text(size = 16))

## Phi ##
specPlotData <- specData[which(specData$patch == "Cheek" | specData$patch == "Side"),c("Bird_ID","patch","Species","phi")]
specPlotData <- melt(specPlotData, id.vars = c("Bird_ID","patch","Species"), variable.name = "Variable", value.name = "Value")
specPlotData %>% ggplot(aes(x = Variable, y = Value, fill = Species)) +
  geom_boxplot() +
  scale_fill_manual(values = c("blue","brown1")) +
  facet_wrap(~ patch, scales = "free") +
  theme_bw() +
  theme(legend.title = element_blank(), axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), strip.background = element_rect(color = "grey80", fill = "grey80"), strip.text = element_text(size = 16))

## Relative Reflectance: Species ##
specPlotData <- specData[which(specData$patch == "Side"),c("Bird_ID","patch","Species","relative_uv")]
specPlotData <- melt(specPlotData, id.vars = c("Bird_ID","patch","Species"), variable.name = "Variable", value.name = "Value")
specPlotData %>% ggplot(aes(x = Variable, y = Value, fill = Species)) +
  geom_boxplot() +
  scale_fill_manual(values = c("blue","brown1")) +
  facet_wrap(~ patch, scales = "free") +
  theme_bw() +
  theme(legend.title = element_blank(), axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), strip.background = element_rect(color = "grey80", fill = "grey80"), strip.text = element_text(size = 16))

## Relative Reflectance: Sex ##
specPlotData <- specData[which(specData$patch == "Cheek"),c("Bird_ID","patch","Sex","achieved_r","relative_mw","relative_uv")]
specPlotData <- melt(specPlotData, id.vars = c("Bird_ID","patch","Sex"), variable.name = "Variable", value.name = "Value")
specPlotData %>% ggplot(aes(x = Variable, y = Value, fill = Sex)) +
  geom_boxplot()

## Chroma ##
specPlotData <- specData[which(specData$patch == "Cheek" | specData$patch == "Side"),c("Bird_ID","patch","Species","achieved_r")]
specPlotData <- melt(specPlotData, id.vars = c("Bird_ID","patch","Species"), variable.name = "Variable", value.name = "Value")
specPlotData %>% ggplot(aes(x = Variable, y = Value, fill = Species)) +
  geom_boxplot() +
  facet_wrap(~ patch, scales = "free")

## COLOR ## (Point Fill: Prop.Nestlings, Julian.Date, Location)
pca_plot(NA, "Species", pcaSpecData, "Spectrometer", "SpeciesSex", NA)

##### MAPS #####

locations <- read.csv("Locations.csv")
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

boxes <- read.csv("Boxes.csv")
myLocation <- c(-105.6, 39.95, -105.2, 40.1)

myMap <- get_map(location = myLocation, source = "google", maptype ="terrain", zoom = 12, color = "bw")
ggmap(myMap) +
  geom_point(data = boxes, aes(x = LON, y = LAT, col = Species), size = 2.5, position = position_jitter(width = 0.015, height = 0.015)) +
  scale_color_manual(values = c("coral1","navy"))

##### Corrections #####
cameraSpecies <- c(0.0679,0.00479,0.5438,0.174,0.000101,0.000883,0.000000000000000665,0.64901,0.00001,0.0000000000000002,0.825,0.847,0.0000865,0.000152,0.133)
specSpecies <- c(0.000000776,0.000000341,0.0669,0.00000463,0.00271,0.883)
cameraSex <- c(0.888,0.153,0.97805,0.4,0.2042,0.415,0.833044,0.288,0.69244,0.4184,0.1154,0.827,0.954106,0.563163,0.1951,0.2486,0.414,0.6716,0.35,0.324,0.04137,0.2278,0.571898,0.8537,0.646,0.127,0.1106,0.2762)
specSex <- c(0.00657,0.628,0.888,0.0531,0.0462,0.0288,0.58,0.363,0.0222,0.0127,0.119,0.991,0.725,0.374)

cameraSpecies.fdr <- p.adjust(cameraSpecies, method = "fdr")
specSpecies.fdr <- p.adjust(specSpecies, method = "fdr")
cameraSex.fdr <- p.adjust(cameraSex, method = "fdr")
specSex.fdr <- p.adjust(specSex, method = "fdr")

