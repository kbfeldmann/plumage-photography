library(stringr)

setwd("/Volumes/Feldmann_Research/Taylor_Lab_Honors_Thesis/RAW_Photos/Museum_Specimens/DMNS")
folders <- list.dirs("/Volumes/Feldmann_Research/Taylor_Lab_Honors_Thesis/RAW_Photos/Museum_Specimens/DMNS", full.names = F ,recursive = F)

areaList <- list()
for (i in folders){
  if (file.exists(paste(i,"/",i,"_areaEYEBROW.csv",sep = ""))){
    data <- read.csv(paste(i,"/",i,"_areaEYEBROW.csv",sep = ""))
    #chord1 <- data[grep(paste(i,"_LSide_Eyebrow.tif:wingChord",sep = ""),data$Label),"Area"]
    #chord2 <- data[grep(paste(i,"_RSide_Eyebrow.tif:wingChord", sep = ""),data$Label),"Area"]
    #averageChord <- (chord1 + chord2)/2
    #data[,6] <- averageChord
    areaList[[i]] <- data
  }
}
areaData <- do.call(rbind, areaList)

scaleTest <- areaData[grep("scaleTest", areaData$Label),]
min(scaleTest$Length)
max(scaleTest$Length)

## File Processing
areaData[grep("scaleTest", areaData$Label),] <- NA
areaData <- na.omit(areaData)
areaData$Specimen_ID <- str_match(areaData$Label, "MOCH[0-9]*")
areaData$Side <- str_sub(str_match(areaData$Label, "_\\w*_"), 2, -2)
areaData$Patch <- str_sub(str_match(areaData$Label, ":.*"), 2)
modAreaData <- areaData[,c("Specimen_ID", "Side", "Patch", "Area")]
rownames(modAreaData) <- c()

write.csv(modAreaData, "/Users/katherinefeldmann/Desktop/CU_Boulder/Taylor_Lab/Honors Thesis/R Code/DMNS_area_eyebrow.csv")
