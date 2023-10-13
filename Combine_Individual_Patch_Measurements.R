library(stringr)

setwd("/Volumes/KBF_Backup/Taylor_Lab_Honors_Thesis/RAW_Photos_Revised/Museum_Specimens/")
folders <- list.dirs("/Volumes/KBF_Backup/Taylor_Lab_Honors_Thesis/RAW_Photos_Revised/Museum_Specimens", full.names = F ,recursive = T)

clusterData <- data.frame()
individualData <- data.frame()
for (i in folders){
  if(substr(i, str_length(i)-3, str_length(i)) == "Side" || substr(i, str_length(i)-3, str_length(i)) == "Back" || substr(i, str_length(i)-4, str_length(i)) == "Belly"){
    clusterdata <- read.csv(paste(i,"/Cluster Particle Analysis Summary Results.csv", sep = ""))
    clusterdata$Individual <- i
    indivdata <- read.csv(paste(i,"/Individual Particle Results.csv",sep = ""))
    indivdata$Individual <- i
  }
  clusterData <- rbind(clusterData, clusterdata)
  individualData <- rbind(individualData, indivdata)
}
write.csv(clusterData, file = "Cluster_Particle_Results.csv")
write.csv(individualData, file = "Individual_Particle_Results.csv")
