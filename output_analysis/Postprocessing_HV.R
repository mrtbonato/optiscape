
###################################################

# Title: Post-processing of optimization results
# Purpose: This code is used to post-process the optimization results
# Reference: 
# Author: Marta Bonato 
# Date: last modified on 25 April 2024

###################################################


# Required packages
library(mco)
library(plyr)
library(ggplot2)
library(viridis)



## Set working directories
# Working directory for land use map
setwd("Y:/Gruppen/cle/MichaS/Marta/Optimization_Analysis")
path = paste(getwd(),'DATA', sep="/")
path_input = paste(path, 'input',sep="/")
list.files(path_input)

# Working directory for optimization result
path_res1 = paste(getwd(),'CoMOLA_results_2202', sep="/")
list.files(path_res1)



# Upload and clean individual file
ind_all <- read.csv(paste0(path_res1,'/22-02-2024_14-37-39_individuals_file.csv'), h=F, skip=1, as.is=T)
ind_all.fitness <- data.frame(fit1 = as.numeric(gsub('\\[', '', ind_all$V3))*-1/max(as.numeric(gsub('\\[', '', ind_all$V3))),
                              fit2 = ind_all$V4*-1/max(ind_all$V4),
                              fit3 = -1/(ind_all$V5/max(ind_all$V5)),
                              fit4 = as.numeric(gsub('\\]', '', ind_all$V6))*-1/max(as.numeric(gsub('\\]', '', ind_all$V6))))

# Calculate hypervolume (HV)
ref <- matrix(data=0,nrow=1, ncol=4)
HV <- data.frame(matrix(data=NA,nrow=max(ind_all$V1)+1,ncol=1))

pF1 <- paretoFilter(as.matrix(ind_all.fitness[ind_all$V1==0,]))
HV[1,1] <- dominatedHypervolume(pF1, ref)

k=1

for(l in 2:151){
  pF <- paretoFilter(as.matrix(ind_all.fitness[ind_all$V1==k,]))
  pF_ <-paretoFilter(as.matrix(rbind(pF,pF1)))
  pF_ <- pF_[!duplicated(pF_),]
  HV[l,1] <- dominatedHypervolume(pF_, ref)
  pF1 <- pF_
  k=k+1
}


# Write table
#write.table(pF_, "BS_final.txt", col.names=T, row.names=F, quote=F)

### plot HV
#HV <- read.table("HV_CoMOLA.txt", sep="", h=T)
labs <- c("0","20","40", "60", "80", "100")
#pdf("HV_CoMOLA.pdf", height = 4.25, width=3.75)
plot(HV[,1], yaxt="n", xaxt="n", xlab="N° of generations", ylab ="Hypervolume (HV)", cex = 2, cex.lab =1.2)
axis(2, las=1, cex.axis = 1.2)
axis(1, at=seq(0,100,20), labels=labs, cex.axis = 1.2)
#dev.off()





# Run only until jump in evolution
# ref <- matrix(data=0,nrow=1, ncol=4)
# HV_b <- data.frame(matrix(data=NA,nrow=max(ind_all$V1)+1,ncol=1))
# 
# pF_b1 <- paretoFilter(as.matrix(ind_all.fitness[ind_all$V1==0,]))
# HV_b[1,1] <- dominatedHypervolume(pF_b1, ref)
# 
# k=1
# 
# for(l in 2:100){
#   pF_b <- paretoFilter(as.matrix(ind_all.fitness[ind_all$V1==k,]))
#   pF_b_ <-paretoFilter(as.matrix(rbind(pF_b,pF_b1)))
#   pF_b_ <- pF_b_[!duplicated(pF_b_),]
#   HV_b[l,1] <- dominatedHypervolume(pF_b_, ref)
#   pF_b1 <- pF_b_
#   k=k+1
# }
# 
# write.table(pF_b_, "BS_beforejump.txt", col.names=T, row.names=F, quote=F)

#write.table(HV, "HV_CoMOLA.txt", col.names=T, row.names=F, quote=F)








################################################################################


# Merge Best Solutions coming from different optimization

# Required packages
library(tidyverse) 


## Set working directories
# Working directory for optimization result
# Working directory for optimization result
path_res1 = paste(getwd(),'CoMOLA_results_2202', sep="/")
list.files(path_res1)

path_res2 = paste(getwd(),'CoMOLA_results_2603', sep="/")
list.files(path_res2)


# Upload individual files
ind_all <- read.csv(paste0(path_res1,'/22-02-2024_14-37-39_individuals_file.csv'), h=F, skip=1, as.is=T)
ind_all2 <- read.csv(paste0(path_res2,'/26-03-2024_18-19-47_individuals_file.csv'), h=F, skip=1, as.is=T)

# modify number of generations in results second optimization
# based on numebr of generation of first optimization run
ind_all2 <- ind_all2 %>%
  mutate(V1 = V1 + 151)

# merge
ind_all_merge <- ind_all %>%
  bind_rows(ind_all2)


# Clean individual file
ind_all.fitness_merge <- data.frame(fit1 = as.numeric(gsub('\\[', '', ind_all_merge$V3))*-1/max(as.numeric(gsub('\\[', '', ind_all_merge$V3))),
                               fit2 = ind_all_merge$V4*-1/max(ind_all_merge$V4),
                               fit3 = -1/(ind_all_merge$V5/max(ind_all_merge$V5)),
                               fit4 = as.numeric(gsub('\\]', '', ind_all_merge$V6))*-1/max(as.numeric(gsub('\\]', '', ind_all_merge$V6))))



# Calculate HV
ref <- matrix(data=0,nrow=1, ncol=4)
HV <- data.frame(matrix(data=NA,nrow=max(ind_all_merge$V1)+1,ncol=1))

pF1 <- paretoFilter(as.matrix(ind_all.fitness_merge[ind_all_merge$V1==0,]))
HV[1,1] <- dominatedHypervolume(pF1, ref)

k=1

for(l in 2:202){
  pF <- paretoFilter(as.matrix(ind_all.fitness_merge[ind_all_merge$V1==k,]))
  pF_ <-paretoFilter(as.matrix(rbind(pF,pF1)))
  pF_ <- pF_[!duplicated(pF_),]
  HV[l,1] <- dominatedHypervolume(pF_, ref)
  pF1 <- pF_
  k=k+1
}


# Write table
#write.table(pF_b_, "BS_beforejump.txt", col.names=T, row.names=F, quote=F)

#write.table(HV, "HV_CoMOLA.txt", col.names=T, row.names=F, quote=F)


### plot HV
#HV <- read.table("HV_CoMOLA.txt", sep="", h=T)
labs <- c("0","25","50","75","100","125","150","175","200")
#pdf("HV_CoMOLA_merge.pdf", height = 4.25, width=3.75)
plot(HV[,1], yaxt="n", xaxt="n", xlab="N° of generations", ylab ="Hypervolume (HV)", cex = 2, cex.lab =1.2)
axis(2, las=1, cex.axis = 1.2)
axis(1, at=seq(0,200,25), labels=labs, cex.axis = 1.2)
#dev.off()







# Retrieve fitness and genome of optimal solutions
pF1_copy <- as.data.frame(pF1)
pF1_copy$join_ID <- paste(pF1_copy$fit1, pF1_copy$fit2, pF1_copy$fit3, pF1_copy$fit4, sep='_')


ind_all_code <- ind_all %>%
mutate(fit1 = as.numeric(gsub('\\[', '', ind_all$V3))*-1/max(as.numeric(gsub('\\[', '', ind_all$V3))),
           fit2 = ind_all$V4*-1/max(ind_all$V4),
           fit3 = -1/(ind_all$V5/max(ind_all$V5)),
           fit4 = as.numeric(gsub('\\]', '', ind_all$V6))*-1/max(as.numeric(gsub('\\]', '', ind_all$V6))))

ind_all_code$join_ID <- paste(ind_all_code$fit1, ind_all_code$fit2, ind_all_code$fit3, ind_all_code$fit4, sep='_')


# eliminate duplicates
ind_all_unique <- ind_all_code[!duplicated(ind_all_code[,c('join_ID')]),]


# Join
Best_solutions_merge <- pF1_copy %>%
  left_join(ind_all_unique, by = "join_ID")
# Use this file for analysis of pareto frontier or frequency of AEP implementation


