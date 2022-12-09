# Dan Bobkov, 2022, email: bobkov@incras.ru
# Image analysis of intravital
# immunofluorescense confocal data of brain tumors
options(scipen=.1)
# Import libraries
#install.packages("BiocManager") 
#BiocManager::install("EBImage")
library(EBImage)
#library(xlsx)
library(raster)
library(ggpubr)
library(readxl)
#library(rJava)
library(xlsxjars)
library(DescTools)
library(ggplot2)
library(dplyr)
library(xlsx)
library(ggsignif)
library(PMCMRplus)
library(tidyverse)
library(EnvStats)

########################################
############# LOAD IMAGES ##############
########################################

# Choose dir function for mac <<<<<<<<<<<<<<<<<<
choose.dir <- function() {
  system("osascript -e 'tell app \"R\" to POSIX path of (choose folder with prompt \"Choose Folder:\")' > /tmp/R_folder",
         intern = FALSE, ignore.stderr = TRUE)
  p <- system("cat /tmp/R_folder && rm -f /tmp/R_folder", intern = TRUE)
  return(ifelse(length(p), p, NA))
}

##### DATA FRAME with pixel intensities

df_median_mean <- data.frame(name = character(),
                             zone = character(),
                             red_median = integer(),
                             green_median = integer(),
                             blue_median = integer(),
                             red_mean = integer(),
                             green_mean = integer(),
                             blue_mean = integer())

##### Images importing from local dirs ##########
#################################################

# Choose dir ---------

file_list_NegativeControl_AAA160221 <- list.files(path = , choose.dir(),
                                       pattern = "tif",
                                       all.files = FALSE,
                                       full.names = TRUE, recursive = TRUE,
                                       ignore.case = FALSE, include.dirs = FALSE,
                                       no.. = FALSE)


file_list_ANI09041964_NZ <- list.files(path = , choose.dir(),
                                       pattern = "tif",
                                       all.files = FALSE,
                                       full.names = TRUE, recursive = TRUE,
                                       ignore.case = FALSE, include.dirs = FALSE,
                                       no.. = FALSE)

file_list_ANI09041964_PZ <- list.files(path = , choose.dir(),
                                       pattern = "tif",
                                       all.files = FALSE,
                                       full.names = TRUE, recursive = TRUE,
                                       ignore.case = FALSE, include.dirs = FALSE,
                                       no.. = FALSE)

file_list_AUS300805_Norm <- list.files(path = , choose.dir(),
                                       pattern = "tif",
                                       all.files = FALSE,
                                       full.names = TRUE, recursive = TRUE,
                                       ignore.case = FALSE, include.dirs = FALSE,
                                       no.. = FALSE)

file_list_GML150219_CZ <- list.files(path = , choose.dir(),
                                       pattern = "tif",
                                       all.files = FALSE,
                                       full.names = TRUE, recursive = TRUE,
                                       ignore.case = FALSE, include.dirs = FALSE,
                                       no.. = FALSE)

file_list_GNG19101961_CZ <- list.files(path = , choose.dir(),
                                       pattern = "tif",
                                       all.files = FALSE,
                                       full.names = TRUE, recursive = TRUE,
                                       ignore.case = FALSE, include.dirs = FALSE,
                                       no.. = FALSE)

file_list_GNG19101961_NZ <- list.files(path = , choose.dir(),
                                     pattern = "tif",
                                     all.files = FALSE,
                                     full.names = TRUE, recursive = TRUE,
                                     ignore.case = FALSE, include.dirs = FALSE,
                                     no.. = FALSE)

file_list_GNG19101961_PZ <- list.files(path = , choose.dir(),
                                       pattern = "tif",
                                       all.files = FALSE,
                                       full.names = TRUE, recursive = TRUE,
                                       ignore.case = FALSE, include.dirs = FALSE,
                                       no.. = FALSE)

file_list_GZhE27042021_CZ <- list.files(path = , choose.dir(),
                                       pattern = "tif",
                                       all.files = FALSE,
                                       full.names = TRUE, recursive = TRUE,
                                       ignore.case = FALSE, include.dirs = FALSE,
                                       no.. = FALSE)

file_list_GZhE27042021_NZ <- list.files(path = , choose.dir(),
                                        pattern = "tif",
                                        all.files = FALSE,
                                        full.names = TRUE, recursive = TRUE,
                                        ignore.case = FALSE, include.dirs = FALSE,
                                        no.. = FALSE)

file_list_GZhE27042021_PZ <- list.files(path = , choose.dir(),
                                        pattern = "tif",
                                        all.files = FALSE,
                                        full.names = TRUE, recursive = TRUE,
                                        ignore.case = FALSE, include.dirs = FALSE,
                                        no.. = FALSE)

file_list_HTA210412_CZ <- list.files(path = , choose.dir(),
                                        pattern = "tif",
                                        all.files = FALSE,
                                        full.names = TRUE, recursive = TRUE,
                                        ignore.case = FALSE, include.dirs = FALSE,
                                        no.. = FALSE)

file_list_HTA210412_PZ <- list.files(path = , choose.dir(),
                                     pattern = "tif",
                                     all.files = FALSE,
                                     full.names = TRUE, recursive = TRUE,
                                     ignore.case = FALSE, include.dirs = FALSE,
                                     no.. = FALSE)


file_list_KAS18052021_CZ <- list.files(path = , choose.dir(),
                                     pattern = "tif",
                                     all.files = FALSE,
                                     full.names = TRUE, recursive = TRUE,
                                     ignore.case = FALSE, include.dirs = FALSE,
                                     no.. = FALSE)

file_list_KAS18052021_PZ <- list.files(path = , choose.dir(),
                                       pattern = "tif",
                                       all.files = FALSE,
                                       full.names = TRUE, recursive = TRUE,
                                       ignore.case = FALSE, include.dirs = FALSE,
                                       no.. = FALSE)

file_list_MVI18021961_CZ <- list.files(path = , choose.dir(),
                                       pattern = "tif",
                                       all.files = FALSE,
                                       full.names = TRUE, recursive = TRUE,
                                       ignore.case = FALSE, include.dirs = FALSE,
                                       no.. = FALSE)

file_list_MVI18021961_NZ <- list.files(path = , choose.dir(),
                                       pattern = "tif",
                                       all.files = FALSE,
                                       full.names = TRUE, recursive = TRUE,
                                       ignore.case = FALSE, include.dirs = FALSE,
                                       no.. = FALSE)

file_list_MVI18021961_PZ <- list.files(path = , choose.dir(),
                                       pattern = "tif",
                                       all.files = FALSE,
                                       full.names = TRUE, recursive = TRUE,
                                       ignore.case = FALSE, include.dirs = FALSE,
                                       no.. = FALSE)

file_list_SRA09062005_Norm_Control <- list.files(path = , choose.dir(),
                                       pattern = "tif",
                                       all.files = FALSE,
                                       full.names = TRUE, recursive = TRUE,
                                       ignore.case = FALSE, include.dirs = FALSE,
                                       no.. = FALSE)

file_list_SSF210158_CZ <- list.files(path = , choose.dir(),
                                                 pattern = "tif",
                                                 all.files = FALSE,
                                                 full.names = TRUE, recursive = TRUE,
                                                 ignore.case = FALSE, include.dirs = FALSE,
                                                 no.. = FALSE)

file_list_SSF210158_NZ <- list.files(path = , choose.dir(),
                                     pattern = "tif",
                                     all.files = FALSE,
                                     full.names = TRUE, recursive = TRUE,
                                     ignore.case = FALSE, include.dirs = FALSE,
                                     no.. = FALSE)

file_list_SSF210158_PZ <- list.files(path = , choose.dir(),
                                     pattern = "tif",
                                     all.files = FALSE,
                                     full.names = TRUE, recursive = TRUE,
                                     ignore.case = FALSE, include.dirs = FALSE,
                                     no.. = FALSE)



file_list_SVE04081947_CZ <- list.files(path = , choose.dir(),
                                     pattern = "tif",
                                     all.files = FALSE,
                                     full.names = TRUE, recursive = TRUE,
                                     ignore.case = FALSE, include.dirs = FALSE,
                                     no.. = FALSE)

file_list_SVE04081947_NZ <- list.files(path = , choose.dir(),
                                       pattern = "tif",
                                       all.files = FALSE,
                                       full.names = TRUE, recursive = TRUE,
                                       ignore.case = FALSE, include.dirs = FALSE,
                                       no.. = FALSE)

file_list_SVE04081947_PZ <- list.files(path = , choose.dir(),
                                       pattern = "tif",
                                       all.files = FALSE,
                                       full.names = TRUE, recursive = TRUE,
                                       ignore.case = FALSE, include.dirs = FALSE,
                                       no.. = FALSE)



file_list_VIR140421_CZ <- list.files(path = , choose.dir(),
                                       pattern = "tif",
                                       all.files = FALSE,
                                       full.names = TRUE, recursive = TRUE,
                                       ignore.case = FALSE, include.dirs = FALSE,
                                       no.. = FALSE)

file_list_VIR140421_NZ <- list.files(path = , choose.dir(),
                                     pattern = "tif",
                                     all.files = FALSE,
                                     full.names = TRUE, recursive = TRUE,
                                     ignore.case = FALSE, include.dirs = FALSE,
                                     no.. = FALSE)

file_list_VIR140421_PZ <- list.files(path = , choose.dir(),
                                     pattern = "tif",
                                     all.files = FALSE,
                                     full.names = TRUE, recursive = TRUE,
                                     ignore.case = FALSE, include.dirs = FALSE,
                                     no.. = FALSE)

file_list_EAA200917_NZ <- list.files(path = , choose.dir(),
                                     pattern = "tif",
                                     all.files = FALSE,
                                     full.names = TRUE, recursive = TRUE,
                                     ignore.case = FALSE, include.dirs = FALSE,
                                     no.. = FALSE)

file_list_EAA200917_PZ <- list.files(path = , choose.dir(),
                                     pattern = "tif",
                                     all.files = FALSE,
                                     full.names = TRUE, recursive = TRUE,
                                     ignore.case = FALSE, include.dirs = FALSE,
                                     no.. = FALSE)

file_list_LIM22031967_CZ <- list.files(path = , choose.dir(),
                                     pattern = "tif",
                                     all.files = FALSE,
                                     full.names = TRUE, recursive = TRUE,
                                     ignore.case = FALSE, include.dirs = FALSE,
                                     no.. = FALSE)

file_list_LIM22031967_NZ <- list.files(path = , choose.dir(),
                             pattern = "tif",
                             all.files = FALSE,
                             full.names = TRUE, recursive = TRUE,
                             ignore.case = FALSE, include.dirs = FALSE,
                             no.. = FALSE)

file_list_GEN26011949_CZ <- list.files(path = , choose.dir(),
                             pattern = "tif",
                             all.files = FALSE,
                             full.names = TRUE, recursive = TRUE,
                             ignore.case = FALSE, include.dirs = FALSE,
                             no.. = FALSE)

file_list_GEN26011949_NZ <- list.files(path = , choose.dir(),
                             pattern = "tif",
                             all.files = FALSE,
                             full.names = TRUE, recursive = TRUE,
                             ignore.case = FALSE, include.dirs = FALSE,
                             no.. = FALSE)


###################################
############# Read Images #########
############# Call to function ####
############# Display histogram ###
###################################

######## NegativeControl_AAA160221 ########
temp_name <- as.character()

for (i in 1:length(file_list_NegativeControl_AAA160221)) {
  temp_name <- c(temp_name, as.character(paste('NegativeControl_AAA160221', i, sep = '_')))}


for (nm in temp_name) assign(nm, 
                             readImage(file_list_NegativeControl_AAA160221[match(nm, temp_name)]), 
                             .GlobalEnv)

for (i in 1:length(file_list_NegativeControl_AAA160221)) {
  
  tmp <- data.frame(name = 'AAA160221', # average pixel values in
                    zone = 'NegativeControl',
                    red_median =   apply(get(temp_name[i]), 3, median)[1], # Red
                    green_median = apply(get(temp_name[i]), 3, median)[2], # Green
                    blue_median =  apply(get(temp_name[i]), 3, median)[3],
                    red_mean =   apply(get(temp_name[i]), 3, mean)[1], # Red
                    green_mean = apply(get(temp_name[i]), 3, mean)[2], # Green
                    blue_mean =  apply(get(temp_name[i]), 3, mean)[3])
  
  df_median_mean <- rbind(df_median_mean, tmp)}

NegativeControl_AAA160221 <- 0

for (i in 1:length(file_list_NegativeControl_AAA160221)) {
  NegativeControl_AAA160221 <- NegativeControl_AAA160221 + get(temp_name[i])}

NegativeControl_AAA160221 <- NegativeControl_AAA160221/length(file_list_NegativeControl_AAA160221)

hist(NegativeControl_AAA160221, 
     breaks = 200,
     main = "NegativeControl_AAA160221 (n=15)",
     ylim = c(0, 500000),
     xlim = c(0, .1),
     lwd = 1.7)

dev.print(png, 
          'NegativeControl_AAA160221.png',
          width=300)


######## ANI09041964_NZ ########
temp_name <- as.character()

for (i in 1:length(file_list_ANI09041964_NZ)) {
  temp_name <- c(temp_name, as.character(paste('ANI09041964_NZ', i, sep = '_')))}


for (nm in temp_name) assign(nm, 
                             readImage(file_list_ANI09041964_NZ[match(nm, temp_name)]), 
                             .GlobalEnv)

for (i in 1:length(file_list_ANI09041964_NZ)) {
  
  tmp <- data.frame(name = 'ANI09041964', # average pixel values in
                    zone = 'Necrosis',
                    red_median =   apply(get(temp_name[i]), 3, median)[1], # Red
                    green_median = apply(get(temp_name[i]), 3, median)[2], # Green
                    blue_median =  apply(get(temp_name[i]), 3, median)[3],
                    red_mean =   apply(get(temp_name[i]), 3, mean)[1], # Red
                    green_mean = apply(get(temp_name[i]), 3, mean)[2], # Green
                    blue_mean =  apply(get(temp_name[i]), 3, mean)[3])
  
  df_median_mean <- rbind(df_median_mean, tmp)}

ANI09041964_NZ <- 0

for (i in 1:length(file_list_ANI09041964_NZ)) {
  ANI09041964_NZ <- ANI09041964_NZ + get(temp_name[i])}

ANI09041964_NZ <- ANI09041964_NZ/length(file_list_ANI09041964_NZ)

hist(ANI09041964_NZ, 
     breaks = 200,
     main = "ANI09041964_NZ (n=13)",
     ylim = c(0, 500000),
     xlim = c(0, .1),
     lwd = 1.7)

dev.print(png, 
          'ANI09041964_NZ.png',
          width=300)



######## ANI09041964_PZ ########
temp_name <- as.character()

for (i in 1:length(file_list_ANI09041964_PZ)) {
  temp_name <- c(temp_name, as.character(paste('ANI09041964_PZ', i, sep = '_')))}


for (nm in temp_name) assign(nm, 
                             readImage(file_list_ANI09041964_PZ[match(nm, temp_name)]), 
                             .GlobalEnv)

for (i in 1:length(file_list_ANI09041964_PZ)) {
  
  tmp <- data.frame(name = 'ANI09041964', # average pixel values in
                    zone = 'Perifocal',
                    red_median =   apply(get(temp_name[i]), 3, median)[1], # Red
                    green_median = apply(get(temp_name[i]), 3, median)[2], # Green
                    blue_median =  apply(get(temp_name[i]), 3, median)[3],
                    red_mean =   apply(get(temp_name[i]), 3, mean)[1], # Red
                    green_mean = apply(get(temp_name[i]), 3, mean)[2], # Green
                    blue_mean =  apply(get(temp_name[i]), 3, mean)[3])
  
  df_median_mean <- rbind(df_median_mean, tmp)}

ANI09041964_PZ <- 0

for (i in 1:length(file_list_ANI09041964_PZ)) {
  ANI09041964_PZ <- ANI09041964_PZ + get(temp_name[i])}

ANI09041964_PZ <- ANI09041964_PZ/length(file_list_ANI09041964_PZ)

hist(ANI09041964_PZ, 
     breaks = 200,
     main = "ANI09041964_PZ (n=11)",
     ylim = c(0, 500000),
     xlim = c(0, .1),
     lwd = 1.7)

dev.print(png, 
          'ANI09041964_PZ.png',
          width=300)

### hist all zones ###
ANI09041964 <- (ANI09041964_NZ + ANI09041964_PZ)/2

hist(ANI09041964, 
     breaks = 200,
     main = "ANI09041964 (n=24)",
     ylim = c(0, 500000),
     xlim = c(0, .1),
     lwd = 1.7)

dev.print(png, 
          'ANI09041964.png',
          width=300)
#####################

######## AUS300805_Norm ########
temp_name <- as.character()

for (i in 1:length(file_list_AUS300805_Norm)) {
  temp_name <- c(temp_name, as.character(paste('AUS300805_Norm', i, sep = '_')))}


for (nm in temp_name) assign(nm, 
                             readImage(file_list_AUS300805_Norm[match(nm, temp_name)]), 
                             .GlobalEnv)

for (i in 1:length(file_list_AUS300805_Norm)) {
  
  tmp <- data.frame(name = 'AUS300805', # average pixel values in
                    zone = 'Norm',
                    red_median =   apply(get(temp_name[i]), 3, median)[1], # Red
                    green_median = apply(get(temp_name[i]), 3, median)[2], # Green
                    blue_median =  apply(get(temp_name[i]), 3, median)[3],
                    red_mean =   apply(get(temp_name[i]), 3, mean)[1], # Red
                    green_mean = apply(get(temp_name[i]), 3, mean)[2], # Green
                    blue_mean =  apply(get(temp_name[i]), 3, mean)[3])
  
  df_median_mean <- rbind(df_median_mean, tmp)}

AUS300805_Norm <- 0

for (i in 1:length(file_list_AUS300805_Norm)) {
  AUS300805_Norm <- AUS300805_Norm + get(temp_name[i])}

AUS300805_Norm <- AUS300805_Norm/length(file_list_AUS300805_Norm)

hist(AUS300805_Norm, 
     breaks = 200,
     main = "AUS300805_Norm (n=20)",
     ylim = c(0, 500000),
     xlim = c(0, .1),
     lwd = 1.7)

dev.print(png, 
          'AUS300805_Norm.png',
          width=300)

######## GML150219_CZ ########
temp_name <- as.character()

for (i in 1:length(file_list_GML150219_CZ)) {
  temp_name <- c(temp_name, as.character(paste('GML150219_CZ', i, sep = '_')))}


for (nm in temp_name) assign(nm, 
                             readImage(file_list_GML150219_CZ[match(nm, temp_name)]), 
                             .GlobalEnv)

for (i in 1:length(file_list_GML150219_CZ)) {
  
  tmp <- data.frame(name = 'GML150219', # average pixel values in
                    zone = 'Contrast',
                    red_median =   apply(get(temp_name[i]), 3, median)[1], # Red
                    green_median = apply(get(temp_name[i]), 3, median)[2], # Green
                    blue_median =  apply(get(temp_name[i]), 3, median)[3],
                    red_mean =   apply(get(temp_name[i]), 3, mean)[1], # Red
                    green_mean = apply(get(temp_name[i]), 3, mean)[2], # Green
                    blue_mean =  apply(get(temp_name[i]), 3, mean)[3])
  
  df_median_mean <- rbind(df_median_mean, tmp)}

GML150219_CZ <- 0

for (i in 1:length(file_list_GML150219_CZ)) {
  GML150219_CZ <- GML150219_CZ + get(temp_name[i])}

GML150219_CZ <- GML150219_CZ/length(file_list_GML150219_CZ)

hist(GML150219_CZ, 
     breaks = 200,
     main = "GML150219_CZ (n=17)",
     ylim = c(0, 500000),
     xlim = c(0, .1),
     lwd = 1.7)

dev.print(png, 
          'GML150219_CZ.png',
          width=300)

######## GNG19101961_CZ ########
temp_name <- as.character()

for (i in 1:length(file_list_GNG19101961_CZ)) {
  temp_name <- c(temp_name, as.character(paste('GNG19101961_CZ', i, sep = '_')))}


for (nm in temp_name) assign(nm, 
                             readImage(file_list_GNG19101961_CZ[match(nm, temp_name)]), 
                             .GlobalEnv)

for (i in 1:length(file_list_GNG19101961_CZ)) {
  
  tmp <- data.frame(name = 'GNG19101961', # average pixel values in
                    zone = 'Contrast',
                    red_median =   apply(get(temp_name[i]), 3, median)[1], # Red
                    green_median = apply(get(temp_name[i]), 3, median)[2], # Green
                    blue_median =  apply(get(temp_name[i]), 3, median)[3],
                    red_mean =   apply(get(temp_name[i]), 3, mean)[1], # Red
                    green_mean = apply(get(temp_name[i]), 3, mean)[2], # Green
                    blue_mean =  apply(get(temp_name[i]), 3, mean)[3])
  
  df_median_mean <- rbind(df_median_mean, tmp)}

GNG19101961_CZ <- 0

for (i in 1:length(file_list_GNG19101961_CZ)) {
  GNG19101961_CZ <- GNG19101961_CZ + get(temp_name[i])}

GNG19101961_CZ <- GNG19101961_CZ/length(file_list_GNG19101961_CZ)

hist(GNG19101961_CZ, 
     breaks = 200,
     main = "GNG19101961_CZ (n=20)",
     ylim = c(0, 500000),
     xlim = c(0, .1),
     lwd = 1.7)

dev.print(png, 
          'GNG19101961_CZ.png',
          width=300)

######## GNG19101961_NZ ########
temp_name <- as.character()

for (i in 1:length(file_list_GNG19101961_NZ)) {
  temp_name <- c(temp_name, as.character(paste('GNG19101961_NZ', i, sep = '_')))}


for (nm in temp_name) assign(nm, 
                             readImage(file_list_GNG19101961_NZ[match(nm, temp_name)]), 
                             .GlobalEnv)

for (i in 1:length(file_list_GNG19101961_NZ)) {
  
  tmp <- data.frame(name = 'GNG19101961', # average pixel values in
                    zone = 'Necrosis',
                    red_median =   apply(get(temp_name[i]), 3, median)[1], # Red
                    green_median = apply(get(temp_name[i]), 3, median)[2], # Green
                    blue_median =  apply(get(temp_name[i]), 3, median)[3],
                    red_mean =   apply(get(temp_name[i]), 3, mean)[1], # Red
                    green_mean = apply(get(temp_name[i]), 3, mean)[2], # Green
                    blue_mean =  apply(get(temp_name[i]), 3, mean)[3])
  
  df_median_mean <- rbind(df_median_mean, tmp)}

GNG19101961_NZ <- 0

for (i in 1:length(file_list_GNG19101961_NZ)) {
  GNG19101961_NZ <- GNG19101961_NZ + get(temp_name[i])}

GNG19101961_NZ <- GNG19101961_NZ/length(file_list_GNG19101961_NZ)

hist(GNG19101961_NZ, 
     breaks = 200,
     main = "GNG19101961_NZ (n=6)",
     ylim = c(0, 500000),
     xlim = c(0, .1),
     lwd = 1.7)

dev.print(png, 
          'GNG19101961_NZ.png',
          width=300)

######## GNG19101961_PZ ########
temp_name <- as.character()

for (i in 1:length(file_list_GNG19101961_PZ)) {
  temp_name <- c(temp_name, as.character(paste('GNG19101961_PZ', i, sep = '_')))}


for (nm in temp_name) assign(nm, 
                             readImage(file_list_GNG19101961_PZ[match(nm, temp_name)]), 
                             .GlobalEnv)

for (i in 1:length(file_list_GNG19101961_PZ)) {
  
  tmp <- data.frame(name = 'GNG19101961', # average pixel values in
                    zone = 'Perifocal',
                    red_median =   apply(get(temp_name[i]), 3, median)[1], # Red
                    green_median = apply(get(temp_name[i]), 3, median)[2], # Green
                    blue_median =  apply(get(temp_name[i]), 3, median)[3],
                    red_mean =   apply(get(temp_name[i]), 3, mean)[1], # Red
                    green_mean = apply(get(temp_name[i]), 3, mean)[2], # Green
                    blue_mean =  apply(get(temp_name[i]), 3, mean)[3])
  
  df_median_mean <- rbind(df_median_mean, tmp)}

GNG19101961_PZ <- 0

for (i in 1:length(file_list_GNG19101961_PZ)) {
  GNG19101961_PZ <- GNG19101961_PZ + get(temp_name[i])}

GNG19101961_PZ <- GNG19101961_PZ/length(file_list_GNG19101961_PZ)

hist(GNG19101961_PZ, 
     breaks = 200,
     main = "GNG19101961_PZ (n=18)",
     ylim = c(0, 500000),
     xlim = c(0, .1),
     lwd = 1.7)

dev.print(png, 
          'GNG19101961_PZ.png',
          width=300)

### hist all zones ###
GNG19101961 <- (GNG19101961_CZ + GNG19101961_NZ + GNG19101961_PZ)/3

hist(GNG19101961, 
     breaks = 200,
     main = "GNG19101961 (n=44)",
     ylim = c(0, 500000),
     xlim = c(0, .1),
     lwd = 1.7)

dev.print(png, 
          'GNG19101961.png',
          width=300)
#####################

######## GZhE27042021_CZ ########
temp_name <- as.character()

for (i in 1:length(file_list_GZhE27042021_CZ)) {
  temp_name <- c(temp_name, as.character(paste('GZhE27042021_CZ', i, sep = '_')))}


for (nm in temp_name) assign(nm, 
                             readImage(file_list_GZhE27042021_CZ[match(nm, temp_name)]), 
                             .GlobalEnv)

for (i in 1:length(file_list_GZhE27042021_CZ)) {
  
  tmp <- data.frame(name = 'GZhE27042021', # average pixel values in
                    zone = 'Contrast',
                    red_median =   apply(get(temp_name[i]), 3, median)[1], # Red
                    green_median = apply(get(temp_name[i]), 3, median)[2], # Green
                    blue_median =  apply(get(temp_name[i]), 3, median)[3],
                    red_mean =   apply(get(temp_name[i]), 3, mean)[1], # Red
                    green_mean = apply(get(temp_name[i]), 3, mean)[2], # Green
                    blue_mean =  apply(get(temp_name[i]), 3, mean)[3])
  
  df_median_mean <- rbind(df_median_mean, tmp)}

GZhE27042021_CZ <- 0

for (i in 1:length(file_list_GZhE27042021_CZ)) {
  GZhE27042021_CZ <- GZhE27042021_CZ + get(temp_name[i])}

GZhE27042021_CZ <- GZhE27042021_CZ/length(file_list_GZhE27042021_CZ)

hist(GZhE27042021_CZ, 
     breaks = 200,
     main = "GZhE27042021_CZ (n=15)",
     ylim = c(0, 500000),
     xlim = c(0, .1),
     lwd = 1.7)

dev.print(png, 
          'GZhE27042021_CZ.png',
          width=300)

######## GZhE27042021_NZ ########
temp_name <- as.character()

for (i in 1:length(file_list_GZhE27042021_NZ)) {
  temp_name <- c(temp_name, as.character(paste('GZhE27042021_NZ', i, sep = '_')))}


for (nm in temp_name) assign(nm, 
                             readImage(file_list_GZhE27042021_NZ[match(nm, temp_name)]), 
                             .GlobalEnv)

for (i in 1:length(file_list_GZhE27042021_NZ)) {
  
  tmp <- data.frame(name = 'GZhE27042021', # average pixel values in
                    zone = 'Necrosis',
                    red_median =   apply(get(temp_name[i]), 3, median)[1], # Red
                    green_median = apply(get(temp_name[i]), 3, median)[2], # Green
                    blue_median =  apply(get(temp_name[i]), 3, median)[3],
                    red_mean =   apply(get(temp_name[i]), 3, mean)[1], # Red
                    green_mean = apply(get(temp_name[i]), 3, mean)[2], # Green
                    blue_mean =  apply(get(temp_name[i]), 3, mean)[3])
  
  df_median_mean <- rbind(df_median_mean, tmp)}

GZhE27042021_NZ <- 0

for (i in 1:length(file_list_GZhE27042021_NZ)) {
  GZhE27042021_NZ <- GZhE27042021_NZ + get(temp_name[i])}

GZhE27042021_NZ <- GZhE27042021_NZ/length(file_list_GZhE27042021_NZ)

hist(GZhE27042021_NZ, 
     breaks = 200,
     main = "GZhE27042021_NZ (n=18)",
     ylim = c(0, 500000),
     xlim = c(0, .1),
     lwd = 1.7)

dev.print(png, 
          'GZhE27042021_NZ.png',
          width=300)

######## GZhE27042021_PZ ########
temp_name <- as.character()

for (i in 1:length(file_list_GZhE27042021_PZ)) {
  temp_name <- c(temp_name, as.character(paste('GZhE27042021_PZ', i, sep = '_')))}


for (nm in temp_name) assign(nm, 
                             readImage(file_list_GZhE27042021_PZ[match(nm, temp_name)]), 
                             .GlobalEnv)

for (i in 1:length(file_list_GZhE27042021_PZ)) {
  
  tmp <- data.frame(name = 'GZhE27042021', # average pixel values in
                    zone = 'Perifocal',
                    red_median =   apply(get(temp_name[i]), 3, median)[1], # Red
                    green_median = apply(get(temp_name[i]), 3, median)[2], # Green
                    blue_median =  apply(get(temp_name[i]), 3, median)[3],
                    red_mean =   apply(get(temp_name[i]), 3, mean)[1], # Red
                    green_mean = apply(get(temp_name[i]), 3, mean)[2], # Green
                    blue_mean =  apply(get(temp_name[i]), 3, mean)[3])
  
  df_median_mean <- rbind(df_median_mean, tmp)}

GZhE27042021_PZ <- 0

for (i in 1:length(file_list_GZhE27042021_PZ)) {
  GZhE27042021_PZ <- GZhE27042021_PZ + get(temp_name[i])}

GZhE27042021_PZ <- GZhE27042021_PZ/length(file_list_GZhE27042021_PZ)

hist(GZhE27042021_PZ, 
     breaks = 200,
     main = "GZhE27042021_PZ (n=11)",
     ylim = c(0, 500000),
     xlim = c(0, .1),
     lwd = 1.7)

dev.print(png, 
          'GZhE27042021_PZ.png',
          width=300)

### hist all zones ###
GZhE27042021 <- (GZhE27042021_CZ + GZhE27042021_NZ + GZhE27042021_PZ)/3

hist(GZhE27042021, 
     breaks = 200,
     main = "GZhE27042021 (n=44)",
     ylim = c(0, 500000),
     xlim = c(0, .1),
     lwd = 1.7)

dev.print(png, 
          'GZhE27042021.png',
          width=300)
#####################

######## HTA210412_CZ ########
temp_name <- as.character()

for (i in 1:length(file_list_HTA210412_CZ)) {
  temp_name <- c(temp_name, as.character(paste('HTA210412_CZ', i, sep = '_')))}


for (nm in temp_name) assign(nm, 
                             readImage(file_list_HTA210412_CZ[match(nm, temp_name)]), 
                             .GlobalEnv)

for (i in 1:length(file_list_HTA210412_CZ)) {
  
  tmp <- data.frame(name = 'HTA210412', # average pixel values in
                    zone = 'Contrast',
                    red_median =   apply(get(temp_name[i]), 3, median)[1], # Red
                    green_median = apply(get(temp_name[i]), 3, median)[2], # Green
                    blue_median =  apply(get(temp_name[i]), 3, median)[3],
                    red_mean =   apply(get(temp_name[i]), 3, mean)[1], # Red
                    green_mean = apply(get(temp_name[i]), 3, mean)[2], # Green
                    blue_mean =  apply(get(temp_name[i]), 3, mean)[3])
  
  df_median_mean <- rbind(df_median_mean, tmp)}

HTA210412_CZ <- 0

for (i in 1:length(file_list_HTA210412_CZ)) {
  HTA210412_CZ <- HTA210412_CZ + get(temp_name[i])}

HTA210412_CZ <- HTA210412_CZ/length(file_list_HTA210412_CZ)

hist(HTA210412_CZ, 
     breaks = 200,
     main = "HTA210412_CZ (n=7)",
     ylim = c(0, 500000),
     xlim = c(0, .1),
     lwd = 1.7)

dev.print(png, 
          'HTA210412_CZ.png',
          width=300)

######## HTA210412_PZ ########
temp_name <- as.character()

for (i in 1:length(file_list_HTA210412_PZ)) {
  temp_name <- c(temp_name, as.character(paste('HTA210412_PZ', i, sep = '_')))}


for (nm in temp_name) assign(nm, 
                             readImage(file_list_HTA210412_PZ[match(nm, temp_name)]), 
                             .GlobalEnv)

for (i in 1:length(file_list_HTA210412_PZ)) {
  
  tmp <- data.frame(name = 'HTA210412', # average pixel values in
                    zone = 'Perifocal',
                    red_median =   apply(get(temp_name[i]), 3, median)[1], # Red
                    green_median = apply(get(temp_name[i]), 3, median)[2], # Green
                    blue_median =  apply(get(temp_name[i]), 3, median)[3],
                    red_mean =   apply(get(temp_name[i]), 3, mean)[1], # Red
                    green_mean = apply(get(temp_name[i]), 3, mean)[2], # Green
                    blue_mean =  apply(get(temp_name[i]), 3, mean)[3])
  
  df_median_mean <- rbind(df_median_mean, tmp)}

HTA210412_PZ <- 0

for (i in 1:length(file_list_HTA210412_PZ)) {
  HTA210412_PZ <- HTA210412_PZ + get(temp_name[i])}

HTA210412_PZ <- HTA210412_PZ/length(file_list_HTA210412_PZ)

hist(HTA210412_PZ, 
     breaks = 200,
     main = "HTA210412_PZ (n=20)",
     ylim = c(0, 500000),
     xlim = c(0, .1),
     lwd = 1.7)

dev.print(png, 
          'HTA210412_PZ.png',
          width=300)

### hist all zones ###
HTA210412 <- (HTA210412_CZ + HTA210412_PZ)/2

hist(HTA210412, 
     breaks = 200,
     main = "HTA210412 (n=27)",
     ylim = c(0, 500000),
     xlim = c(0, .1),
     lwd = 1.7)

dev.print(png, 
          'HTA210412.png',
          width=300)
#####################

######## KAS18052021_CZ ########
temp_name <- as.character()

for (i in 1:length(file_list_KAS18052021_CZ)) {
  temp_name <- c(temp_name, as.character(paste('KAS18052021_CZ', i, sep = '_')))}


for (nm in temp_name) assign(nm, 
                             readImage(file_list_KAS18052021_CZ[match(nm, temp_name)]), 
                             .GlobalEnv)

for (i in 1:length(file_list_KAS18052021_CZ)) {
  
  tmp <- data.frame(name = 'KAS18052021', # average pixel values in
                    zone = 'Contrast',
                    red_median =   apply(get(temp_name[i]), 3, median)[1], # Red
                    green_median = apply(get(temp_name[i]), 3, median)[2], # Green
                    blue_median =  apply(get(temp_name[i]), 3, median)[3],
                    red_mean =   apply(get(temp_name[i]), 3, mean)[1], # Red
                    green_mean = apply(get(temp_name[i]), 3, mean)[2], # Green
                    blue_mean =  apply(get(temp_name[i]), 3, mean)[3])
  
  df_median_mean <- rbind(df_median_mean, tmp)}

KAS18052021_CZ <- 0

for (i in 1:length(file_list_KAS18052021_CZ)) {
  KAS18052021_CZ <- KAS18052021_CZ + get(temp_name[i])}

KAS18052021_CZ <- KAS18052021_CZ/length(file_list_KAS18052021_CZ)

hist(KAS18052021_CZ, 
     breaks = 200,
     main = "KAS18052021_CZ (n=18)",
     ylim = c(0, 500000),
     xlim = c(0, .1),
     lwd = 1.7)

dev.print(png, 
          'KAS18052021_CZ.png',
          width=300)

######## KAS18052021_PZ ########
temp_name <- as.character()

for (i in 1:length(file_list_KAS18052021_PZ)) {
  temp_name <- c(temp_name, as.character(paste('KAS18052021_PZ', i, sep = '_')))}


for (nm in temp_name) assign(nm, 
                             readImage(file_list_KAS18052021_PZ[match(nm, temp_name)]), 
                             .GlobalEnv)

for (i in 1:length(file_list_KAS18052021_PZ)) {
  
  tmp <- data.frame(name = 'KAS18052021', # average pixel values in
                    zone = 'Perifocal',
                    red_median =   apply(get(temp_name[i]), 3, median)[1], # Red
                    green_median = apply(get(temp_name[i]), 3, median)[2], # Green
                    blue_median =  apply(get(temp_name[i]), 3, median)[3],
                    red_mean =   apply(get(temp_name[i]), 3, mean)[1], # Red
                    green_mean = apply(get(temp_name[i]), 3, mean)[2], # Green
                    blue_mean =  apply(get(temp_name[i]), 3, mean)[3])
  
  df_median_mean <- rbind(df_median_mean, tmp)}

KAS18052021_PZ <- 0

for (i in 1:length(file_list_KAS18052021_PZ)) {
  KAS18052021_PZ <- KAS18052021_PZ + get(temp_name[i])}

KAS18052021_PZ <- KAS18052021_PZ/length(file_list_KAS18052021_PZ)

hist(KAS18052021_PZ, 
     breaks = 200,
     main = "KAS18052021_PZ (n=16)",
     ylim = c(0, 500000),
     xlim = c(0, .1),
     lwd = 1.7)

dev.print(png, 
          'KAS18052021_PZ.png',
          width=300)

### hist all zones ###
KAS18052021 <- (KAS18052021_CZ + KAS18052021_PZ)/2

hist(KAS18052021, 
     breaks = 200,
     main = "KAS18052021 (n=34)",
     ylim = c(0, 500000),
     xlim = c(0, .1),
     lwd = 1.7)

dev.print(png, 
          'KAS18052021.png',
          width=300)
#####################


######## MVI18021961_CZ ########
temp_name <- as.character()

for (i in 1:length(file_list_MVI18021961_CZ)) {
  temp_name <- c(temp_name, as.character(paste('MVI18021961_CZ', i, sep = '_')))}


for (nm in temp_name) assign(nm, 
                             readImage(file_list_MVI18021961_CZ[match(nm, temp_name)]), 
                             .GlobalEnv)

for (i in 1:length(file_list_MVI18021961_CZ)) {
  
  tmp <- data.frame(name = 'MVI18021961', # average pixel values in
                    zone = 'Contrast',
                    red_median =   apply(get(temp_name[i]), 3, median)[1], # Red
                    green_median = apply(get(temp_name[i]), 3, median)[2], # Green
                    blue_median =  apply(get(temp_name[i]), 3, median)[3],
                    red_mean =   apply(get(temp_name[i]), 3, mean)[1], # Red
                    green_mean = apply(get(temp_name[i]), 3, mean)[2], # Green
                    blue_mean =  apply(get(temp_name[i]), 3, mean)[3])
  
  df_median_mean <- rbind(df_median_mean, tmp)}

MVI18021961_CZ <- 0

for (i in 1:length(file_list_MVI18021961_CZ)) {
  MVI18021961_CZ <- MVI18021961_CZ + get(temp_name[i])}

MVI18021961_CZ <- MVI18021961_CZ/length(file_list_MVI18021961_CZ)

hist(MVI18021961_CZ, 
     breaks = 200,
     main = "MVI18021961_CZ (n=11)",
     ylim = c(0, 500000),
     xlim = c(0, .1),
     lwd = 1.7)

dev.print(png, 
          'MVI18021961_CZ.png',
          width=300)

######## MVI18021961_NZ ########
temp_name <- as.character()

for (i in 1:length(file_list_MVI18021961_NZ)) {
  temp_name <- c(temp_name, as.character(paste('MVI18021961_NZ', i, sep = '_')))}


for (nm in temp_name) assign(nm, 
                             readImage(file_list_MVI18021961_NZ[match(nm, temp_name)]), 
                             .GlobalEnv)

for (i in 1:length(file_list_MVI18021961_NZ)) {
  
  tmp <- data.frame(name = 'MVI18021961', # average pixel values in
                    zone = 'Necrosis',
                    red_median =   apply(get(temp_name[i]), 3, median)[1], # Red
                    green_median = apply(get(temp_name[i]), 3, median)[2], # Green
                    blue_median =  apply(get(temp_name[i]), 3, median)[3],
                    red_mean =   apply(get(temp_name[i]), 3, mean)[1], # Red
                    green_mean = apply(get(temp_name[i]), 3, mean)[2], # Green
                    blue_mean =  apply(get(temp_name[i]), 3, mean)[3])
  
  df_median_mean <- rbind(df_median_mean, tmp)}

MVI18021961_NZ <- 0

for (i in 1:length(file_list_MVI18021961_NZ)) {
  MVI18021961_NZ <- MVI18021961_NZ + get(temp_name[i])}

MVI18021961_NZ <- MVI18021961_NZ/length(file_list_MVI18021961_NZ)

hist(MVI18021961_NZ, 
     breaks = 200,
     main = "MVI18021961_NZ (n=7)",
     ylim = c(0, 500000),
     xlim = c(0, .1),
     lwd = 1.7)

dev.print(png, 
          'MVI18021961_NZ.png',
          width=300)

######## MVI18021961_PZ ########
temp_name <- as.character()

for (i in 1:length(file_list_MVI18021961_PZ)) {
  temp_name <- c(temp_name, as.character(paste('MVI18021961_PZ', i, sep = '_')))}


for (nm in temp_name) assign(nm, 
                             readImage(file_list_MVI18021961_PZ[match(nm, temp_name)]), 
                             .GlobalEnv)

for (i in 1:length(file_list_MVI18021961_PZ)) {
  
  tmp <- data.frame(name = 'MVI18021961', # average pixel values in
                    zone = 'Perifocal',
                    red_median =   apply(get(temp_name[i]), 3, median)[1], # Red
                    green_median = apply(get(temp_name[i]), 3, median)[2], # Green
                    blue_median =  apply(get(temp_name[i]), 3, median)[3],
                    red_mean =   apply(get(temp_name[i]), 3, mean)[1], # Red
                    green_mean = apply(get(temp_name[i]), 3, mean)[2], # Green
                    blue_mean =  apply(get(temp_name[i]), 3, mean)[3])
  
  df_median_mean <- rbind(df_median_mean, tmp)}

MVI18021961_PZ <- 0

for (i in 1:length(file_list_MVI18021961_PZ)) {
  MVI18021961_PZ <- MVI18021961_PZ + get(temp_name[i])}

MVI18021961_PZ <- MVI18021961_PZ/length(file_list_MVI18021961_PZ)

hist(MVI18021961_PZ, 
     breaks = 200,
     main = "MVI18021961_PZ (n=9)",
     ylim = c(0, 500000),
     xlim = c(0, .1),
     lwd = 1.7)

dev.print(png, 
          'MVI18021961_PZ.png',
          width=300)

### hist all zones ###
MVI18021961 <- (MVI18021961_CZ + MVI18021961_NZ + MVI18021961_PZ)/3

hist(MVI18021961, 
     breaks = 200,
     main = "MVI18021961 (n=27)",
     ylim = c(0, 500000),
     xlim = c(0, .1),
     lwd = 1.7)

dev.print(png, 
          'MVI18021961.png',
          width=300)
#####################


######## SRA09062005_Norm_Control ########
temp_name <- as.character()

for (i in 1:length(file_list_SRA09062005_Norm_Control)) {
  temp_name <- c(temp_name, as.character(paste('SRA09062005_Norm_Control', i, sep = '_')))}


for (nm in temp_name) assign(nm, 
                             readImage(file_list_SRA09062005_Norm_Control[match(nm, temp_name)]), 
                             .GlobalEnv)

for (i in 1:length(file_list_SRA09062005_Norm_Control)) {
  
  tmp <- data.frame(name = 'SRA09062005', # average pixel values in
                    zone = 'Control',
                    red_median =   apply(get(temp_name[i]), 3, median)[1], # Red
                    green_median = apply(get(temp_name[i]), 3, median)[2], # Green
                    blue_median =  apply(get(temp_name[i]), 3, median)[3],
                    red_mean =   apply(get(temp_name[i]), 3, mean)[1], # Red
                    green_mean = apply(get(temp_name[i]), 3, mean)[2], # Green
                    blue_mean =  apply(get(temp_name[i]), 3, mean)[3])
  
  df_median_mean <- rbind(df_median_mean, tmp)}

SRA09062005_Norm_Control <- 0

for (i in 1:length(file_list_SRA09062005_Norm_Control)) {
  SRA09062005_Norm_Control <- SRA09062005_Norm_Control + get(temp_name[i])}

SRA09062005_Norm_Control <- SRA09062005_Norm_Control/length(file_list_SRA09062005_Norm_Control)

hist(SRA09062005_Norm_Control, 
     breaks = 200,
     main = "SRA09062005_Norm_Control (n=15)",
     ylim = c(0, 500000),
     xlim = c(0, .1),
     lwd = 1.7)

dev.print(png, 
          'SRA09062005_Norm_Control.png',
          width=300)


######## SSF210158_CZ ########
temp_name <- as.character()

for (i in 1:length(file_list_SSF210158_CZ)) {
  temp_name <- c(temp_name, as.character(paste('SSF210158_CZ', i, sep = '_')))}


for (nm in temp_name) assign(nm, 
                             readImage(file_list_SSF210158_CZ[match(nm, temp_name)]), 
                             .GlobalEnv)

for (i in 1:length(file_list_SSF210158_CZ)) {
  
  tmp <- data.frame(name = 'SSF210158', # average pixel values in
                    zone = 'Contrast',
                    red_median =   apply(get(temp_name[i]), 3, median)[1], # Red
                    green_median = apply(get(temp_name[i]), 3, median)[2], # Green
                    blue_median =  apply(get(temp_name[i]), 3, median)[3],
                    red_mean =   apply(get(temp_name[i]), 3, mean)[1], # Red
                    green_mean = apply(get(temp_name[i]), 3, mean)[2], # Green
                    blue_mean =  apply(get(temp_name[i]), 3, mean)[3])
  
  df_median_mean <- rbind(df_median_mean, tmp)}

SSF210158_CZ <- 0

for (i in 1:length(file_list_SSF210158_CZ)) {
  SSF210158_CZ <- SSF210158_CZ + get(temp_name[i])}

SSF210158_CZ <- SSF210158_CZ/length(file_list_SSF210158_CZ)

hist(SSF210158_CZ, 
     breaks = 200,
     main = "SSF210158_CZ (n=31)",
     ylim = c(0, 500000),
     xlim = c(0, .1),
     lwd = 1.7)

dev.print(png, 
          'SSF210158_CZ.png',
          width=300)

######## SSF210158_NZ ########
temp_name <- as.character()

for (i in 1:length(file_list_SSF210158_NZ)) {
  temp_name <- c(temp_name, as.character(paste('SSF210158_NZ', i, sep = '_')))}


for (nm in temp_name) assign(nm, 
                             readImage(file_list_SSF210158_NZ[match(nm, temp_name)]), 
                             .GlobalEnv)

for (i in 1:length(file_list_SSF210158_NZ)) {
  
  tmp <- data.frame(name = 'SSF210158', # average pixel values in
                    zone = 'Necrosis',
                    red_median =   apply(get(temp_name[i]), 3, median)[1], # Red
                    green_median = apply(get(temp_name[i]), 3, median)[2], # Green
                    blue_median =  apply(get(temp_name[i]), 3, median)[3],
                    red_mean =   apply(get(temp_name[i]), 3, mean)[1], # Red
                    green_mean = apply(get(temp_name[i]), 3, mean)[2], # Green
                    blue_mean =  apply(get(temp_name[i]), 3, mean)[3])
  
  df_median_mean <- rbind(df_median_mean, tmp)}

SSF210158_NZ <- 0

for (i in 1:length(file_list_SSF210158_NZ)) {
  SSF210158_NZ <- SSF210158_NZ + get(temp_name[i])}

SSF210158_NZ <- SSF210158_NZ/length(file_list_SSF210158_NZ)

hist(SSF210158_NZ, 
     breaks = 200,
     main = "SSF210158_NZ (n=13)",
     ylim = c(0, 500000),
     xlim = c(0, .1),
     lwd = 1.7)

dev.print(png, 
          'SSF210158_NZ.png',
          width=300)

######## SSF210158_PZ ########
temp_name <- as.character()

for (i in 1:length(file_list_SSF210158_PZ)) {
  temp_name <- c(temp_name, as.character(paste('SSF210158_PZ', i, sep = '_')))}


for (nm in temp_name) assign(nm, 
                             readImage(file_list_SSF210158_PZ[match(nm, temp_name)]), 
                             .GlobalEnv)

for (i in 1:length(file_list_SSF210158_PZ)) {
  
  tmp <- data.frame(name = 'SSF210158', # average pixel values in
                    zone = 'Perifocal',
                    red_median =   apply(get(temp_name[i]), 3, median)[1], # Red
                    green_median = apply(get(temp_name[i]), 3, median)[2], # Green
                    blue_median =  apply(get(temp_name[i]), 3, median)[3],
                    red_mean =   apply(get(temp_name[i]), 3, mean)[1], # Red
                    green_mean = apply(get(temp_name[i]), 3, mean)[2], # Green
                    blue_mean =  apply(get(temp_name[i]), 3, mean)[3])
  
  df_median_mean <- rbind(df_median_mean, tmp)}

SSF210158_PZ <- 0

for (i in 1:length(file_list_SSF210158_PZ)) {
  SSF210158_PZ <- SSF210158_PZ + get(temp_name[i])}

SSF210158_PZ <- SSF210158_PZ/length(file_list_SSF210158_PZ)

hist(SSF210158_PZ, 
     breaks = 200,
     main = "SSF210158_PZ (n=17)",
     ylim = c(0, 500000),
     xlim = c(0, .1),
     lwd = 1.7)

dev.print(png, 
          'SSF210158_PZ.png',
          width=300)

### hist all zones ###
SSF210158 <- (SSF210158_CZ + SSF210158_NZ + SSF210158_PZ)/3

hist(SSF210158, 
     breaks = 200,
     main = "SSF210158 (n=61)",
     ylim = c(0, 500000),
     xlim = c(0, .1),
     lwd = 1.7)

dev.print(png, 
          'SSF210158.png',
          width=300)
#####################


######## SVE04081947_CZ ########
temp_name <- as.character()

for (i in 1:length(file_list_SVE04081947_CZ)) {
  temp_name <- c(temp_name, as.character(paste('SVE04081947_CZ', i, sep = '_')))}


for (nm in temp_name) assign(nm, 
                             readImage(file_list_SVE04081947_CZ[match(nm, temp_name)]), 
                             .GlobalEnv)

for (i in 1:length(file_list_SVE04081947_CZ)) {
  
  tmp <- data.frame(name = 'SVE04081947', # average pixel values in
                    zone = 'Contrast',
                    red_median =   apply(get(temp_name[i]), 3, median)[1], # Red
                    green_median = apply(get(temp_name[i]), 3, median)[2], # Green
                    blue_median =  apply(get(temp_name[i]), 3, median)[3],
                    red_mean =   apply(get(temp_name[i]), 3, mean)[1], # Red
                    green_mean = apply(get(temp_name[i]), 3, mean)[2], # Green
                    blue_mean =  apply(get(temp_name[i]), 3, mean)[3])
  
  df_median_mean <- rbind(df_median_mean, tmp)}

SVE04081947_CZ <- 0

for (i in 1:length(file_list_SVE04081947_CZ)) {
  SVE04081947_CZ <- SVE04081947_CZ + get(temp_name[i])}

SVE04081947_CZ <- SVE04081947_CZ/length(file_list_SVE04081947_CZ)

hist(SVE04081947_CZ, 
     breaks = 200,
     main = "SVE04081947_CZ (n=14)",
     ylim = c(0, 500000),
     xlim = c(0, .1),
     lwd = 1.7)

dev.print(png, 
          'SVE04081947_CZ.png',
          width=300)

######## SVE04081947_NZ ########
temp_name <- as.character()

for (i in 1:length(file_list_SVE04081947_NZ)) {
  temp_name <- c(temp_name, as.character(paste('SVE04081947_NZ', i, sep = '_')))}


for (nm in temp_name) assign(nm, 
                             readImage(file_list_SVE04081947_NZ[match(nm, temp_name)]), 
                             .GlobalEnv)

for (i in 1:length(file_list_SVE04081947_NZ)) {
  
  tmp <- data.frame(name = 'SVE04081947', # average pixel values in
                    zone = 'Necrosis',
                    red_median =   apply(get(temp_name[i]), 3, median)[1], # Red
                    green_median = apply(get(temp_name[i]), 3, median)[2], # Green
                    blue_median =  apply(get(temp_name[i]), 3, median)[3],
                    red_mean =   apply(get(temp_name[i]), 3, mean)[1], # Red
                    green_mean = apply(get(temp_name[i]), 3, mean)[2], # Green
                    blue_mean =  apply(get(temp_name[i]), 3, mean)[3])
  
  df_median_mean <- rbind(df_median_mean, tmp)}

SVE04081947_NZ <- 0

for (i in 1:length(file_list_SVE04081947_NZ)) {
  SVE04081947_NZ <- SVE04081947_NZ + get(temp_name[i])}

SVE04081947_NZ <- SVE04081947_NZ/length(file_list_SVE04081947_NZ)

hist(SVE04081947_NZ, 
     breaks = 200,
     main = "SVE04081947_NZ (n=12)",
     ylim = c(0, 500000),
     xlim = c(0, .1),
     lwd = 1.7)

dev.print(png, 
          'SVE04081947_NZ.png',
          width=300)

######## SVE04081947_PZ ########
temp_name <- as.character()

for (i in 1:length(file_list_SVE04081947_PZ)) {
  temp_name <- c(temp_name, as.character(paste('SVE04081947_PZ', i, sep = '_')))}


for (nm in temp_name) assign(nm, 
                             readImage(file_list_SVE04081947_PZ[match(nm, temp_name)]), 
                             .GlobalEnv)

for (i in 1:length(file_list_SVE04081947_PZ)) {
  
  tmp <- data.frame(name = 'SVE04081947', # average pixel values in
                    zone = 'Perifocal',
                    red_median =   apply(get(temp_name[i]), 3, median)[1], # Red
                    green_median = apply(get(temp_name[i]), 3, median)[2], # Green
                    blue_median =  apply(get(temp_name[i]), 3, median)[3],
                    red_mean =   apply(get(temp_name[i]), 3, mean)[1], # Red
                    green_mean = apply(get(temp_name[i]), 3, mean)[2], # Green
                    blue_mean =  apply(get(temp_name[i]), 3, mean)[3])
  
  df_median_mean <- rbind(df_median_mean, tmp)}

SVE04081947_PZ <- 0

for (i in 1:length(file_list_SVE04081947_PZ)) {
  SVE04081947_PZ <- SVE04081947_PZ + get(temp_name[i])}

SVE04081947_PZ <- SVE04081947_PZ/length(file_list_SVE04081947_PZ)

hist(SVE04081947_PZ, 
     breaks = 200,
     main = "SVE04081947_PZ (n=11)",
     ylim = c(0, 500000),
     xlim = c(0, .1),
     lwd = 1.7)

dev.print(png, 
          'SVE04081947_PZ.png',
          width=300)

### hist all zones ###
SVE04081947 <- (SVE04081947_CZ + SVE04081947_NZ + SVE04081947_PZ)/3

hist(SVE04081947, 
     breaks = 200,
     main = "SVE04081947 (n=37)",
     ylim = c(0, 500000),
     xlim = c(0, .1),
     lwd = 1.7)

dev.print(png, 
          'SVE04081947.png',
          width=300)
#####################


######## VIR140421_CZ ########
temp_name <- as.character()

for (i in 1:length(file_list_VIR140421_CZ)) {
  temp_name <- c(temp_name, as.character(paste('VIR140421_CZ', i, sep = '_')))}


for (nm in temp_name) assign(nm, 
                             readImage(file_list_VIR140421_CZ[match(nm, temp_name)]), 
                             .GlobalEnv)

for (i in 1:length(file_list_VIR140421_CZ)) {
  
  tmp <- data.frame(name = 'VIR140421', # average pixel values in
                    zone = 'Contrast',
                    red_median =   apply(get(temp_name[i]), 3, median)[1], # Red
                    green_median = apply(get(temp_name[i]), 3, median)[2], # Green
                    blue_median =  apply(get(temp_name[i]), 3, median)[3],
                    red_mean =   apply(get(temp_name[i]), 3, mean)[1], # Red
                    green_mean = apply(get(temp_name[i]), 3, mean)[2], # Green
                    blue_mean =  apply(get(temp_name[i]), 3, mean)[3])
  
  df_median_mean <- rbind(df_median_mean, tmp)}

VIR140421_CZ <- 0

for (i in 1:length(file_list_VIR140421_CZ)) {
  VIR140421_CZ <- VIR140421_CZ + get(temp_name[i])}

VIR140421_CZ <- VIR140421_CZ/length(file_list_VIR140421_CZ)

hist(VIR140421_CZ, 
     breaks = 200,
     main = "VIR140421_CZ (n=13)",
     ylim = c(0, 500000),
     xlim = c(0, .1),
     lwd = 1.7)

dev.print(png, 
          'VIR140421_CZ.png',
          width=300)

######## VIR140421_NZ ########
temp_name <- as.character()

for (i in 1:length(file_list_VIR140421_NZ)) {
  temp_name <- c(temp_name, as.character(paste('VIR140421_NZ', i, sep = '_')))}


for (nm in temp_name) assign(nm, 
                             readImage(file_list_VIR140421_NZ[match(nm, temp_name)]), 
                             .GlobalEnv)

for (i in 1:length(file_list_VIR140421_NZ)) {
  
  tmp <- data.frame(name = 'VIR140421', # average pixel values in
                    zone = 'Necrosis',
                    red_median =   apply(get(temp_name[i]), 3, median)[1], # Red
                    green_median = apply(get(temp_name[i]), 3, median)[2], # Green
                    blue_median =  apply(get(temp_name[i]), 3, median)[3],
                    red_mean =   apply(get(temp_name[i]), 3, mean)[1], # Red
                    green_mean = apply(get(temp_name[i]), 3, mean)[2], # Green
                    blue_mean =  apply(get(temp_name[i]), 3, mean)[3])
  
  df_median_mean <- rbind(df_median_mean, tmp)}

VIR140421_NZ <- 0

for (i in 1:length(file_list_VIR140421_NZ)) {
  VIR140421_NZ <- VIR140421_NZ + get(temp_name[i])}

VIR140421_NZ <- VIR140421_NZ/length(file_list_VIR140421_NZ)

hist(VIR140421_NZ, 
     breaks = 200,
     main = "VIR140421_NZ (n=11)",
     ylim = c(0, 500000),
     xlim = c(0, .1),
     lwd = 1.7)

dev.print(png, 
          'VIR140421_NZ.png',
          width=300)

######## VIR140421_PZ ########
temp_name <- as.character()

for (i in 1:length(file_list_VIR140421_PZ)) {
  temp_name <- c(temp_name, as.character(paste('VIR140421_PZ', i, sep = '_')))}


for (nm in temp_name) assign(nm, 
                             readImage(file_list_VIR140421_PZ[match(nm, temp_name)]), 
                             .GlobalEnv)

for (i in 1:length(file_list_VIR140421_PZ)) {
  
  tmp <- data.frame(name = 'VIR140421', # average pixel values in
                    zone = 'Perifocal',
                    red_median =   apply(get(temp_name[i]), 3, median)[1], # Red
                    green_median = apply(get(temp_name[i]), 3, median)[2], # Green
                    blue_median =  apply(get(temp_name[i]), 3, median)[3],
                    red_mean =   apply(get(temp_name[i]), 3, mean)[1], # Red
                    green_mean = apply(get(temp_name[i]), 3, mean)[2], # Green
                    blue_mean =  apply(get(temp_name[i]), 3, mean)[3])
  
  df_median_mean <- rbind(df_median_mean, tmp)}

VIR140421_PZ <- 0

for (i in 1:length(file_list_VIR140421_PZ)) {
  VIR140421_PZ <- VIR140421_PZ + get(temp_name[i])}

VIR140421_PZ <- VIR140421_PZ/length(file_list_VIR140421_PZ)

hist(VIR140421_PZ, 
     breaks = 200,
     main = "VIR140421_PZ (n=10)",
     ylim = c(0, 500000),
     xlim = c(0, .1),
     lwd = 1.7)

dev.print(png, 
          'VIR140421_PZ.png',
          width=300)

### hist all zones ###
VIR140421 <- (VIR140421_CZ + VIR140421_NZ + VIR140421_PZ)/3

hist(VIR140421, 
     breaks = 200,
     main = "VIR140421 (n=34)",
     ylim = c(0, 500000),
     xlim = c(0, .1),
     lwd = 1.7)

dev.print(png, 
          'VIR140421.png',
          width=300)
#####################

######## EAA200917_NZ ########
temp_name <- as.character()

for (i in 1:length(file_list_EAA200917_NZ)) {
  temp_name <- c(temp_name, as.character(paste('EAA200917_NZ', i, sep = '_')))}


for (nm in temp_name) assign(nm, 
                             readImage(file_list_EAA200917_NZ[match(nm, temp_name)]), 
                             .GlobalEnv)

for (i in 1:length(file_list_EAA200917_NZ)) {
  
  tmp <- data.frame(name = 'EAA200917', # average pixel values in
                    zone = 'Necrosis',
                    red_median =   apply(get(temp_name[i]), 3, median)[1], # Red
                    green_median = apply(get(temp_name[i]), 3, median)[2], # Green
                    blue_median =  apply(get(temp_name[i]), 3, median)[3],
                    red_mean =   apply(get(temp_name[i]), 3, mean)[1], # Red
                    green_mean = apply(get(temp_name[i]), 3, mean)[2], # Green
                    blue_mean =  apply(get(temp_name[i]), 3, mean)[3])
  
  df_median_mean <- rbind(df_median_mean, tmp)}

EAA200917_NZ <- 0

for (i in 1:length(file_list_EAA200917_NZ)) {
  EAA200917_NZ <- EAA200917_NZ + get(temp_name[i])}

EAA200917_NZ <- EAA200917_NZ/length(file_list_EAA200917_NZ)

hist(EAA200917_NZ, 
     breaks = 200,
     main = "EAA200917_NZ (n=28)",
     ylim = c(0, 500000),
     xlim = c(0, .1),
     lwd = 1.7)

dev.print(png, 
          'EAA200917_NZ.png',
          width=300)

######## EAA200917_PZ ########
temp_name <- as.character()

for (i in 1:length(file_list_EAA200917_PZ)) {
  temp_name <- c(temp_name, as.character(paste('EAA200917_PZ', i, sep = '_')))}


for (nm in temp_name) assign(nm, 
                             readImage(file_list_EAA200917_PZ[match(nm, temp_name)]), 
                             .GlobalEnv)

for (i in 1:length(file_list_EAA200917_PZ)) {
  
  tmp <- data.frame(name = 'EAA200917', # average pixel values in
                    zone = 'Perifocal',
                    red_median =   apply(get(temp_name[i]), 3, median)[1], # Red
                    green_median = apply(get(temp_name[i]), 3, median)[2], # Green
                    blue_median =  apply(get(temp_name[i]), 3, median)[3],
                    red_mean =   apply(get(temp_name[i]), 3, mean)[1], # Red
                    green_mean = apply(get(temp_name[i]), 3, mean)[2], # Green
                    blue_mean =  apply(get(temp_name[i]), 3, mean)[3])
  
  df_median_mean <- rbind(df_median_mean, tmp)}

EAA200917_PZ <- 0

for (i in 1:length(file_list_EAA200917_PZ)) {
  EAA200917_PZ <- EAA200917_PZ + get(temp_name[i])}

EAA200917_PZ <- EAA200917_PZ/length(file_list_EAA200917_PZ)

hist(EAA200917_PZ, 
     breaks = 200,
     main = "EAA200917_PZ (n=21)",
     ylim = c(0, 500000),
     xlim = c(0, .1),
     lwd = 1.7)

dev.print(png, 
          'EAA200917_PZ.png',
          width=300)


### hist all zones ###
EAA200917 <- (EAA200917_NZ + EAA200917_PZ)/2

hist(EAA200917, 
     breaks = 200,
     main = "EAA200917 (n=49)",
     ylim = c(0, 500000),
     xlim = c(0, .1),
     lwd = 1.7)

dev.print(png, 
          'EAA200917.png',
          width=300)
#####################


######## LIM22031967_CZ ########
temp_name <- as.character()

for (i in 1:length(file_list_LIM22031967_CZ)) {
  temp_name <- c(temp_name, as.character(paste('LIM22031967_CZ', i, sep = '_')))}


for (nm in temp_name) assign(nm, 
                             readImage(file_list_LIM22031967_CZ[match(nm, temp_name)]), 
                             .GlobalEnv)

for (i in 1:length(file_list_LIM22031967_CZ)) {
  
  tmp <- data.frame(name = 'LIM22031967', # average pixel values in
                    zone = 'Contrast',
                    red_median =   apply(get(temp_name[i]), 3, median)[1], # Red
                    green_median = apply(get(temp_name[i]), 3, median)[2], # Green
                    blue_median =  apply(get(temp_name[i]), 3, median)[3],
                    red_mean =   apply(get(temp_name[i]), 3, mean)[1], # Red
                    green_mean = apply(get(temp_name[i]), 3, mean)[2], # Green
                    blue_mean =  apply(get(temp_name[i]), 3, mean)[3])
  
  df_median_mean <- rbind(df_median_mean, tmp)}

LIM22031967_CZ <- 0

for (i in 1:length(file_list_LIM22031967_CZ)) {
  LIM22031967_CZ <- LIM22031967_CZ + get(temp_name[i])}

LIM22031967_CZ <- LIM22031967_CZ/length(file_list_LIM22031967_CZ)

hist(LIM22031967_CZ, 
     breaks = 200,
     main = "LIM22031967_CZ (n=8)",
     ylim = c(0, 500000),
     xlim = c(0, .1),
     lwd = 1.7)

dev.print(png, 
          'LIM22031967_CZ.png',
          width=300)


######## LIM22031967_NZ ########
temp_name <- as.character()

for (i in 1:length(file_list_LIM22031967_NZ)) {
  temp_name <- c(temp_name, as.character(paste('LIM22031967_NZ', i, sep = '_')))}


for (nm in temp_name) assign(nm, 
                             readImage(file_list_LIM22031967_NZ[match(nm, temp_name)]), 
                             .GlobalEnv)

for (i in 1:length(file_list_LIM22031967_NZ)) {
  
  tmp <- data.frame(name = 'LIM22031967', # average pixel values in
                    zone = 'Necrosis',
                    red_median =   apply(get(temp_name[i]), 3, median)[1], # Red
                    green_median = apply(get(temp_name[i]), 3, median)[2], # Green
                    blue_median =  apply(get(temp_name[i]), 3, median)[3],
                    red_mean =   apply(get(temp_name[i]), 3, mean)[1], # Red
                    green_mean = apply(get(temp_name[i]), 3, mean)[2], # Green
                    blue_mean =  apply(get(temp_name[i]), 3, mean)[3])
  
  df_median_mean <- rbind(df_median_mean, tmp)}

LIM22031967_NZ <- 0

for (i in 1:length(file_list_LIM22031967_NZ)) {
  LIM22031967_NZ <- LIM22031967_NZ + get(temp_name[i])}

LIM22031967_NZ <- LIM22031967_NZ/length(file_list_LIM22031967_NZ)

hist(LIM22031967_NZ, 
     breaks = 200,
     main = "LIM22031967_NZ (n=8)",
     ylim = c(0, 500000),
     xlim = c(0, .1),
     lwd = 1.7)

dev.print(png, 
          'LIM22031967_NZ.png',
          width=300)


### hist all zones ###
LIM22031967 <- (LIM22031967_CZ + LIM22031967_NZ)/2

hist(LIM22031967, 
     breaks = 200,
     main = "LIM22031967 (n=16)",
     ylim = c(0, 500000),
     xlim = c(0, .1),
     lwd = 1.7)

dev.print(png, 
          'LIM22031967.png',
          width=300)
#####################

######## GEN26011949_CZ ########
temp_name <- as.character()

for (i in 1:length(file_list_GEN26011949_CZ)) {
  temp_name <- c(temp_name, as.character(paste('GEN26011949_CZ', i, sep = '_')))}


for (nm in temp_name) assign(nm, 
                             readImage(file_list_GEN26011949_CZ[match(nm, temp_name)]), 
                             .GlobalEnv)

for (i in 1:length(file_list_GEN26011949_CZ)) {
  
  tmp <- data.frame(name = 'GEN26011949', # average pixel values in
                    zone = 'Contrast',
                    red_median =   apply(get(temp_name[i]), 3, median)[1], # Red
                    green_median = apply(get(temp_name[i]), 3, median)[2], # Green
                    blue_median =  apply(get(temp_name[i]), 3, median)[3],
                    red_mean =   apply(get(temp_name[i]), 3, mean)[1], # Red
                    green_mean = apply(get(temp_name[i]), 3, mean)[2], # Green
                    blue_mean =  apply(get(temp_name[i]), 3, mean)[3])
  
  df_median_mean <- rbind(df_median_mean, tmp)}

GEN26011949_CZ <- 0

for (i in 1:length(file_list_GEN26011949_CZ)) {
  GEN26011949_CZ <- GEN26011949_CZ + get(temp_name[i])}

GEN26011949_CZ <- GEN26011949_CZ/length(file_list_GEN26011949_CZ)

hist(GEN26011949_CZ, 
     breaks = 200,
     main = "GEN26011949_CZ (n=14)",
     ylim = c(0, 500000),
     xlim = c(0, .1),
     lwd = 1.7)

dev.print(png, 
          'GEN26011949_CZ.png',
          width=300)

######## GEN26011949_NZ ########
temp_name <- as.character()

for (i in 1:length(file_list_GEN26011949_NZ)) {
  temp_name <- c(temp_name, as.character(paste('GEN26011949_NZ', i, sep = '_')))}


for (nm in temp_name) assign(nm, 
                             readImage(file_list_GEN26011949_NZ[match(nm, temp_name)]), 
                             .GlobalEnv)

for (i in 1:length(file_list_GEN26011949_NZ)) {
  
  tmp <- data.frame(name = 'GEN26011949', # average pixel values in
                    zone = 'Necrosis',
                    red_median =   apply(get(temp_name[i]), 3, median)[1], # Red
                    green_median = apply(get(temp_name[i]), 3, median)[2], # Green
                    blue_median =  apply(get(temp_name[i]), 3, median)[3],
                    red_mean =   apply(get(temp_name[i]), 3, mean)[1], # Red
                    green_mean = apply(get(temp_name[i]), 3, mean)[2], # Green
                    blue_mean =  apply(get(temp_name[i]), 3, mean)[3])
  
  df_median_mean <- rbind(df_median_mean, tmp)}

GEN26011949_NZ <- 0

for (i in 1:length(file_list_GEN26011949_NZ)) {
  GEN26011949_NZ <- GEN26011949_NZ + get(temp_name[i])}

GEN26011949_NZ <- GEN26011949_NZ/length(file_list_GEN26011949_NZ)

hist(GEN26011949_NZ, 
     breaks = 200,
     main = "GEN26011949_NZ (n=15)",
     ylim = c(0, 500000),
     xlim = c(0, .1),
     lwd = 1.7)

dev.print(png, 
          'GEN26011949_NZ.png',
          width=300)


### hist all zones ###
GEN26011949 <- (GEN26011949_CZ + GEN26011949_NZ)/2

hist(GEN26011949, 
     breaks = 200,
     main = "GEN26011949 (n=29)",
     ylim = c(0, 500000),
     xlim = c(0, .1),
     lwd = 1.7)

dev.print(png, 
          'GEN26011949.png',
          width=300)
#####################

######################################################

#write.xlsx(df_median,
#           file = 'df_median.xlsx',
#           sheetName = 'df_median_median', append = FALSE)

write.csv(df_median_mean,'df_median_mean.csv')


# Total observations: 493 
######################################################




