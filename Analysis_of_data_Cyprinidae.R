###################### *******************  March 10 2022 *************************
# * Code for Accolla et al. ... Modeling pesticide effects on multiple listed fish species. 
# * A case study of chlorothalonil effects on Cyprinidae **** ###
# This code analyses the data created by Cyprinidae_population_model.nlogo and create the figures that are presented in the paper.
# 1) Effects of different EMFs on the 4 species of Cyprinidae
# 2) Effects of adding up effect-sub-models for the 4 species of Cyprinidae
# 3) Population dynamics without stress : a) effects of stochastic droughts; b) effects of DD
###################### ********************************** ############################################################################
####################### **********************************  ********************************** #######################################

library(dplyr)
library(ggplot2)

###################### ********************************** ############################################################################
####################### **********************************  ********************************** #######################################
################## EMFs
setwd("....")
file_TS_noTox = "TX_TS-W.csv"
# simulations without exposure effects
TS_noTox=read.csv(file_TS_noTox, header = T, as.is = T)
# define the length of the simulation and the length of the exposure
# (time of the simulations)
Tstart_exp_TS = 15
Tend_exp_TS = Tstart_exp_TS + 5
Tsim_TS = Tend_exp_TS + 3
DIM_TS = 365-149+(Tsim_TS - 2)*365+1
Start_tox_TS = 365-149+(Tstart_exp_TS -2)*365 +1
End_tox_TS = 365-149+(Tend_exp_TS - 2)*365 
End_tox_TS_bis=End_tox_TS  + 1 
#  4 variables are presents: total abundance (all organisms after DEB-birth), abundance adults, total weight (all organisms after DEB-birth), weight adults
n_var = 4
R=nrow(TS_noTox)
C=ncol(TS_noTox)
n_replicates = (C - 1)/n_var

# put together the data referring to the same variable (e.g., total abundance) 
Tot <- matrix(, nrow = R, ncol = 1)
rowTot=nrow(Tot)
# first column is not relevant
i=2
M1=TS_noTox[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  TS_noTox[,c]
  M1=cbind(M1,x)
}
#  calculate mean over time (corresponding to the time of exposure)
Average_time_TS = colMeans(M1[Start_tox_TS:End_tox_TS,],na.rm = TRUE)

# Average_no_tox_TS = mean(Average_time_TS,na.rm = TRUE)
# sd_no_tox_TS = sd(Average_time_TS,na.rm = TRUE)


############################################################################################################################################
##########---------------------- MF 1
setwd("....")
file_TS_MF1 = "TX_TS-MF1.csv"
TS_Tox_MF1=read.csv(file_TS_MF1, header = T, as.is = T)
Tot <- matrix(, nrow = R, ncol = 1)

Tstart_exp_TS = 15
Tend_exp_TS = Tstart_exp_TS + 5
Tsim_TS = Tend_exp_TS + 3
DIM_TS = 365-149+(Tsim_TS - 2)*365+1
Start_tox_TS = 365-149+(Tstart_exp_TS -2)*365 +1
End_tox_TS = 365-149+(Tend_exp_TS - 2)*365 
End_tox_TS_bis=End_tox_TS  + 1 

n_var =4

R=nrow(TS_Tox_MF1)
C=ncol(TS_Tox_MF1)
n_replicates = (C - 1)/n_var

TS_Tox_MF1[is.na(TS_Tox_MF1)] = 0

i=2
M1_Tox_S=TS_Tox_MF1[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  TS_Tox_MF1[,c]
  M1_Tox_S=cbind(M1_Tox_S,x)
}

Average_time_TS_Tox_MF1 = colMeans(M1_Tox_S[Start_tox_TS:End_tox_TS,],na.rm = TRUE)

# Average_tox_TS_MF1 = mean(Average_time_TS_Tox_MF1,na.rm = TRUE)
# sd_tox_TS_MF1 = sd(Average_time_TS_Tox_MF1,na.rm = TRUE)


############################################################################################################################################
##########---------------------- MF 2
setwd("....")
file_TS_MF2 = "TX_TS-MF2.csv"
TS_Tox_MF2=read.csv(file_TS_MF2, header = T, as.is = T)

Tstart_exp_TS = 15
Tend_exp_TS = Tstart_exp_TS + 5
Tsim_TS = Tend_exp_TS + 3
DIM_TS = 365-149+(Tsim_TS - 2)*365+1
Start_tox_TS = 365-149+(Tstart_exp_TS -2)*365 +1
End_tox_TS = 365-149+(Tend_exp_TS - 2)*365 
End_tox_TS_bis=End_tox_TS  + 1 
n_var =4
Tot <- matrix(, nrow = R, ncol = 1)

R=nrow(TS_Tox_MF2)
C=ncol(TS_Tox_MF2)
n_replicates = (C - 1)/n_var
TS_Tox_MF2[is.na(TS_Tox_MF2)] = 0

i=2
M1_Tox_S=TS_Tox_MF2[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  TS_Tox_MF2[,c]
  M1_Tox_S=cbind(M1_Tox_S,x)
}

Average_time_TS_Tox_MF2 = colMeans(M1_Tox_S[Start_tox_TS:End_tox_TS,],na.rm = TRUE)

# Average_tox_TS_MF2 = mean(Average_time_TS_Tox_MF2,na.rm = TRUE)
# sd_tox_TS_MF2 = sd(Average_time_TS_Tox_MF2,na.rm = TRUE)


############################################################################################################################################
##########---------------------- MF 4

setwd("....")
file_TS_MF4 = "TX_TS-MF4.csv"
TS_Tox_MF4=read.csv(file_TS_MF4, header = T, as.is = T)
Tstart_exp_TS = 15
Tend_exp_TS = Tstart_exp_TS + 5
Tsim_TS = Tend_exp_TS + 3
DIM_TS = 365-149+(Tsim_TS - 2)*365+1
Start_tox_TS = 365-149+(Tstart_exp_TS -2)*365 +1
End_tox_TS = 365-149+(Tend_exp_TS - 2)*365 
End_tox_TS_bis=End_tox_TS  + 1 

n_var =4
R=nrow(TS_Tox_MF4)
C=ncol(TS_Tox_MF4)
n_replicates = (C - 1)/n_var

TS_Tox_MF4[is.na(TS_Tox_MF4)] = 0

i=2
M1_Tox_S=TS_Tox_MF4[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  TS_Tox_MF4[,c]
  M1_Tox_S=cbind(M1_Tox_S,x)
}

Average_time_TS_Tox_MF4 = colMeans(M1_Tox_S[Start_tox_TS:End_tox_TS,],na.rm = TRUE)

# Average_tox_TS_MF4 = mean(Average_time_TS_Tox_MF4,na.rm = TRUE)
# sd_tox_TS_MF4 = sd(Average_time_TS_Tox_MF4,na.rm = TRUE)


############################################################################################################################################
##########---------------------- MF 5

setwd("....")
file_TS_MF5 = "TX_TS-MF5.csv"
TS_Tox_MF5=read.csv(file_TS_MF5, header = T, as.is = T)
Tstart_exp_TS = 15
Tend_exp_TS = Tstart_exp_TS + 5
Tsim_TS = Tend_exp_TS + 3
DIM_TS = 365-149+(Tsim_TS - 2)*365+1
Start_tox_TS = 365-149+(Tstart_exp_TS -2)*365 +1
End_tox_TS = 365-149+(Tend_exp_TS - 2)*365 
End_tox_TS_bis=End_tox_TS  + 1 

n_var =4
R=nrow(TS_Tox_MF5)
C=ncol(TS_Tox_MF5)
n_replicates = (C - 1)/n_var

# With high EMF, population gets extinct --> translated to 0
TS_Tox_MF5[is.na(TS_Tox_MF5)] = 0
i=2

M1_Tox_S=TS_Tox_MF5[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  TS_Tox_MF5[,c]
  M1_Tox_S=cbind(M1_Tox_S,x)
}

Average_time_TS_Tox_MF5 = colMeans(M1_Tox_S[Start_tox_TS:End_tox_TS,],na.rm = TRUE)

# Average_tox_TS_MF5 = mean(Average_time_TS_Tox_MF5,na.rm = TRUE)
# sd_tox_TS_MF5 = sd(Average_time_TS_Tox_MF5,na.rm = TRUE)



############################################################################################################################################
##########---------------------- MF 7 

setwd("....")
file_TS_MF7 = "TX_TS-MF7.csv"
TS_Tox_MF7=read.csv(file_TS_MF7, header = T, as.is = T)

Tstart_exp_TS = 15
Tend_exp_TS = Tstart_exp_TS + 5
Tsim_TS = Tend_exp_TS + 3
DIM_TS = 365-149+(Tsim_TS - 2)*365+1
Start_tox_TS = 365-149+(Tstart_exp_TS -2)*365 +1
End_tox_TS = 365-149+(Tend_exp_TS - 2)*365 
End_tox_TS_bis=End_tox_TS  + 1 

n_var =4

R=nrow(TS_Tox_MF7)
C=ncol(TS_Tox_MF7)
n_replicates = (C - 1)/n_var
TS_Tox_MF7[is.na(TS_Tox_MF7)] = 0

i=2

M1_Tox_S=TS_Tox_MF7[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  TS_Tox_MF7[,c]
  M1_Tox_S=cbind(M1_Tox_S,x)
}

Average_time_TS_Tox_MF7 = colMeans(M1_Tox_S[Start_tox_TS:End_tox_TS,],na.rm = TRUE)

# Average_tox_TS_MF7 = mean(Average_time_TS_Tox_MF7,na.rm = TRUE)
# sd_tox_TS_MF7 = sd(Average_time_TS_Tox_MF7,na.rm = TRUE)

lablist<-as.vector(c(2:Tsim_TS))

#########################  MF 8

setwd("....")
file_TS_MF8 = "TX_TS-MF8.csv"
TS_Tox_MF8=read.csv(file_TS_MF8, header = T, as.is = T)

Tstart_exp_TS = 15
Tend_exp_TS = Tstart_exp_TS + 5
Tsim_TS = Tend_exp_TS + 3
DIM_TS = 365-149+(Tsim_TS - 2)*365+1
Start_tox_TS = 365-149+(Tstart_exp_TS -2)*365 +1
End_tox_TS = 365-149+(Tend_exp_TS - 2)*365 
End_tox_TS_bis=End_tox_TS  + 1 

n_var =4
R=nrow(TS_Tox_MF8)
C=ncol(TS_Tox_MF8)
n_replicates = (C - 1)/n_var

TS_Tox_MF8[is.na(TS_Tox_MF8)] = 0


i=2
M1_Tox_S=TS_Tox_MF8[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  TS_Tox_MF8[,c]
  M1_Tox_S=cbind(M1_Tox_S,x)
}

Average_time_TS_Tox_MF8 = colMeans(M1_Tox_S[Start_tox_TS:End_tox_TS,],na.rm = TRUE)

# Average_tox_TS_MF8 = mean(Average_time_TS_Tox_MF8,na.rm = TRUE)
# sd_tox_TS_MF8 = sd(Average_time_TS_Tox_MF8,na.rm = TRUE)


#########################  MF 10

setwd("....")
file_TS_MF10 = "TX_TS-MF10.csv"
TS_Tox_MF10=read.csv(file_TS_MF10, header = T, as.is = T)
Tstart_exp_TS = 15
Tend_exp_TS = Tstart_exp_TS + 5
Tsim_TS = Tend_exp_TS + 3
DIM_TS = 365-149+(Tsim_TS - 2)*365+1
Start_tox_TS = 365-149+(Tstart_exp_TS -2)*365 +1
End_tox_TS = 365-149+(Tend_exp_TS - 2)*365 
End_tox_TS_bis=End_tox_TS  + 1 

n_var =4
R=nrow(TS_Tox_MF10)
C=ncol(TS_Tox_MF10)
n_replicates = (C - 1)/n_var

# With high EMF, population gets extinct --> translated to 0
absent_rec = DIM_TS-R
alpha = matrix (0,nrow = absent_rec, ncol = C)

nameCol = colnames(TS_Tox_MF10)
colnames(alpha) = nameCol
TS_Tox_MF10 = rbind(TS_Tox_MF10,alpha)

TS_Tox_MF10[is.na(TS_Tox_MF10)] = 0

i=2
M1_Tox_S=TS_Tox_MF10[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  TS_Tox_MF10[,c]
  M1_Tox_S=cbind(M1_Tox_S,x)
}

Average_time_TS_Tox_MF10 = colMeans(M1_Tox_S[Start_tox_TS:End_tox_TS,],na.rm = TRUE)

# Average_tox_TS_MF10 = mean(Average_time_TS_Tox_MF10,na.rm = TRUE)
# sd_tox_TS_MF10 = sd(Average_time_TS_Tox_MF10,na.rm = TRUE)


#########################  MF 11


setwd("....")
file_TS_MF11 = "TX_TS-MF11.csv"

TS_Tox_MF11=read.csv(file_TS_MF11, header = T, as.is = T)

Tstart_exp_TS = 15
Tend_exp_TS = Tstart_exp_TS + 5
Tsim_TS = Tend_exp_TS + 3
DIM_TS = 365-149+(Tsim_TS - 2)*365+1
Start_tox_TS = 365-149+(Tstart_exp_TS -2)*365 +1
End_tox_TS = 365-149+(Tend_exp_TS - 2)*365 
End_tox_TS_bis=End_tox_TS  + 1 
n_var =4

R=nrow(TS_Tox_MF11)
C=ncol(TS_Tox_MF11)
n_replicates = (C - 1)/n_var

absent_rec = DIM_TS-R
alpha = matrix (0,nrow = absent_rec, ncol = C)

nameCol = colnames(TS_Tox_MF11)
colnames(alpha) = nameCol
TS_Tox_MF11 = rbind(TS_Tox_MF11,alpha)
TS_Tox_MF11[is.na(TS_Tox_MF11)] = 0

i=2
M1_Tox_S=TS_Tox_MF11[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  TS_Tox_MF11[,c]
  M1_Tox_S=cbind(M1_Tox_S,x)
}

Average_time_TS_Tox_MF11 = colMeans(M1_Tox_S[Start_tox_TS:End_tox_TS,],na.rm = TRUE)

# Average_tox_TS_MF11 = mean(Average_time_TS_Tox_MF11,na.rm = TRUE)
# sd_tox_TS_MF11 = sd(Average_time_TS_Tox_MF11,na.rm = TRUE)

##  starting time of the simulation (different for each species). time is for plot purposes
time_TS = 365-148

# ratio_TS = c(Average_tox_TS_MF1 / Average_no_tox_TS, Average_tox_TS_MF2 / Average_no_tox_TS, Average_tox_TS_MF4 / Average_no_tox_TS,
#              Average_tox_TS_MF5 / Average_no_tox_TS, Average_tox_TS_MF7 / Average_no_tox_TS,
#              Average_tox_TS_MF8 / Average_no_tox_TS, Average_tox_TS_MF10 / Average_no_tox_TS,
#              Average_tox_TS_MF11 / Average_no_tox_TS)#, Average_tox_TS_MF20 / Average_no_tox_TS)

###################### ********************************** ############################################################################
####################### **********************************  ********************************** #######################################
####################### DRM

#### NO TOX

setwd("....")
file_DRM = "Cyprinidae_TX_DRM DRM-table.csv"
#  read table - variables of the same run are not organized per columns like for the other species, so values for the same run have to be sorted
DRM_table=read.csv(file_DRM, header=F,stringsAsFactors=FALSE)
NAMES <- read.csv(file_DRM, nrow = 6, sep = ";") # headings
DRM_table <- read.csv(file_DRM, skip = 6, stringsAsFactors = FALSE, sep = ",") # values
words <- strsplit(NAMES[6,1], ",")[[1]]
xx=unique(tolower(words))

names(DRM_table) <- xx

Tstart_exp_DRM = 20
Tend_exp_DRM = Tstart_exp_DRM + 5
Tsim_DRM = Tend_exp_DRM + 3
DIM_DRM = 364*24+365*(Tsim_DRM - 2)*24+3
Start_tox_DRM = 364*24+365*(Tstart_exp_DRM -2)*24 +3
End_tox_DRM = 364*24+365*(Tend_exp_DRM - 2)*24 +2
End_tox_DRM_bis = End_tox_DRM + 1 
n_var = 4
R=nrow(DRM_table)

n_replicates = 40
#  select the variables depending on the run number
for (i in 1:n_replicates)
{
  dataframe = DRM_table[DRM_table[, "[run number]"] == i,]
  assign(paste("Matrix",i, sep=""), select(dataframe, 
                                           c("count turtles with [u_h > u_h^b]","count turtles with [u_h > u_h^p]","sum [weight] of turtles with [u_h > u_h^b]","w-turtles")))
}

for (i in 1:n_replicates)
{
  x= nrow(get(paste("Matrix",i, sep="")))
  if(x != DIM_DRM)
  {print (i)
    print (x)}
}

#  organize the data: one column per each of the 4 variables, repeated for the number of replicates
DRM_m = Matrix1
for (i in 2:n_replicates)
{
  DRM_m = cbind (DRM_m,get(paste("Matrix",i, sep="")))}

colTot=ncol(DRM_m)
col.names = rep(c("Tot pop","Adult pop","Tot weight","Adult weight"),n_replicates)
colnames(DRM_m) <-col.names

# average over exposure time
Average_no_tox_DRM_NEW = colMeans(DRM_m[Start_tox_DRM:End_tox_DRM,],na.rm = TRUE)
#  organize data per variables - here only interested in total abundance
mean_time_tot_pop <- matrix(, nrow = n_replicates, ncol = 1)
ii = 1
i = 1
while (ii <= length(Average_no_tox_DRM_NEW))
{
  mean_time_tot_pop[i] = Average_no_tox_DRM_NEW[ii]
  i = i + 1
  ii = ii + 4
}
# Average_no_tox_DRMS = mean(mean_time_tot_pop,na.rm = TRUE)
# sd_no_tox_DRMS = sd(mean_time_tot_pop,na.rm = TRUE)

################### MF 5

setwd("....")
file_DRM_MF5 = "Cyprinidae_TX_DRM - Copy MF5-12-13-table.csv"
DRM_table_MF5=read.csv(file_DRM_MF5, header=F,stringsAsFactors=FALSE)
NAMES <- read.csv(file_DRM_MF5, nrow = 6, sep = ";")
DRM_table_MF5 <- read.csv(file_DRM_MF5, skip = 6, stringsAsFactors = FALSE, sep = ",")
words <- strsplit(NAMES[6,1], ",")[[1]]
xx=unique(tolower(words))
names(DRM_table_MF5) <- xx

Tstart_exp_DRM = 20
Tend_exp_DRM = Tstart_exp_DRM + 5
Tsim_DRM = Tend_exp_DRM + 3
DIM_DRM = 364*24+365*(Tsim_DRM - 2)*24+3
Start_tox_DRM = 364*24+365*(Tstart_exp_DRM -2)*24 +3
End_tox_DRM = 364*24+365*(Tend_exp_DRM - 2)*24 +2
End_tox_DRM_bis = End_tox_DRM + 1 
n_var = 4
R=nrow(DRM_table_MF5)

n_replicates = 40 

for (i in 1:n_replicates)
{
  dataframe = DRM_table_MF5[DRM_table_MF5[, "[run number]"] == i,]
  assign(paste("Matrix",i, sep=""), select(dataframe, 
                                           c("count turtles with [u_h > u_h^b]","count turtles with [u_h > u_h^p]","sum [weight] of turtles with [u_h > u_h^b]","w-turtles")))
}

for (i in 1:n_replicates)
{
  x=  nrow(get(paste("Matrix",i, sep="")))
  if(x != DIM_DRM)
  {print (i)
    print (x)
    absent_rec = DIM_DRM-x
    alpha = matrix (0,nrow = absent_rec, ncol = 4)
    g=get(paste("Matrix",i, sep=""))
    colnames(alpha) <- colnames(g)
    gg=rbind(g,alpha)
    assign(paste("Matrix",i, sep=""), gg)
  }
}

DRM_m_MF5 = Matrix1

for (i in 2:n_replicates)
{
  DRM_m_MF5 = cbind (DRM_m_MF5,get(paste("Matrix",i, sep="")))}
colTot=ncol(DRM_m_MF5)

col.names = rep(c("Tot pop","Adult pop","Tot weight","Adult weight"),n_replicates)
colnames(DRM_m_MF5) <-col.names

Average_MF5_DRM_NEW = colMeans(DRM_m_MF5[Start_tox_DRM:End_tox_DRM,],na.rm = TRUE)
mean_time_tot_pop_MF5 <- matrix(, nrow = n_replicates, ncol = 1)

ii = 1
i = 1
while (ii <= length(Average_MF5_DRM_NEW))
{
  mean_time_tot_pop_MF5[i] = Average_MF5_DRM_NEW[ii]
  i = i + 1
  ii = ii + 4
}

# Average_DRM_MF5 = mean(mean_time_tot_pop_MF5,na.rm = TRUE)
# sd_DRM_MF5 = sd(mean_time_tot_pop_MF5,na.rm = TRUE)


######### MF 1
setwd("....")
file_DRM_MF1 = "Cyprinidae_TX_EMF DRM-MF1-table.csv"
DRM_table_MF1=read.csv(file_DRM_MF1, header=F,stringsAsFactors=FALSE)

NAMES <- read.csv(file_DRM_MF1, nrow = 6, sep = ";")
DRM_table_MF1 <- read.csv(file_DRM_MF1, skip = 6, stringsAsFactors = FALSE, sep = ",")
words <- strsplit(NAMES[6,1], ",")[[1]]
xx=unique(tolower(words))
names(DRM_table_MF1) <- xx

Tstart_exp_DRM = 20
Tend_exp_DRM = Tstart_exp_DRM + 5
Tsim_DRM = Tend_exp_DRM + 3
DIM_DRM = 364*24+365*(Tsim_DRM - 2)*24+3
Start_tox_DRM = 364*24+365*(Tstart_exp_DRM -2)*24 +3
End_tox_DRM = 364*24+365*(Tend_exp_DRM - 2)*24 +2
End_tox_DRM_bis = End_tox_DRM + 1 
n_var = 4

R=nrow(DRM_table_MF1)
DRM_matrix <- matrix(, nrow = R, ncol = 1)

n_replicates = 40
for (i in 1:n_replicates)
{
  dataframe = DRM_table_MF1[DRM_table_MF1[, "[run number]"] == i,]
  assign(paste("Matrix",i, sep=""), select(dataframe, 
                                           c("count turtles with [u_h > u_h^b]","count turtles with [u_h > u_h^p]","sum [weight] of turtles with [u_h > u_h^b]","w-turtles")))
}

for (i in 1:n_replicates)
{
  x= nrow(get(paste("Matrix",i, sep="")))
  if(x != DIM_DRM)
  {print (i)
    print (x)
    absent_rec = DIM_DRM-x
    alpha = matrix (0,nrow = absent_rec, ncol = 4)
    g=get(paste("Matrix",i, sep=""))
    colnames(alpha) <- colnames(g)
    gg=rbind(g,alpha)
    assign(paste("Matrix",i, sep=""), gg)
  }
}


DRM_m_MF1 = Matrix1
DRM_m_MF1[is.na(DRM_m_MF1)] = 0

for (i in 2:n_replicates)
{
  DRM_m_MF1 = cbind (DRM_m_MF1,get(paste("Matrix",i, sep="")))}
colTot=ncol(DRM_m_MF1)

col.names = rep(c("Tot pop","Adult pop","Tot weight","Adult weight"),n_replicates)
colnames(DRM_m_MF1) <-col.names
Average_MF1_DRM_NEW = colMeans(DRM_m_MF1[Start_tox_DRM:End_tox_DRM,],na.rm = TRUE)

mean_time_tot_pop_MF1 <- matrix(, nrow = n_replicates, ncol = 1)

ii = 1
i = 1
while (ii <= length(Average_MF1_DRM_NEW))
{
  mean_time_tot_pop_MF1[i] = Average_MF1_DRM_NEW[ii]
  i = i + 1
  ii = ii + 4
}

# 
# Average_DRM_MF1 = mean(mean_time_tot_pop_MF1,na.rm = TRUE)
# sd_DRM_MF1 = sd(mean_time_tot_pop_MF1,na.rm = TRUE)


######### MF 2
setwd("....")
file_DRM_MF2 = "Cyprinidae_TX_EMF DRM-MF2-table.csv"
DRM_table_MF2=read.csv(file_DRM_MF2, header=F,stringsAsFactors=FALSE)

NAMES <- read.csv(file_DRM_MF2, nrow = 6, sep = ";")
DRM_table_MF2 <- read.csv(file_DRM_MF2, skip = 6, stringsAsFactors = FALSE, sep = ",")
words <- strsplit(NAMES[6,1], ",")[[1]]
xx=unique(tolower(words))

names(DRM_table_MF2) <- xx


Tstart_exp_DRM = 20
Tend_exp_DRM = Tstart_exp_DRM + 5
Tsim_DRM = Tend_exp_DRM + 3
DIM_DRM = 364*24+365*(Tsim_DRM - 2)*24+3
Start_tox_DRM = 364*24+365*(Tstart_exp_DRM -2)*24 +3
End_tox_DRM = 364*24+365*(Tend_exp_DRM - 2)*24 +2
End_tox_DRM_bis = End_tox_DRM + 1 
n_var = 4

R=nrow(DRM_table_MF2)

DRM_matrix <- matrix(, nrow = R, ncol = 1)

n_replicates = 40 
for (i in 1:n_replicates)
{
  dataframe = DRM_table_MF2[DRM_table_MF2[, "[run number]"] == i,]
  assign(paste("Matrix",i, sep=""), select(dataframe, 
                                           c("count turtles with [u_h > u_h^b]","count turtles with [u_h > u_h^p]","sum [weight] of turtles with [u_h > u_h^b]","w-turtles")))
}

for (i in 1:n_replicates)
{
  x= nrow(get(paste("Matrix",i, sep="")))
  if(x != DIM_DRM)
  {print (i)
    print (x)}
}

DRM_m_MF2 = Matrix1
# DRM_m_MF2[is.na(DRM_m_MF2)] = 0  careful if simulation ended before because of extinction --> consider 0 values


for (i in 2:n_replicates)
{
  DRM_m_MF2 = cbind (DRM_m_MF2,get(paste("Matrix",i, sep="")))}
colTot=ncol(DRM_m_MF2)

col.names = rep(c("Tot pop","Adult pop","Tot weight","Adult weight"),n_replicates)
colnames(DRM_m_MF2) <-col.names

Average_MF2_DRM_NEW = colMeans(DRM_m_MF2[Start_tox_DRM:End_tox_DRM,],na.rm = TRUE)
mean_time_tot_pop_MF2 <- matrix(, nrow = n_replicates, ncol = 1)

ii = 1
i = 1
while (ii <= length(Average_MF2_DRM_NEW))
{
  mean_time_tot_pop_MF2[i] = Average_MF2_DRM_NEW[ii]
  i = i + 1
  ii = ii + 4
}

# Average_DRM_MF2 = mean(mean_time_tot_pop_MF2,na.rm = TRUE)
# sd_DRM_MF2 = sd(mean_time_tot_pop_MF2,na.rm = TRUE)


######### MF 4
setwd("....")
file_DRM_MF4 = "Cyprinidae_TX_EMF DRM-MF4-table.csv"
DRM_table_MF4=read.csv(file_DRM_MF4, header=F,stringsAsFactors=FALSE)

NAMES <- read.csv(file_DRM_MF4, nrow = 6, sep = ";")
DRM_table_MF4 <- read.csv(file_DRM_MF4, skip = 6, stringsAsFactors = FALSE, sep = ",")
words <- strsplit(NAMES[6,1], ",")[[1]]
xx=unique(tolower(words))
names(DRM_table_MF4) <- xx

Tstart_exp_DRM = 20
Tend_exp_DRM = Tstart_exp_DRM + 5
Tsim_DRM = Tend_exp_DRM + 3
DIM_DRM = 364*24+365*(Tsim_DRM - 2)*24+3
Start_tox_DRM = 364*24+365*(Tstart_exp_DRM -2)*24 +3
End_tox_DRM = 364*24+365*(Tend_exp_DRM - 2)*24 +2
End_tox_DRM_bis = End_tox_DRM + 1 
n_var = 4
R=nrow(DRM_table_MF4)

DRM_matrix <- matrix(, nrow = R, ncol = 1)
n_replicates = 40 # change here

for (i in 1:n_replicates)
{
  dataframe = DRM_table_MF4[DRM_table_MF4[, "[run number]"] == i,]
  assign(paste("Matrix",i, sep=""), select(dataframe, 
                                           c("count turtles with [u_h > u_h^b]","count turtles with [u_h > u_h^p]","sum [weight] of turtles with [u_h > u_h^b]","w-turtles")))
}

for (i in 1:n_replicates)
{
  x= nrow(get(paste("Matrix",i, sep="")))
  if(x != DIM_DRM)
  {print (i)
    print (x)}
}


DRM_m_MF4 = Matrix1
DRM_m_MF4[is.na(DRM_m_MF4)] = 0

for (i in 2:n_replicates)
{
  DRM_m_MF4 = cbind (DRM_m_MF4,get(paste("Matrix",i, sep="")))}
colTot=ncol(DRM_m_MF4)


col.names = rep(c("Tot pop","Adult pop","Tot weight","Adult weight"),n_replicates)
colnames(DRM_m_MF4) <-col.names

Average_MF4_DRM_NEW = colMeans(DRM_m_MF4[Start_tox_DRM:End_tox_DRM,],na.rm = TRUE)
mean_time_tot_pop_MF4 <- matrix(, nrow = n_replicates, ncol = 1)

ii = 1
i = 1
while (ii <= length(Average_MF4_DRM_NEW))
{
  mean_time_tot_pop_MF4[i] = Average_MF4_DRM_NEW[ii]
  i = i + 1
  ii = ii + 4
}

# Average_DRM_MF4 = mean(mean_time_tot_pop_MF4,na.rm = TRUE)
# sd_DRM_MF4 = sd(mean_time_tot_pop_MF4,na.rm = TRUE)


######### MF 7
setwd("....")
file_DRM_MF7 = "Cyprinidae_TX_EMF DRM-MF7-table.csv"
DRM_table_MF7=read.csv(file_DRM_MF7, header=F,stringsAsFactors=FALSE)
NAMES <- read.csv(file_DRM_MF7, nrow = 6, sep = ";")
DRM_table_MF7 <- read.csv(file_DRM_MF7, skip = 6, stringsAsFactors = FALSE, sep = ",")
words <- strsplit(NAMES[6,1], ",")[[1]]
xx=unique(tolower(words))
names(DRM_table_MF7) <- xx

Tstart_exp_DRM = 20
Tend_exp_DRM = Tstart_exp_DRM + 5
Tsim_DRM = Tend_exp_DRM + 3
DIM_DRM = 364*24+365*(Tsim_DRM - 2)*24+3
Start_tox_DRM = 364*24+365*(Tstart_exp_DRM -2)*24 +3
End_tox_DRM = 364*24+365*(Tend_exp_DRM - 2)*24 +2
End_tox_DRM_bis = End_tox_DRM + 1 
n_var = 4
R=nrow(DRM_table_MF7)

DRM_matrix <- matrix(, nrow = R, ncol = 1)
n_replicates = 40 # change here

for (i in 1:n_replicates)
{
  dataframe = DRM_table_MF7[DRM_table_MF7[, "[run number]"] == i,]
  assign(paste("Matrix",i, sep=""), select(dataframe, 
                                           c("count turtles with [u_h > u_h^b]","count turtles with [u_h > u_h^p]","sum [weight] of turtles with [u_h > u_h^b]","w-turtles")))
}

for (i in 1:n_replicates)
{
  x= nrow(get(paste("Matrix",i, sep="")))
  if(x != DIM_DRM)
  {print (i)
    print (x)
    absent_rec = DIM_DRM-x
    alpha = matrix (0,nrow = absent_rec, ncol = 4)
    g=get(paste("Matrix",i, sep=""))
    colnames(alpha) <- colnames(g)
    gg=rbind(g,alpha)
    assign(paste("Matrix",i, sep=""), gg)
  }
}

DRM_m_MF7 = Matrix1
DRM_m_MF7[is.na(DRM_m_MF7)] = 0

for (i in 2:n_replicates)
{
  DRM_m_MF7 = cbind (DRM_m_MF7,get(paste("Matrix",i, sep="")))}
colTot=ncol(DRM_m_MF7)

col.names = rep(c("Tot pop","Adult pop","Tot weight","Adult weight"),n_replicates)
colnames(DRM_m_MF7) <-col.names

Average_MF7_DRM_NEW = colMeans(DRM_m_MF7[Start_tox_DRM:End_tox_DRM,],na.rm = TRUE)
mean_time_tot_pop_MF7 <- matrix(, nrow = n_replicates, ncol = 1)

ii = 1
i = 1
while (ii <= length(Average_MF7_DRM_NEW))
{
  mean_time_tot_pop_MF7[i] = Average_MF7_DRM_NEW[ii]
  i = i + 1
  ii = ii + 4
}

# Average_DRM_MF7 = mean(mean_time_tot_pop_MF7,na.rm = TRUE)
# sd_DRM_MF7 = sd(mean_time_tot_pop_MF7,na.rm = TRUE)


# ######### MF 8
setwd("....")
file_DRM_MF8 = "Cyprinidae_TX_EMF DRM-MF8-table.csv"
DRM_table_MF8=read.csv(file_DRM_MF8, header=F,stringsAsFactors=FALSE)
NAMES <- read.csv(file_DRM_MF8, nrow = 6, sep = ";")
DRM_table_MF8 <- read.csv(file_DRM_MF8, skip = 6, stringsAsFactors = FALSE, sep = ",")
words <- strsplit(NAMES[6,1], ",")[[1]]
xx=unique(tolower(words))
names(DRM_table_MF8) <- xx

Tstart_exp_DRM = 20
Tend_exp_DRM = Tstart_exp_DRM + 5
Tsim_DRM = Tend_exp_DRM + 3
DIM_DRM = 364*24+365*(Tsim_DRM - 2)*24+3
Start_tox_DRM = 364*24+365*(Tstart_exp_DRM -2)*24 +3
End_tox_DRM = 364*24+365*(Tend_exp_DRM - 2)*24 +2
End_tox_DRM_bis = End_tox_DRM + 1 
n_var = 4
R=nrow(DRM_table_MF8)

DRM_matrix <- matrix(, nrow = R, ncol = 1)
n_replicates = 40 

for (i in 1:n_replicates)
{
  dataframe = DRM_table_MF8[DRM_table_MF8[, "[run number]"] == i,]
  assign(paste("Matrix",i, sep=""), select(dataframe, 
                                           c("count turtles with [u_h > u_h^b]","count turtles with [u_h > u_h^p]","sum [weight] of turtles with [u_h > u_h^b]","w-turtles")))
}

for (i in 1:n_replicates)
{
  x= nrow(get(paste("Matrix",i, sep="")))
  if(x != DIM_DRM)
  {print (i)
    print (x)
    absent_rec = DIM_DRM-x
    alpha = matrix (0,nrow = absent_rec, ncol = 4)
    g=get(paste("Matrix",i, sep=""))
    colnames(alpha) <- colnames(g)
    gg=rbind(g,alpha)
    assign(paste("Matrix",i, sep=""), gg)
  }
}

DRM_m_MF8 = Matrix1
DRM_m_MF8[is.na(DRM_m_MF8)] = 0

for (i in 2:n_replicates)
{
  DRM_m_MF8 = cbind (DRM_m_MF8,get(paste("Matrix",i, sep="")))}
colTot=ncol(DRM_m_MF8)

col.names = rep(c("Tot pop","Adult pop","Tot weight","Adult weight"),n_replicates)
colnames(DRM_m_MF8) <-col.names

Average_MF8_DRM_NEW = colMeans(DRM_m_MF8[Start_tox_DRM:End_tox_DRM,],na.rm = TRUE)
mean_time_tot_pop_MF8 <- matrix(, nrow = n_replicates, ncol = 1)

ii = 1
i = 1
while (ii <= length(Average_MF8_DRM_NEW))
{
  mean_time_tot_pop_MF8[i] = Average_MF8_DRM_NEW[ii]
  i = i + 1
  ii = ii + 4
}

# Average_DRM_MF8 = mean(mean_time_tot_pop_MF8,na.rm = TRUE)
# sd_DRM_MF8 = sd(mean_time_tot_pop_MF8,na.rm = TRUE)


######### MF 10
setwd("....")
file_DRM_MF10 = "Cyprinidae_TX_EMF DRM-MF10-table.csv"
DRM_table_MF10=read.csv(file_DRM_MF10, header=F,stringsAsFactors=FALSE)
NAMES <- read.csv(file_DRM_MF10, nrow = 6, sep = ";")
DRM_table_MF10 <- read.csv(file_DRM_MF10, skip = 6, stringsAsFactors = FALSE, sep = ",")
words <- strsplit(NAMES[6,1], ",")[[1]]
xx=unique(tolower(words))
names(DRM_table_MF10) <- xx

Tstart_exp_DRM = 20
Tend_exp_DRM = Tstart_exp_DRM + 5
Tsim_DRM = Tend_exp_DRM + 3
DIM_DRM = 364*24+365*(Tsim_DRM - 2)*24+3
Start_tox_DRM = 364*24+365*(Tstart_exp_DRM -2)*24 +3
End_tox_DRM = 364*24+365*(Tend_exp_DRM - 2)*24 +2
End_tox_DRM_bis = End_tox_DRM + 1 
n_var = 4
R=nrow(DRM_table_MF10)

DRM_matrix <- matrix(, nrow = R, ncol = 1)
n_replicates = 40 

for (i in 1:n_replicates)
{
  dataframe = DRM_table_MF10[DRM_table_MF10[, "[run number]"] == i,]
  assign(paste("Matrix",i, sep=""), select(dataframe, 
                                           c("count turtles with [u_h > u_h^b]","count turtles with [u_h > u_h^p]","sum [weight] of turtles with [u_h > u_h^b]","w-turtles")))
}

for (i in 1:n_replicates)
{
  x= nrow(get(paste("Matrix",i, sep="")))
  if(x != DIM_DRM)
  {print (i)
    print (x)
    absent_rec = DIM_DRM-x
    alpha = matrix (0,nrow = absent_rec, ncol = 4)
    g=get(paste("Matrix",i, sep=""))
    colnames(alpha) <- colnames(g)
    gg=rbind(g,alpha)
    assign(paste("Matrix",i, sep=""), gg)
  }
}

DRM_m_MF10 = Matrix1
DRM_m_MF10[is.na(DRM_m_MF10)] = 0

for (i in 2:n_replicates)
{
  DRM_m_MF10 = cbind (DRM_m_MF10,get(paste("Matrix",i, sep="")))}
colTot=ncol(DRM_m_MF10)

col.names = rep(c("Tot pop","Adult pop","Tot weight","Adult weight"),n_replicates)
colnames(DRM_m_MF10) <-col.names


Average_MF10_DRM_NEW = colMeans(DRM_m_MF10[Start_tox_DRM:End_tox_DRM,],na.rm = TRUE)
mean_time_tot_pop_MF10 <- matrix(, nrow = n_replicates, ncol = 1)

ii = 1
i = 1
while (ii <= length(Average_MF10_DRM_NEW))
{
  mean_time_tot_pop_MF10[i] = Average_MF10_DRM_NEW[ii]
  i = i + 1
  ii = ii + 4
}

# Average_DRM_MF10 = mean(mean_time_tot_pop_MF10,na.rm = TRUE)
# sd_DRM_MF10 = sd(mean_time_tot_pop_MF10,na.rm = TRUE)


######### MF 11
setwd("....")
file_DRM_MF11 = "Cyprinidae_TX_EMF DRM-MF11-table.csv"
DRM_table_MF11=read.csv(file_DRM_MF11, header=F,stringsAsFactors=FALSE)
NAMES <- read.csv(file_DRM_MF11, nrow = 6, sep = ";")
DRM_table_MF11 <- read.csv(file_DRM_MF11, skip = 6, stringsAsFactors = FALSE, sep = ",")
words <- strsplit(NAMES[6,1], ",")[[1]]
xx=unique(tolower(words))
names(DRM_table_MF11) <- xx

Tstart_exp_DRM = 20
Tend_exp_DRM = Tstart_exp_DRM + 5
Tsim_DRM = Tend_exp_DRM + 3
DIM_DRM = 364*24+365*(Tsim_DRM - 2)*24+3
Start_tox_DRM = 364*24+365*(Tstart_exp_DRM -2)*24 +3
End_tox_DRM = 364*24+365*(Tend_exp_DRM - 2)*24 +2
End_tox_DRM_bis = End_tox_DRM + 1 
n_var = 4
R=nrow(DRM_table_MF11)
DRM_matrix <- matrix(, nrow = R, ncol = 1)

n_replicates = 40 # change here

for (i in 1:n_replicates)
{
  dataframe = DRM_table_MF11[DRM_table_MF11[, "[run number]"] == i,]
  assign(paste("Matrix",i, sep=""), select(dataframe, 
                                           c("count turtles with [u_h > u_h^b]","count turtles with [u_h > u_h^p]","sum [weight] of turtles with [u_h > u_h^b]","w-turtles")))
}

for (i in 1:n_replicates)
{
  x= nrow(get(paste("Matrix",i, sep="")))
  if(x != DIM_DRM)
  {print (i)
    print (x)
    absent_rec = DIM_DRM-x
    alpha = matrix (0,nrow = absent_rec, ncol = 4)
    g=get(paste("Matrix",i, sep=""))
    colnames(alpha) <- colnames(g)
    gg=rbind(g,alpha)
    assign(paste("Matrix",i, sep=""), gg)
  }
}

DRM_m_MF11 = Matrix1
DRM_m_MF11[is.na(DRM_m_MF11)] = 0

for (i in 2:n_replicates)
{
  DRM_m_MF11 = cbind (DRM_m_MF11,get(paste("Matrix",i, sep="")))}
colTot=ncol(DRM_m_MF11)


col.names = rep(c("Tot pop","Adult pop","Tot weight","Adult weight"),n_replicates)
colnames(DRM_m_MF11) <-col.names

Average_MF11_DRM_NEW = colMeans(DRM_m_MF11[Start_tox_DRM:End_tox_DRM,],na.rm = TRUE)
mean_time_tot_pop_MF11 <- matrix(, nrow = n_replicates, ncol = 1)

ii = 1
i = 1
while (ii <= length(Average_MF11_DRM_NEW))
{
  mean_time_tot_pop_MF11[i] = Average_MF11_DRM_NEW[ii]
  i = i + 1
  ii = ii + 4
}

# Average_DRM_MF11 = mean(mean_time_tot_pop_MF11,na.rm = TRUE)
# sd_DRM_MF11 = sd(mean_time_tot_pop_MF11,na.rm = TRUE)

time_DRM = 1
rowTot=nrow(DRM_mr)


# ratio_DRM = c(Average_no_tox_DRM_MF1 / Average_no_tox_DRM,Average_no_tox_DRM_MF2 / Average_no_tox_DRM,Average_no_tox_DRM_MF4 / Average_no_tox_DRM,
#               Average_no_tox_DRM_MF5 / Average_no_tox_DRM, Average_no_tox_DRM_MF7 / Average_no_tox_DRM,Average_no_tox_DRM_MF8 / Average_no_tox_DRM,
#               Average_no_tox_DRM_MF10 / Average_no_tox_DRM,Average_no_tox_DRM_MF11 / Average_no_tox_DRM)#, Average_tox_SD_MF70 / Average_no_tox_DRM)

####################### **********************************  ********************************** #######################################
###################### ********************************** ############################################################################
####################### **********************************  ********************************** #######################################
######## SD

setwd("....")
file_SD_noTox = "TX_SD-W.csv"
SD_noTox=read.csv(file_SD_noTox, header = T, as.is = T)
Tstart_exp_SD = 15
Tend_exp_SD = Tstart_exp_SD + 5
Tsim_SD = Tend_exp_SD + 3
DIM_SD = 365-89+(Tsim_SD - 2)*365+1
Start_tox_SD = 365-89+(Tstart_exp_SD -2)*365 +1
End_tox_SD = 365-89+(Tend_exp_SD - 2)*365
End_tox_SD_bis = End_tox_SD +1

n_var =4
R=nrow(SD_noTox)
C=ncol(SD_noTox)
n_replicates = (C - 1)/n_var

Tot <- matrix(, nrow = R, ncol = 1)

i=2
M1=SD_noTox[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  SD_noTox[,c]
  M1=cbind(M1,x)
}

Average_time_SD = colMeans(M1[Start_tox_SD:End_tox_SD,],na.rm = TRUE)

# Average_no_tox_SD2 = mean(Average_time_SD,na.rm = TRUE)
# sd_no_tox_SD2 = sd(Average_time_SD,na.rm = TRUE)


############################################################################################################################################
##########---------------------- MF 1

setwd("....")
file_SD_MF1 = "TX_SD-MF1.csv"
SD_Tox_MF1=read.csv(file_SD_MF1, header = T, as.is = T)
Tstart_exp_SD = 15
Tend_exp_SD = Tstart_exp_SD + 5
Tsim_SD = Tend_exp_SD + 3
DIM_SD = 365-89+(Tsim_SD - 2)*365+1
Start_tox_SD = 365-89+(Tstart_exp_SD -2)*365 +1
End_tox_SD = 365-89+(Tend_exp_SD - 2)*365
End_tox_SD_bis = End_tox_SD +1

n_var =4
R=nrow(SD_Tox_MF1)
C=ncol(SD_Tox_MF1)
n_replicates = (C - 1)/n_var
Tot_Tox_MF1 <- matrix(, nrow = R, ncol = 1)

SD_Tox_MF1[is.na(SD_Tox_MF1)] = 0

i=2
M1_Tox_MF1 = SD_Tox_MF1[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  SD_Tox_MF1[,c]
  M1_Tox_MF1=cbind(M1_Tox_MF1,x)
}

Average_time_SD_Tox_MF1 = colMeans(M1_Tox_MF1[Start_tox_SD:End_tox_SD,],na.rm = TRUE)

# Average_tox_SD_MF1S = mean(Average_time_SD_Tox_MF1,na.rm = TRUE)
# sd_tox_SD_MF1S = sd(Average_time_SD_Tox_MF1,na.rm = TRUE)


#### MF2
setwd("....")
file_SD_MF2 = "TX_SD-MF2.csv"
SD_Tox_MF2=read.csv(file_SD_MF2, header = T, as.is = T)
Tstart_exp_SD = 15
Tend_exp_SD = Tstart_exp_SD + 5
Tsim_SD = Tend_exp_SD + 3
DIM_SD = 365-89+(Tsim_SD - 2)*365+1
Start_tox_SD = 365-89+(Tstart_exp_SD -2)*365 +1
End_tox_SD = 365-89+(Tend_exp_SD - 2)*365
End_tox_SD_bis = End_tox_SD +1

n_var =4
R=nrow(SD_Tox_MF2)
C=ncol(SD_Tox_MF2)
n_replicates = (C - 1)/n_var
Tot_Tox_MF2 <- matrix(, nrow = R, ncol = 1)

SD_Tox_MF2[is.na(SD_Tox_MF2)] = 0

i=2
M1_Tox_MF2 = SD_Tox_MF2[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  SD_Tox_MF2[,c]
  M1_Tox_MF2=cbind(M1_Tox_MF2,x)
}

Average_time_SD_Tox_MF2 = colMeans(M1_Tox_MF2[Start_tox_SD:End_tox_SD,],na.rm = TRUE)

# Average_tox_SD_MF2S = mean(Average_time_SD_Tox_MF2,na.rm = TRUE)
# sd_tox_SD_MF2S = sd(Average_time_SD_Tox_MF2,na.rm = TRUE)


#### MF4
setwd("....")
file_SD_MF4 = "TX_SD-MF4.csv"
SD_Tox_MF4=read.csv(file_SD_MF4, header = T, as.is = T)
Tstart_exp_SD = 15
Tend_exp_SD = Tstart_exp_SD + 5
Tsim_SD = Tend_exp_SD + 3
DIM_SD = 365-89+(Tsim_SD - 2)*365+1
Start_tox_SD = 365-89+(Tstart_exp_SD -2)*365 +1
End_tox_SD = 365-89+(Tend_exp_SD - 2)*365
End_tox_SD_bis = End_tox_SD +1

n_var =4
R=nrow(SD_Tox_MF4)
C=ncol(SD_Tox_MF4)
n_replicates = (C - 1)/n_var

SD_Tox_MF4[is.na(SD_Tox_MF4)] = 0


i=2
M1_Tox_MF4 = SD_Tox_MF4[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  SD_Tox_MF4[,c]
  M1_Tox_MF4=cbind(M1_Tox_MF4,x)
}

Average_time_SD_Tox_MF4 = colMeans(M1_Tox_MF4[Start_tox_SD:End_tox_SD,],na.rm = TRUE)

# Average_tox_SD_MF4S = mean(Average_time_SD_Tox_MF4,na.rm = TRUE)
# sd_tox_SD_MF4S = sd(Average_time_SD_Tox_MF4,na.rm = TRUE)

### MF 5
setwd("....")
file_SD_MF5 = "TX_SD-MF5.csv"
SD_Tox_MF5=read.csv(file_SD_MF5, header = T, as.is = T)
Tstart_exp_SD = 15
Tend_exp_SD = Tstart_exp_SD + 5
Tsim_SD = Tend_exp_SD + 3
DIM_SD = 365-89+(Tsim_SD - 2)*365+1
Start_tox_SD = 365-89+(Tstart_exp_SD -2)*365 +1
End_tox_SD = 365-89+(Tend_exp_SD - 2)*365
End_tox_SD_bis = End_tox_SD +1

n_var =4
R=nrow(SD_Tox_MF5)
C=ncol(SD_Tox_MF5)
n_replicates = (C - 1)/n_var

SD_Tox_MF5[is.na(SD_Tox_MF5)] = 0

i=2
M1_Tox_MF5 = SD_Tox_MF5[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  SD_Tox_MF5[,c]
  M1_Tox_MF5=cbind(M1_Tox_MF5,x)
}

Average_time_SD_Tox_MF5 = colMeans(M1_Tox_MF5[Start_tox_SD:End_tox_SD,],na.rm = TRUE)

# Average_tox_SD_MF5S = mean(Average_time_SD_Tox_MF5,na.rm = TRUE)
# sd_tox_SD_MF5S = sd(Average_time_SD_Tox_MF5,na.rm = TRUE)

############################################################################################################################################
##########---------------------- MF 7 
setwd("....")
file_SD_MF7 = "TX_SD-MF7.csv"
SD_Tox_MF7=read.csv(file_SD_MF7, header = T, as.is = T)
Tstart_exp_SD = 15
Tend_exp_SD = Tstart_exp_SD + 5
Tsim_SD = Tend_exp_SD + 3
DIM_SD = 365-89+(Tsim_SD - 2)*365+1
Start_tox_SD = 365-89+(Tstart_exp_SD -2)*365 +1
End_tox_SD = 365-89+(Tend_exp_SD - 2)*365
End_tox_SD_bis = End_tox_SD +1

n_var =4
R=nrow(SD_Tox_MF7)
C=ncol(SD_Tox_MF7)
n_replicates = (C - 1)/n_var
SD_Tox_MF7[is.na(SD_Tox_MF7)] = 0

i=2
M1_Tox_MF7 = SD_Tox_MF7[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  SD_Tox_MF7[,c]
  M1_Tox_MF7=cbind(M1_Tox_MF7,x)
}

Average_time_SD_Tox_MF7 = colMeans(M1_Tox_MF7[Start_tox_SD:End_tox_SD,],na.rm = TRUE)

# Average_tox_SD_MF7S = mean(Average_time_SD_Tox_MF7,na.rm = TRUE)
# sd_tox_SD_MF7S = sd(Average_time_SD_Tox_MF7,na.rm = TRUE)

############################################################################################################################################
##########---------------------- MF 8 
setwd("....")
file_SD_MF8 = "TX_SD-MF8.csv"
SD_Tox_MF8=read.csv(file_SD_MF8, header = T, as.is = T)
Tstart_exp_SD = 15
Tend_exp_SD = Tstart_exp_SD + 5
Tsim_SD = Tend_exp_SD + 3
DIM_SD = 365-89+(Tsim_SD - 2)*365+1
Start_tox_SD = 365-89+(Tstart_exp_SD -2)*365 +1
End_tox_SD = 365-89+(Tend_exp_SD - 2)*365
End_tox_SD_bis = End_tox_SD +1

n_var =4
R=nrow(SD_Tox_MF8)
C=ncol(SD_Tox_MF8)
n_replicates = (C - 1)/n_var
SD_Tox_MF8[is.na(SD_Tox_MF8)] = 0

i=2
M1_Tox_MF8 = SD_Tox_MF8[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  SD_Tox_MF8[,c]
  M1_Tox_MF8=cbind(M1_Tox_MF8,x)
}

Average_time_SD_Tox_MF8 = colMeans(M1_Tox_MF8[Start_tox_SD:End_tox_SD,],na.rm = TRUE)

# Average_tox_SD_MF8S = mean(Average_time_SD_Tox_MF8,na.rm = TRUE)
# sd_tox_SD_MF8S = sd(Average_time_SD_Tox_MF8,na.rm = TRUE)


############################################################################################################################################
##########---------------------- MF 10
setwd("....")
file_SD_MF10 = "TX_SD-MF10.csv"
SD_Tox_MF10=read.csv(file_SD_MF10, header = T, as.is = T)
Tstart_exp_SD = 15
Tend_exp_SD = Tstart_exp_SD + 5
Tsim_SD = Tend_exp_SD + 3
DIM_SD = 365-89+(Tsim_SD - 2)*365+1
Start_tox_SD = 365-89+(Tstart_exp_SD -2)*365 +1
End_tox_SD = 365-89+(Tend_exp_SD - 2)*365
End_tox_SD_bis = End_tox_SD +1

n_var =4
R=nrow(SD_Tox_MF10)
C=ncol(SD_Tox_MF10)
n_replicates = (C - 1)/n_var
Tot_Tox_MF10 <- matrix(, nrow = R, ncol = 1)

absent_rec = DIM_SD-R
alpha = matrix (0,nrow = absent_rec, ncol = C)

colnames(alpha) = nameCol
SD_Tox_MF10 = rbind(SD_Tox_MF10,alpha)
SD_Tox_MF10[is.na(SD_Tox_MF10)] = 0

i=2
M1_Tox_MF10 = SD_Tox_MF10[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  SD_Tox_MF10[,c]
  M1_Tox_MF10=cbind(M1_Tox_MF10,x)
}

Average_time_SD_Tox_MF10 = colMeans(M1_Tox_MF10[Start_tox_SD:End_tox_SD,],na.rm = TRUE)

# Average_tox_SD_MF10S = mean(Average_time_SD_Tox_MF10,na.rm = TRUE)
# sd_tox_SD_MF10S = sd(Average_time_SD_Tox_MF10,na.rm = TRUE)

############################################################################################################################################
##########---------------------- MF 11
setwd("....")
file_SD_MF11 = "TX_SD-MF11.csv"
SD_Tox_MF11=read.csv(file_SD_MF11, header = T, as.is = T)
Tstart_exp_SD = 15
Tend_exp_SD = Tstart_exp_SD + 5
Tsim_SD = Tend_exp_SD + 3
DIM_SD = 365-89+(Tsim_SD - 2)*365+1
Start_tox_SD = 365-89+(Tstart_exp_SD -2)*365 +1
End_tox_SD = 365-89+(Tend_exp_SD - 2)*365
End_tox_SD_bis = End_tox_SD +1

n_var =4
R=nrow(SD_Tox_MF11)
C=ncol(SD_Tox_MF11)
n_replicates = (C - 1)/n_var

absent_rec = DIM_SD-R
alpha = matrix (0,nrow = absent_rec, ncol = C)
nameCol = colnames(SD_Tox_MF11)
colnames(alpha) = nameCol
SD_Tox_MF11 = rbind(SD_Tox_MF11,alpha)
SD_Tox_MF11[is.na(SD_Tox_MF11)] = 0

i=2
M1_Tox_MF11 = SD_Tox_MF11[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  SD_Tox_MF11[,c]
  M1_Tox_MF11=cbind(M1_Tox_MF11,x)
}

Average_time_SD_Tox_MF11 = colMeans(M1_Tox_MF11[Start_tox_SD:End_tox_SD,],na.rm = TRUE)

# Average_tox_SD_MF11S = mean(Average_time_SD_Tox_MF11,na.rm = TRUE)
# sd_tox_SD_MF11S = sd(Average_time_SD_Tox_MF11,na.rm = TRUE)

time_SD=365-88

# ratio_SD = c(Average_tox_SD_MF1 / Average_no_tox_SD,Average_tox_SD_MF2 / Average_no_tox_SD, Average_tox_SD_MF4 / Average_no_tox_SD,
#              Average_tox_SD_MF5 / Average_no_tox_SD,
#              Average_tox_SD_MF7 / Average_no_tox_SD,
#              Average_tox_SD_MF8 / Average_no_tox_SD, Average_tox_SD_MF10 / Average_no_tox_SD, Average_tox_SD_MF11 / Average_no_tox_SD)


#############
###################### ********************************** ############################################################################
####################### **********************************  ********************************** #######################################
#####   HC


setwd("....")
file_HC_noTox = "TX_HC-W.csv"
HC_noTox=read.csv(file_HC_noTox, header = T, as.is = T)
Tstart_exp_HC = 25
Tend_exp_HC = Tstart_exp_HC + 5
Tsim_HC = Tend_exp_HC + 3
DIM_HC = 365-79+(Tsim_HC - 2)*365+1
Start_tox_HC = 365-79+(Tstart_exp_HC -2)*365 +1
End_tox_HC = 365-79+(Tend_exp_HC - 2)*365
End_tox_HC_bis = End_tox_HC +1
n_var =4

R=nrow(HC_noTox)
C=ncol(HC_noTox)
n_replicates = (C - 1)/n_var

i=2
M1=HC_noTox[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  HC_noTox[,c]
  M1=cbind(M1,x)
}

Average_time_HC = colMeans(M1[Start_tox_HC:End_tox_HC,],na.rm = TRUE)
# Average_no_tox_HC2 = mean(Average_time_HC,na.rm = TRUE)
# sd_no_tox_HC2 = sd(Average_time_HC,na.rm = TRUE)


############################################################################################################################################
##########---------------------- MF 1

setwd("....")
file_HC_MF1 = "TX_HC-MF1.csv"
HC_Tox_MF1=read.csv(file_HC_MF1, header = T, as.is = T)
Tstart_exp_HC = 25
Tend_exp_HC = Tstart_exp_HC + 5
Tsim_HC = Tend_exp_HC + 3
DIM_HC = 365-79+(Tsim_HC - 2)*365+1
Start_tox_HC = 365-79+(Tstart_exp_HC -2)*365 +1
End_tox_HC = 365-79+(Tend_exp_HC - 2)*365
End_tox_HC_bis = End_tox_HC +1
n_var =4

R=nrow(HC_Tox_MF1)
C=ncol(HC_Tox_MF1)
n_replicates = (C - 1)/n_var

HC_Tox_MF1[is.na(HC_Tox_MF1)] = 0

i=2
M1_Tox_MF1 = HC_Tox_MF1[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  HC_Tox_MF1[,c]
  M1_Tox_MF1=cbind(M1_Tox_MF1,x)
}

Average_time_HC_Tox_MF1 = colMeans(M1_Tox_MF1[Start_tox_HC:End_tox_HC,],na.rm = TRUE)
# 
# Average_tox_HC_MF1S = mean(Average_time_HC_Tox_MF1,na.rm = TRUE)
# sd_tox_HC_MF1S = sd(Average_time_HC_Tox_MF1,na.rm = TRUE)

setwd("...."))
file_HC_MF2 = "TX_HC-MF2.csv"
HC_Tox_MF2=read.csv(file_HC_MF2, header = T, as.is = T)
Tstart_exp_HC = 25
Tend_exp_HC = Tstart_exp_HC + 5
Tsim_HC = Tend_exp_HC + 3
DIM_HC = 365-79+(Tsim_HC - 2)*365+1
Start_tox_HC = 365-79+(Tstart_exp_HC -2)*365 +1
End_tox_HC = 365-79+(Tend_exp_HC - 2)*365
End_tox_HC_bis = End_tox_HC +1
n_var =4

R=nrow(HC_Tox_MF2)
C=ncol(HC_Tox_MF2)
n_replicates = (C - 1)/n_var
HC_Tox_MF2[is.na(HC_Tox_MF2)] = 0

i=2
M1_Tox_MF2 = HC_Tox_MF2[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  HC_Tox_MF2[,c]
  M1_Tox_MF2=cbind(M1_Tox_MF2,x)
}

Average_time_HC_Tox_MF2 = colMeans(M1_Tox_MF2[Start_tox_HC:End_tox_HC,],na.rm = TRUE)

# Average_tox_HC_MF2S = mean(Average_time_HC_Tox_MF2,na.rm = TRUE)
# sd_tox_HC_MF2S = sd(Average_time_HC_Tox_MF2,na.rm = TRUE)

setwd("....")
file_HC_MF4 = "TX_HC-MF4.csv"
HC_Tox_MF4=read.csv(file_HC_MF4, header = T, as.is = T)
Tstart_exp_HC = 25
Tend_exp_HC = Tstart_exp_HC + 5
Tsim_HC = Tend_exp_HC + 3
DIM_HC = 365-79+(Tsim_HC - 2)*365+1
Start_tox_HC = 365-79+(Tstart_exp_HC -2)*365 +1
End_tox_HC = 365-79+(Tend_exp_HC - 2)*365
End_tox_HC_bis = End_tox_HC +1
n_var =4

R=nrow(HC_Tox_MF4)
C=ncol(HC_Tox_MF4)
n_replicates = (C - 1)/n_var
HC_Tox_MF4[is.na(HC_Tox_MF4)] = 0

i=2
M1_Tox_MF4 = HC_Tox_MF4[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  HC_Tox_MF4[,c]
  M1_Tox_MF4=cbind(M1_Tox_MF4,x)
}

Average_time_HC_Tox_MF4 = colMeans(M1_Tox_MF4[Start_tox_HC:End_tox_HC,],na.rm = TRUE)

# Average_tox_HC_MF4S = mean(Average_time_HC_Tox_MF4,na.rm = TRUE)
# sd_tox_HC_MF4S = sd(Average_time_HC_Tox_MF4,na.rm = TRUE)

setwd("....")
file_HC_MF5 = "TX_HC-MF5.csv"
HC_Tox_MF5=read.csv(file_HC_MF5, header = T, as.is = T)
Tstart_exp_HC = 25
Tend_exp_HC = Tstart_exp_HC + 5
Tsim_HC = Tend_exp_HC + 3
DIM_HC = 365-79+(Tsim_HC - 2)*365+1
Start_tox_HC = 365-79+(Tstart_exp_HC -2)*365 +1
End_tox_HC = 365-79+(Tend_exp_HC - 2)*365
End_tox_HC_bis = End_tox_HC +1
n_var =4

R=nrow(HC_Tox_MF5)
C=ncol(HC_Tox_MF5)
n_replicates = (C - 1)/n_var

HC_Tox_MF5[is.na(HC_Tox_MF5)] = 0

i=2
M1_Tox_MF5 = HC_Tox_MF5[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  HC_Tox_MF5[,c]
  M1_Tox_MF5=cbind(M1_Tox_MF5,x)
}

Average_time_HC_Tox_MF5 = colMeans(M1_Tox_MF5[Start_tox_HC:End_tox_HC,],na.rm = TRUE)

# Average_tox_HC_MF5S = mean(Average_time_HC_Tox_MF5,na.rm = TRUE)
# sd_tox_HC_MF5S = sd(Average_time_HC_Tox_MF5,na.rm = TRUE)

############################################################################################################################################
##########---------------------- MF 7 
setwd("....")
file_HC_MF7 = "TX_HC-MF7.csv"
HC_Tox_MF7=read.csv(file_HC_MF7, header = T, as.is = T)
Tstart_exp_HC = 25
Tend_exp_HC = Tstart_exp_HC + 5
Tsim_HC = Tend_exp_HC + 3
DIM_HC = 365-79+(Tsim_HC - 2)*365+1
Start_tox_HC = 365-79+(Tstart_exp_HC -2)*365 +1
End_tox_HC = 365-79+(Tend_exp_HC - 2)*365
End_tox_HC_bis = End_tox_HC +1
n_var =4

R=nrow(HC_Tox_MF7)
C=ncol(HC_Tox_MF7)
n_replicates = (C - 1)/n_var

HC_Tox_MF7[is.na(HC_Tox_MF7)] = 0

i=2
M1_Tox_MF7 = HC_Tox_MF7[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  HC_Tox_MF7[,c]
  M1_Tox_MF7=cbind(M1_Tox_MF7,x)
}

Average_time_HC_Tox_MF7 = colMeans(M1_Tox_MF7[Start_tox_HC:End_tox_HC,],na.rm = TRUE)

# Average_tox_HC_MF7S = mean(Average_time_HC_Tox_MF7,na.rm = TRUE)
# sd_tox_HC_MF7S = sd(Average_time_HC_Tox_MF7,na.rm = TRUE)


############################################################################################################################################
##########---------------------- MF 8 
setwd("....")
file_HC_MF8 = "TX_HC-MF8.csv"
HC_Tox_MF8=read.csv(file_HC_MF8, header = T, as.is = T)
Tstart_exp_HC = 25
Tend_exp_HC = Tstart_exp_HC + 5
Tsim_HC = Tend_exp_HC + 3
DIM_HC = 365-79+(Tsim_HC - 2)*365+1
Start_tox_HC = 365-79+(Tstart_exp_HC -2)*365 +1
End_tox_HC = 365-79+(Tend_exp_HC - 2)*365
End_tox_HC_bis = End_tox_HC +1
n_var =4

R=nrow(HC_Tox_MF8)
C=ncol(HC_Tox_MF8)
n_replicates = (C - 1)/n_var
Tot_Tox_MF8 <- matrix(, nrow = R, ncol = 1)
#
HC_Tox_MF8[is.na(HC_Tox_MF8)] = 0

i=2
M1_Tox_MF8 = HC_Tox_MF8[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  HC_Tox_MF8[,c]
  M1_Tox_MF8=cbind(M1_Tox_MF8,x)
}

Average_time_HC_Tox_MF8 = colMeans(M1_Tox_MF8[Start_tox_HC:End_tox_HC,],na.rm = TRUE)

# Average_tox_HC_MF8S = mean(Average_time_HC_Tox_MF8,na.rm = TRUE)
# sd_tox_HC_MF8S = sd(Average_time_HC_Tox_MF8,na.rm = TRUE)

############################################################################################################################################
##########---------------------- MF 10
setwd("....")
file_HC_MF10 = "TX_HC-MF10.csv"
HC_Tox_MF10=read.csv(file_HC_MF10, header = T, as.is = T)
Tstart_exp_HC = 25
Tend_exp_HC = Tstart_exp_HC + 5
Tsim_HC = Tend_exp_HC + 3
DIM_HC = 365-79+(Tsim_HC - 2)*365+1
Start_tox_HC = 365-79+(Tstart_exp_HC -2)*365 +1
End_tox_HC = 365-79+(Tend_exp_HC - 2)*365
End_tox_HC_bis = End_tox_HC +1
n_var =4

R=nrow(HC_Tox_MF10)
C=ncol(HC_Tox_MF10)
n_replicates = (C - 1)/n_var
HC_Tox_MF10[is.na(HC_Tox_MF10)] = 0

i=2
M1_Tox_MF10 = HC_Tox_MF10[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  HC_Tox_MF10[,c]
  M1_Tox_MF10=cbind(M1_Tox_MF10,x)
}

Average_time_HC_Tox_MF10 = colMeans(M1_Tox_MF10[Start_tox_HC:End_tox_HC,],na.rm = TRUE)

# Average_tox_HC_MF10S = mean(Average_time_HC_Tox_MF10,na.rm = TRUE)
# sd_tox_HC_MF10S = sd(Average_time_HC_Tox_MF10,na.rm = TRUE)


############################################################################################################################################
##########---------------------- MF 11
setwd("....")
file_HC_MF11 = "TX_HC-MF11.csv"
HC_Tox_MF11=read.csv(file_HC_MF11, header = T, as.is = T)
Tstart_exp_HC = 25
Tend_exp_HC = Tstart_exp_HC + 5
Tsim_HC = Tend_exp_HC + 3
DIM_HC = 365-79+(Tsim_HC - 2)*365+1
Start_tox_HC = 365-79+(Tstart_exp_HC -2)*365 +1
End_tox_HC = 365-79+(Tend_exp_HC - 2)*365
End_tox_HC_bis = End_tox_HC +1
n_var =4

R=nrow(HC_Tox_MF11)
C=ncol(HC_Tox_MF11)
n_replicates = (C - 1)/n_var

absent_rec = DIM_HC-R
alpha = matrix (0,nrow = absent_rec, ncol = C)

nameCol = colnames(HC_Tox_MF11)
colnames(alpha) = nameCol
HC_Tox_MF11 = rbind(HC_Tox_MF11,alpha)
HC_Tox_MF11[is.na(HC_Tox_MF11)] = 0

i=2

M1_Tox_MF11 = HC_Tox_MF11[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  HC_Tox_MF11[,c]
  M1_Tox_MF11=cbind(M1_Tox_MF11,x)
}

Average_time_HC_Tox_MF11 = colMeans(M1_Tox_MF11[Start_tox_HC:End_tox_HC,],na.rm = TRUE)

# Average_tox_HC_MF11S = mean(Average_time_HC_Tox_MF11,na.rm = TRUE)
# sd_tox_HC_MF11S = sd(Average_time_HC_Tox_MF11,na.rm = TRUE)


## ratio of replicates for each EMF and species - calculate minimum and maximum of the ratios

ratio_replicates_DRM_MF1 = mean_time_tot_pop_MF1/mean_time_tot_pop
Average_ratio_replicates_DRM_MF1 = mean(ratio_replicates_DRM_MF1,na.rm = TRUE) # here the average is over replicates
Max_ratio_replicates_DRM_MF1 = max(ratio_replicates_DRM_MF1, na.rm = TRUE)
Min_ratio_replicates_DRM_MF1 = min(ratio_replicates_DRM_MF1, na.rm = TRUE)
std_ratio_replicates_DRM_MF1 = sd(ratio_replicates_DRM_MF1,na.rm = TRUE)
 

ratio_replicates_DRM_MF2 = mean_time_tot_pop_MF2/mean_time_tot_pop
Average_ratio_replicates_DRM_MF2 = mean(ratio_replicates_DRM_MF2,na.rm = TRUE)
Max_ratio_replicates_DRM_MF2 = max(ratio_replicates_DRM_MF2, na.rm = TRUE)
Min_ratio_replicates_DRM_MF2 = min(ratio_replicates_DRM_MF2, na.rm = TRUE)
std_ratio_replicates_DRM_MF2 = sd(ratio_replicates_DRM_MF2,na.rm = TRUE)


ratio_replicates_DRM_MF4 = mean_time_tot_pop_MF4/mean_time_tot_pop
Average_ratio_replicates_DRM_MF4 = mean(ratio_replicates_DRM_MF4,na.rm = TRUE)
Max_ratio_replicates_DRM_MF4 = max(ratio_replicates_DRM_MF4, na.rm = TRUE)
Min_ratio_replicates_DRM_MF4 = min(ratio_replicates_DRM_MF4, na.rm = TRUE)
std_ratio_replicates_DRM_MF4 = sd(ratio_replicates_DRM_MF4,na.rm = TRUE)


ratio_replicates_DRM_MF5 = mean_time_tot_pop_MF5/mean_time_tot_pop
Average_ratio_replicates_DRM_MF5 = mean(ratio_replicates_DRM_MF5,na.rm = TRUE)
Max_ratio_replicates_DRM_MF5 = max(ratio_replicates_DRM_MF5, na.rm = TRUE)
Min_ratio_replicates_DRM_MF5 = min(ratio_replicates_DRM_MF5, na.rm = TRUE)
std_ratio_replicates_DRM_MF5 = sd(ratio_replicates_DRM_MF5, na.rm = TRUE)


ratio_replicates_DRM_MF7 = mean_time_tot_pop_MF7/mean_time_tot_pop
Average_ratio_replicates_DRM_MF7 = mean(ratio_replicates_DRM_MF7,na.rm = TRUE)
Max_ratio_replicates_DRM_MF7 = max(ratio_replicates_DRM_MF7, na.rm = TRUE)
Min_ratio_replicates_DRM_MF7 = min(ratio_replicates_DRM_MF7, na.rm = TRUE)
std_ratio_replicates_DRM_MF7 = sd(ratio_replicates_DRM_MF7, na.rm = TRUE)


ratio_replicates_DRM_MF8 = mean_time_tot_pop_MF8/mean_time_tot_pop
Average_ratio_replicates_DRM_MF8 = mean(ratio_replicates_DRM_MF8,na.rm = TRUE)
Max_ratio_replicates_DRM_MF8 = max(ratio_replicates_DRM_MF8, na.rm = TRUE)
Min_ratio_replicates_DRM_MF8 = min(ratio_replicates_DRM_MF8, na.rm = TRUE)
std_ratio_replicates_DRM_MF8 = sd(ratio_replicates_DRM_MF8, na.rm = TRUE)


ratio_replicates_DRM_MF10 = mean_time_tot_pop_MF10/mean_time_tot_pop
Average_ratio_replicates_DRM_MF10 = mean(ratio_replicates_DRM_MF10,na.rm = TRUE)
Max_ratio_replicates_DRM_MF10 = max(ratio_replicates_DRM_MF10, na.rm = TRUE)
Min_ratio_replicates_DRM_MF10 = min(ratio_replicates_DRM_MF10, na.rm = TRUE)
std_ratio_replicates_DRM_MF10 = sd(ratio_replicates_DRM_MF10, na.rm = TRUE)


ratio_replicates_DRM_MF11 = mean_time_tot_pop_MF11/mean_time_tot_pop
Average_ratio_replicates_DRM_MF11 = mean(ratio_replicates_DRM_MF11,na.rm = TRUE)
Max_ratio_replicates_DRM_MF11 = max(ratio_replicates_DRM_MF11, na.rm = TRUE)
Min_ratio_replicates_DRM_MF11 = min(ratio_replicates_DRM_MF11, na.rm = TRUE)
std_ratio_replicates_DRM_MF11 = sd(ratio_replicates_DRM_MF11, na.rm = TRUE)


ratio_replicates_TS_MF1 = Average_time_TS_Tox_MF1/Average_time_TS
Average_ratio_replicates_TS_MF1 = mean(ratio_replicates_TS_MF1,na.rm = TRUE)
Max_ratio_replicates_TS_MF1 = max(ratio_replicates_TS_MF1, na.rm = TRUE)
Min_ratio_replicates_TS_MF1 = min(ratio_replicates_TS_MF1, na.rm = TRUE)
std_ratio_replicates_TS_MF1 = sd(ratio_replicates_TS_MF1,na.rm = TRUE)


ratio_replicates_TS_MF2 = Average_time_TS_Tox_MF2/Average_time_TS
Average_ratio_replicates_TS_MF2 = mean(ratio_replicates_TS_MF2,na.rm = TRUE)
Max_ratio_replicates_TS_MF2 = max(ratio_replicates_TS_MF2, na.rm = TRUE)
Min_ratio_replicates_TS_MF2 = min(ratio_replicates_TS_MF2, na.rm = TRUE)
std_ratio_replicates_TS_MF2 = sd(ratio_replicates_TS_MF2,na.rm = TRUE)


ratio_replicates_TS_MF4 = Average_time_TS_Tox_MF4/Average_time_TS
Average_ratio_replicates_TS_MF4 = mean(ratio_replicates_TS_MF4,na.rm = TRUE)
Max_ratio_replicates_TS_MF4 = max(ratio_replicates_TS_MF4, na.rm = TRUE)
Min_ratio_replicates_TS_MF4 = min(ratio_replicates_TS_MF4, na.rm = TRUE)
std_ratio_replicates_TS_MF4 = sd(ratio_replicates_TS_MF4,na.rm = TRUE)


ratio_replicates_TS_MF5 = Average_time_TS_Tox_MF5/Average_time_TS
Average_ratio_replicates_TS_MF5 = mean(ratio_replicates_TS_MF5,na.rm = TRUE)
Max_ratio_replicates_TS_MF5 = max(ratio_replicates_TS_MF5, na.rm = TRUE)
Min_ratio_replicates_TS_MF5 = min(ratio_replicates_TS_MF5, na.rm = TRUE)
std_ratio_replicates_TS_MF5 = sd(ratio_replicates_TS_MF5, na.rm = TRUE)


ratio_replicates_TS_MF7 = Average_time_TS_Tox_MF7/Average_time_TS
Average_ratio_replicates_TS_MF7 = mean(ratio_replicates_TS_MF7,na.rm = TRUE)
Max_ratio_replicates_TS_MF7 = max(ratio_replicates_TS_MF7, na.rm = TRUE)
Min_ratio_replicates_TS_MF7 = min(ratio_replicates_TS_MF7, na.rm = TRUE)
std_ratio_replicates_TS_MF7 = sd(ratio_replicates_TS_MF7, na.rm = TRUE)


ratio_replicates_TS_MF8 = Average_time_TS_Tox_MF8/Average_time_TS
Average_ratio_replicates_TS_MF8 = mean(ratio_replicates_TS_MF8,na.rm = TRUE)
Max_ratio_replicates_TS_MF8 = max(ratio_replicates_TS_MF8, na.rm = TRUE)
Min_ratio_replicates_TS_MF8 = min(ratio_replicates_TS_MF8, na.rm = TRUE)
std_ratio_replicates_TS_MF8 = sd(ratio_replicates_TS_MF8, na.rm = TRUE)


ratio_replicates_TS_MF10 = Average_time_TS_Tox_MF10/Average_time_TS
Average_ratio_replicates_TS_MF10 = mean(ratio_replicates_TS_MF10,na.rm = TRUE)
Max_ratio_replicates_TS_MF10 = max(ratio_replicates_TS_MF10, na.rm = TRUE)
Min_ratio_replicates_TS_MF10 = min(ratio_replicates_TS_MF10, na.rm = TRUE)
std_ratio_replicates_TS_MF10 = sd(ratio_replicates_TS_MF10, na.rm = TRUE)


ratio_replicates_TS_MF11 = Average_time_TS_Tox_MF11/Average_time_TS
Average_ratio_replicates_TS_MF11 = mean(ratio_replicates_TS_MF11,na.rm = TRUE)
Max_ratio_replicates_TS_MF11 = max(ratio_replicates_TS_MF11, na.rm = TRUE)
Min_ratio_replicates_TS_MF11 = min(ratio_replicates_TS_MF11, na.rm = TRUE)
std_ratio_replicates_TS_MF11 = sd(ratio_replicates_TS_MF11, na.rm = TRUE)


ratio_replicates_HC_MF1 = Average_time_HC_Tox_MF1/Average_time_HC
Average_ratio_replicates_HC_MF1 = mean(ratio_replicates_HC_MF1,na.rm = TRUE)
Max_ratio_replicates_HC_MF1 = max(ratio_replicates_HC_MF1, na.rm = TRUE)
Min_ratio_replicates_HC_MF1 = min(ratio_replicates_HC_MF1, na.rm = TRUE)
std_ratio_replicates_HC_MF1 = sd(ratio_replicates_HC_MF1,na.rm = TRUE)


ratio_replicates_HC_MF2 = Average_time_HC_Tox_MF2/Average_time_HC
Average_ratio_replicates_HC_MF2 = mean(ratio_replicates_HC_MF2,na.rm = TRUE)
Max_ratio_replicates_HC_MF2 = max(ratio_replicates_HC_MF2, na.rm = TRUE)
Min_ratio_replicates_HC_MF2 = min(ratio_replicates_HC_MF2, na.rm = TRUE)
std_ratio_replicates_HC_MF2 = sd(ratio_replicates_HC_MF2,na.rm = TRUE)


ratio_replicates_HC_MF4 = Average_time_HC_Tox_MF4/Average_time_HC
Average_ratio_replicates_HC_MF4 = mean(ratio_replicates_HC_MF4,na.rm = TRUE)
Max_ratio_replicates_HC_MF4 = max(ratio_replicates_HC_MF4, na.rm = TRUE)
Min_ratio_replicates_HC_MF4 = min(ratio_replicates_HC_MF4, na.rm = TRUE)
std_ratio_replicates_HC_MF4 = sd(ratio_replicates_HC_MF4,na.rm = TRUE)


ratio_replicates_HC_MF5 = Average_time_HC_Tox_MF5/Average_time_HC
Average_ratio_replicates_HC_MF5 = mean(ratio_replicates_HC_MF5,na.rm = TRUE)
Max_ratio_replicates_HC_MF5 = max(ratio_replicates_HC_MF5, na.rm = TRUE)
Min_ratio_replicates_HC_MF5 = min(ratio_replicates_HC_MF5, na.rm = TRUE)
std_ratio_replicates_HC_MF5 = sd(ratio_replicates_HC_MF5, na.rm = TRUE)


ratio_replicates_HC_MF7 = Average_time_HC_Tox_MF7/Average_time_HC
Average_ratio_replicates_HC_MF7 = mean(ratio_replicates_HC_MF7,na.rm = TRUE)
Max_ratio_replicates_HC_MF7 = max(ratio_replicates_HC_MF7, na.rm = TRUE)
Min_ratio_replicates_HC_MF7 = min(ratio_replicates_HC_MF7, na.rm = TRUE)
std_ratio_replicates_HC_MF7 = sd(ratio_replicates_HC_MF7, na.rm = TRUE)


ratio_replicates_HC_MF8 = Average_time_HC_Tox_MF8/Average_time_HC
Average_ratio_replicates_HC_MF8 = mean(ratio_replicates_HC_MF8,na.rm = TRUE)
Max_ratio_replicates_HC_MF8 = max(ratio_replicates_HC_MF8, na.rm = TRUE)
Min_ratio_replicates_HC_MF8 = min(ratio_replicates_HC_MF8, na.rm = TRUE)
std_ratio_replicates_HC_MF8 = sd(ratio_replicates_HC_MF8, na.rm = TRUE)


ratio_replicates_HC_MF10 = Average_time_HC_Tox_MF10/Average_time_HC
Average_ratio_replicates_HC_MF10 = mean(ratio_replicates_HC_MF10,na.rm = TRUE)
Max_ratio_replicates_HC_MF10 = max(ratio_replicates_HC_MF10, na.rm = TRUE)
Min_ratio_replicates_HC_MF10 = min(ratio_replicates_HC_MF10, na.rm = TRUE)
std_ratio_replicates_HC_MF10 = sd(ratio_replicates_HC_MF10, na.rm = TRUE)


ratio_replicates_HC_MF11 = Average_time_HC_Tox_MF11/Average_time_HC
Average_ratio_replicates_HC_MF11 = mean(ratio_replicates_HC_MF11,na.rm = TRUE)
Max_ratio_replicates_HC_MF11 = max(ratio_replicates_HC_MF11, na.rm = TRUE)
Min_ratio_replicates_HC_MF11 = min(ratio_replicates_HC_MF11, na.rm = TRUE)
std_ratio_replicates_HC_MF11 = sd(ratio_replicates_HC_MF11, na.rm = TRUE)


ratio_replicates_SD_MF1 = Average_time_SD_Tox_MF1/Average_time_SD
Average_ratio_replicates_SD_MF1 = mean(ratio_replicates_SD_MF1,na.rm = TRUE)
Max_ratio_replicates_SD_MF1 = max(ratio_replicates_SD_MF1, na.rm = TRUE)
Min_ratio_replicates_SD_MF1 = min(ratio_replicates_SD_MF1, na.rm = TRUE)
std_ratio_replicates_SD_MF1 = sd(ratio_replicates_SD_MF1,na.rm = TRUE)


ratio_replicates_SD_MF2 = Average_time_SD_Tox_MF2/Average_time_SD
Average_ratio_replicates_SD_MF2 = mean(ratio_replicates_SD_MF2,na.rm = TRUE)
Max_ratio_replicates_SD_MF2 = max(ratio_replicates_SD_MF2, na.rm = TRUE)
Min_ratio_replicates_SD_MF2 = min(ratio_replicates_SD_MF2, na.rm = TRUE)
std_ratio_replicates_SD_MF2 = sd(ratio_replicates_SD_MF2,na.rm = TRUE)


ratio_replicates_SD_MF4 = Average_time_SD_Tox_MF4/Average_time_SD
Average_ratio_replicates_SD_MF4 = mean(ratio_replicates_SD_MF4,na.rm = TRUE)
Max_ratio_replicates_SD_MF4 = max(ratio_replicates_SD_MF4, na.rm = TRUE)
Min_ratio_replicates_SD_MF4 = min(ratio_replicates_SD_MF4, na.rm = TRUE)
std_ratio_replicates_SD_MF4 = sd(ratio_replicates_SD_MF4,na.rm = TRUE)


ratio_replicates_SD_MF5 = Average_time_SD_Tox_MF5/Average_time_SD
Average_ratio_replicates_SD_MF5 = mean(ratio_replicates_SD_MF5,na.rm = TRUE)
Max_ratio_replicates_SD_MF5 = max(ratio_replicates_SD_MF5, na.rm = TRUE)
Min_ratio_replicates_SD_MF5 = min(ratio_replicates_SD_MF5, na.rm = TRUE)
std_ratio_replicates_SD_MF5 = sd(ratio_replicates_SD_MF5, na.rm = TRUE)


ratio_replicates_SD_MF7 = Average_time_SD_Tox_MF7/Average_time_SD
Average_ratio_replicates_SD_MF7 = mean(ratio_replicates_SD_MF7,na.rm = TRUE)
Max_ratio_replicates_SD_MF7 = max(ratio_replicates_SD_MF7, na.rm = TRUE)
Min_ratio_replicates_SD_MF7 = min(ratio_replicates_SD_MF7, na.rm = TRUE)
std_ratio_replicates_SD_MF7 = sd(ratio_replicates_SD_MF7, na.rm = TRUE)


ratio_replicates_SD_MF8 = Average_time_SD_Tox_MF8/Average_time_SD
Average_ratio_replicates_SD_MF8 = mean(ratio_replicates_SD_MF8,na.rm = TRUE)
Max_ratio_replicates_SD_MF8 = max(ratio_replicates_SD_MF8, na.rm = TRUE)
Min_ratio_replicates_SD_MF8 = min(ratio_replicates_SD_MF8, na.rm = TRUE)
std_ratio_replicates_SD_MF8 = sd(ratio_replicates_SD_MF8, na.rm = TRUE)


ratio_replicates_SD_MF10 = Average_time_SD_Tox_MF10/Average_time_SD
Average_ratio_replicates_SD_MF10 = mean(ratio_replicates_SD_MF10,na.rm = TRUE)
Max_ratio_replicates_SD_MF10 = max(ratio_replicates_SD_MF10, na.rm = TRUE)
Min_ratio_replicates_SD_MF10 = min(ratio_replicates_SD_MF10, na.rm = TRUE)
std_ratio_replicates_SD_MF10 = sd(ratio_replicates_SD_MF10, na.rm = TRUE)


ratio_replicates_SD_MF11 = Average_time_SD_Tox_MF11/Average_time_SD
Average_ratio_replicates_SD_MF11 = mean(ratio_replicates_SD_MF11,na.rm = TRUE)
Max_ratio_replicates_SD_MF11 = max(ratio_replicates_SD_MF11, na.rm = TRUE)
Min_ratio_replicates_SD_MF11 = min(ratio_replicates_SD_MF11, na.rm = TRUE)
std_ratio_replicates_SD_MF11 = sd(ratio_replicates_SD_MF11, na.rm = TRUE)




ratio_TS = c(Average_ratio_replicates_TS_MF1,Average_ratio_replicates_TS_MF2, Average_ratio_replicates_TS_MF4,
             Average_ratio_replicates_TS_MF5 ,Average_ratio_replicates_TS_MF7 ,
             Average_ratio_replicates_TS_MF8, Average_ratio_replicates_TS_MF10, Average_ratio_replicates_TS_MF11)

max_TS = c (Max_ratio_replicates_TS_MF1, Max_ratio_replicates_TS_MF2, Max_ratio_replicates_TS_MF4, Max_ratio_replicates_TS_MF5,
            Max_ratio_replicates_TS_MF7, Max_ratio_replicates_TS_MF8, Max_ratio_replicates_TS_MF10, Max_ratio_replicates_TS_MF11)
min_TS = c(Min_ratio_replicates_TS_MF1, Min_ratio_replicates_TS_MF2, Min_ratio_replicates_TS_MF4, Min_ratio_replicates_TS_MF5,
           Min_ratio_replicates_TS_MF7, Min_ratio_replicates_TS_MF8, Min_ratio_replicates_TS_MF10, Min_ratio_replicates_TS_MF11)

ratio_DRM = c(Average_ratio_replicates_DRM_MF1,Average_ratio_replicates_DRM_MF2, Average_ratio_replicates_DRM_MF4,
             Average_ratio_replicates_DRM_MF5 ,Average_ratio_replicates_DRM_MF7 ,
             Average_ratio_replicates_DRM_MF8, Average_ratio_replicates_DRM_MF10, Average_ratio_replicates_DRM_MF11)

max_DRM = c (Max_ratio_replicates_DRM_MF1, Max_ratio_replicates_DRM_MF2, Max_ratio_replicates_DRM_MF4, Max_ratio_replicates_DRM_MF5,
            Max_ratio_replicates_DRM_MF7, Max_ratio_replicates_DRM_MF8, Max_ratio_replicates_DRM_MF10, Max_ratio_replicates_DRM_MF11)
min_DRM = c(Min_ratio_replicates_DRM_MF1, Min_ratio_replicates_DRM_MF2, Min_ratio_replicates_DRM_MF4, Min_ratio_replicates_DRM_MF5,
           Min_ratio_replicates_DRM_MF7, Min_ratio_replicates_DRM_MF8, Min_ratio_replicates_DRM_MF10, Min_ratio_replicates_DRM_MF11)


ratio_SD = c(Average_ratio_replicates_SD_MF1,Average_ratio_replicates_SD_MF2, Average_ratio_replicates_SD_MF4,
             Average_ratio_replicates_SD_MF5 ,Average_ratio_replicates_SD_MF7 ,
             Average_ratio_replicates_SD_MF8, Average_ratio_replicates_SD_MF10, Average_ratio_replicates_SD_MF11)


max_SD = c (Max_ratio_replicates_SD_MF1, Max_ratio_replicates_SD_MF2, Max_ratio_replicates_SD_MF4, Max_ratio_replicates_SD_MF5,
            Max_ratio_replicates_SD_MF7, Max_ratio_replicates_SD_MF8, Max_ratio_replicates_SD_MF10, Max_ratio_replicates_SD_MF11)
min_SD = c(Min_ratio_replicates_SD_MF1, Min_ratio_replicates_SD_MF2, Min_ratio_replicates_SD_MF4, Min_ratio_replicates_SD_MF5,
           Min_ratio_replicates_SD_MF7, Min_ratio_replicates_SD_MF8, Min_ratio_replicates_SD_MF10, Min_ratio_replicates_SD_MF11)


ratio_HC = c(Average_ratio_replicates_HC_MF1,Average_ratio_replicates_HC_MF2, Average_ratio_replicates_HC_MF4,
             Average_ratio_replicates_HC_MF5 ,Average_ratio_replicates_HC_MF7 ,
             Average_ratio_replicates_HC_MF8, Average_ratio_replicates_HC_MF10, Average_ratio_replicates_HC_MF11)

max_HC = c (Max_ratio_replicates_HC_MF1, Max_ratio_replicates_HC_MF2, Max_ratio_replicates_HC_MF4, Max_ratio_replicates_HC_MF5,
            Max_ratio_replicates_HC_MF7, Max_ratio_replicates_HC_MF8, Max_ratio_replicates_HC_MF10, Max_ratio_replicates_HC_MF11)
min_HC = c(Min_ratio_replicates_HC_MF1, Min_ratio_replicates_HC_MF2, Min_ratio_replicates_HC_MF4, Min_ratio_replicates_HC_MF5,
           Min_ratio_replicates_HC_MF7, Min_ratio_replicates_HC_MF8, Min_ratio_replicates_HC_MF10, Min_ratio_replicates_HC_MF11)



##################################################################################################################################
######################### PLOTS 

ratio=rbind(ratio_TS, ratio_DRM, ratio_SD, ratio_HC)
max_ratio = rbind(max_TS, max_DRM, max_SD, max_HC)
min_ratio = rbind(min_TS, min_DRM, min_SD, min_HC)

x_axis= c(1,1,1,1,2,2,2,2,4,4,4,4,5,5,5,5,7,7,7,7,8,8,8,8,10,10,10,10,11,11,11,11)
x_axis_line= c(1,2,5,7,8,10)

x_axis2= c(1,2,4,5,7,8,10,11)


x11()
pdf("F:/LAVORO/WB/Projects/Fish_project_Syngenta/PopGuide_decision_steps/Draft/FIGURES PAPER/AUC_EMF-meanreplicates-MAXMIN.pdf",width=25,height=18,onefile=T,paper='A4r')
par(mar=c(4,5.2,3.5,2),oma=c(2,2,0,0)) 
plot(x_axis2, ratio_TS,col= 5, pch =19, cex = 2.5,ylim=c(0,1.6),xaxt="n",
     ylab="",xlab = "", main="")
arrows(x_axis2,min_TS,x_axis2,max_TS, code=3, length=0.02,col = 5, angle = 90)
axis(1, at=seq(1,11,by=1) )
lines(c(1,2,4,5,7,8,10,11), na.omit(ratio[1,]),col=5)

par(new=T)
plot(x_axis2, ratio_DRM,col= 6, pch =19, cex = 2.5,ylim=c(0,1.6),xaxt="n",
     ylab="",xlab = "", main="")
arrows(x_axis2,min_DRM,x_axis2,max_DRM, code=3, length=0.02,col = 6, angle = 90)
axis(1, at=seq(1,11,by=1) )
lines(c(1,2,4,5,7,8,10,11), na.omit(ratio[2,]),col=6)

par(new=T)
plot(x_axis2, ratio_SD,col= 7, pch =19, cex = 2.5,ylim=c(0,1.6),xaxt="n",
     ylab="",xlab = "", main="")
arrows(x_axis2,min_SD,x_axis2,max_SD, code=3, length=0.02,col = 7, angle = 90)
axis(1, at=seq(1,11,by=1) )
lines(c(1,2,4,5,7,8,10,11), na.omit(ratio[3,]),col=7)

par(new=T)
plot(x_axis2, ratio_HC,col= 8, pch =19, cex = 2.5,ylim=c(0,1.6),xaxt="n",
     ylab="",xlab = "", main="")
arrows(x_axis2,min_HC,x_axis2,max_HC, code=3, length=0.02,col =8, angle = 90)
axis(1, at=seq(1,11,by=1) )
lines(c(1,2,4,5,7,8,10,11), na.omit(ratio[4,]),col=8)
legend(9,1.5, legend=c("TS","DRM","SD","HC"),
       col=c(5,6,7,8),  lty=1, cex=0.8, lwd = 2)
lines(1:11,c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5), lty = 2)

title(main="Ratio of population abundances during exposure",cex.main = 2.8,  line=1, font.main = 1)
mtext("Magnification factors",side=1,line=3,cex=2.4)
mtext("Ratio with/without chemical",side=2,line=3,cex=2.4,las=0)


dev.off()





####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################

###################### ********************************** ##########################################################################
####################### **********************************  ********************************** #####################################
##################                       ADD UP EFFECT SUB-MODELS

library(dplyr)
library(ggplot2)
library(ggpubr)

setwd("....")
file_DRM = "Cyprinidae_TX_DRM DRM-table.csv"
DRM_table=read.csv(file_DRM, header=F,stringsAsFactors=FALSE)
NAMES <- read.csv(file_DRM, nrow = 6, sep = ";")
DRM_table <- read.csv(file_DRM, skip = 6, stringsAsFactors = FALSE, sep = ",")
words <- strsplit(NAMES[6,1], ",")[[1]]
xx=unique(tolower(words))
names(DRM_table) <- xx

Tstart_exp_DRM = 20
Tend_exp_DRM = Tstart_exp_DRM + 5
Tsim_DRM = Tend_exp_DRM + 3
DIM_DRM = 364*24+365*(Tsim_DRM - 2)*24+3
Start_tox_DRM = 364*24+365*(Tstart_exp_DRM -2)*24 +3
End_tox_DRM = 364*24+365*(Tend_exp_DRM - 2)*24 +2
End_tox_DRM_bis = End_tox_DRM + 1 
n_var = 4
R=nrow(DRM_table)

DRM_matrix <- matrix(, nrow = R, ncol = 1)
n_replicates = 40
for (i in 1:n_replicates)
{
  dataframe = DRM_table[DRM_table[, "[run number]"] == i,]
  assign(paste("Matrix",i, sep=""), select(dataframe, 
                                           c("count turtles with [u_h > u_h^b]","count turtles with [u_h > u_h^p]","sum [weight] of turtles with [u_h > u_h^b]","w-turtles")))
}

for (i in 1:n_replicates)
{
  x= nrow(get(paste("Matrix",i, sep="")))
  if(x != DIM_DRM)
  {print (i)
    print (x)}
}

DRM_m = Matrix1

for (i in 2:n_replicates)
{
  
  DRM_m = cbind (DRM_m,get(paste("Matrix",i, sep="")))
} 
colTot=ncol(DRM_m)
col.names = rep(c("Tot pop","Adult pop","Tot weight","Adult weight"),n_replicates)
colnames(DRM_m) <-col.names

# TOTAL ABUNDANCE
Average_no_tox_DRM_NEW = colMeans(DRM_m[Start_tox_DRM:End_tox_DRM,],na.rm = TRUE)
mean_time_tot_pop <- matrix(, nrow = n_replicates, ncol = 1)

ii = 1
i = 1
while (ii <= length(Average_no_tox_DRM_NEW))
{
  mean_time_tot_pop[i] = Average_no_tox_DRM_NEW[ii]
  i = i + 1
  ii = ii + 4
}

#  TOTAL WEIGHT here interested to the third variable too
mean_time_tot_popW <- matrix(, nrow = n_replicates, ncol = 1)
ii = 3
i = 1
while (ii <= length(Average_no_tox_DRM_NEW))
{
  mean_time_tot_popW[i] = Average_no_tox_DRM_NEW[ii]
  i = i + 1
  ii = ii + 4
}

# Average_no_tox_DRM = mean(mean_time_tot_pop,na.rm = TRUE)
# sd_no_tox_DRM = sd(mean_time_tot_pop,na.rm = TRUE)
Average_no_tox_DRM_W = mean(mean_time_tot_popW,na.rm = TRUE)
sd_no_tox_DRM_W = sd(mean_time_tot_popW,na.rm = TRUE)

DRM_mr <- as.data.frame( # sapply returns a list here, so we convert it to a data.frame
  sapply(unique(names(DRM_m)), # for each unique column name
         function(col) rowMeans(DRM_m[names(DRM_m) == col]) # calculate row means
  )
)


########################################################################################
######### E1
setwd("....")
file_DRM_E1 = "Cyprinidae_TX_EMF DRM-E1-table.csv"
Tstart_exp_DRM = 20
Tend_exp_DRM = Tstart_exp_DRM + 5
Tsim_DRM = Tend_exp_DRM + 3
DIM_DRM = 364*24+365*(Tsim_DRM - 2)*24+3
Start_tox_DRM = 364*24+365*(Tstart_exp_DRM -2)*24 +3
End_tox_DRM = 364*24+365*(Tend_exp_DRM - 2)*24 +2
End_tox_DRM_bis = End_tox_DRM + 1 
n_var = 4

DRM_table_E1=read.csv(file_DRM_E1, header=F,stringsAsFactors=FALSE)
NAMES <- read.csv(file_DRM_E1, nrow = 6, sep = ";")
DRM_table_E1 <- read.csv(file_DRM_E1, skip = 6, stringsAsFactors = FALSE, sep = ",")
words <- strsplit(NAMES[6,1], ",")[[1]]
xx=unique(tolower(words))
names(DRM_table_E1) <- xx
R=nrow(DRM_table_E1)

DRM_matrix_E1 <- matrix(, nrow = R, ncol = 1)

n_replicates = 40 
for (i in 1:n_replicates)
{
  dataframe = DRM_table_E1[DRM_table_E1[, "[run number]"] == i,]
  assign(paste("Matrix_E1",i, sep=""), select(dataframe, 
                                              c("count turtles with [u_h > u_h^b]","count turtles with [u_h > u_h^p]","sum [weight] of turtles with [u_h > u_h^b]","w-turtles")))
}

for (i in 1:n_replicates)
{
  x= nrow(get(paste("Matrix_E1",i, sep="")))
  if(x != DIM_DRM)
  {print (i)
    print (x)}
}


DRM_m_E1 = Matrix_E11
for (i in 2:n_replicates)
{
  DRM_m_E1 = cbind (DRM_m_E1,get(paste("Matrix_E1",i, sep="")))
} 
colTot=ncol(DRM_m_E1)

col.names = rep(c("Tot pop","Adult pop","Tot weight","Adult weight"),n_replicates)
colnames(DRM_m_E1) <-col.names
Average_E1_DRM_NEW = colMeans(DRM_m_E1[Start_tox_DRM:End_tox_DRM,],na.rm = TRUE)
mean_time_tot_pop_E1 <- matrix(, nrow = n_replicates, ncol = 1)

ii = 1
i = 1
while (ii <= length(Average_E1_DRM_NEW))
{
  mean_time_tot_pop_E1[i] = Average_E1_DRM_NEW[ii]
  i = i + 1
  ii = ii + 4
}

# Average_no_tox_DRM_E1 = mean(mean_time_tot_pop_E1,na.rm = TRUE)
# sd_no_tox_DRM_E1 = sd(mean_time_tot_pop_E1,na.rm = TRUE)

mean_time_tot_pop_E1W <- matrix(, nrow = n_replicates, ncol = 1)
ii = 3
i = 1
while (ii <= length(Average_E1_DRM_NEW))
{
  mean_time_tot_pop_E1W[i] = Average_E1_DRM_NEW[ii]
  i = i + 1
  ii = ii + 4
}

Average_no_tox_DRM_E1_W = mean(mean_time_tot_pop_E1W,na.rm = TRUE)
sd_no_tox_DRM_E1_W = sd(mean_time_tot_pop_E1W,na.rm = TRUE)


DRM_mr_E1 <- as.data.frame( # sapply returns a list here, so we convert it to a data.frame
  sapply(unique(names(DRM_m_E1)), # for each unique column name
         function(col) rowMeans(DRM_m_E1[names(DRM_m_E1) == col]) # calculate row means
  )
)


###################################################################################################
######### ######### ######### E12
setwd("....")
file_DRM_E12 = "Cyprinidae_TX_EMF DRM-E12-table.csv"
Tstart_exp_DRM = 20
Tend_exp_DRM = Tstart_exp_DRM + 5
Tsim_DRM = Tend_exp_DRM + 3
DIM_DRM = 364*24+365*(Tsim_DRM - 2)*24+3
Start_tox_DRM = 364*24+365*(Tstart_exp_DRM -2)*24 +3
End_tox_DRM = 364*24+365*(Tend_exp_DRM - 2)*24 +2
End_tox_DRM_bis = End_tox_DRM + 1 
n_var = 4
DRM_table_E12=read.csv(file_DRM_E12, header=F,stringsAsFactors=FALSE)

NAMES <- read.csv(file_DRM_E12, nrow = 6, sep = ";")
DRM_table_E12 <- read.csv(file_DRM_E12, skip = 6, stringsAsFactors = FALSE, sep = ",")
words <- strsplit(NAMES[6,1], ",")[[1]]
xx=unique(tolower(words))
names(DRM_table_E12) <- xx
R=nrow(DRM_table_E12)

DRM_matrix_E12 <- matrix(, nrow = R, ncol = 1)

n_replicates = 40 
for (i in 1:n_replicates)
{
  dataframe = DRM_table_E12[DRM_table_E12[, "[run number]"] == i,]
  assign(paste("Matrix_E12",i, sep=""), select(dataframe, 
                                               c("count turtles with [u_h > u_h^b]","count turtles with [u_h > u_h^p]","sum [weight] of turtles with [u_h > u_h^b]","w-turtles")))
}

for (i in 1:n_replicates)
{
  x= nrow(get(paste("Matrix_E12",i, sep="")))
  if(x != DIM_DRM)
  {print (i)
    print (x)}
}

DRM_m_E12 = Matrix_E121

for (i in 2:n_replicates)
{
  DRM_m_E12 = cbind (DRM_m_E12,get(paste("Matrix_E12",i, sep="")))
} 
colTot=ncol(DRM_m_E12)

col.names = rep(c("Tot pop","Adult pop","Tot weight","Adult weight"),n_replicates)
colnames(DRM_m_E12) <-col.names

Average_E12_DRM_NEW = colMeans(DRM_m_E12[Start_tox_DRM:End_tox_DRM,],na.rm = TRUE)
mean_time_tot_pop_E12 <- matrix(, nrow = n_replicates, ncol = 1)

ii = 1
i = 1
while (ii <= length(Average_E12_DRM_NEW))
{
  mean_time_tot_pop_E12[i] = Average_E12_DRM_NEW[ii]
  i = i + 1
  ii = ii + 4
}


# Average_no_tox_DRM_E12 = mean(mean_time_tot_pop_E12,na.rm = TRUE)
# sd_no_tox_DRM_E12 = sd(mean_time_tot_pop_E12,na.rm = TRUE)

mean_time_tot_pop_E12W <- matrix(, nrow = n_replicates, ncol = 1)

ii = 3
i = 1
while (ii <= length(Average_E12_DRM_NEW))
{
  mean_time_tot_pop_E12W[i] = Average_E12_DRM_NEW[ii]
  i = i + 1
  ii = ii + 4
}

Average_no_tox_DRM_E12_W = mean(mean_time_tot_pop_E12W,na.rm = TRUE)
sd_no_tox_DRM_E12_W = sd(mean_time_tot_pop_E12W,na.rm = TRUE)
 
DRM_mr_E12 <- as.data.frame( # sapply returns a list here, so we convert it to a data.frame
  sapply(unique(names(DRM_m_E12)), # for each unique column name
         function(col) rowMeans(DRM_m_E12[names(DRM_m_E12) == col]) # calculate row means
  )
)
#########################################################
######### ########################### E123
setwd("....")
file_DRM_E123 = "Cyprinidae_TX_EMF DRM-E123-table.csv"
DRM_table_E123=read.csv(file_DRM_E123, header=F,stringsAsFactors=FALSE)

Tstart_exp_DRM = 20
Tend_exp_DRM = Tstart_exp_DRM + 5
Tsim_DRM = Tend_exp_DRM + 3
DIM_DRM = 364*24+365*(Tsim_DRM - 2)*24+3
Start_tox_DRM = 364*24+365*(Tstart_exp_DRM -2)*24 +3
End_tox_DRM = 364*24+365*(Tend_exp_DRM - 2)*24 +2
End_tox_DRM_bis = End_tox_DRM + 1 
n_var = 4
NAMES <- read.csv(file_DRM_E123, nrow = 6, sep = ";")
DRM_table_E123 <- read.csv(file_DRM_E123, skip = 6, stringsAsFactors = FALSE, sep = ",")

words <- strsplit(NAMES[6,1], ",")[[1]]
xx=unique(tolower(words))
names(DRM_table_E123) <- xx
R=nrow(DRM_table_E123)

DRM_matrix_E123 <- matrix(, nrow = R, ncol = 1)
n_replicates = 40 
for (i in 1:n_replicates)
{
  dataframe = DRM_table_E123[DRM_table_E123[, "[run number]"] == i,]
  assign(paste("Matrix_E123",i, sep=""), select(dataframe, 
                                                c("count turtles with [u_h > u_h^b]","count turtles with [u_h > u_h^p]","sum [weight] of turtles with [u_h > u_h^b]","w-turtles")))
}

for (i in 1:n_replicates)
{
  x= nrow(get(paste("Matrix_E123",i, sep="")))
  if(x != DIM_DRM)
  {print (i)
    print (x)}
}

DRM_m_E123 = Matrix_E1231

for (i in 2:n_replicates)
{
  DRM_m_E123 = cbind (DRM_m_E123,get(paste("Matrix_E123",i, sep="")))
} 
colTot=ncol(DRM_m_E123)

col.names = rep(c("Tot pop","Adult pop","Tot weight","Adult weight"),n_replicates)
colnames(DRM_m_E123) <-col.names

Average_E123_DRM_NEW = colMeans(DRM_m_E123[Start_tox_DRM:End_tox_DRM,],na.rm = TRUE)
mean_time_tot_pop_E123 <- matrix(, nrow = n_replicates, ncol = 1)

ii = 1
i = 1
while (ii <= length(Average_E123_DRM_NEW))
{
  mean_time_tot_pop_E123[i] = Average_E123_DRM_NEW[ii]
  i = i + 1
  ii = ii + 4
}

# Average_no_tox_DRM_E123 = mean(mean_time_tot_pop_E123,na.rm = TRUE)
# sd_no_tox_DRM_E123 = sd(mean_time_tot_pop_E123,na.rm = TRUE)

mean_time_tot_pop_E123W <- matrix(, nrow = n_replicates, ncol = 1)
ii = 3
i = 1
while (ii <= length(Average_E123_DRM_NEW))
{
  mean_time_tot_pop_E123W[i] = Average_E123_DRM_NEW[ii]
  i = i + 1
  ii = ii + 4
}

Average_no_tox_DRM_E123_W = mean(mean_time_tot_pop_E123W,na.rm = TRUE)
sd_no_tox_DRM_E123_W = sd(mean_time_tot_pop_E123W,na.rm = TRUE)

DRM_mr_E123 <- as.data.frame( # sapply returns a list here, so we convert it to a data.frame
  sapply(unique(names(DRM_m_E123)), # for each unique column name
         function(col) rowMeans(DRM_m_E123[names(DRM_m_E123) == col]) # calculate row means
  )
)


######### ######################################################
######### ####################################  E1234
# 
setwd("....")
file_DRM_E1234 = "Cyprinidae_TX_DRM - Copy MF5-12-13-table.csv"
Tstart_exp_DRM = 20
Tend_exp_DRM = Tstart_exp_DRM + 5
Tsim_DRM = Tend_exp_DRM + 3
DIM_DRM = 364*24+365*(Tsim_DRM - 2)*24+3
Start_tox_DRM = 364*24+365*(Tstart_exp_DRM -2)*24 +3
End_tox_DRM = 364*24+365*(Tend_exp_DRM - 2)*24 +2
End_tox_DRM_bis = End_tox_DRM + 1 
n_var = 4

DRM_table_E1234=read.csv(file_DRM_E1234, header=F,stringsAsFactors=FALSE)
NAMES <- read.csv(file_DRM_E1234, nrow = 6, sep = ";")
DRM_table_E1234 <- read.csv(file_DRM_E1234, skip = 6, stringsAsFactors = FALSE, sep = ",")

words <- strsplit(NAMES[6,1], ",")[[1]]
xx=unique(tolower(words))
names(DRM_table_E1234) <- xx
R=nrow(DRM_table_E1234)

DRM_matrix_E1234 <- matrix(, nrow = R, ncol = 1)
n_replicates = 40 

for (i in 1:n_replicates)
{
  dataframe = DRM_table_E1234[DRM_table_E1234[, "[run number]"] == i,]
  assign(paste("Matrix_E1234",i, sep=""), select(dataframe, 
                                                 c("count turtles with [u_h > u_h^b]","count turtles with [u_h > u_h^p]","sum [weight] of turtles with [u_h > u_h^b]","w-turtles")))
}

for (i in 1:n_replicates)
{
  x= nrow(get(paste("Matrix_E1234",i, sep="")))
  if(x != DIM_DRM)
  {print (i)
    print (x)}
}


DRM_m_E1234 = Matrix_E12341

for (i in 2:n_replicates)
{
  DRM_m_E1234 = cbind (DRM_m_E1234,get(paste("Matrix_E1234",i, sep="")))
} 
colTot=ncol(DRM_m_E1234)

col.names = rep(c("Tot pop","Adult pop","Tot weight","Adult weight"),n_replicates)
colnames(DRM_m_E1234) <-col.names

Average_E1234_DRM_NEW = colMeans(DRM_m_E1234[Start_tox_DRM:End_tox_DRM,],na.rm = TRUE)
mean_time_tot_pop_E1234 <- matrix(, nrow = n_replicates, ncol = 1)

ii = 1
i = 1
while (ii <= length(Average_E1234_DRM_NEW))
{
  mean_time_tot_pop_E1234[i] = Average_E1234_DRM_NEW[ii]
  i = i + 1
  ii = ii + 4
}

# Average_no_tox_DRM_E1234 = mean(mean_time_tot_pop_E1234,na.rm = TRUE)
# sd_no_tox_DRM_E1234 = sd(mean_time_tot_pop_E1234,na.rm = TRUE)

mean_time_tot_pop_E1234W <- matrix(, nrow = n_replicates, ncol = 1)

ii = 3
i = 1
while (ii <= length(Average_E1234_DRM_NEW))
{
  mean_time_tot_pop_E1234W[i] = Average_E1234_DRM_NEW[ii]
  i = i + 1
  ii = ii + 4
}


Average_no_tox_DRM_E1234_W = mean(mean_time_tot_pop_E1234W,na.rm = TRUE)
sd_no_tox_DRM_E1234_W = sd(mean_time_tot_pop_E1234W,na.rm = TRUE)

DRM_mr_E1234 <- as.data.frame( # sapply returns a list here, so we convert it to a data.frame
  sapply(unique(names(DRM_m_E1234)), # for each unique column name
         function(col) rowMeans(DRM_m_E1234[names(DRM_m_E1234) == col]) # calculate row means
  )
)


i1 = colnames(DRM_m) == "Tot pop"
M = DRM_m[,i1 , drop=FALSE]
Mm = colMeans(M[Start_tox_DRM:End_tox_DRM,],na.rm = TRUE)

i1 = colnames(DRM_m_E1) == "Tot pop"
M_E1 = DRM_m_E1[,i1 , drop=FALSE]
Mm_E1 = colMeans(M_E1[Start_tox_DRM:End_tox_DRM,],na.rm = TRUE)

i1 = colnames(DRM_m_E12) == "Tot pop"
M_E12 = DRM_m_E12[,i1 , drop=FALSE]
Mm_E12 = colMeans(M_E12[Start_tox_DRM:End_tox_DRM,],na.rm = TRUE)

i1 = colnames(DRM_m_E123) == "Tot pop"
M_E123 = DRM_m_E123[,i1 , drop=FALSE]
Mm_E123 = colMeans(M_E123[Start_tox_DRM:End_tox_DRM,],na.rm = TRUE)

i1 = colnames(DRM_m_E1234) == "Tot pop"
M_E1234 = DRM_m_E1234[,i1 , drop=FALSE]
Mm_E1234 = colMeans(M_E1234[Start_tox_DRM:End_tox_DRM,],na.rm = TRUE)

ratios1 = c(M[Start_tox_DRM:End_tox_DRM,1]/M[Start_tox_DRM:End_tox_DRM,1],
            M_E1[Start_tox_DRM:End_tox_DRM,1]/M[Start_tox_DRM:End_tox_DRM,1],
            M_E12[Start_tox_DRM:End_tox_DRM,1]/M[Start_tox_DRM:End_tox_DRM,1],
            M_E123[Start_tox_DRM:End_tox_DRM,1]/M[Start_tox_DRM:End_tox_DRM,1],
            M_E1234[Start_tox_DRM:End_tox_DRM,1]/M[Start_tox_DRM:End_tox_DRM,1])


ratios = cbind(Mm/Mm,Mm_E1/Mm,Mm_E12/Mm,Mm_E123/Mm,Mm_E1234/Mm)



## ratio of replicates

ratio_replicates_DRM_E1 = mean_time_tot_pop_E1/mean_time_tot_pop
Average_ratio_replicates_DRM_E1 = mean(ratio_replicates_DRM_E1,na.rm = TRUE)
Max_ratio_replicates_DRM_E1 = max(ratio_replicates_DRM_E1, na.rm = TRUE)
Min_ratio_replicates_DRM_E1 = min(ratio_replicates_DRM_E1, na.rm = TRUE)
std_ratio_replicates_DRM_E1 = sd(ratio_replicates_DRM_E1,na.rm = TRUE)

ratio_replicates_DRM_E12 = mean_time_tot_pop_E12/mean_time_tot_pop
Average_ratio_replicates_DRM_E12 = mean(ratio_replicates_DRM_E12,na.rm = TRUE)
Max_ratio_replicates_DRM_E12 = max(ratio_replicates_DRM_E12, na.rm = TRUE)
Min_ratio_replicates_DRM_E12 = min(ratio_replicates_DRM_E12, na.rm = TRUE)
std_ratio_replicates_DRM_E12 = sd(ratio_replicates_DRM_E12,na.rm = TRUE)


ratio_replicates_DRM_E123 = mean_time_tot_pop_E123/mean_time_tot_pop
Average_ratio_replicates_DRM_E123 = mean(ratio_replicates_DRM_E123,na.rm = TRUE)
Max_ratio_replicates_DRM_E123 = max(ratio_replicates_DRM_E123, na.rm = TRUE)
Min_ratio_replicates_DRM_E123 = min(ratio_replicates_DRM_E123, na.rm = TRUE)
std_ratio_replicates_DRM_E123 = sd(ratio_replicates_DRM_E123,na.rm = TRUE)


ratio_replicates_DRM_E1234 = mean_time_tot_pop_E1234/mean_time_tot_pop
Average_ratio_replicates_DRM_E1234 = mean(ratio_replicates_DRM_E1234,na.rm = TRUE)
Max_ratio_replicates_DRM_E1234 = max(ratio_replicates_DRM_E1234, na.rm = TRUE)
Min_ratio_replicates_DRM_E1234 = min(ratio_replicates_DRM_E1234, na.rm = TRUE)
std_ratio_replicates_DRM_E1234 = sd(ratio_replicates_DRM_E1234,na.rm = TRUE)



###############################
####################### **********************************  ********************************** #######################################
################# TS


setwd("....")
file_TS_noTox = "TX_TS-W.csv"
TS_noTox=read.csv(file_TS_noTox, header = T, as.is = T)
Tstart_exp_TS = 15
Tend_exp_TS = Tstart_exp_TS + 5
Tsim_TS = Tend_exp_TS + 3
DIM_TS = 365-149+(Tsim_TS - 2)*365+1
Start_tox_TS = 365-149+(Tstart_exp_TS -2)*365 +1
End_tox_TS = 365-149+(Tend_exp_TS - 2)*365 
End_tox_TS_bis=End_tox_TS  + 1 

n_var =4
R=nrow(TS_noTox)
C=ncol(TS_noTox)
n_replicates = (C - 1)/n_var
Tot <- matrix(, nrow = R, ncol = 1)


i=2
M1=TS_noTox[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  TS_noTox[,c]
  M1=cbind(M1,x)
}

Average_time_TS = colMeans(M1[Start_tox_TS:End_tox_TS,],na.rm = TRUE)
# Average_no_tox_TS = mean(Average_time_TS,na.rm = TRUE)
# sd_no_tox_TS = sd(Average_time_TS,na.rm = TRUE)

i=4

M1W=TS_noTox[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  TS_noTox[,c]
  M1W=cbind(M1W,x)
}

Average_time_TSW = colMeans(M1W[Start_tox_TS:End_tox_TS,],na.rm = TRUE)

Average_no_tox_TS_W = mean(Average_time_TSW,na.rm = TRUE)
sd_no_tox_TS_W = sd(Average_time_TSW,na.rm = TRUE)



############################################################################################################################################
##########---------------------- Survival

setwd("....")
file_TS_S = "TX_TS_E1-W.csv"
TS_Tox_S=read.csv(file_TS_S, header = T, as.is = T)
Tstart_exp_TS = 15
Tend_exp_TS = Tstart_exp_TS + 5
Tsim_TS = Tend_exp_TS + 3
DIM_TS = 365-149+(Tsim_TS - 2)*365+1
Start_tox_TS = 365-149+(Tstart_exp_TS -2)*365 +1
End_tox_TS = 365-149+(Tend_exp_TS - 2)*365 
End_tox_TS_bis=End_tox_TS  + 1 

n_var =4
R=nrow(TS_Tox_S)
C=ncol(TS_Tox_S)
n_replicates = (C - 1)/n_var

Tot_Tox_S <- matrix(, nrow = R, ncol = 1)

i=2
M1_Tox_S=TS_Tox_S[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  TS_Tox_S[,c]
  M1_Tox_S=cbind(M1_Tox_S,x)
}

Average_time_TS_Tox_S = colMeans(M1_Tox_S[Start_tox_TS:End_tox_TS,],na.rm = TRUE)
# Average_tox_TS_S = mean(Average_time_TS_Tox_S,na.rm = TRUE)
# sd_tox_TS_S = sd(Average_time_TS_Tox_S,na.rm = TRUE)

i=4
M1_Tox_SW=TS_Tox_S[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  TS_Tox_S[,c]
  M1_Tox_SW=cbind(M1_Tox_SW,x)
}

Average_time_TS_Tox_SW = colMeans(M1_Tox_SW[Start_tox_TS:End_tox_TS,],na.rm = TRUE)

Average_tox_TS_S_W = mean(Average_time_TS_Tox_SW,na.rm = TRUE)
sd_tox_TS_S_W = sd(Average_time_TS_Tox_SW,na.rm = TRUE)


############################################################################################################################################
##########---------------------- Survival - Egg
setwd("....")
file_TS_S_E = "TX_TS_E12-W.csv"
TS_Tox_S_E=read.csv(file_TS_S_E, header = T, as.is = T)
Tstart_exp_TS = 15
Tend_exp_TS = Tstart_exp_TS + 5
Tsim_TS = Tend_exp_TS + 3
DIM_TS = 365-149+(Tsim_TS - 2)*365+1
Start_tox_TS = 365-149+(Tstart_exp_TS -2)*365 +1
End_tox_TS = 365-149+(Tend_exp_TS - 2)*365 
End_tox_TS_bis=End_tox_TS  + 1 

n_var = 4
R=nrow(TS_Tox_S_E)
C=ncol(TS_Tox_S_E)
n_replicates = (C - 1)/ n_var
Tot_Tox_S_E <- matrix(, nrow = R, ncol = 1)


i=2

M1_Tox_S_E=TS_Tox_S_E[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  TS_Tox_S_E[,c]
  M1_Tox_S_E=cbind(M1_Tox_S_E,x)
}

Average_time_TS_Tox_S_E= colMeans(M1_Tox_S_E[Start_tox_TS:End_tox_TS,],na.rm = TRUE)

# Average_tox_TS_S_E = mean(Average_time_TS_Tox_S_E,na.rm = TRUE)
# sd_tox_TS_S_E = sd(Average_time_TS_Tox_S_E,na.rm = TRUE)

i=4
M1_Tox_S_EW=TS_Tox_S_E[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  TS_Tox_S_E[,c]
  M1_Tox_S_EW=cbind(M1_Tox_S_EW,x)
}

Average_time_TS_Tox_S_EW= colMeans(M1_Tox_S_EW[Start_tox_TS:End_tox_TS,],na.rm = TRUE)

Average_tox_TS_S_E_W = mean(Average_time_TS_Tox_S_EW,na.rm = TRUE)
sd_tox_TS_S_E_W = sd(Average_time_TS_Tox_S_EW,na.rm = TRUE)


############################################################################################################################################
##########---------------------- Survival - Egg - Hatch
setwd("....")
file_TS_S_E_H = "TX_TS_E123-W.csv"
TS_Tox_S_E_H=read.csv(file_TS_S_E_H, header = T, as.is = T)
Tstart_exp_TS = 15
Tend_exp_TS = Tstart_exp_TS + 5
Tsim_TS = Tend_exp_TS + 3
DIM_TS = 365-149+(Tsim_TS - 2)*365+1
Start_tox_TS = 365-149+(Tstart_exp_TS -2)*365 +1
End_tox_TS = 365-149+(Tend_exp_TS - 2)*365 
End_tox_TS_bis=End_tox_TS  + 1 

n_var = 4
R=nrow(TS_Tox_S_E_H)
C=ncol(TS_Tox_S_E_H)
n_replicates = (C - 1)/n_var

Tot_Tox_S_E_H <- matrix(, nrow = R, ncol = 1)

i=2
M1_Tox_S_E_H=TS_Tox_S_E_H[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  TS_Tox_S_E_H[,c]
  M1_Tox_S_E_H=cbind(M1_Tox_S_E_H,x)
}

Average_time_TS_Tox_S_E_H = colMeans(M1_Tox_S_E_H[Start_tox_TS:End_tox_TS,],na.rm = TRUE)

Average_tox_TS_S_E_H = mean(Average_time_TS_Tox_S_E_H,na.rm = TRUE)
sd_tox_TS_S_E_H = sd(Average_time_TS_Tox_S_E_H,na.rm = TRUE)

i=4
M1_Tox_S_E_HW=TS_Tox_S_E_H[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  TS_Tox_S_E_H[,c]
  M1_Tox_S_E_HW=cbind(M1_Tox_S_E_HW,x)
}

Average_time_TS_Tox_S_E_HW = colMeans(M1_Tox_S_E_HW[Start_tox_TS:End_tox_TS,],na.rm = TRUE)

Average_tox_TS_S_E_H_W = mean(Average_time_TS_Tox_S_E_HW,na.rm = TRUE)
sd_tox_TS_S_E_H_W = sd(Average_time_TS_Tox_S_E_HW,na.rm = TRUE)


############################################################################################################################################
##########---------------------- All
setwd("....")
file_TS = "TX_TS_all-W.csv"
TS_Tox_MF8=read.csv(file_TS, header = T, as.is = T)
Tstart_exp_TS = 15
Tend_exp_TS = Tstart_exp_TS + 5
Tsim_TS = Tend_exp_TS + 3
DIM_TS = 365-149+(Tsim_TS - 2)*365+1
Start_tox_TS = 365-149+(Tstart_exp_TS -2)*365 +1
End_tox_TS = 365-149+(Tend_exp_TS - 2)*365 
End_tox_TS_bis=End_tox_TS  + 1 

n_var = 4 
R=nrow(TS_Tox_MF8)
C=ncol(TS_Tox_MF8)
n_replicates = (C - 1)/n_var

Tot_Tox_MF8 <- matrix(, nrow = R, ncol = 1)

i=2
M1_Tox_S_E_H_I = TS_Tox_MF8[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  TS_Tox_MF8[,c]
  M1_Tox_S_E_H_I=cbind(M1_Tox_S_E_H_I,x)
}

Average_time_TS_Tox_S_E_H_I= colMeans(M1_Tox_S_E_H_I[Start_tox_TS:End_tox_TS,],na.rm = TRUE)

# Average_tox_TS_S_E_H_I = mean(Average_time_TS_Tox_S_E_H_I,na.rm = TRUE)
# sd_tox_TS_S_E_H_I = sd(Average_time_TS_Tox_S_E_H_I,na.rm = TRUE)

i=4
M1_Tox_S_E_H_IW = TS_Tox_MF8[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  TS_Tox_MF8[,c]
  M1_Tox_S_E_H_IW=cbind(M1_Tox_S_E_H_IW,x)
}

Average_time_TS_Tox_S_E_H_IW= colMeans(M1_Tox_S_E_H_IW[Start_tox_TS:End_tox_TS,],na.rm = TRUE)

Average_tox_TS_S_E_H_I_W = mean(Average_time_TS_Tox_S_E_H_IW,na.rm = TRUE)
sd_tox_TS_S_E_H_I_W = sd(Average_time_TS_Tox_S_E_H_IW,na.rm = TRUE)

## ratio of replicates

ratio_replicates_TS_E1 = Average_time_TS_Tox_S/Average_time_TS
Average_ratio_replicates_TS_E1 = mean(ratio_replicates_TS_E1,na.rm = TRUE)
Max_ratio_replicates_TS_E1 = max(ratio_replicates_TS_E1, na.rm = TRUE)
Min_ratio_replicates_TS_E1 = min(ratio_replicates_TS_E1, na.rm = TRUE)
std_ratio_replicates_TS_E1 = sd(ratio_replicates_TS_E1,na.rm = TRUE)

ratio_replicates_TS_E12 = Average_time_TS_Tox_S_E/Average_time_TS
Average_ratio_replicates_TS_E12 = mean(ratio_replicates_TS_E12,na.rm = TRUE)
Max_ratio_replicates_TS_E12 = max(ratio_replicates_TS_E12, na.rm = TRUE)
Min_ratio_replicates_TS_E12 = min(ratio_replicates_TS_E12, na.rm = TRUE)
std_ratio_replicates_TS_E12 = sd(ratio_replicates_TS_E12,na.rm = TRUE)


ratio_replicates_TS_E123 = Average_time_TS_Tox_S_E_H/Average_time_TS
Average_ratio_replicates_TS_E123 = mean(ratio_replicates_TS_E123,na.rm = TRUE)
Max_ratio_replicates_TS_E123 = max(ratio_replicates_TS_E123, na.rm = TRUE)
Min_ratio_replicates_TS_E123 = min(ratio_replicates_TS_E123, na.rm = TRUE)
std_ratio_replicates_TS_E123 = sd(ratio_replicates_TS_E123,na.rm = TRUE)


ratio_replicates_TS_E1234 = Average_time_TS_Tox_S_E_H_I/Average_time_TS
Average_ratio_replicates_TS_E1234 = mean(ratio_replicates_TS_E1234,na.rm = TRUE)
Max_ratio_replicates_TS_E1234 = max(ratio_replicates_TS_E1234, na.rm = TRUE)
Min_ratio_replicates_TS_E1234 = min(ratio_replicates_TS_E1234, na.rm = TRUE)
std_ratio_replicates_TS_E1234 = sd(ratio_replicates_TS_E1234, na.rm = TRUE)


###################### ********************************** ############################################################################
####################### **********************************  ********************************** #######################################
###################### SD

setwd("....")
file_SD_noTox = "TX_SD-W.csv"
SD_noTox=read.csv(file_SD_noTox, header = T, as.is = T)
Tstart_exp_SD = 15
Tend_exp_SD = Tstart_exp_SD + 5
Tsim_SD = Tend_exp_SD + 3
DIM_SD = 365-89+(Tsim_SD - 2)*365+1
Start_tox_SD = 365-89+(Tstart_exp_SD -2)*365 +1
End_tox_SD = 365-89+(Tend_exp_SD - 2)*365
End_tox_SD_bis = End_tox_SD +1

n_var =4
R=nrow(SD_noTox)
C=ncol(SD_noTox)
n_replicates = (C - 1)/n_var
Tot <- matrix(, nrow = R, ncol = 1)



i=2

M1=SD_noTox[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  SD_noTox[,c]
  M1=cbind(M1,x)
}

Average_time_SD = colMeans(M1[Start_tox_SD:End_tox_SD,],na.rm = TRUE)

# Average_no_tox_SD = mean(Average_time_SD,na.rm = TRUE)
# sd_no_tox_SD = sd(Average_time_SD,na.rm = TRUE)

i=4
M1W=SD_noTox[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  SD_noTox[,c]
  M1W=cbind(M1W,x)
}

Average_time_SDW = colMeans(M1W[Start_tox_SD:End_tox_SD,],na.rm = TRUE)

Average_no_tox_SD_W = mean(Average_time_SDW,na.rm = TRUE)
sd_no_tox_SD_W = sd(Average_time_SDW,na.rm = TRUE)


############################################################################################################################################
##########---------------------- Survival
setwd("....")
file_SD_S = "TX_SD_E1-W.csv"
SD_Tox_S=read.csv(file_SD_S, header = T, as.is = T)
Tstart_exp_SD = 15
Tend_exp_SD = Tstart_exp_SD + 5
Tsim_SD = Tend_exp_SD + 3
DIM_SD = 365-89+(Tsim_SD - 2)*365+1
Start_tox_SD = 365-89+(Tstart_exp_SD -2)*365 +1
End_tox_SD = 365-89+(Tend_exp_SD - 2)*365
End_tox_SD_bis = End_tox_SD +1

n_var = 4
R=nrow(SD_Tox_S)
C=ncol(SD_Tox_S)
n_replicates = (C - 1)/n_var

Tot_Tox_S <- matrix(, nrow = R, ncol = 1)

i=2
M1_Tox_S = SD_Tox_S[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  SD_Tox_S[,c]
  M1_Tox_S=cbind(M1_Tox_S,x)
}

Average_time_SD_Tox_S = colMeans(M1_Tox_S[Start_tox_SD:End_tox_SD,],na.rm = TRUE)

Average_tox_SD_S = mean(Average_time_SD_Tox_S,na.rm = TRUE)
sd_tox_SD_S = sd(Average_time_SD_Tox_S,na.rm = TRUE)

i=4
M1_Tox_SW = SD_Tox_S[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  SD_Tox_S[,c]
  M1_Tox_SW=cbind(M1_Tox_SW,x)
}

Average_time_SD_Tox_SW = colMeans(M1_Tox_SW[Start_tox_SD:End_tox_SD,],na.rm = TRUE)

# 
Average_tox_SD_S_W = mean(Average_time_SD_Tox_SW,na.rm = TRUE)
sd_tox_SD_S_W = sd(Average_time_SD_Tox_SW,na.rm = TRUE)
# 
# ratio_SD_S = Tot_Tox_S[Start_tox_SD:End_tox_SD,1]/Tot[Start_tox_SD:End_tox_SD,1]
# mean_ratio_SD_S = mean(ratio_SD_S)
# min_SD_S = min(ratio_SD_S)
# max_SD_S = max(ratio_SD_S)

############################################################################################################################################
##########---------------------- Survival - Egg
setwd("....")
file_SD_S_E = "TX_SD_E12-W.csv"
SD_Tox_S_E=read.csv(file_SD_S_E, header = T, as.is = T)
Tstart_exp_SD = 15
Tend_exp_SD = Tstart_exp_SD + 5
Tsim_SD = Tend_exp_SD + 3
DIM_SD = 365-89+(Tsim_SD - 2)*365+1
Start_tox_SD = 365-89+(Tstart_exp_SD -2)*365 +1
End_tox_SD = 365-89+(Tend_exp_SD - 2)*365
End_tox_SD_bis = End_tox_SD +1

n_var = 4
R=nrow(SD_Tox_S_E)
C=ncol(SD_Tox_S_E)
n_replicates = (C - 1)/ n_var


i=2

M1_Tox_S_E = SD_Tox_S_E[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  SD_Tox_S_E[,c]
  M1_Tox_S_E = cbind(M1_Tox_S_E,x)
}

Average_time_SD_Tox_S_E = colMeans(M1_Tox_S_E[Start_tox_SD:End_tox_SD,],na.rm = TRUE)

# Average_tox_SD_S_E = mean(Average_time_SD_Tox_S_E,na.rm = TRUE)
# sd_tox_SD_S_E = sd(Average_time_SD_Tox_S_E,na.rm = TRUE)

i=4
M1_Tox_S_EW = SD_Tox_S_E[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  SD_Tox_S_E[,c]
  M1_Tox_S_EW = cbind(M1_Tox_S_EW,x)
}

Average_time_SD_Tox_S_EW = colMeans(M1_Tox_S_EW[Start_tox_SD:End_tox_SD,],na.rm = TRUE)

Average_tox_SD_S_E_W = mean(Average_time_SD_Tox_S_EW,na.rm = TRUE)
sd_tox_SD_S_E_W = sd(Average_time_SD_Tox_S_EW,na.rm = TRUE)

# ratio_SD_S_E = Tot_Tox_S_E[Start_tox_SD:End_tox_SD,1]/Tot[Start_tox_SD:End_tox_SD,1]
# mean_ratio_SD_S_E = mean(ratio_SD_S_E)
# min_SD_S_E = min(ratio_SD_S_E)
# max_SD_S_E = max(ratio_SD_S_E)

############################################################################################################################################
##########---------------------- Survival - Egg - Hatch
setwd("....")
file_SD_S_E_H = "TX_SD_E123-W.csv"
SD_Tox_S_E_H=read.csv(file_SD_S_E_H, header = T, as.is = T)
Tstart_exp_SD = 15
Tend_exp_SD = Tstart_exp_SD + 5
Tsim_SD = Tend_exp_SD + 3
DIM_SD = 365-89+(Tsim_SD - 2)*365+1
Start_tox_SD = 365-89+(Tstart_exp_SD -2)*365 +1
End_tox_SD = 365-89+(Tend_exp_SD - 2)*365
End_tox_SD_bis = End_tox_SD +1

n_var = 4
R=nrow(SD_Tox_S_E_H)
C=ncol(SD_Tox_S_E_H)
n_replicates = (C - 1)/n_var
Tot_Tox_S_E_H <- matrix(, nrow = R, ncol = 1)


i=2
M1_Tox_S_E_H = SD_Tox_S_E_H[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  SD_Tox_S_E_H[,c]
  M1_Tox_S_E_H = cbind(M1_Tox_S_E_H,x)
}

Average_time_SD_Tox_S_E_H = colMeans(M1_Tox_S_E_H[Start_tox_SD:End_tox_SD,],na.rm = TRUE)

# Average_tox_SD_S_E_H = mean(Average_time_SD_Tox_S_E_H,na.rm = TRUE)
# sd_tox_SD_S_E_H = sd(Average_time_SD_Tox_S_E_H,na.rm = TRUE)

i=4
M1_Tox_S_E_HW = SD_Tox_S_E_H[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  SD_Tox_S_E_H[,c]
  M1_Tox_S_E_HW = cbind(M1_Tox_S_E_HW,x)
}

Average_time_SD_Tox_S_E_HW = colMeans(M1_Tox_S_E_HW[Start_tox_SD:End_tox_SD,],na.rm = TRUE)

Average_tox_SD_S_E_H_W = mean(Average_time_SD_Tox_S_E_HW,na.rm = TRUE)
sd_tox_SD_S_E_H_W = sd(Average_time_SD_Tox_S_E_HW,na.rm = TRUE)

# ratio_SD_S_E_H = Tot_Tox_S_E_H[Start_tox_SD:End_tox_SD,1]/Tot[Start_tox_SD:End_tox_SD,1]
# mean_ratio_SD_S_E_H = mean(ratio_SD_S_E_H)
# min_SD_S_E_H = min(ratio_SD_S_E_H)
# max_SD_S_E_H = max(ratio_SD_S_E_H)

############################################################################################################################################
##########---------------------- weight 
setwd("....")
file_SD = "TX_SD_all-W.csv"
SD_Tox_MF8=read.csv(file_SD, header = T, as.is = T)
Tstart_exp_SD = 15
Tend_exp_SD = Tstart_exp_SD + 5
Tsim_SD = Tend_exp_SD + 3
DIM_SD = 365-89+(Tsim_SD - 2)*365+1
Start_tox_SD = 365-89+(Tstart_exp_SD -2)*365 +1
End_tox_SD = 365-89+(Tend_exp_SD - 2)*365
End_tox_SD_bis = End_tox_SD +1

n_var = 4 
R=nrow(SD_Tox_MF8)
C=ncol(SD_Tox_MF8)
n_replicates = (C - 1)/n_var
Tot_Tox_MF8 <- matrix(, nrow = R, ncol = 1)

i=2

M1_Tox_S_E_H_I = SD_Tox_MF8[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  SD_Tox_MF8[,c]
  M1_Tox_S_E_H_I = cbind(M1_Tox_S_E_H_I,x)
}

Average_time_SD_Tox_S_E_H_I = colMeans(M1_Tox_S_E_H_I[Start_tox_SD:End_tox_SD,],na.rm = TRUE)
# 
# Average_tox_SD_S_E_H_I = mean(Average_time_SD_Tox_S_E_H_I,na.rm = TRUE)
# sd_tox_SD_S_E_H_I = sd(Average_time_SD_Tox_S_E_H_I,na.rm = TRUE)


i=4
M1_Tox_S_E_H_IW = SD_Tox_MF8[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  SD_Tox_MF8[,c]
  M1_Tox_S_E_H_IW = cbind(M1_Tox_S_E_H_IW,x)
}

Average_time_SD_Tox_S_E_H_IW = colMeans(M1_Tox_S_E_H_IW[Start_tox_SD:End_tox_SD,],na.rm = TRUE)

Average_tox_SD_S_E_H_I_W = mean(Average_time_SD_Tox_S_E_H_IW,na.rm = TRUE)
sd_tox_SD_S_E_H_I_W = sd(Average_time_SD_Tox_S_E_H_IW,na.rm = TRUE)

# ratio_SD_All = Tot_Tox_MF8[Start_tox_SD:End_tox_SD,1]/Tot[Start_tox_SD:End_tox_SD,1]
# mean_ratio_SD_All = mean(ratio_SD_All)
# min_SD_All = min(ratio_SD_All)
# max_SD_All = max(ratio_SD_All)


## ratio of replicates

ratio_replicates_SD_E1 = Average_time_SD_Tox_S/Average_time_SD
Average_ratio_replicates_SD_E1 = mean(ratio_replicates_SD_E1,na.rm = TRUE)
Max_ratio_replicates_SD_E1 = max(ratio_replicates_SD_E1, na.rm = TRUE)
Min_ratio_replicates_SD_E1 = min(ratio_replicates_SD_E1, na.rm = TRUE)
std_ratio_replicates_SD_E1 = sd(ratio_replicates_SD_E1,na.rm = TRUE)

ratio_replicates_SD_E12 = Average_time_SD_Tox_S_E/Average_time_SD
Average_ratio_replicates_SD_E12 = mean(ratio_replicates_SD_E12,na.rm = TRUE)
Max_ratio_replicates_SD_E12 = max(ratio_replicates_SD_E12, na.rm = TRUE)
Min_ratio_replicates_SD_E12 = min(ratio_replicates_SD_E12, na.rm = TRUE)
std_ratio_replicates_SD_E12 = sd(ratio_replicates_SD_E12,na.rm = TRUE)


ratio_replicates_SD_E123 = Average_time_SD_Tox_S_E_H/Average_time_SD
Average_ratio_replicates_SD_E123 = mean(ratio_replicates_SD_E123,na.rm = TRUE)
Max_ratio_replicates_SD_E123 = max(ratio_replicates_SD_E123, na.rm = TRUE)
Min_ratio_replicates_SD_E123 = min(ratio_replicates_SD_E123, na.rm = TRUE)
std_ratio_replicates_SD_E123 = sd(ratio_replicates_SD_E123,na.rm = TRUE)


ratio_replicates_SD_E1234 = Average_time_SD_Tox_S_E_H_I/Average_time_SD
Average_ratio_replicates_SD_E1234 = mean(ratio_replicates_SD_E1234,na.rm = TRUE)
Max_ratio_replicates_SD_E1234 = max(ratio_replicates_SD_E1234, na.rm = TRUE)
Min_ratio_replicates_SD_E1234 = min(ratio_replicates_SD_E1234, na.rm = TRUE)
std_ratio_replicates_SD_E1234 = sd(ratio_replicates_SD_E1234, na.rm = TRUE)


###################### ********************************** ############################################################################
####################### **********************************  ********************************** #######################################
###################### HC

setwd("....")
file_HC_noTox = "TX_HC-W.csv"
HC_noTox=read.csv(file_HC_noTox, header = T, as.is = T)
Tstart_exp_HC = 25
Tend_exp_HC = Tstart_exp_HC + 5
Tsim_HC = Tend_exp_HC + 3
DIM_HC = 365-79+(Tsim_HC - 2)*365+1
Start_tox_HC = 365-79+(Tstart_exp_HC -2)*365 +1
End_tox_HC = 365-79+(Tend_exp_HC - 2)*365
End_tox_HC_bis = End_tox_HC +1
n_var =4

R=nrow(HC_noTox)
C=ncol(HC_noTox)
n_replicates = (C - 1)/n_var
Tot <- matrix(, nrow = R, ncol = 1)



i=2
M1=HC_noTox[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  HC_noTox[,c]
  M1=cbind(M1,x)
}

Average_time_HC = colMeans(M1[Start_tox_HC:End_tox_HC,],na.rm = TRUE)

# Average_no_tox_HC = mean(Average_time_HC,na.rm = TRUE)
# sd_no_tox_HC = sd(Average_time_HC,na.rm = TRUE)


i=4
M1W=HC_noTox[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  HC_noTox[,c]
  M1W=cbind(M1W,x)
}

Average_time_HCW = colMeans(M1W[Start_tox_HC:End_tox_HC,],na.rm = TRUE)

Average_no_tox_HC_W = mean(Average_time_HCW,na.rm = TRUE)
sd_no_tox_HC_W = sd(Average_time_HCW,na.rm = TRUE)

############################################################################################################################################
##########---------------------- Survival
setwd("....")
file_HC_S = "TX_HC-E1-W.csv"
HC_Tox_S=read.csv(file_HC_S, header = T, as.is = T)

Tstart_exp_HC = 25
Tend_exp_HC = Tstart_exp_HC + 5

Tsim_HC = Tend_exp_HC + 3
DIM_HC = 365-79+(Tsim_HC - 2)*365+1
Start_tox_HC = 365-79+(Tstart_exp_HC -2)*365 +1
End_tox_HC = 365-79+(Tend_exp_HC - 2)*365
End_tox_HC_bis = End_tox_HC +1
n_var = 4
R=nrow(HC_Tox_S)
C=ncol(HC_Tox_S)
n_replicates = (C - 1)/n_var

Tot_Tox_S <- matrix(, nrow = R, ncol = 1)

i=2

M1_Tox_S = HC_Tox_S[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  HC_Tox_S[,c]
  M1_Tox_S = cbind(M1_Tox_S,x)
}

Average_time_HC_Tox_S = colMeans(M1_Tox_S[Start_tox_HC:End_tox_HC,],na.rm = TRUE)

# Average_tox_HC_S = mean(Average_time_HC_Tox_S,na.rm = TRUE)
# sd_tox_HC_S = sd(Average_time_HC_Tox_S,na.rm = TRUE)


i=4
M1_Tox_SW = HC_Tox_S[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  HC_Tox_S[,c]
  M1_Tox_SW = cbind(M1_Tox_SW,x)
}

Average_time_HC_Tox_SW = colMeans(M1_Tox_SW[Start_tox_HC:End_tox_HC,],na.rm = TRUE)

Average_tox_HC_S_W = mean(Average_time_HC_Tox_SW,na.rm = TRUE)
sd_tox_HC_S_W = sd(Average_time_HC_Tox_SW,na.rm = TRUE)


############################################################################################################################################
##########---------------------- Survival - Egg
setwd("....")
file_HC_S_E = "TX_HC-E12-W.csv"
HC_Tox_S_E=read.csv(file_HC_S_E, header = T, as.is = T)
Tstart_exp_HC = 25
Tend_exp_HC = Tstart_exp_HC + 5
Tsim_HC = Tend_exp_HC + 3
DIM_HC = 365-79+(Tsim_HC - 2)*365+1
Start_tox_HC = 365-79+(Tstart_exp_HC -2)*365 +1
End_tox_HC = 365-79+(Tend_exp_HC - 2)*365
End_tox_HC_bis = End_tox_HC +1
n_var = 4

R=nrow(HC_Tox_S_E)
C=ncol(HC_Tox_S_E)
n_replicates = (C - 1)/ n_var

Tot_Tox_S_E <- matrix(, nrow = R, ncol = 1)

i=2
M1_Tox_S_E = HC_Tox_S_E[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  HC_Tox_S_E[,c]
  M1_Tox_S_E = cbind(M1_Tox_S_E,x)
}

Average_time_HC_Tox_S_E = colMeans(M1_Tox_S_E[Start_tox_HC:End_tox_HC,],na.rm = TRUE)

# Average_tox_HC_S_E = mean(Average_time_HC_Tox_S_E,na.rm = TRUE)
# sd_tox_HC_S_E = sd(Average_time_HC_Tox_S_E,na.rm = TRUE)


i=4
M1_Tox_S_EW = HC_Tox_S_E[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  HC_Tox_S_E[,c]
  M1_Tox_S_EW = cbind(M1_Tox_S_EW,x)
}

Average_time_HC_Tox_S_EW = colMeans(M1_Tox_S_EW[Start_tox_HC:End_tox_HC,],na.rm = TRUE)

Average_tox_HC_S_E_W = mean(Average_time_HC_Tox_S_EW,na.rm = TRUE)
sd_tox_HC_S_E_W = sd(Average_time_HC_Tox_S_EW,na.rm = TRUE)


############################################################################################################################################
##########---------------------- Survival - Egg - Hatch
setwd("....")
file_HC_S_E_H = "TX_HC-E123-W.csv"
HC_Tox_S_E_H=read.csv(file_HC_S_E_H, header = T, as.is = T)
Tstart_exp_HC = 25
Tend_exp_HC = Tstart_exp_HC + 5
Tsim_HC = Tend_exp_HC + 3
DIM_HC = 365-79+(Tsim_HC - 2)*365+1
Start_tox_HC = 365-79+(Tstart_exp_HC -2)*365 +1
End_tox_HC = 365-79+(Tend_exp_HC - 2)*365
End_tox_HC_bis = End_tox_HC +1
n_var = 4
R=nrow(HC_Tox_S_E_H)
C=ncol(HC_Tox_S_E_H)
n_replicates = (C - 1)/n_var

Tot_Tox_S_E_H <- matrix(, nrow = R, ncol = 1)

i=2
M1_Tox_S_E_H = HC_Tox_S_E_H[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  HC_Tox_S_E_H[,c]
  M1_Tox_S_E_H = cbind(M1_Tox_S_E_H,x)
}

Average_time_HC_Tox_S_E_H = colMeans(M1_Tox_S_E_H[Start_tox_HC:End_tox_HC,],na.rm = TRUE)

# Average_tox_HC_S_E_H = mean(Average_time_HC_Tox_S_E_H,na.rm = TRUE)
# sd_tox_HC_S_E_H = sd(Average_time_HC_Tox_S_E_H,na.rm = TRUE)

i=4
M1_Tox_S_E_HW = HC_Tox_S_E_H[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  HC_Tox_S_E_H[,c]
  M1_Tox_S_E_HW = cbind(M1_Tox_S_E_HW,x)
}

Average_time_HC_Tox_S_E_HW = colMeans(M1_Tox_S_E_HW[Start_tox_HC:End_tox_HC,],na.rm = TRUE)

Average_tox_HC_S_E_H_W = mean(Average_time_HC_Tox_S_E_HW,na.rm = TRUE)
sd_tox_HC_S_E_H_W = sd(Average_time_HC_Tox_S_E_HW,na.rm = TRUE)



############################################################################################################################################
##########---------------------- All
setwd("....")
file_HC = "TX_HC_all-W.csv"
HC_Tox_MF8=read.csv(file_HC, header = T, as.is = T)
Tstart_exp_HC = 25
Tend_exp_HC = Tstart_exp_HC + 5
Tsim_HC = Tend_exp_HC + 3
DIM_HC = 365-79+(Tsim_HC - 2)*365+1
Start_tox_HC = 365-79+(Tstart_exp_HC -2)*365 +1
End_tox_HC = 365-79+(Tend_exp_HC - 2)*365
End_tox_HC_bis = End_tox_HC +1
n_var = 4 
R=nrow(HC_Tox_MF8)
C=ncol(HC_Tox_MF8)
n_replicates = (C - 1)/n_var


i=2

M1_Tox_S_E_H_I = HC_Tox_MF8[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  HC_Tox_MF8[,c]
  M1_Tox_S_E_H_I = cbind(M1_Tox_S_E_H_I,x)
}

Average_time_HC_Tox_S_E_H_I = colMeans(M1_Tox_S_E_H_I[Start_tox_HC:End_tox_HC,],na.rm = TRUE)

# Average_tox_HC_S_E_H_I = mean(Average_time_HC_Tox_S_E_H_I,na.rm = TRUE)
# sd_tox_HC_S_E_H_I = sd(Average_time_HC_Tox_S_E_H_I,na.rm = TRUE)

i=4
M1_Tox_S_E_H_IW = HC_Tox_MF8[,i]
for (j in 1:(n_replicates-1))
{
  c=i+j*n_var
  x =  HC_Tox_MF8[,c]
  M1_Tox_S_E_H_IW = cbind(M1_Tox_S_E_H_IW,x)
}

Average_time_HC_Tox_S_E_H_IW = colMeans(M1_Tox_S_E_H_IW[Start_tox_HC:End_tox_HC,],na.rm = TRUE)

Average_tox_HC_S_E_H_I_W = mean(Average_time_HC_Tox_S_E_H_IW,na.rm = TRUE)
sd_tox_HC_S_E_H_I_W = sd(Average_time_HC_Tox_S_E_H_IW,na.rm = TRUE)

## ratio of replicates

ratio_replicates_HC_E1 = Average_time_HC_Tox_S/Average_time_HC
Average_ratio_replicates_HC_E1 = mean(ratio_replicates_HC_E1,na.rm = TRUE)
Max_ratio_replicates_HC_E1 = max(ratio_replicates_HC_E1, na.rm = TRUE)
Min_ratio_replicates_HC_E1 = min(ratio_replicates_HC_E1, na.rm = TRUE)
std_ratio_replicates_HC_E1 = sd(ratio_replicates_HC_E1,na.rm = TRUE)

ratio_replicates_HC_E12 = Average_time_HC_Tox_S_E/Average_time_HC
Average_ratio_replicates_HC_E12 = mean(ratio_replicates_HC_E12,na.rm = TRUE)
Max_ratio_replicates_HC_E12 = max(ratio_replicates_HC_E12, na.rm = TRUE)
Min_ratio_replicates_HC_E12 = min(ratio_replicates_HC_E12, na.rm = TRUE)
std_ratio_replicates_HC_E12 = sd(ratio_replicates_HC_E12,na.rm = TRUE)


ratio_replicates_HC_E123 = Average_time_HC_Tox_S_E_H/Average_time_HC
Average_ratio_replicates_HC_E123 = mean(ratio_replicates_HC_E123,na.rm = TRUE)
Max_ratio_replicates_HC_E123 = max(ratio_replicates_HC_E123, na.rm = TRUE)
Min_ratio_replicates_HC_E123 = min(ratio_replicates_HC_E123, na.rm = TRUE)
std_ratio_replicates_HC_E123 = sd(ratio_replicates_HC_E123,na.rm = TRUE)


ratio_replicates_HC_E1234 = Average_time_HC_Tox_S_E_H_I/Average_time_HC
Average_ratio_replicates_HC_E1234 = mean(ratio_replicates_HC_E1234,na.rm = TRUE)
Max_ratio_replicates_HC_E1234 = max(ratio_replicates_HC_E1234, na.rm = TRUE)
Min_ratio_replicates_HC_E1234 = min(ratio_replicates_HC_E1234, na.rm = TRUE)
std_ratio_replicates_HC_E1234 = sd(ratio_replicates_HC_E1234, na.rm = TRUE)




#################*******************

ratio_TS_2 = c(Average_ratio_replicates_TS_E1,Average_ratio_replicates_TS_E12,Average_ratio_replicates_TS_E123,Average_ratio_replicates_TS_E1234)
min_ratio_TS_2 = c(Min_ratio_replicates_TS_E1,Min_ratio_replicates_TS_E12,Min_ratio_replicates_TS_E123,Min_ratio_replicates_TS_E1234)
max_ratio_TS_2 = c(Max_ratio_replicates_TS_E1,Max_ratio_replicates_TS_E12,Max_ratio_replicates_TS_E123,Max_ratio_replicates_TS_E1234)
sd_ratio_TS_2 = c(std_ratio_replicates_TS_E1,std_ratio_replicates_TS_E12,std_ratio_replicates_TS_E123,std_ratio_replicates_TS_E1234)

table= cbind((ratio_TS_2),min_ratio_TS_2, max_ratio_TS_2)
# require("xlsx")
write.table(table,sep=", ",
            file = "F:/LAVORO/WB/Projects/Fish_project_Syngenta/PopGuide_decision_steps/Draft/Ratios_TS.csv",
            # col.names = NA,
            row.names=c("E1","E1-E2","E1-E2-E3","ALL"),
            col.names=c("mean","min","max")
)

xaxis = c(1,2,3,4)

ratio_SD_2 = c(Average_ratio_replicates_SD_E1,Average_ratio_replicates_SD_E12,Average_ratio_replicates_SD_E123,Average_ratio_replicates_SD_E1234)
min_ratio_SD_2 = c(Min_ratio_replicates_SD_E1,Min_ratio_replicates_SD_E12,Min_ratio_replicates_SD_E123,Min_ratio_replicates_SD_E1234)
max_ratio_SD_2 = c(Max_ratio_replicates_SD_E1,Max_ratio_replicates_SD_E12,Max_ratio_replicates_SD_E123,Max_ratio_replicates_SD_E1234)
sd_ratio_SD_2 = c(std_ratio_replicates_SD_E1,std_ratio_replicates_SD_E12,std_ratio_replicates_SD_E123,std_ratio_replicates_SD_E1234)


table= cbind((ratio_SD_2),min_ratio_SD_2, max_ratio_SD_2)
# require("xlsx")
write.table(table,sep=", ",
            file = "F:/LAVORO/WB/Projects/Fish_project_Syngenta/PopGuide_decision_steps/Draft/Ratios_SD.csv",
            # col.names = NA,
            row.names=c("E1","E1-E2","E1-E2-E3","ALL"),
            col.names=c("mean","min","max")
)
xaxis = c(1,2,3,4)


ratio_HC_2 = c(Average_ratio_replicates_HC_E1,Average_ratio_replicates_HC_E12,Average_ratio_replicates_HC_E123,Average_ratio_replicates_HC_E1234)
min_ratio_HC_2 = c(Min_ratio_replicates_HC_E1,Min_ratio_replicates_HC_E12,Min_ratio_replicates_HC_E123,Min_ratio_replicates_HC_E1234)
max_ratio_HC_2 = c(Max_ratio_replicates_HC_E1,Max_ratio_replicates_HC_E12,Max_ratio_replicates_HC_E123,Max_ratio_replicates_HC_E1234)
sd_ratio_HC_2 = c(std_ratio_replicates_HC_E1,std_ratio_replicates_HC_E12,std_ratio_replicates_HC_E123,std_ratio_replicates_HC_E1234)



table= cbind((ratio_HC_2),min_ratio_HC_2, max_ratio_HC_2)
# require("xlsx")
write.table(table,sep=", ",
            file = "F:/LAVORO/WB/Projects/Fish_project_Syngenta/PopGuide_decision_steps/Draft/Ratios_HC.csv",
            # col.names = NA,
            row.names=c("E1","E1-E2","E1-E2-E3","ALL"),
            col.names=c("mean","min","max")
)



ratio_DRM_2 = c(Average_ratio_replicates_DRM_E1,Average_ratio_replicates_DRM_E12,Average_ratio_replicates_DRM_E123,Average_ratio_replicates_DRM_E1234)
min_ratio_DRM_2 = c(Min_ratio_replicates_DRM_E1,Min_ratio_replicates_DRM_E12,Min_ratio_replicates_DRM_E123,Min_ratio_replicates_DRM_E1234)
max_ratio_DRM_2 = c(Max_ratio_replicates_DRM_E1,Max_ratio_replicates_DRM_E12,Max_ratio_replicates_DRM_E123,Max_ratio_replicates_DRM_E1234)
sd_ratio_DRM_2 = c(std_ratio_replicates_DRM_E1,std_ratio_replicates_DRM_E12,std_ratio_replicates_DRM_E123,std_ratio_replicates_DRM_E1234)

table= cbind((ratio_DRM_2),min_ratio_DRM_2, max_ratio_DRM_2)
# require("xlsx")
write.table(table,sep=", ",
            file = "F:/LAVORO/WB/Projects/Fish_project_Syngenta/PopGuide_decision_steps/Draft/Ratios_DRM.csv",
            # col.names = NA,
            row.names=c("E1","E1-E2","E1-E2-E3","ALL"),
            col.names=c("mean","min","max")
)


xaxis = c(1,2,3,4)

x11()
pdf("F:/LAVORO/WB/Projects/Fish_project_Syngenta/PopGuide_decision_steps/Draft/FIGURES PAPER/Effects_MEAN_REPLICATES_maxMin.pdf",width=25,height=18,onefile=T,paper='A4r')
par(mar=c(5.1+1.5,5.1+1.5,3.1,2.1),oma=c(2,2,0,0),mai=c(0.4,0.45,0.3,0.3)) # mai specifies the argins in inches - bottom left - up -right
par(mfrow=c(2,2))#,oma = c(0, 0, 2, 0))
plot(xaxis,ratio_TS_2, pch =19, cex = 3, xaxt='n', 
     ylab="",xlab = "", 
     # main="Topeka shiner" , 
     col = c(1,2,3,4), ylim=c(0,1.6),xaxt="n")
arrows(xaxis,min_ratio_TS_2,xaxis,max_ratio_TS_2, code=3, length=0.02,col = c(1,2,3,4), angle = 90)

mtext("(a)", side = 3, adj = 0.05, 
      line = -1.3)

legend(3,1.6, legend=c("E1","E1+E2","E1+E2+E3","E1+E2+E3+E4"),
       col=c(1,2,3,4),  pch=19, cex=0.9)
lines(1:4,c(0.5,0.5,0.5,0.5), lty = 2)


plot(xaxis,ratio_DRM_2, pch =19, cex = 3, xaxt='n', 
     ylab="",xlab = "", 
     # main="Devils River minnow" , 
     col = c(1,2,3,4), ylim=c(0,1.6),xaxt="n")
arrows(xaxis,min_ratio_DRM_2,xaxis,max_ratio_DRM_2, code=3, length=0.02,col = c(1,2,3,4), angle = 90)

mtext("(b)", side = 3, adj = 0.05, 
      line = -1.3)

legend(3,1.6, legend=c("E1","E1+E2","E1+E2+E3","E1+E2+E3+E4"),
       col=c(1,2,3,4),  pch=19, cex=0.9)
lines(1:4,c(0.5,0.5,0.5,0.5), lty = 2)


plot(xaxis,ratio_SD_2, pch =19, cex = 3, xaxt='n', 
     ylab="",xlab = "", 
     # main="Spikedace" , 
     col = c(1,2,3,4), ylim=c(0,1.6),xaxt="n")
arrows(xaxis,min_ratio_SD_2,xaxis,max_ratio_SD_2, code=3, length=0.02,col = c(1,2,3,4), angle = 90)

mtext("(c)", side = 3, adj = 0.05, 
      line = -1.3)


legend(3,1.6, legend=c("E1","E1+E2","E1+E2+E3","E1+E2+E3+E4"),
       col=c(1,2,3,4),  pch=19, cex=0.9)
lines(1:4,c(0.5,0.5,0.5,0.5), lty = 2)


plot(xaxis,ratio_HC_2, pch =19, cex = 3, xaxt='n', 
     ylab="",xlab = "", 
     # main="Humpback chub" , 
     col = c(1,2,3,4), ylim=c(0,1.6),xaxt="n")
arrows(xaxis,min_ratio_HC_2,xaxis,max_ratio_HC_2, code=3, length=0.02,col = c(1,2,3,4), angle = 90)

mtext("(d)", side = 3, adj = 0.05, 
      line = -1.3)

legend(3,1.6, legend=c("E1","E1+E2","E1+E2+E3","E1+E2+E3+E4"),
       col=c(1,2,3,4),  pch=19, cex=0.9)
lines(1:4,c(0.5,0.5,0.5,0.5), lty = 2)


mtext("Effect modules",side=1,line=0,cex=1.3,outer=TRUE)
mtext("Ratio with/without chemical",side=2,line=0,outer=TRUE,cex=1.3,las=0)

dev.off()


# 
# pdf("F:/LAVORO/WB/Projects/Fish_project_Syngenta/PopGuide_decision_steps/Draft/FIGURES PAPER/Effects_MEAN_REPLICATES-sd.pdf",width=25,height=18,onefile=T,paper='A4r')
# par(mar=c(5.1+1.5,5.1+2,3.1,2.1))
# # par(mar=c(1.5,1.5,1.5,1.5))
# # par(cex.axis=2)
# par(mfrow=c(2,2))#,oma = c(0, 0, 2, 0))
# 
# xaxis = c(1,2,3,4)
# # x11()
# plot(xaxis,ratio_TS_2, pch =19, cex = 3, xaxt='n', 
#      ylab="Ratio with/without chemical",xlab = "Effect modules", 
#      main="Topeka shiner" , 
#      col = c(1,2,3,4), ylim=c(0,1.2),xaxt="n")
# arrows(xaxis,ratio_TS_2-sd_ratio_TS_2,xaxis,ratio_TS_2+sd_ratio_TS_2, code=3, length=0.02,col = c(1,2,3,4), angle = 90)
# legend(3,1.2, legend=c("E1","E1+E2","E1+E2+E3","E1+E2+E3+E4"),
#        col=c(1,2,3,4),  pch=19, cex=0.9)
# lines(1:4,c(0.5,0.5,0.5,0.5), lty = 2)
# 
# xaxis = c(1,2,3,4)
# # x11()
# plot(xaxis,ratio_DRM_2, pch =19, cex = 3, xaxt='n', 
#      ylab="Ratio with/without chemical",xlab = "Effect modules", 
#      main="Devils River minnow" , 
#      col = c(1,2,3,4), ylim=c(0,1.2),xaxt="n")
# arrows(xaxis,ratio_DRM_2-sd_ratio_DRM_2,xaxis,ratio_DRM_2+sd_ratio_DRM_2, code=3, length=0.02,col = c(1,2,3,4), angle = 90)
# legend(3,1.2, legend=c("E1","E1+E2","E1+E2+E3","E1+E2+E3+E4"),
#        col=c(1,2,3,4),  pch=19, cex=0.9)
# lines(1:4,c(0.5,0.5,0.5,0.5), lty = 2)
# 
# 
# xaxis = c(1,2,3,4)
# # x11()
# plot(xaxis,ratio_SD_2, pch =19, cex = 3, xaxt='n', 
#      ylab="Ratio with/without chemical",xlab = "Effect modules", 
#      main="Spikedace" , 
#      col = c(1,2,3,4), ylim=c(0,1.2),xaxt="n")
# arrows(xaxis,ratio_SD_2-sd_ratio_SD_2,xaxis,ratio_SD_2+sd_ratio_SD_2, code=3, length=0.02,col = c(1,2,3,4), angle = 90)
# legend(3,1.2, legend=c("E1","E1+E2","E1+E2+E3","E1+E2+E3+E4"),
#        col=c(1,2,3,4),  pch=19, cex=0.9)
# lines(1:4,c(0.5,0.5,0.5,0.5), lty = 2)
# 
# xaxis = c(1,2,3,4)
# # x11()
# plot(xaxis,ratio_HC_2, pch =19, cex = 3, xaxt='n', 
#      ylab="Ratio with/without chemical",xlab = "Effect modules", 
#      main="Humpback chub" , 
#      col = c(1,2,3,4), ylim=c(0,1.2),xaxt="n")
# arrows(xaxis,ratio_HC_2-sd_ratio_HC_2,xaxis,ratio_HC_2+sd_ratio_HC_2, code=3, length=0.02,col = c(1,2,3,4), angle = 90)
# legend(3,1.2, legend=c("E1","E1+E2","E1+E2+E3","E1+E2+E3+E4"),
#        col=c(1,2,3,4),  pch=19, cex=0.9)
# lines(1:4,c(0.5,0.5,0.5,0.5), lty = 2)
# 
# dev.off()


# #### -------------------------- comparison among MF ------------------------------

# #### BOXPLOT W BIOMASS
# 

df_TS<-data.frame(mean=c(Average_no_tox_TS_W, Average_tox_TS_S_W,Average_tox_TS_S_E_W,Average_tox_TS_S_E_H_W,Average_tox_TS_S_E_H_I_W),
                  sd=c(sd_no_tox_TS_W,sd_tox_TS_S_W,sd_tox_TS_S_E_W,sd_tox_TS_S_E_H_W,sd_tox_TS_S_E_H_I_W),
                  Effect=as.factor(c("No effects","E1","E1+E2","E1+E2+E3","E1+E2+E3+E4")))


df_TS$Effect<- factor(df_TS$Effect, levels=c("No effects","E1","E1+E2","E1+E2+E3","E1+E2+E3+E4"))

df_TS$name <- factor(df_TS$Effect, levels = df_TS$Effect)

df_DRM<-data.frame(mean=c(Average_no_tox_DRM_W, Average_no_tox_DRM_E1_W,Average_no_tox_DRM_E12_W,Average_no_tox_DRM_E123_W,Average_no_tox_DRM_E1234_W),
                   sd=c(sd_no_tox_DRM_W,sd_no_tox_DRM_E1_W,sd_no_tox_DRM_E12_W,sd_no_tox_DRM_E123_W,sd_no_tox_DRM_E1234_W),
                   Effect=as.factor(c("No effects","E1","E1+E2","E1+E2+E3","E1+E2+E3+E4")))


df_DRM$Effect<- factor(df_DRM$Effect, levels=c("No effects","E1","E1+E2","E1+E2+E3","E1+E2+E3+E4"))

df_DRM$name <- factor(df_DRM$Effect, levels = df_DRM$Effect)

# 
# # X11()
df_SD<-data.frame(mean=c(Average_no_tox_SD_W, Average_tox_SD_S_W,Average_tox_SD_S_E_W,Average_tox_SD_S_E_H_W,Average_tox_SD_S_E_H_I_W),
                  sd=c(sd_no_tox_SD_W,sd_tox_SD_S_W,sd_tox_SD_S_E_W,sd_tox_SD_S_E_H_W,sd_tox_SD_S_E_H_I_W),
                  Effect=as.factor(c("No effects","E1","E1+E2","E1+E2+E3","E1+E2+E3+E4")))


df_SD$Effect<- factor(df_SD$Effect, levels=c("No effects","E1","E1+E2","E1+E2+E3","E1+E2+E3+E4"))

df_SD$name <- factor(df_SD$Effect, levels = df_SD$Effect)
# 
# 

df_HC<-data.frame(mean=c(Average_no_tox_HC_W, Average_tox_HC_S_W,Average_tox_HC_S_E_W,Average_tox_HC_S_E_H_W,Average_tox_HC_S_E_H_I_W),
                  sd=c(sd_no_tox_HC_W,sd_tox_HC_S_W,sd_tox_HC_S_E_W,sd_tox_HC_S_E_H_W,sd_tox_HC_S_E_H_I_W),
                  Effect=as.factor(c("No effects","E1","E1+E2","E1+E2+E3","E1+E2+E3+E4")))
df_HC$Effect<- factor(df_HC$Effect, levels=c("No effects","E1","E1+E2","E1+E2+E3","E1+E2+E3+E4"))

df_HC$name <- factor(df_HC$Effect, levels = df_HC$Effect)

# # X11()
# 

p_TS= ggplot(df_TS,aes(x=name))+geom_boxplot(color=c(5,1,2,3,4), fill=c(5,1,2,3,4),
                                             alpha=0.2,aes(lower=mean-sd,upper=mean+sd,middle=mean,ymin=mean-3*sd,ymax=mean+3*sd),stat="identity")
p_TS + ggtitle("Average pop biom over 5 years ")


X11()
p_DRM= ggplot(df_DRM,aes(x=name))+geom_boxplot(color=c(5,1,2,3,4), fill=c(5,1,2,3,4),
                                               alpha=0.2,aes(lower=mean-sd,upper=mean+sd,middle=mean,ymin=mean-3*sd,ymax=mean+3*sd),stat="identity")
p_DRM + ggtitle("Average pop iom over 5 years 40 replicates")


X11()
p_SD= ggplot(df_SD,aes(x=name))+geom_boxplot(color=c(5,1,2,3,4), fill=c(5,1,2,3,4),
                                             alpha=0.2,aes(lower=mean-sd,upper=mean+sd,middle=mean,ymin=mean-3*sd,ymax=mean+3*sd),stat="identity")
p_SD + ggtitle("Average pop biom over 5 years ")

# X11()

X11()
p_HC= ggplot(df_HC,aes(x=name))+geom_boxplot(color=c(5,1,2,3,4), fill=c(5,1,2,3,4),
                                             alpha=0.2,aes(lower=mean-sd,upper=mean+sd,middle=mean,ymin=mean-3*sd,ymax=mean+3*sd),stat="identity")
p_HC + ggtitle("Average pop biom over 5 years ")  + theme(legend.position = "none")

X11()
pdf("F:/LAVORO/WB/Projects/Fish_project_Syngenta/PopGuide_decision_steps/Draft/FIGURES PAPER/Effects_tot_pop_W-ratio-repl.pdf",width=25,height=18,onefile=T,paper='A4r')
par(mar=c(5.1+1.5,5.1+2,3.1,2.1))
# par(mar=c(1.5,1.5,1.5,1.5))
# par(cex.axis=2)
par(mfrow=c(2,2))#,oma = c(0, 0, 2, 0))
figure <- ggarrange(p_TS+ theme(axis.title.x = element_blank()), p_DRM+ theme(axis.title.x = element_blank()),
                    p_SD+ theme(axis.title.x = element_blank()), p_HC+ theme(axis.title.x = element_blank()),
                    labels = c("(a)", "(b)", "(c)", "(d)"),hjust= -2,
                    ncol = 2, nrow = 2)
annotate_figure(figure,
                left = text_grob("Biomass [g]", rot = 90),# vjust = 1, gp = gpar(cex = 1.3)),
                top = text_grob("", color = "black", face = "bold", size = 14))

dev.off()


# #### BOXPLOT
df_TS<-data.frame(mean=c(Average_no_tox_TS, Average_tox_TS_S,Average_tox_TS_S_E,Average_tox_TS_S_E_H,Average_tox_TS_S_E_H_I),
                  sd=c(sd_no_tox_TS,sd_tox_TS_S,sd_tox_TS_S_E,sd_tox_TS_S_E_H,sd_tox_TS_S_E_H_I),
                  Effect=as.factor(c("No effects","E1","E1+E2","E1+E2+E3","E1+E2+E3+E4")))


df_TS$Effect<- factor(df_TS$Effect, levels=c("No effects","E1","E1+E2","E1+E2+E3","E1+E2+E3+E4"))

df_TS$name <- factor(df_TS$Effect, levels = df_TS$Effect)

df_DRM<-data.frame(mean=c(Average_no_tox_DRM, Average_no_tox_DRM_E1,Average_no_tox_DRM_E12,Average_no_tox_DRM_E123,Average_no_tox_DRM_E1234),
                   sd=c(sd_no_tox_DRM,sd_no_tox_DRM_E1,sd_no_tox_DRM_E12,sd_no_tox_DRM_E123,sd_no_tox_DRM_E1234),
                   Effect=as.factor(c("No effects","E1","E1+E2","E1+E2+E3","E1+E2+E3+E4")))


df_DRM$Effect<- factor(df_DRM$Effect, levels=c("No effects","E1","E1+E2","E1+E2+E3","E1+E2+E3+E4"))

df_DRM$name <- factor(df_DRM$Effect, levels = df_DRM$Effect)

# 
# # X11()
df_SD<-data.frame(mean=c(Average_no_tox_SD, Average_tox_SD_S,Average_tox_SD_S_E,Average_tox_SD_S_E_H,Average_tox_SD_S_E_H_I),
                  sd=c(sd_no_tox_SD,sd_tox_SD_S,sd_tox_SD_S_E,sd_tox_SD_S_E_H,sd_tox_SD_S_E_H_I),
                  Effect=as.factor(c("No effects","E1","E1+E2","E1+E2+E3","E1+E2+E3+E4")))


df_SD$Effect<- factor(df_SD$Effect, levels=c("No effects","E1","E1+E2","E1+E2+E3","E1+E2+E3+E4"))

df_SD$name <- factor(df_SD$Effect, levels = df_SD$Effect)
# 
# 

df_HC<-data.frame(mean=c(Average_no_tox_HC, Average_tox_HC_S,Average_tox_HC_S_E,Average_tox_HC_S_E_H,Average_tox_HC_S_E_H_I),
                  sd=c(sd_no_tox_HC,sd_tox_HC_S,sd_tox_HC_S_E,sd_tox_HC_S_E_H,sd_tox_HC_S_E_H_I),
                  Effect=as.factor(c("No effects","E1","E1+E2","E1+E2+E3","E1+E2+E3+E4")))
df_HC$Effect<- factor(df_HC$Effect, levels=c("No effects","E1","E1+E2","E1+E2+E3","E1+E2+E3+E4"))

df_HC$name <- factor(df_HC$Effect, levels = df_HC$Effect)

# # X11()
# 
X11()
p_TS= ggplot(df_TS,aes(x=name))+geom_boxplot(color=c(1,2,3,4,5), fill=c(1,2,3,4,5),
                                             alpha=0.2,aes(lower=mean-sd,upper=mean+sd,middle=mean,ymin=mean-3*sd,ymax=mean+3*sd),stat="identity")
p_TS + ggtitle("Average pop abundances over 5 years ")


X11()
p_DRM= ggplot(df_DRM,aes(x=name))+geom_boxplot(color=c(1,2,3,4,5), fill=c(1,2,3,4,5),
                                               alpha=0.2,aes(lower=mean-sd,upper=mean+sd,middle=mean,ymin=mean-3*sd,ymax=mean+3*sd),stat="identity")
p_DRM + ggtitle("Average pop abundances over 5 years 40 replicates")


X11()
p_SD= ggplot(df_SD,aes(x=name))+geom_boxplot(color=c(1,2,3,4,5), fill=c(1,2,3,4,5),
                                             alpha=0.2,aes(lower=mean-sd,upper=mean+sd,middle=mean,ymin=mean-3*sd,ymax=mean+3*sd),stat="identity")
p_SD + ggtitle("Average pop abundances over 5 years ")

# X11()

X11()
p_HC= ggplot(df_HC,aes(x=name))+geom_boxplot(color=c(1,2,3,4,5), fill=c(1,2,3,4,5),
                                             alpha=0.2,aes(lower=mean-sd,upper=mean+sd,middle=mean,ymin=mean-3*sd,ymax=mean+3*sd),stat="identity")
p_HC + ggtitle("Average pop abundances over 5 years ")  + theme(legend.position = "none")

X11()
# pdf("F:/LAVORO/WB/Projects/Fish_project_Syngenta/PopGuide_decision_steps/Draft/FIGURES PAPER/Effects_tot_pop.pdf",width=25,height=18,onefile=T,paper='A4r')
par(mar=c(5.1+1.5,5.1+2,3.1,2.1))
# par(mar=c(1.5,1.5,1.5,1.5))
# par(cex.axis=2)
par(mfrow=c(2,2))#,oma = c(0, 0, 2, 0))
figure <- ggarrange(p_TS+ theme(axis.title.x = element_blank()), p_DRM+ theme(axis.title.x = element_blank()),
                    p_SD+ theme(axis.title.x = element_blank()), p_HC+ theme(axis.title.x = element_blank()),
                    labels = c("Topeka shiner", "Devils River minnow", "Spikedace", "Humpback chub"),
                    ncol = 2, nrow = 2)
annotate_figure(figure,
                top = text_grob("Average population abundance", color = "black", face = "bold", size = 14))
# figure
# dev.off()










####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################

###################### ********************************** ##########################################################################
####################### **********************************  ********************************** #####################################
##################                      POPULATION DYNAMICS



##### TS
setwd("....")
file_TS_noTox = "TX_TS-W.csv"
TS_noTox=read.csv(file_TS_noTox, header = T, as.is = T)

Tstart_exp_TS = 15
Tend_exp_TS = Tstart_exp_TS + 5
Tsim_TS = Tend_exp_TS + 3
DIM_TS = 365-149+(Tsim_TS - 2)*365+1
Start_tox_TS = 365-149+(Tstart_exp_TS -2)*365 +1
End_tox_TS = 365-149+(Tend_exp_TS - 2)*365 
End_tox_TS_bis=End_tox_TS  + 1 
n_var =4

R=nrow(TS_noTox)
C=ncol(TS_noTox)
n_replicates = (C - 1)/n_var
Tot <- matrix(, nrow = R, ncol = 1)
Tot_sd <- matrix(, nrow = R, ncol = 1)

i=2
while ( i<= n_var+1)
{
  M1=TS_noTox[,i]
  for (j in 1:(n_replicates-1))
  {
    c=i+j*n_var
    x =  TS_noTox[,c]
    M1=cbind(M1,x)
  }
  Average=rowMeans(M1,na.rm = TRUE)
  sd_tot=rowSds(M1,na.rm = TRUE)
  Tot=cbind( Tot, Average)
  Tot_sd = cbind(Tot_sd, sd_tot)
  i=i+1
  i
}
colTot=ncol(Tot)
Tot = Tot[,2:colTot]   
Tot_sd = Tot_sd[,2:colTot] 

Average_no_tox_TS = mean(Tot[Start_tox_TS:End_tox_TS,1],na.rm = TRUE)
sd_no_tox_TS = sd(Tot[Start_tox_TS:End_tox_TS,1],na.rm = TRUE)
Average_no_tox_END_TS = mean(Tot[End_tox_TS_bis:DIM_TS,1],na.rm = TRUE)
sd_no_tox_END_TS = sd(Tot[End_tox_TS_bis:DIM_TS,1],na.rm = TRUE)

Average_no_tox_TSa = mean(Tot[Start_tox_TS:End_tox_TS,2],na.rm = TRUE)
sd_no_tox_TSa = sd(Tot[Start_tox_TS:End_tox_TS,2],na.rm = TRUE)
Average_no_tox_END_TSa = mean(Tot[End_tox_TS_bis:DIM_TS,2],na.rm = TRUE)
sd_no_tox_END_TSa = sd(Tot[End_tox_TS_bis:DIM_TS,2],na.rm = TRUE)


Average_no_tox_TS_W = mean(Tot[Start_tox_TS:End_tox_TS,3],na.rm = TRUE)
sd_no_tox_TS_W = sd(Tot[Start_tox_TS:End_tox_TS,3],na.rm = TRUE)
Average_no_tox_END_TS_W = mean(Tot[End_tox_TS_bis:DIM_TS,3],na.rm = TRUE)
sd_no_tox_END_TS_W = sd(Tot[End_tox_TS_bis:DIM_TS,3],na.rm = TRUE)

Average_no_tox_TS_Wa = mean(Tot[Start_tox_TS:End_tox_TS,4],na.rm = TRUE)
sd_no_tox_TS_Wa = sd(Tot[Start_tox_TS:End_tox_TS,4],na.rm = TRUE)
Average_no_tox_END_TS_Wa = mean(Tot[End_tox_TS_bis:DIM_TS,4],na.rm = TRUE)
sd_no_tox_END_TS_Wa = sd(Tot[End_tox_TS_bis:DIM_TS,4],na.rm = TRUE)

Tot_TS = Tot
Tot_sd_TS = Tot_sd
rowTot_TS=nrow(Tot_TS)
time_TS = 365-148




############## DRM
setwd("....")
file_DRM = "Cyprinidae_TX_DRM DRM-table.csv"
DRM_table=read.csv(file_DRM, header=F,stringsAsFactors=FALSE)
NAMES <- read.csv(file_DRM, nrow = 6, sep = ";")
DRM_table <- read.csv(file_DRM, skip = 6, stringsAsFactors = FALSE, sep = ",")
words <- strsplit(NAMES[6,1], ",")[[1]]
xx=unique(tolower(words))

names(DRM_table) <- xx


Tstart_exp_DRM = 20
Tend_exp_DRM = Tstart_exp_DRM + 5
Tsim_DRM = Tend_exp_DRM + 3
DIM_DRM = 364*24+365*(Tsim_DRM - 2)*24+3
Start_tox_DRM = 364*24+365*(Tstart_exp_DRM -2)*24 +3
End_tox_DRM = 364*24+365*(Tend_exp_DRM - 2)*24 +2
End_tox_DRM_bis = End_tox_DRM + 1 
n_var = 4

R=nrow(DRM_table)

DRM_matrix <- matrix(, nrow = R, ncol = 1)
n_replicates = 40 

for (i in 1:n_replicates)
{
  dataframe = DRM_table[DRM_table[, "[run number]"] == i,]
  assign(paste("Matrix",i, sep=""), select(dataframe, 
                                           c("count turtles with [u_h > u_h^b]","count turtles with [u_h > u_h^p]","sum [weight] of turtles with [u_h > u_h^b]","w-turtles")))
}

for (i in 1:n_replicates)
{
  x= nrow(get(paste("Matrix",i, sep="")))
  if(x != DIM_DRM)
  {print (i)
    print (x)}
}


DRM_m = Matrix1

for (i in 2:n_replicates)
{
  
  DRM_m = cbind (DRM_m,get(paste("Matrix",i, sep="")))
} 
colTot=ncol(DRM_m)


col.names = rep(c("Tot pop","Adult pop","Tot weight","Adult weight"),n_replicates)
colnames(DRM_m) <-col.names


DRM_mr <- as.data.frame( # sapply returns a list here, so we convert it to a data.frame
  sapply(unique(names(DRM_m)), # for each unique column name
         function(col) rowMeans(DRM_m[names(DRM_m) == col]) # calculate row means
  )
)

DRM_mr_sd <- as.data.frame( # sapply returns a list here, so we convert it to a data.frame
  sapply(unique(names(DRM_m)), # for each unique column name
         function(col) rowSds(as.matrix(DRM_m[names(DRM_m) == col])) # calculate row means
  )
)


##################

Average_no_tox_DRM = mean(DRM_mr[Start_tox_DRM:End_tox_DRM,1],na.rm = TRUE)
sd_no_tox_DRM = sd(DRM_mr[Start_tox_DRM:End_tox_DRM,1],na.rm = TRUE)
Average_no_tox_END_DRM = mean(DRM_mr[End_tox_DRM_bis:DIM_DRM,1],na.rm = TRUE)
sd_no_tox_END_DRM = sd(DRM_mr[End_tox_DRM_bis:DIM_DRM,1],na.rm = TRUE)


Average_no_tox_DRMa = mean(DRM_mr[Start_tox_DRM:End_tox_DRM,2],na.rm = TRUE)
sd_no_tox_DRMa = sd(DRM_mr[Start_tox_DRM:End_tox_DRM,2],na.rm = TRUE)
Average_no_tox_END_DRMa = mean(DRM_mr[End_tox_DRM_bis:DIM_DRM,2],na.rm = TRUE)
sd_no_tox_END_DRMa = sd(DRM_mr[End_tox_DRM_bis:DIM_DRM,2],na.rm = TRUE)

Average_no_tox_DRM_W = mean(DRM_mr[Start_tox_DRM:End_tox_DRM,3],na.rm = TRUE)
sd_no_tox_DRM_W = sd(DRM_mr[Start_tox_DRM:End_tox_DRM,3],na.rm = TRUE)
Average_no_tox_END_DRM_W = mean(DRM_mr[End_tox_DRM_bis:DIM_DRM,3],na.rm = TRUE)
sd_no_tox_END_DRM_W = sd(DRM_mr[End_tox_DRM_bis:DIM_DRM,3],na.rm = TRUE)

Average_no_tox_DRM_Wa = mean(DRM_mr[Start_tox_DRM:End_tox_DRM,4],na.rm = TRUE)
sd_no_tox_DRM_Wa = sd(DRM_mr[Start_tox_DRM:End_tox_DRM,4],na.rm = TRUE)
Average_no_tox_END_DRM_Wa = mean(DRM_mr[End_tox_DRM_bis:DIM_DRM,4],na.rm = TRUE)
sd_no_tox_END_DRM_Wa = sd(DRM_mr[End_tox_DRM_bis:DIM_DRM,4],na.rm = TRUE)

Tot_DRM = DRM_mr
Tot_sd_DRM = DRM_mr_sd
rowTot_DRM=nrow(Tot_DRM)
time_DRM = 1



########## SD
setwd("....")
file_SD_noTox = "TX_SD-W.csv"

SD_noTox=read.csv(file_SD_noTox, header = T, as.is = T)
Tstart_exp_SD = 15
Tend_exp_SD = Tstart_exp_SD + 5
Tsim_SD = Tend_exp_SD + 3
DIM_SD = 365-89+(Tsim_SD - 2)*365+1
Start_tox_SD = 365-89+(Tstart_exp_SD -2)*365 +1
End_tox_SD = 365-89+(Tend_exp_SD - 2)*365
End_tox_SD_bis = End_tox_SD +1

n_var =4
R=nrow(SD_noTox)
C=ncol(SD_noTox)
n_replicates = (C - 1)/n_var
Tot <- matrix(, nrow = R, ncol = 1)
Tot_sd <- matrix(, nrow = R, ncol = 1)

i=2
while ( i<= n_var+1)
{
  M1=SD_noTox[,i]
  for (j in 1:(n_replicates-1))
  {
    c=i+j*n_var
    x =  SD_noTox[,c]
    M1=cbind(M1,x)
  }
  Average=rowMeans(M1,na.rm = TRUE)
  sd_tot=rowSds(M1,na.rm = TRUE)
  Tot=cbind( Tot, Average)
  Tot_sd = cbind(Tot_sd, sd_tot)
  i=i+1
  i
}
colTot=ncol(Tot)
Tot = Tot[,2:colTot]   
Tot_sd = Tot_sd[,2:colTot] 

Average_no_tox_SD = mean(Tot[Start_tox_SD:End_tox_SD,1],na.rm = TRUE)
sd_no_tox_SD = sd(Tot[Start_tox_SD:End_tox_SD,1],na.rm = TRUE)
Average_no_tox_END_SD = mean(Tot[End_tox_SD_bis:DIM_SD,1],na.rm = TRUE)
sd_no_tox_END_SD = sd(Tot[End_tox_SD_bis:DIM_SD,1],na.rm = TRUE)

Average_no_tox_SDa = mean(Tot[Start_tox_SD:End_tox_SD,2],na.rm = TRUE)
sd_no_tox_SDa = sd(Tot[Start_tox_SD:End_tox_SD,2],na.rm = TRUE)
Average_no_tox_END_SDa = mean(Tot[End_tox_SD_bis:DIM_SD,2],na.rm = TRUE)
sd_no_tox_END_SDa = sd(Tot[End_tox_SD_bis:DIM_SD,2],na.rm = TRUE)

Average_no_tox_SD_W = mean(Tot[Start_tox_SD:End_tox_SD,3],na.rm = TRUE)
sd_no_tox_SD_W = sd(Tot[Start_tox_SD:End_tox_SD,3],na.rm = TRUE)
Average_no_tox_END_SD_W = mean(Tot[End_tox_SD_bis:DIM_SD,3],na.rm = TRUE)
sd_no_tox_END_SD_W = sd(Tot[End_tox_SD_bis:DIM_SD,3],na.rm = TRUE)

Average_no_tox_SD_Wa = mean(Tot[Start_tox_SD:End_tox_SD,4],na.rm = TRUE)
sd_no_tox_SD_Wa = sd(Tot[Start_tox_SD:End_tox_SD,4],na.rm = TRUE)
Average_no_tox_END_SD_Wa = mean(Tot[End_tox_SD_bis:DIM_SD,4],na.rm = TRUE)
sd_no_tox_END_SD_Wa = sd(Tot[End_tox_SD_bis:DIM_SD,4],na.rm = TRUE)

Tot_SD = Tot
Tot_sd_SD = Tot_sd
rowTot_SD=nrow(Tot_SD)
time_SD = 365-88



############# HC
setwd("....")
file_HC_noTox = "TX_HC-W.csv"

HC_noTox=read.csv(file_HC_noTox, header = T, as.is = T)
Tstart_exp_HC = 25
Tend_exp_HC = Tstart_exp_HC + 5
Tsim_HC = Tend_exp_HC + 3
DIM_HC = 365-79+(Tsim_HC - 2)*365+1
Start_tox_HC = 365-79+(Tstart_exp_HC -2)*365 +1
End_tox_HC = 365-79+(Tend_exp_HC - 2)*365
End_tox_HC_bis = End_tox_HC +1
n_var =4

R=nrow(HC_noTox)
C=ncol(HC_noTox)
n_replicates = (C - 1)/n_var

Tot <- matrix(, nrow = R, ncol = 1)
Tot_sd <- matrix(, nrow = R, ncol = 1)

i=2
while ( i<= n_var+1)
{
  M1=HC_noTox[,i]
  for (j in 1:(n_replicates-1))
  {
    c=i+j*n_var
    x =  HC_noTox[,c]
    M1=cbind(M1,x)
  }
  Average=rowMeans(M1,na.rm = TRUE)
  sd_tot=rowSds(M1,na.rm = TRUE)
  Tot=cbind( Tot, Average)
  Tot_sd = cbind(Tot_sd, sd_tot)
  i=i+1
  i
}
colTot=ncol(Tot)
Tot = Tot[,2:colTot]   
Tot_sd = Tot_sd[,2:colTot] 

Average_no_tox_HC = mean(Tot[Start_tox_HC:End_tox_HC,1],na.rm = TRUE)
sd_no_tox_HC = sd(Tot[Start_tox_HC:End_tox_HC,1],na.rm = TRUE)
Average_no_tox_END_HC = mean(Tot[End_tox_HC_bis:DIM_HC,1],na.rm = TRUE)
sd_no_tox_END_HC = sd(Tot[End_tox_HC_bis:DIM_HC,1],na.rm = TRUE)


Average_no_tox_HC_W = mean(Tot[Start_tox_HC:End_tox_HC,3],na.rm = TRUE)
sd_no_tox_HC_W = sd(Tot[Start_tox_HC:End_tox_HC,3],na.rm = TRUE)
Average_no_tox_END_HC_W = mean(Tot[End_tox_HC_bis:DIM_HC,3],na.rm = TRUE)
sd_no_tox_END_HC_W = sd(Tot[End_tox_HC_bis:DIM_HC,3],na.rm = TRUE)


#adults

Average_no_tox_HCa = mean(Tot[Start_tox_HC:End_tox_HC,2],na.rm = TRUE)
sd_no_tox_HCa = sd(Tot[Start_tox_HC:End_tox_HC,2],na.rm = TRUE)
Average_no_tox_END_HCa = mean(Tot[End_tox_HC_bis:DIM_HC,2],na.rm = TRUE)
sd_no_tox_END_HCa = sd(Tot[End_tox_HC_bis:DIM_HC,2],na.rm = TRUE)

Average_no_tox_HC_Wa = mean(Tot[Start_tox_HC:End_tox_HC,4],na.rm = TRUE)
sd_no_tox_HC_Wa = sd(Tot[Start_tox_HC:End_tox_HC,4],na.rm = TRUE)
Average_no_tox_END_HC_Wa = mean(Tot[End_tox_HC_bis:DIM_HC,4],na.rm = TRUE)
sd_no_tox_END_HC_Wa = sd(Tot[End_tox_HC_bis:DIM_HC,4],na.rm = TRUE)


Average_no_tox_HC_first = mean(Tot[1:Start_tox_HC,1],na.rm = TRUE)
sd_no_tox_HC_first = sd(Tot[1:Start_tox_HC,1],na.rm = TRUE)

Tot_HC = Tot
Tot_sd_HC = Tot_sd
rowTot_HC=nrow(Tot_HC)
time_HC = 365-78



##### fig
yy = 0:3000
x11()
pdf("F:/LAVORO/WB/Projects/Fish_project_Syngenta/PopGuide_decision_steps/Draft/FIGURES PAPER/Pop_dynamics.pdf",width=25,height=18,onefile=T,paper='A4r')
par(mar=c(5.1+1.5,5.1+1.5,3.1,2.1),oma=c(2,2,0,0),mai=c(0.4,0.45,0.3,0.3)) # mai specifies the argins in inches - bottom left - up -right
par(mfrow=c(2,2))

x =1:rowTot_TS

plot(Tot_TS[,1], col ="black", type = "l",lwd = 2,xlim=c(0,rowTot_TS), ylim=c(0,3000), #main= "Topeka shiner ", 
     ylab = "", xlab = "",xaxt="n")
lablist<-as.vector(c(2:Tsim_TS))
lablist<-as.vector(c(2,4,6,8,10,12,14,16,18,20,22))
axis(1, at=seq(time_TS, rowTot_TS, by=365), labels = FALSE)
text(seq(time_TS, rowTot_TS, by=2*365),  par("usr")[3] ,labels = lablist, pos = 1, xpd = TRUE)

mtext("(a)", side = 3, adj = 0.05, 
      line = -1.3)

lines(Tot_TS[,1]+Tot_sd_TS[,1] , lwd=1, lty=2)
lines(Tot_TS[,1]-Tot_sd_TS[,1] , lwd=1, lty=2)
xx = seq(time_TS, rowTot_TS, by=365)
lines(rep(xx[14],3001),yy,lwd=2, lty=2, col=2)

x =1:rowTot_DRM

plot(Tot_DRM[,1], col ="black", type = "l",lwd = 2,xlim=c(0,rowTot_DRM), ylim=c(0,3000),# main= "Devils River minnow ",
     ylab = "", xlab = "",xaxt="n")
lablist<-as.vector(c(1:(Tsim_DRM-1)))
lablist<-as.vector(c(1,3,5,7,9,11,13,15,17,19,21,23,25,27))
axis(1, at=seq(time_DRM, rowTot_DRM, by=365*24), labels = FALSE)
text(seq(time_DRM, rowTot_DRM, by=2*365*24),  par("usr")[3] ,labels = lablist, pos = 1, xpd = TRUE)

mtext("(b)", side = 3, adj = 0.05, 
      line = -1.3)

lines(Tot_DRM[,1]+Tot_sd_DRM[,1] , lwd=1, lty=2)
lines(Tot_DRM[,1]-Tot_sd_DRM[,1] , lwd=1, lty=2)
xx = seq(time_DRM, rowTot_DRM, by=365*24)
lines(rep(xx[20],3001),yy,lwd=2, lty=2, col=2)

x =1:rowTot_SD

plot(Tot_SD[,1], col = "black" , type = "l",lwd = 2,xlim=c(0,rowTot_SD), ylim=c(0,3000), #main= "Spikedace", 
     ylab = "", xlab = "",xaxt="n")
lablist<-as.vector(c(2:Tsim_SD))
lablist<-as.vector(c(2,4,6,8,10,12,14,16,18,20,22))
axis(1, at=seq(time_SD, rowTot_SD, by=365), labels = FALSE)
text(seq(time_SD, rowTot_SD, by=2*365),  par("usr")[3] ,labels = lablist, pos = 1, xpd = TRUE)

mtext("(c)", side = 3, adj = 0.05, 
      line = -1.3)

lines(Tot_SD[,1]+Tot_sd_SD[,1] , lwd=1, lty=2)
lines(Tot_SD[,1]-Tot_sd_SD[,1] , lwd=1, lty=2)
xx = seq(time_SD, rowTot_SD, by=365)
lines(rep(xx[14],3001),yy,lwd=2, lty=2, col=2)

x =1:rowTot_HC

plot(Tot_HC[,1], col ="black", type = "l",lwd = 2,xlim=c(0,rowTot_HC), ylim=c(0,3000), #main= "Humpback chub",
     ylab = "", xlab = "",xaxt="n")
par(new=T)
lablist<-as.vector(c(2:Tsim_HC))
lablist<-as.vector(c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32))
axis(1, at=seq(time_HC, rowTot_HC, by=365), labels = FALSE)
text(seq(time_HC, rowTot_HC, by=2*365),  par("usr")[3] ,labels = lablist, pos = 1, xpd = TRUE)

mtext("(d)", side = 3, adj = 0.05, 
      line = -1.3)

lines(Tot_HC[,1]+Tot_sd_HC[,1] , lwd=1, lty=2)
lines(Tot_HC[,1]-Tot_sd_HC[,1] , lwd=1, lty=2)
xx = seq(time_HC, rowTot_HC, by=365)
lines(rep(xx[24],3001),yy,lwd=2, lty=2, col=2)

mtext("Time [d]",side=1,line=0,cex=1.3,outer=TRUE)
mtext("Total population [#]",side=2,line=0,outer=TRUE,cex=1.3,las=0)

dev.off()






####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################

###################### ********************************** ##########################################################################
####################### **********************************  ********************************** #####################################
##################                      EFFECTS OF STOCHASTIC DROUGHTS

library(matrixStats)
library(dplyr)
library(ggplot2)


##### TS
setwd("....")
file_TS_noTox = "TS.csv"
TS_noTox=read.csv(file_TS_noTox, header = T, as.is = T)
Tstart_exp_TS = 15
Tend_exp_TS = Tstart_exp_TS + 5
Tsim_TS = Tend_exp_TS + 3
DIM_TS = 365-149+(Tsim_TS - 2)*365+1
Start_tox_TS = 365-149+(Tstart_exp_TS -2)*365 +1
End_tox_TS = 365-149+(Tend_exp_TS - 2)*365 
End_tox_TS_bis=End_tox_TS  + 1 
n_var =4

R=nrow(TS_noTox)
C=ncol(TS_noTox)
n_replicates = (C - 1)/n_var
Tot <- matrix(, nrow = R, ncol = 1)
Tot_sd <- matrix(, nrow = R, ncol = 1)

i=2
while ( i<= n_var+1)
{
  M1=TS_noTox[,i]
  for (j in 1:(n_replicates-1))
  {
    c=i+j*n_var
    x =  TS_noTox[,c]
    M1=cbind(M1,x)
  }
  Average=rowMeans(M1,na.rm = TRUE)
  sd_tot=rowSds(M1,na.rm = TRUE)
  Tot=cbind( Tot, Average)
  Tot_sd = cbind(Tot_sd, sd_tot)
  i=i+1
  i
}
colTot=ncol(Tot)
Tot = Tot[,2:colTot]   
Tot_sd = Tot_sd[,2:colTot] 

Average_no_tox_TS = mean(Tot[Start_tox_TS:End_tox_TS,1],na.rm = TRUE)
sd_no_tox_TS = sd(Tot[Start_tox_TS:End_tox_TS,1],na.rm = TRUE)
Average_no_tox_END_TS = mean(Tot[End_tox_TS_bis:DIM_TS,1],na.rm = TRUE)
sd_no_tox_END_TS = sd(Tot[End_tox_TS_bis:DIM_TS,1],na.rm = TRUE)

Average_no_tox_TSa = mean(Tot[Start_tox_TS:End_tox_TS,2],na.rm = TRUE)
sd_no_tox_TSa = sd(Tot[Start_tox_TS:End_tox_TS,2],na.rm = TRUE)
Average_no_tox_END_TSa = mean(Tot[End_tox_TS_bis:DIM_TS,2],na.rm = TRUE)
sd_no_tox_END_TSa = sd(Tot[End_tox_TS_bis:DIM_TS,2],na.rm = TRUE)


Average_no_tox_TS_W = mean(Tot[Start_tox_TS:End_tox_TS,3],na.rm = TRUE)
sd_no_tox_TS_W = sd(Tot[Start_tox_TS:End_tox_TS,3],na.rm = TRUE)
Average_no_tox_END_TS_W = mean(Tot[End_tox_TS_bis:DIM_TS,3],na.rm = TRUE)
sd_no_tox_END_TS_W = sd(Tot[End_tox_TS_bis:DIM_TS,3],na.rm = TRUE)

Average_no_tox_TS_Wa = mean(Tot[Start_tox_TS:End_tox_TS,4],na.rm = TRUE)
sd_no_tox_TS_Wa = sd(Tot[Start_tox_TS:End_tox_TS,4],na.rm = TRUE)
Average_no_tox_END_TS_Wa = mean(Tot[End_tox_TS_bis:DIM_TS,4],na.rm = TRUE)
sd_no_tox_END_TS_Wa = sd(Tot[End_tox_TS_bis:DIM_TS,4],na.rm = TRUE)

Tot_TS = Tot
Tot_sd_TS = Tot_sd
rowTot_TS=nrow(Tot_TS)
time_TS = 365-148




############## DRM

file_DRM = "Cyprinidae_TX_EMF DRM-noStoch-table.csv"
DRM_table=read.csv(file_DRM, header=F,stringsAsFactors=FALSE)
NAMES <- read.csv(file_DRM, nrow = 6, sep = ";")
DRM_table <- read.csv(file_DRM, skip = 6, stringsAsFactors = FALSE, sep = ",")
words <- strsplit(NAMES[6,1], ",")[[1]]
xx=unique(tolower(words))
names(DRM_table) <- xx

Tstart_exp_DRM = 20
Tend_exp_DRM = Tstart_exp_DRM + 5
Tsim_DRM = Tend_exp_DRM + 3
DIM_DRM = 364*24+365*(Tsim_DRM - 2)*24+3
Start_tox_DRM = 364*24+365*(Tstart_exp_DRM -2)*24 +3
End_tox_DRM = 364*24+365*(Tend_exp_DRM - 2)*24 +2
End_tox_DRM_bis = End_tox_DRM + 1 
n_var = 4
R=nrow(DRM_table)

DRM_matrix <- matrix(, nrow = R, ncol = 1)
n_replicates = 40 

for (i in 1:n_replicates)
{
  dataframe = DRM_table[DRM_table[, "[run number]"] == i,]
  assign(paste("Matrix",i, sep=""), select(dataframe, 
                                           c("count turtles with [u_h > u_h^b]","count turtles with [u_h > u_h^p]","sum [weight] of turtles with [u_h > u_h^b]","w-turtles")))
}

for (i in 1:n_replicates)
{
  x= nrow(get(paste("Matrix",i, sep="")))
  if(x != DIM_DRM)
  {print (i)
    print (x)}
}


DRM_m = Matrix1

for (i in 2:n_replicates)
{
  
  DRM_m = cbind (DRM_m,get(paste("Matrix",i, sep="")))
} 
colTot=ncol(DRM_m)


col.names = rep(c("Tot pop","Adult pop","Tot weight","Adult weight"),n_replicates)
colnames(DRM_m) <-col.names


DRM_mr <- as.data.frame( # sapply returns a list here, so we convert it to a data.frame
  sapply(unique(names(DRM_m)), # for each unique column name
         function(col) rowMeans(DRM_m[names(DRM_m) == col]) # calculate row means
  )
)

DRM_mr_sd <- as.data.frame( # sapply returns a list here, so we convert it to a data.frame
  sapply(unique(names(DRM_m)), # for each unique column name
         function(col) rowSds(as.matrix(DRM_m[names(DRM_m) == col])) # calculate row means
  )
)

##################

Average_no_tox_DRM = mean(DRM_mr[Start_tox_DRM:End_tox_DRM,1],na.rm = TRUE)
sd_no_tox_DRM = sd(DRM_mr[Start_tox_DRM:End_tox_DRM,1],na.rm = TRUE)
Average_no_tox_END_DRM = mean(DRM_mr[End_tox_DRM_bis:DIM_DRM,1],na.rm = TRUE)
sd_no_tox_END_DRM = sd(DRM_mr[End_tox_DRM_bis:DIM_DRM,1],na.rm = TRUE)


Average_no_tox_DRMa = mean(DRM_mr[Start_tox_DRM:End_tox_DRM,2],na.rm = TRUE)
sd_no_tox_DRMa = sd(DRM_mr[Start_tox_DRM:End_tox_DRM,2],na.rm = TRUE)
Average_no_tox_END_DRMa = mean(DRM_mr[End_tox_DRM_bis:DIM_DRM,2],na.rm = TRUE)
sd_no_tox_END_DRMa = sd(DRM_mr[End_tox_DRM_bis:DIM_DRM,2],na.rm = TRUE)

Average_no_tox_DRM_W = mean(DRM_mr[Start_tox_DRM:End_tox_DRM,3],na.rm = TRUE)
sd_no_tox_DRM_W = sd(DRM_mr[Start_tox_DRM:End_tox_DRM,3],na.rm = TRUE)
Average_no_tox_END_DRM_W = mean(DRM_mr[End_tox_DRM_bis:DIM_DRM,3],na.rm = TRUE)
sd_no_tox_END_DRM_W = sd(DRM_mr[End_tox_DRM_bis:DIM_DRM,3],na.rm = TRUE)

Average_no_tox_DRM_Wa = mean(DRM_mr[Start_tox_DRM:End_tox_DRM,4],na.rm = TRUE)
sd_no_tox_DRM_Wa = sd(DRM_mr[Start_tox_DRM:End_tox_DRM,4],na.rm = TRUE)
Average_no_tox_END_DRM_Wa = mean(DRM_mr[End_tox_DRM_bis:DIM_DRM,4],na.rm = TRUE)
sd_no_tox_END_DRM_Wa = sd(DRM_mr[End_tox_DRM_bis:DIM_DRM,4],na.rm = TRUE)

Tot_DRM = DRM_mr
Tot_sd_DRM = DRM_mr_sd
rowTot_DRM=nrow(Tot_DRM)
time_DRM = 1



########## SD

file_SD_noTox = "SD.csv"
SD_noTox=read.csv(file_SD_noTox, header = T, as.is = T)
Tstart_exp_SD = 15
Tend_exp_SD = Tstart_exp_SD + 5
Tsim_SD = Tend_exp_SD + 3
DIM_SD = 365-89+(Tsim_SD - 2)*365+1
Start_tox_SD = 365-89+(Tstart_exp_SD -2)*365 +1
End_tox_SD = 365-89+(Tend_exp_SD - 2)*365
End_tox_SD_bis = End_tox_SD +1

n_var =4
R=nrow(SD_noTox)
C=ncol(SD_noTox)
n_replicates = (C - 1)/n_var
Tot <- matrix(, nrow = R, ncol = 1)
Tot_sd <- matrix(, nrow = R, ncol = 1)

i=2
while ( i<= n_var+1)
{
  M1=SD_noTox[,i]
  for (j in 1:(n_replicates-1))
  {
    c=i+j*n_var
    x =  SD_noTox[,c]
    M1=cbind(M1,x)
  }
  Average=rowMeans(M1,na.rm = TRUE)
  sd_tot=rowSds(M1,na.rm = TRUE)
  Tot=cbind( Tot, Average)
  Tot_sd = cbind(Tot_sd, sd_tot)
  i=i+1
  i
}
colTot=ncol(Tot)
Tot = Tot[,2:colTot]   
Tot_sd = Tot_sd[,2:colTot] 

Average_no_tox_SD = mean(Tot[Start_tox_SD:End_tox_SD,1],na.rm = TRUE)
sd_no_tox_SD = sd(Tot[Start_tox_SD:End_tox_SD,1],na.rm = TRUE)
Average_no_tox_END_SD = mean(Tot[End_tox_SD_bis:DIM_SD,1],na.rm = TRUE)
sd_no_tox_END_SD = sd(Tot[End_tox_SD_bis:DIM_SD,1],na.rm = TRUE)

Average_no_tox_SDa = mean(Tot[Start_tox_SD:End_tox_SD,2],na.rm = TRUE)
sd_no_tox_SDa = sd(Tot[Start_tox_SD:End_tox_SD,2],na.rm = TRUE)
Average_no_tox_END_SDa = mean(Tot[End_tox_SD_bis:DIM_SD,2],na.rm = TRUE)
sd_no_tox_END_SDa = sd(Tot[End_tox_SD_bis:DIM_SD,2],na.rm = TRUE)

Average_no_tox_SD_W = mean(Tot[Start_tox_SD:End_tox_SD,3],na.rm = TRUE)
sd_no_tox_SD_W = sd(Tot[Start_tox_SD:End_tox_SD,3],na.rm = TRUE)
Average_no_tox_END_SD_W = mean(Tot[End_tox_SD_bis:DIM_SD,3],na.rm = TRUE)
sd_no_tox_END_SD_W = sd(Tot[End_tox_SD_bis:DIM_SD,3],na.rm = TRUE)

Average_no_tox_SD_Wa = mean(Tot[Start_tox_SD:End_tox_SD,4],na.rm = TRUE)
sd_no_tox_SD_Wa = sd(Tot[Start_tox_SD:End_tox_SD,4],na.rm = TRUE)
Average_no_tox_END_SD_Wa = mean(Tot[End_tox_SD_bis:DIM_SD,4],na.rm = TRUE)
sd_no_tox_END_SD_Wa = sd(Tot[End_tox_SD_bis:DIM_SD,4],na.rm = TRUE)

Tot_SD = Tot
Tot_sd_SD = Tot_sd
rowTot_SD=nrow(Tot_SD)
time_SD = 365-88



############# HC
file_HC_noTox = "HC.csv"
HC_noTox=read.csv(file_HC_noTox, header = T, as.is = T)
Tstart_exp_HC = 25
Tend_exp_HC = Tstart_exp_HC + 5
Tsim_HC = Tend_exp_HC + 3
DIM_HC = 365-79+(Tsim_HC - 2)*365+1
Start_tox_HC = 365-79+(Tstart_exp_HC -2)*365 +1
End_tox_HC = 365-79+(Tend_exp_HC - 2)*365
End_tox_HC_bis = End_tox_HC +1
n_var =4

R=nrow(HC_noTox)
C=ncol(HC_noTox)
n_replicates = (C - 1)/n_var
Tot <- matrix(, nrow = R, ncol = 1)
Tot_sd <- matrix(, nrow = R, ncol = 1)

i=2
while ( i<= n_var+1)
{
  M1=HC_noTox[,i]
  for (j in 1:(n_replicates-1))
  {
    c=i+j*n_var
    x =  HC_noTox[,c]
    M1=cbind(M1,x)
  }
  Average=rowMeans(M1,na.rm = TRUE)
  sd_tot=rowSds(M1,na.rm = TRUE)
  Tot=cbind( Tot, Average)
  Tot_sd = cbind(Tot_sd, sd_tot)
  i=i+1
  i
}
colTot=ncol(Tot)
Tot = Tot[,2:colTot]   
Tot_sd = Tot_sd[,2:colTot] 

Average_no_tox_HC = mean(Tot[Start_tox_HC:End_tox_HC,1],na.rm = TRUE)
sd_no_tox_HC = sd(Tot[Start_tox_HC:End_tox_HC,1],na.rm = TRUE)
Average_no_tox_END_HC = mean(Tot[End_tox_HC_bis:DIM_HC,1],na.rm = TRUE)
sd_no_tox_END_HC = sd(Tot[End_tox_HC_bis:DIM_HC,1],na.rm = TRUE)


Average_no_tox_HC_W = mean(Tot[Start_tox_HC:End_tox_HC,3],na.rm = TRUE)
sd_no_tox_HC_W = sd(Tot[Start_tox_HC:End_tox_HC,3],na.rm = TRUE)
Average_no_tox_END_HC_W = mean(Tot[End_tox_HC_bis:DIM_HC,3],na.rm = TRUE)
sd_no_tox_END_HC_W = sd(Tot[End_tox_HC_bis:DIM_HC,3],na.rm = TRUE)


#adults

Average_no_tox_HCa = mean(Tot[Start_tox_HC:End_tox_HC,2],na.rm = TRUE)
sd_no_tox_HCa = sd(Tot[Start_tox_HC:End_tox_HC,2],na.rm = TRUE)
Average_no_tox_END_HCa = mean(Tot[End_tox_HC_bis:DIM_HC,2],na.rm = TRUE)
sd_no_tox_END_HCa = sd(Tot[End_tox_HC_bis:DIM_HC,2],na.rm = TRUE)

Average_no_tox_HC_Wa = mean(Tot[Start_tox_HC:End_tox_HC,4],na.rm = TRUE)
sd_no_tox_HC_Wa = sd(Tot[Start_tox_HC:End_tox_HC,4],na.rm = TRUE)
Average_no_tox_END_HC_Wa = mean(Tot[End_tox_HC_bis:DIM_HC,4],na.rm = TRUE)
sd_no_tox_END_HC_Wa = sd(Tot[End_tox_HC_bis:DIM_HC,4],na.rm = TRUE)



Average_no_tox_HC_first = mean(Tot[1:Start_tox_HC,1],na.rm = TRUE)
sd_no_tox_HC_first = sd(Tot[1:Start_tox_HC,1],na.rm = TRUE)

Tot_HC = Tot
Tot_sd_HC = Tot_sd
rowTot_HC=nrow(Tot_HC)
time_HC = 365-78




#############################################
########### WITH STOCHASTICITY ##############

##### TS
setwd("....")
file_TS_noTox = "TX_TS-W.csv"
TS_noTox=read.csv(file_TS_noTox, header = T, as.is = T)
Tstart_exp_TS = 15
Tend_exp_TS = Tstart_exp_TS + 5
Tsim_TS = Tend_exp_TS + 3
DIM_TS = 365-149+(Tsim_TS - 2)*365+1
Start_tox_TS = 365-149+(Tstart_exp_TS -2)*365 +1
End_tox_TS = 365-149+(Tend_exp_TS - 2)*365 
End_tox_TS_bis=End_tox_TS  + 1 

n_var =4
R=nrow(TS_noTox)
C=ncol(TS_noTox)
n_replicates = (C - 1)/n_var
Tot_Stoch <- matrix(, nrow = R, ncol = 1)
Tot_Stoch_sd <- matrix(, nrow = R, ncol = 1)

i=2
while ( i<= n_var+1)
{
  M1=TS_noTox[,i]
  for (j in 1:(n_replicates-1))
  {
    c=i+j*n_var
    x =  TS_noTox[,c]
    M1=cbind(M1,x)
  }
  Average=rowMeans(M1,na.rm = TRUE)
  sd_tot=rowSds(M1,na.rm = TRUE)
  Tot_Stoch=cbind( Tot_Stoch, Average)
  Tot_Stoch_sd = cbind(Tot_Stoch_sd, sd_tot)
  i=i+1
  i
}
colTot=ncol(Tot_Stoch)
Tot_Stoch = Tot_Stoch[,2:colTot]   
Tot_Stoch_sd = Tot_Stoch_sd[,2:colTot] 

Average_no_tox_TS = mean(Tot_Stoch[Start_tox_TS:End_tox_TS,1],na.rm = TRUE)
sd_no_tox_TS = sd(Tot_Stoch[Start_tox_TS:End_tox_TS,1],na.rm = TRUE)
Average_no_tox_END_TS = mean(Tot_Stoch[End_tox_TS_bis:DIM_TS,1],na.rm = TRUE)
sd_no_tox_END_TS = sd(Tot_Stoch[End_tox_TS_bis:DIM_TS,1],na.rm = TRUE)

Average_no_tox_TSa = mean(Tot_Stoch[Start_tox_TS:End_tox_TS,2],na.rm = TRUE)
sd_no_tox_TSa = sd(Tot_Stoch[Start_tox_TS:End_tox_TS,2],na.rm = TRUE)
Average_no_tox_END_TSa = mean(Tot_Stoch[End_tox_TS_bis:DIM_TS,2],na.rm = TRUE)
sd_no_tox_END_TSa = sd(Tot_Stoch[End_tox_TS_bis:DIM_TS,2],na.rm = TRUE)


Average_no_tox_TS_W = mean(Tot_Stoch[Start_tox_TS:End_tox_TS,3],na.rm = TRUE)
sd_no_tox_TS_W = sd(Tot_Stoch[Start_tox_TS:End_tox_TS,3],na.rm = TRUE)
Average_no_tox_END_TS_W = mean(Tot_Stoch[End_tox_TS_bis:DIM_TS,3],na.rm = TRUE)
sd_no_tox_END_TS_W = sd(Tot_Stoch[End_tox_TS_bis:DIM_TS,3],na.rm = TRUE)

Average_no_tox_TS_Wa = mean(Tot_Stoch[Start_tox_TS:End_tox_TS,4],na.rm = TRUE)
sd_no_tox_TS_Wa = sd(Tot_Stoch[Start_tox_TS:End_tox_TS,4],na.rm = TRUE)
Average_no_tox_END_TS_Wa = mean(Tot_Stoch[End_tox_TS_bis:DIM_TS,4],na.rm = TRUE)
sd_no_tox_END_TS_Wa = sd(Tot_Stoch[End_tox_TS_bis:DIM_TS,4],na.rm = TRUE)

Tot_Stoch_TS = Tot_Stoch
Tot_Stoch_sd_TS = Tot_Stoch_sd
rowTot_Stoch_TS=nrow(Tot_Stoch_TS)
time_TS = 365-148


############## DRM

setwd("....")
file_DRM = "Cyprinidae_TX_DRM DRM-table.csv"
DRM_table=read.csv(file_DRM, header=F,stringsAsFactors=FALSE)
NAMES <- read.csv(file_DRM, nrow = 6, sep = ";")
DRM_table <- read.csv(file_DRM, skip = 6, stringsAsFactors = FALSE, sep = ",")
words <- strsplit(NAMES[6,1], ",")[[1]]
xx=unique(tolower(words))
names(DRM_table) <- xx

Tstart_exp_DRM = 20
Tend_exp_DRM = Tstart_exp_DRM + 5
Tsim_DRM = Tend_exp_DRM + 3
DIM_DRM = 364*24+365*(Tsim_DRM - 2)*24+3
Start_tox_DRM = 364*24+365*(Tstart_exp_DRM -2)*24 +3
End_tox_DRM = 364*24+365*(Tend_exp_DRM - 2)*24 +2
End_tox_DRM_bis = End_tox_DRM + 1 
n_var = 4
R=nrow(DRM_table)
DRM_matrix <- matrix(, nrow = R, ncol = 1)

n_replicates = 40 
for (i in 1:n_replicates)
{
  dataframe = DRM_table[DRM_table[, "[run number]"] == i,]
  assign(paste("Matrix",i, sep=""), select(dataframe, 
                                           c("count turtles with [u_h > u_h^b]","count turtles with [u_h > u_h^p]","sum [weight] of turtles with [u_h > u_h^b]","w-turtles")))
}

for (i in 1:n_replicates)
{
  x= nrow(get(paste("Matrix",i, sep="")))
  if(x != DIM_DRM)
  {print (i)
    print (x)}
}


DRM_m = Matrix1

for (i in 2:n_replicates)
{
  
  DRM_m = cbind (DRM_m,get(paste("Matrix",i, sep="")))
} 
colTot=ncol(DRM_m)


col.names = rep(c("Tot pop","Adult pop","Tot weight","Adult weight"),n_replicates)
colnames(DRM_m) <-col.names


DRM_mr_Stoch <- as.data.frame( # sapply returns a list here, so we convert it to a data.frame
  sapply(unique(names(DRM_m)), # for each unique column name
         function(col) rowMeans(DRM_m[names(DRM_m) == col]) # calculate row means
  )
)

DRM_mr_Stoch_sd <- as.data.frame( # sapply returns a list here, so we convert it to a data.frame
  sapply(unique(names(DRM_m)), # for each unique column name
         function(col) rowSds(as.matrix(DRM_m[names(DRM_m) == col])) # calculate row means
  )
)


##################

Average_no_tox_DRM = mean(DRM_mr_Stoch[Start_tox_DRM:End_tox_DRM,1],na.rm = TRUE)
sd_no_tox_DRM = sd(DRM_mr_Stoch[Start_tox_DRM:End_tox_DRM,1],na.rm = TRUE)
Average_no_tox_END_DRM = mean(DRM_mr_Stoch[End_tox_DRM_bis:DIM_DRM,1],na.rm = TRUE)
sd_no_tox_END_DRM = sd(DRM_mr_Stoch[End_tox_DRM_bis:DIM_DRM,1],na.rm = TRUE)


Average_no_tox_DRMa = mean(DRM_mr_Stoch[Start_tox_DRM:End_tox_DRM,2],na.rm = TRUE)
sd_no_tox_DRMa = sd(DRM_mr_Stoch[Start_tox_DRM:End_tox_DRM,2],na.rm = TRUE)
Average_no_tox_END_DRMa = mean(DRM_mr_Stoch[End_tox_DRM_bis:DIM_DRM,2],na.rm = TRUE)
sd_no_tox_END_DRMa = sd(DRM_mr_Stoch[End_tox_DRM_bis:DIM_DRM,2],na.rm = TRUE)

Average_no_tox_DRM_W = mean(DRM_mr_Stoch[Start_tox_DRM:End_tox_DRM,3],na.rm = TRUE)
sd_no_tox_DRM_W = sd(DRM_mr_Stoch[Start_tox_DRM:End_tox_DRM,3],na.rm = TRUE)
Average_no_tox_END_DRM_W = mean(DRM_mr_Stoch[End_tox_DRM_bis:DIM_DRM,3],na.rm = TRUE)
sd_no_tox_END_DRM_W = sd(DRM_mr_Stoch[End_tox_DRM_bis:DIM_DRM,3],na.rm = TRUE)

Average_no_tox_DRM_Wa = mean(DRM_mr_Stoch[Start_tox_DRM:End_tox_DRM,4],na.rm = TRUE)
sd_no_tox_DRM_Wa = sd(DRM_mr_Stoch[Start_tox_DRM:End_tox_DRM,4],na.rm = TRUE)
Average_no_tox_END_DRM_Wa = mean(DRM_mr_Stoch[End_tox_DRM_bis:DIM_DRM,4],na.rm = TRUE)
sd_no_tox_END_DRM_Wa = sd(DRM_mr_Stoch[End_tox_DRM_bis:DIM_DRM,4],na.rm = TRUE)

Tot_Stoch_DRM = DRM_mr_Stoch
Tot_Stoch_sd_DRM = DRM_mr_Stoch_sd
rowTot_DRM=nrow(Tot_Stoch_DRM)
time_DRM = 1



########## SD
setwd("....")
file_SD_noTox = "TX_SD-W.csv"
SD_noTox=read.csv(file_SD_noTox, header = T, as.is = T)
Tstart_exp_SD = 15
Tend_exp_SD = Tstart_exp_SD + 5
Tsim_SD = Tend_exp_SD + 3
DIM_SD = 365-89+(Tsim_SD - 2)*365+1
Start_tox_SD = 365-89+(Tstart_exp_SD -2)*365 +1
End_tox_SD = 365-89+(Tend_exp_SD - 2)*365
End_tox_SD_bis = End_tox_SD +1

n_var =4
R=nrow(SD_noTox)
C=ncol(SD_noTox)
n_replicates = (C - 1)/n_var
Tot_Stoch <- matrix(, nrow = R, ncol = 1)
Tot_Stoch_sd <- matrix(, nrow = R, ncol = 1)

i=2
while ( i<= n_var+1)
{
  M1=SD_noTox[,i]
  for (j in 1:(n_replicates-1))
  {
    c=i+j*n_var
    x =  SD_noTox[,c]
    M1=cbind(M1,x)
  }
  Average=rowMeans(M1,na.rm = TRUE)
  sd_tot=rowSds(M1,na.rm = TRUE)
  Tot_Stoch=cbind( Tot_Stoch, Average)
  Tot_Stoch_sd = cbind(Tot_Stoch_sd, sd_tot)
  i=i+1
  i
}
colTot=ncol(Tot_Stoch)
Tot_Stoch = Tot_Stoch[,2:colTot]   
Tot_Stoch_sd = Tot_Stoch_sd[,2:colTot] 

Average_no_tox_SD = mean(Tot_Stoch[Start_tox_SD:End_tox_SD,1],na.rm = TRUE)
sd_no_tox_SD = sd(Tot_Stoch[Start_tox_SD:End_tox_SD,1],na.rm = TRUE)
Average_no_tox_END_SD = mean(Tot_Stoch[End_tox_SD_bis:DIM_SD,1],na.rm = TRUE)
sd_no_tox_END_SD = sd(Tot_Stoch[End_tox_SD_bis:DIM_SD,1],na.rm = TRUE)

Average_no_tox_SDa = mean(Tot_Stoch[Start_tox_SD:End_tox_SD,2],na.rm = TRUE)
sd_no_tox_SDa = sd(Tot_Stoch[Start_tox_SD:End_tox_SD,2],na.rm = TRUE)
Average_no_tox_END_SDa = mean(Tot_Stoch[End_tox_SD_bis:DIM_SD,2],na.rm = TRUE)
sd_no_tox_END_SDa = sd(Tot_Stoch[End_tox_SD_bis:DIM_SD,2],na.rm = TRUE)

Average_no_tox_SD_W = mean(Tot_Stoch[Start_tox_SD:End_tox_SD,3],na.rm = TRUE)
sd_no_tox_SD_W = sd(Tot_Stoch[Start_tox_SD:End_tox_SD,3],na.rm = TRUE)
Average_no_tox_END_SD_W = mean(Tot_Stoch[End_tox_SD_bis:DIM_SD,3],na.rm = TRUE)
sd_no_tox_END_SD_W = sd(Tot_Stoch[End_tox_SD_bis:DIM_SD,3],na.rm = TRUE)

Average_no_tox_SD_Wa = mean(Tot_Stoch[Start_tox_SD:End_tox_SD,4],na.rm = TRUE)
sd_no_tox_SD_Wa = sd(Tot_Stoch[Start_tox_SD:End_tox_SD,4],na.rm = TRUE)
Average_no_tox_END_SD_Wa = mean(Tot_Stoch[End_tox_SD_bis:DIM_SD,4],na.rm = TRUE)
sd_no_tox_END_SD_Wa = sd(Tot_Stoch[End_tox_SD_bis:DIM_SD,4],na.rm = TRUE)

Tot_Stoch_SD = Tot_Stoch
Tot_Stoch_sd_SD = Tot_Stoch_sd
rowTot_Stoch_SD=nrow(Tot_Stoch_SD)
time_SD = 365-88



############# HC
setwd("....")
file_HC_noTox = "TX_HC-W.csv"
HC_noTox=read.csv(file_HC_noTox, header = T, as.is = T)
Tstart_exp_HC = 25
Tend_exp_HC = Tstart_exp_HC + 5
Tsim_HC = Tend_exp_HC + 3
DIM_HC = 365-79+(Tsim_HC - 2)*365+1
Start_tox_HC = 365-79+(Tstart_exp_HC -2)*365 +1
End_tox_HC = 365-79+(Tend_exp_HC - 2)*365
End_tox_HC_bis = End_tox_HC +1
n_var =4

R=nrow(HC_noTox)
C=ncol(HC_noTox)
n_replicates = (C - 1)/n_var

Tot_Stoch <- matrix(, nrow = R, ncol = 1)
Tot_Stoch_sd <- matrix(, nrow = R, ncol = 1)

i=2
while ( i<= n_var+1)
{
  M1=HC_noTox[,i]
  for (j in 1:(n_replicates-1))
  {
    c=i+j*n_var
    x =  HC_noTox[,c]
    M1=cbind(M1,x)
  }
  Average=rowMeans(M1,na.rm = TRUE)
  sd_tot=rowSds(M1,na.rm = TRUE)
  Tot_Stoch=cbind( Tot_Stoch, Average)
  Tot_Stoch_sd = cbind(Tot_Stoch_sd, sd_tot)
  i=i+1
  i
}
colTot=ncol(Tot_Stoch)
Tot_Stoch = Tot_Stoch[,2:colTot]   
Tot_Stoch_sd = Tot_Stoch_sd[,2:colTot] 

Average_no_tox_HC = mean(Tot_Stoch[Start_tox_HC:End_tox_HC,1],na.rm = TRUE)
sd_no_tox_HC = sd(Tot_Stoch[Start_tox_HC:End_tox_HC,1],na.rm = TRUE)
Average_no_tox_END_HC = mean(Tot_Stoch[End_tox_HC_bis:DIM_HC,1],na.rm = TRUE)
sd_no_tox_END_HC = sd(Tot_Stoch[End_tox_HC_bis:DIM_HC,1],na.rm = TRUE)


Average_no_tox_HC_W = mean(Tot_Stoch[Start_tox_HC:End_tox_HC,3],na.rm = TRUE)
sd_no_tox_HC_W = sd(Tot_Stoch[Start_tox_HC:End_tox_HC,3],na.rm = TRUE)
Average_no_tox_END_HC_W = mean(Tot_Stoch[End_tox_HC_bis:DIM_HC,3],na.rm = TRUE)
sd_no_tox_END_HC_W = sd(Tot_Stoch[End_tox_HC_bis:DIM_HC,3],na.rm = TRUE)


#adults

Average_no_tox_HCa = mean(Tot_Stoch[Start_tox_HC:End_tox_HC,2],na.rm = TRUE)
sd_no_tox_HCa = sd(Tot_Stoch[Start_tox_HC:End_tox_HC,2],na.rm = TRUE)
Average_no_tox_END_HCa = mean(Tot_Stoch[End_tox_HC_bis:DIM_HC,2],na.rm = TRUE)
sd_no_tox_END_HCa = sd(Tot_Stoch[End_tox_HC_bis:DIM_HC,2],na.rm = TRUE)

Average_no_tox_HC_Wa = mean(Tot_Stoch[Start_tox_HC:End_tox_HC,4],na.rm = TRUE)
sd_no_tox_HC_Wa = sd(Tot_Stoch[Start_tox_HC:End_tox_HC,4],na.rm = TRUE)
Average_no_tox_END_HC_Wa = mean(Tot_Stoch[End_tox_HC_bis:DIM_HC,4],na.rm = TRUE)
sd_no_tox_END_HC_Wa = sd(Tot_Stoch[End_tox_HC_bis:DIM_HC,4],na.rm = TRUE)



Average_no_tox_HC_first = mean(Tot_Stoch[1:Start_tox_HC,1],na.rm = TRUE)
sd_no_tox_HC_first = sd(Tot_Stoch[1:Start_tox_HC,1],na.rm = TRUE)

Tot_Stoch_HC = Tot_Stoch
Tot_Stoch_sd_HC = Tot_Stoch_sd
rowTot_Stoch_HC=nrow(Tot_Stoch_HC)
time_HC = 365-78



##### fig
yy = 0:3000
x11()
# pdf("D:/LAVORO/WB/Projects/Fish_project_Syngenta/PopGuide_decision_steps/Draft/FIGURES PAPER/Pop_dynamics.pdf",width=25,height=18,onefile=T,paper='A4r')

# pdf("F:/LAVORO/WB/Projects/Fish_project_Syngenta/PopGuide_decision_steps/Draft/FIGURES PAPER/Pop_dynamicsStoch_NoStoch.pdf",width=25,height=18,onefile=T,paper='A4r')
par(mar=c(5.1+1.5,5.1+2,3.1,2.1))
# par(mar=c(1.5,1.5,1.5,1.5))
# par(cex.axis=2)
par(mfrow=c(2,2))#,oma = c(0, 0, 2, 0))

x =1:rowTot_TS

plot(Tot_TS[,1], col ="black", type = "l",lwd = 2,xlim=c(0,rowTot_TS), ylim=c(0,3000), main= "Topeka shiner ", ylab = "Total population [#]", xlab = "Time [d]",xaxt="n")
lablist<-as.vector(c(2:Tsim_TS))
lablist<-as.vector(c(2,4,6,8,10,12,14,16,18,20,22))
axis(1, at=seq(time_TS, rowTot_TS, by=365), labels = FALSE)
text(seq(time_TS, rowTot_TS, by=2*365),  par("usr")[3] ,labels = lablist, pos = 1, xpd = TRUE)
lines(Tot_TS[,1]+Tot_sd_TS[,1] , lwd=1, lty=2)
lines(Tot_TS[,1]-Tot_sd_TS[,1] , lwd=1, lty=2)
xx = seq(time_TS, rowTot_TS, by=365)
lines(rep(xx[14],3001),yy,lwd=2, lty=2, col=2)
# x11()

par(new=T)
plot(Tot_Stoch_TS[,1], col ="green", type = "l",lwd = 2,xlim=c(0,rowTot_TS), ylim=c(0,3000), main= "Topeka shiner ", ylab = "Total population [#]", xlab = "Time [d]",xaxt="n")
lablist<-as.vector(c(2:Tsim_TS))
lablist<-as.vector(c(2,4,6,8,10,12,14,16,18,20,22))
axis(1, at=seq(time_TS, rowTot_TS, by=365), labels = FALSE)
text(seq(time_TS, rowTot_TS, by=2*365),  par("usr")[3] ,labels = lablist, pos = 1, xpd = TRUE)
lines(Tot_Stoch_TS[,1]+Tot_Stoch_sd_TS[,1] , lwd=1, lty=2, col ="green")
lines(Tot_Stoch_TS[,1]-Tot_Stoch_sd_TS[,1] , lwd=1, lty=2, col ="green")
xx = seq(time_TS, rowTot_TS, by=365)
lines(rep(xx[14],3001),yy,lwd=2, lty=2, col=2)
# x11()



x =1:rowTot_DRM

plot(Tot_DRM[,1], col ="black", type = "l",lwd = 2,xlim=c(0,rowTot_DRM), ylim=c(0,3000), main= "Devils River minnow ", ylab = "Total population [#]", xlab = "Time [d]",xaxt="n")
lablist<-as.vector(c(1:(Tsim_DRM-1)))
lablist<-as.vector(c(1,3,5,7,9,11,13,15,17,19,21,23,25,27))
axis(1, at=seq(time_DRM, rowTot_DRM, by=365*24), labels = FALSE)
text(seq(time_DRM, rowTot_DRM, by=2*365*24),  par("usr")[3] ,labels = lablist, pos = 1, xpd = TRUE)
lines(Tot_DRM[,1]+Tot_sd_DRM[,1] , lwd=1, lty=2)
lines(Tot_DRM[,1]-Tot_sd_DRM[,1] , lwd=1, lty=2)
xx = seq(time_DRM, rowTot_DRM, by=365*24)
lines(rep(xx[20],3001),yy,lwd=2, lty=2, col=2)

par(new=T)

plot(Tot_Stoch_DRM[,1], col ="green", type = "l",lwd = 2,xlim=c(0,rowTot_DRM), ylim=c(0,3000), main= "Devils River minnow ", ylab = "Total population [#]", xlab = "Time [d]",xaxt="n")
lablist<-as.vector(c(1:(Tsim_DRM-1)))
lablist<-as.vector(c(1,3,5,7,9,11,13,15,17,19,21,23,25,27))
axis(1, at=seq(time_DRM, rowTot_DRM, by=365*24), labels = FALSE)
text(seq(time_DRM, rowTot_DRM, by=2*365*24),  par("usr")[3] ,labels = lablist, pos = 1, xpd = TRUE)
lines(Tot_Stoch_DRM[,1]+Tot_Stoch_sd_DRM[,1] , lwd=1, lty=2, col ="green")
lines(Tot_Stoch_DRM[,1]-Tot_Stoch_sd_DRM[,1] , lwd=1, lty=2, col ="green")
xx = seq(time_DRM, rowTot_DRM, by=365*24)
lines(rep(xx[20],3001),yy,lwd=2, lty=2, col=2)


# x11()
x =1:rowTot_SD

plot(Tot_SD[,1], col = "black" , type = "l",lwd = 2,xlim=c(0,rowTot_SD), ylim=c(0,3000), main= "Spikedace",  ylab = "Total population [#]", xlab = "Time [d]",xaxt="n")
lablist<-as.vector(c(2:Tsim_SD))
lablist<-as.vector(c(2,4,6,8,10,12,14,16,18,20,22))
axis(1, at=seq(time_SD, rowTot_SD, by=365), labels = FALSE)
text(seq(time_SD, rowTot_SD, by=2*365),  par("usr")[3] ,labels = lablist, pos = 1, xpd = TRUE)
lines(Tot_SD[,1]+Tot_sd_SD[,1] , lwd=1, lty=2)
lines(Tot_SD[,1]-Tot_sd_SD[,1] , lwd=1, lty=2)
xx = seq(time_SD, rowTot_SD, by=365)
lines(rep(xx[14],3001),yy,lwd=2, lty=2, col=2)

par(new=T)
# x =1:rowTot_SD

plot(Tot_Stoch_SD[,1], col = "green" , type = "l",lwd = 2,xlim=c(0,rowTot_SD), ylim=c(0,3000), main= "Spikedace",  ylab = "Total population [#]", xlab = "Time [d]",xaxt="n")
lablist<-as.vector(c(2:Tsim_SD))
lablist<-as.vector(c(2,4,6,8,10,12,14,16,18,20,22))
axis(1, at=seq(time_SD, rowTot_SD, by=365), labels = FALSE)
text(seq(time_SD, rowTot_SD, by=2*365),  par("usr")[3] ,labels = lablist, pos = 1, xpd = TRUE)
lines(Tot_Stoch_SD[,1]+Tot_Stoch_sd_SD[,1] , lwd=1, lty=2, col ="green")
lines(Tot_Stoch_SD[,1]-Tot_Stoch_sd_SD[,1] , lwd=1, lty=2, col ="green")
xx = seq(time_SD, rowTot_SD, by=365)
lines(rep(xx[14],3001),yy,lwd=2, lty=2, col=2)


# x11()
x =1:rowTot_HC

plot(Tot_HC[,1], col ="black", type = "l",lwd = 2,xlim=c(0,rowTot_HC), ylim=c(0,3000), main= "Humpback chub", ylab = "Total population [#]", xlab = "Time [d]",xaxt="n")
par(new=T)
lablist<-as.vector(c(2:Tsim_HC))
lablist<-as.vector(c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32))
axis(1, at=seq(time_HC, rowTot_HC, by=365), labels = FALSE)
text(seq(time_HC, rowTot_HC, by=2*365),  par("usr")[3] ,labels = lablist, pos = 1, xpd = TRUE)

lines(Tot_HC[,1]+Tot_sd_HC[,1] , lwd=1, lty=2)
lines(Tot_HC[,1]-Tot_sd_HC[,1] , lwd=1, lty=2)
xx = seq(time_HC, rowTot_HC, by=365)
lines(rep(xx[24],3001),yy,lwd=2, lty=2, col=2)

par(new=T)
plot(Tot_Stoch_HC[,1], col ="green", type = "l",lwd = 2,xlim=c(0,rowTot_HC), ylim=c(0,3000), main= "Humpback chub", ylab = "Total population [#]", xlab = "Time [d]",xaxt="n")
par(new=T)
lablist<-as.vector(c(2:Tsim_HC))
lablist<-as.vector(c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32))
axis(1, at=seq(time_HC, rowTot_HC, by=365), labels = FALSE)
text(seq(time_HC, rowTot_HC, by=2*365),  par("usr")[3] ,labels = lablist, pos = 1, xpd = TRUE)

lines(Tot_Stoch_HC[,1]+Tot_Stoch_sd_HC[,1] , lwd=1, lty=2, col ="green")
lines(Tot_Stoch_HC[,1]-Tot_Stoch_sd_HC[,1] , lwd=1, lty=2, col ="green")
xx = seq(time_HC, rowTot_HC, by=365)
lines(rep(xx[24],3001),yy,lwd=2, lty=2, col=2)

# dev.off()











####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################

###################### ********************************** ##########################################################################
####################### **********************************  ********************************** #####################################
##################                      EFFECTS OF DENSITY DEPENDENCE



##### TS
setwd("....")
file_TS = "Cyprinidae_TX_DD-lessmort-postsent TS-postSent-table.csv"
TS_table=read.csv(file_TS, header=F,stringsAsFactors=FALSE)
NAMES <- read.csv(file_TS, nrow = 6, sep = ";")
TS_table <- read.csv(file_TS, skip = 6, stringsAsFactors = FALSE, sep = ",")
words <- strsplit(NAMES[6,1], ",")[[1]]
xx=unique(tolower(words))
names(TS_table) <- xx
Tstart_exp_TS = 15
Tend_exp_TS = Tstart_exp_TS + 5
Tsim_TS = 20
DIM_TS = 365-149+(Tsim_TS - 2)*365+1
Start_tox_TS = 365-149+(Tstart_exp_TS -2)*365 +1
End_tox_TS = 365-149+(Tend_exp_TS - 2)*365 
End_tox_TS_bis=End_tox_TS  + 1 
R=nrow(TS_table)

TS_matrix <- matrix(, nrow = R, ncol = 1)

n_replicates = 10 
for (i in 1:n_replicates)
{
  dataframe = TS_table[TS_table[, "[run number]"] == i,]
  assign(paste("Matrix",i, sep=""), select(dataframe,
                                           c("count turtles with [u_h > u_h^b]")))
}

for (i in 1:n_replicates)
{
  x= nrow(get(paste("Matrix",i, sep="")))
  if(x != DIM_TS)
  {print (i)
    print (x)}
}


TS_m = Matrix1

for (i in 2:n_replicates)
{
  
  TS_m = cbind (TS_m,get(paste("Matrix",i, sep="")))
}
colTot=ncol(TS_m)


col.names = rep(c("Tot pop"),n_replicates)
colnames(TS_m) <-col.names


TS_mr <- as.data.frame( # sapply returns a list here, so we convert it to a data.frame
  sapply(unique(names(TS_m)), # for each unique column name
         function(col) rowMeans(TS_m[names(TS_m) == col]) # calculate row means
  )
)

TS_mr_sd <- as.data.frame( # sapply returns a list here, so we convert it to a data.frame
  sapply(unique(names(TS_m)), # for each unique column name
         function(col) rowSds(as.matrix(TS_m[names(TS_m) == col])) # calculate row means
  )
)

time_TS = 365-148
rowTot_TS=nrow(TS_mr)


############## DRM
file_DRM = "Cyprinidae_TX_DD-lessmort-postsent DRM-postSent-table.csv"
DRM_table=read.csv(file_DRM, header=F,stringsAsFactors=FALSE)
NAMES <- read.csv(file_DRM, nrow = 6, sep = ";")
DRM_table <- read.csv(file_DRM, skip = 6, stringsAsFactors = FALSE, sep = ",")
words <- strsplit(NAMES[6,1], ",")[[1]]
xx=unique(tolower(words))
names(DRM_table) <- xx

Tstart_exp_DRM = 20
Tend_exp_DRM = Tstart_exp_DRM + 5
Tsim_DRM = 20
DIM_DRM = 364*24+365*(Tsim_DRM - 2)*24+3
R=nrow(DRM_table)
DRM_matrix <- matrix(, nrow = R, ncol = 1)
n_replicates = 10 

for (i in 1:n_replicates)
{

  dataframe = DRM_table[DRM_table[, "[run number]"] == i,]
  assign(paste("Matrix",i, sep=""), select(dataframe,
                                           c("count turtles with [u_h > u_h^b]")))
}

for (i in 1:n_replicates)
{
  x= nrow(get(paste("Matrix",i, sep="")))
  if(x != DIM_DRM)
  {print (i)
    print (x)}
}

DRM_m = Matrix1
for (i in 2:n_replicates)
{
  
  DRM_m = cbind (DRM_m,get(paste("Matrix",i, sep="")))
}
colTot=ncol(DRM_m)


col.names = rep(c("Tot pop"),n_replicates)
colnames(DRM_m) <-col.names


DRM_mr <- as.data.frame( # sapply returns a list here, so we convert it to a data.frame
  sapply(unique(names(DRM_m)), # for each unique column name
         function(col) rowMeans(DRM_m[names(DRM_m) == col]) # calculate row means
  )
)

DRM_mr_sd <- as.data.frame( # sapply returns a list here, so we convert it to a data.frame
  sapply(unique(names(DRM_m)), # for each unique column name
         function(col) rowSds(as.matrix(DRM_m[names(DRM_m) == col])) # calculate row means
  )
)

rowTot_DRM=nrow(DRM_mr)

# ########## SD

file_SD = "Cyprinidae_TX_DD-lessmort-postsent SD-postSent-table.csv"
SD_table=read.csv(file_SD, header=F,stringsAsFactors=FALSE)
NAMES <- read.csv(file_SD, nrow = 6, sep = ";")
SD_table <- read.csv(file_SD, skip = 6, stringsAsFactors = FALSE, sep = ",")
words <- strsplit(NAMES[6,1], ",")[[1]]
xx=unique(tolower(words))
names(SD_table) <- xx
Tsim_SD = 20
DIM_SD = 365-89+(Tsim_SD - 2)*365+1
R=nrow(SD_table)

SD_matrix <- matrix(, nrow = R, ncol = 1)

n_replicates = 10
for (i in 1:n_replicates)
{
  dataframe = SD_table[SD_table[, "[run number]"] == i,]
  assign(paste("Matrix",i, sep=""), select(dataframe,
                                           c("count turtles with [u_h > u_h^b]")))
}

for (i in 1:n_replicates)
{
  x= nrow(get(paste("Matrix",i, sep="")))
  if(x != DIM_SD)
  {print (i)
    print (x)}
}


SD_m = Matrix1

for (i in 2:n_replicates)
{
  
  SD_m = cbind (SD_m,get(paste("Matrix",i, sep="")))
}
colTot=ncol(SD_m)

col.names = rep(c("Tot pop"),n_replicates)
colnames(SD_m) <-col.names

SD_mr <- as.data.frame( # sapply returns a list here, so we convert it to a data.frame
  sapply(unique(names(SD_m)), # for each unique column name
         function(col) rowMeans(SD_m[names(SD_m) == col]) # calculate row means
  )
)

SD_mr_sd <- as.data.frame( # sapply returns a list here, so we convert it to a data.frame
  sapply(unique(names(SD_m)), # for each unique column name
         function(col) rowSds(as.matrix(SD_m[names(SD_m) == col])) # calculate row means
  )
)

time_SD = 365-88
rowTot_SD=nrow(SD_mr)

############# HC
setwd("....")
file_HC = "Cyprinidae_TX_DD-lessmort HC-lessmort-table.csv"
HC_table=read.csv(file_HC, header=F,stringsAsFactors=FALSE)
NAMES <- read.csv(file_HC, nrow = 6, sep = ";")
HC_table <- read.csv(file_HC, skip = 6, stringsAsFactors = FALSE, sep = ",")
words <- strsplit(NAMES[6,1], ",")[[1]]
xx=unique(tolower(words))

names(HC_table) <- xx
Tsim_HC = 20
DIM_HC = 365-79+(Tsim_HC - 2)*365+1

R=nrow(HC_table)
HC_matrix <- matrix(, nrow = R, ncol = 1)

n_replicates = 10 
for (i in 1:n_replicates)
{
  dataframe = HC_table[HC_table[, "[run number]"] == i,]
  assign(paste("Matrix",i, sep=""), select(dataframe,
                                           c("count turtles with [u_h > u_h^b]")))
}

for (i in 1:n_replicates)
{
  x= nrow(get(paste("Matrix",i, sep="")))
  if(x != DIM_HC)
  {print (i)
    print (x)}
}


HC_m = Matrix1

for (i in 2:n_replicates)
{
  
  HC_m = cbind (HC_m,get(paste("Matrix",i, sep="")))
}
colTot=ncol(HC_m)


col.names = rep(c("Tot pop"),n_replicates)
colnames(HC_m) <-col.names


HC_mr <- as.data.frame( # sapply returns a list here, so we convert it to a data.frame
  sapply(unique(names(HC_m)), # for each unique column name
         function(col) rowMeans(HC_m[names(HC_m) == col]) # calculate row means
  )
)

HC_mr_sd <- as.data.frame( # sapply returns a list here, so we convert it to a data.frame
  sapply(unique(names(HC_m)), # for each unique column name
         function(col) rowSds(as.matrix(HC_m[names(HC_m) == col])) # calculate row means
  )
)

time_HC = 365-78
rowTot_HC=nrow(HC_mr)

time_DRM = 1

##### fig
yy = 0:3000
x11()
par(mar=c(5.1+1.5,5.1+2,3.1,2.1))
par(mfrow=c(2,2))
x =1:rowTot_TS
plot(TS_mr[,1], col ="black", type = "l",xlim=c(0,rowTot_TS), ylim=c(0,9000), main= "Topeka shiner ", ylab = "Total population [#]", xlab = "Time [d]",xaxt="n")
lablist<-as.vector(c(2:Tsim_TS))
lablist<-as.vector(c(2,4,6,8,10,12,14,16,18,20))
axis(1, at=seq(time_TS, rowTot_TS, by=365), labels = FALSE)
text(seq(time_TS, rowTot_TS, by=2*365),  par("usr")[3] ,labels = lablist, pos = 1, xpd = TRUE)
lines(TS_mr[,1]+TS_mr_sd[,1] , lwd=1, lty=2)
lines(TS_mr[,1]-TS_mr_sd [,1], lwd=1, lty=2)
xx = seq(time_TS, rowTot_TS, by=365)

x =1:rowTot_DRM
# 
plot(DRM_mr[,1], col ="black", type ="l",lwd = 2,xlim=c(0,rowTot_DRM), ylim=c(0,5000), main= "Devils River minnow ", ylab = "Total population [#]", xlab = "Time [d]",xaxt="n")
lablist<-as.vector(c(1:(Tsim_DRM-1)))
lablist<-as.vector(c(1,3,5,7,9,11,13,15,17,19))
axis(1, at=seq(time_DRM, rowTot_DRM, by=365*24), labels = FALSE)
text(seq(time_DRM, rowTot_DRM, by=2*365*24),  par("usr")[3] ,labels = lablist, pos = 1, xpd = TRUE)
lines(DRM_mr[,1]+DRM_mr_sd[,1] , lwd=1, lty=2)
lines(DRM_mr[,1]-DRM_mr_sd[,1] , lwd=1, lty=2)
xx = seq(time_DRM, rowTot_DRM, by=365*24)

x =1:rowTot_SD

plot(SD_mr[,1], col = "black" , type = "l",lwd = 2,xlim=c(0,rowTot_SD), ylim=c(0,8000), main= "Spikedace",  ylab = "Total population [#]", xlab = "Time [d]",xaxt="n")
lablist<-as.vector(c(2:Tsim_SD))
lablist<-as.vector(c(2,4,6,8,10,12,14,16,18,20))
axis(1, at=seq(time_SD, rowTot_SD, by=365), labels = FALSE)
text(seq(time_SD, rowTot_SD, by=2*365),  par("usr")[3] ,labels = lablist, pos = 1, xpd = TRUE)
lines(SD_mr[,1]+SD_mr_sd[,1] , lwd=1, lty=2)
lines(SD_mr[,1]-SD_mr_sd[,1] , lwd=1, lty=2)
xx = seq(time_SD, rowTot_SD, by=365)

x =1:rowTot_HC

plot(HC_mr[,1], col ="black", type = "l",lwd = 2,xlim=c(0,rowTot_HC), ylim=c(0,3000), main= "Humpback chub", ylab = "Total population [#]", xlab = "Time [d]",xaxt="n")
par(new=T)
lablist<-as.vector(c(2:Tsim_HC))
lablist<-as.vector(c(2,4,6,8,10,12,14,16,18,20))
axis(1, at=seq(time_HC, rowTot_HC, by=365), labels = FALSE)
text(seq(time_HC, rowTot_HC, by=2*365),  par("usr")[3] ,labels = lablist, pos = 1, xpd = TRUE)

lines(HC_mr[,1]+HC_mr_sd[,1] , lwd=1, lty=2)
lines(HC_mr[,1]-HC_mr_sd[,1] , lwd=1, lty=2)
xx = seq(time_HC, rowTot_HC, by=365)
# dev.off()
