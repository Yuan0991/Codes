rm(list=ls())
library(raster)
library(ncdf4)
library(climdex.pcic)
library(PCICt)
library(tidyverse)
library(lubridate)
library(colorRamps)
library(ggplot2)
library(viridis) 
library(ggsci)
library(grid)
library(plyr)
library(raster)
library(showtext)
font_add(family='Times',regular='C:/Windows/Fonts/times.ttf')
showtext_auto()
##############################
###############################

world <- shapefile("F:/Flux_paper/dataframe/world_shape/no_anarctic.shp")
Tmax_file <- list.files("E:/ERA5_T/max_T/",full.names=T)
tmp_raster  <- raster(Tmax_file[1],band=1)
tmp_raster  <- mask(tmp_raster,world)
tmp_data  <- rasterToPoints(tmp_raster)
index     <- tmp_data[,1:2]
##########################################GPP
RF_Tx       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Pcor/FLUXCOM/RF/GPP_R_tx.txt",head=F,sep=' ')
RF_NoTx     <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Pcor/FLUXCOM/RF/GPP_R_notx.txt",head=F,sep=' ')
MAR_Tx      <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Pcor/FLUXCOM/MAR/GPP_R_tx.txt",head=F,sep=' ')
MAR_NoTx    <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Pcor/FLUXCOM/MAR/GPP_R_notx.txt",head=F,sep=' ')
ANN_Tx      <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Pcor/FLUXCOM/ANN/GPP_R_tx.txt",head=F,sep=' ')
ANN_NoTx    <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Pcor/FLUXCOM/ANN/GPP_R_notx.txt",head=F,sep=' ')

######p_value

###############


FLUXCOM_GPP_TX   <- (RF_Tx+MAR_Tx+ANN_Tx)/3
FLUXCOM_GPP_NoTX <- (RF_NoTx+MAR_NoTx+ANN_NoTx)/3
##########################################TER
RF_Tx       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Pcor/FLUXCOM/RF/TER_R_tx.txt",head=F,sep=' ')
RF_NoTx     <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Pcor/FLUXCOM/RF/TER_R_notx.txt",head=F,sep=' ')
MAR_Tx      <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Pcor/FLUXCOM/MAR/TER_R_tx.txt",head=F,sep=' ')
MAR_NoTx    <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Pcor/FLUXCOM/MAR/TER_R_notx.txt",head=F,sep=' ')
ANN_Tx      <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Pcor/FLUXCOM/ANN/TER_R_tx.txt",head=F,sep=' ')
ANN_NoTx    <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Pcor/FLUXCOM/ANN/TER_R_notx.txt",head=F,sep=' ')

FLUXCOM_TER_TX   <- (RF_Tx+MAR_Tx+ANN_Tx)/3
FLUXCOM_TER_NoTX <- (RF_NoTx+MAR_NoTx+ANN_NoTx)/3
##########################################NEE
RF_Tx       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Pcor/FLUXCOM/RF/NEE_R_tx.txt",head=F,sep=' ')
RF_NoTx     <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Pcor/FLUXCOM/RF/NEE_R_notx.txt",head=F,sep=' ')
MAR_Tx      <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Pcor/FLUXCOM/MAR/NEE_R_tx.txt",head=F,sep=' ')
MAR_NoTx    <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Pcor/FLUXCOM/MAR/NEE_R_notx.txt",head=F,sep=' ')
ANN_Tx      <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Pcor/FLUXCOM/ANN/NEE_R_tx.txt",head=F,sep=' ')
ANN_NoTx    <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Pcor/FLUXCOM/ANN/NEE_R_notx.txt",head=F,sep=' ')

FLUXCOM_NEE_TX   <- (RF_Tx+MAR_Tx+ANN_Tx)/3
FLUXCOM_NEE_NoTX <- (RF_NoTx+MAR_NoTx+ANN_NoTx)/3

##########################################ESMs_GPP
CM_Tx       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Pcor/ESM/CMCC_CM/GPP_R_tx.txt",head=F,sep=' ')
CM_NoTx     <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Pcor/ESM/CMCC_CM/GPP_R_notx.txt",head=F,sep=' ')
ES_Tx       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Pcor/ESM/CMCC_ES/GPP_R_tx.txt",head=F,sep=' ')
ES_NoTx     <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Pcor/ESM/CMCC_ES/GPP_R_notx.txt",head=F,sep=' ')
LM_Tx       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Pcor/ESM/Nor_LM/GPP_R_tx.txt",head=F,sep=' ')
LM_NoTx     <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Pcor/ESM/Nor_LM/GPP_R_notx.txt",head=F,sep=' ')
MM_Tx       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Pcor/ESM/Nor_MM/GPP_R_tx.txt",head=F,sep=' ')
MM_NoTx     <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Pcor/ESM/Nor_MM/GPP_R_notx.txt",head=F,sep=' ')

ESMs_GPP_TX     <- (CM_Tx+ES_Tx+LM_Tx+MM_Tx)/4
ESMs_GPP_NoTX   <- (CM_NoTx+ES_NoTx+LM_NoTx+MM_NoTx)/4
##########################################ESMs_TER
CM_Tx       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Pcor/ESM/CMCC_CM/TER_R_tx.txt",head=F,sep=' ')
CM_NoTx     <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Pcor/ESM/CMCC_CM/TER_R_notx.txt",head=F,sep=' ')
ES_Tx       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Pcor/ESM/CMCC_ES/TER_R_tx.txt",head=F,sep=' ')
ES_NoTx     <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Pcor/ESM/CMCC_ES/TER_R_notx.txt",head=F,sep=' ')
LM_Tx       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Pcor/ESM/Nor_LM/TER_R_tx.txt",head=F,sep=' ')
LM_NoTx     <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Pcor/ESM/Nor_LM/TER_R_notx.txt",head=F,sep=' ')
MM_Tx       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Pcor/ESM/Nor_MM/TER_R_tx.txt",head=F,sep=' ')
MM_NoTx     <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Pcor/ESM/Nor_MM/TER_R_notx.txt",head=F,sep=' ')

ESMs_TER_TX     <- (CM_Tx+ES_Tx+LM_Tx+MM_Tx)/4
ESMs_TER_NoTX   <- (CM_NoTx+ES_NoTx+LM_NoTx+MM_NoTx)/4
##########################################ESMs_NEE
CM_Tx       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Pcor/ESM/CMCC_CM/NEE_R_tx.txt",head=F,sep=' ')
CM_NoTx     <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Pcor/ESM/CMCC_CM/NEE_R_notx.txt",head=F,sep=' ')
ES_Tx       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Pcor/ESM/CMCC_ES/NEE_R_tx.txt",head=F,sep=' ')
ES_NoTx     <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Pcor/ESM/CMCC_ES/NEE_R_notx.txt",head=F,sep=' ')
LM_Tx       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Pcor/ESM/Nor_LM/NEE_R_tx.txt",head=F,sep=' ')
LM_NoTx     <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Pcor/ESM/Nor_LM/NEE_R_notx.txt",head=F,sep=' ')
MM_Tx       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Pcor/ESM/Nor_MM/NEE_R_tx.txt",head=F,sep=' ')
MM_NoTx     <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Pcor/ESM/Nor_MM/NEE_R_notx.txt",head=F,sep=' ')
ESMs_NEE_TX     <- (CM_Tx+ES_Tx+LM_Tx+MM_Tx)/4
ESMs_NEE_NoTX   <- (CM_NoTx+ES_NoTx+LM_NoTx+MM_NoTx)/4

index_fun <- function(x){
    if (sum(is.na(x))==4) {
      return(NA)}
    else{
      y=abs(x)
      return(which(y==max(y)))
     }
    
    }

FLUXCOM_GPP_TX    <- apply(FLUXCOM_GPP_TX,1,FUN=index_fun)
FLUXCOM_GPP_NoTX  <- apply(FLUXCOM_GPP_NoTX,1,FUN=index_fun)
FLUXCOM_TER_TX    <- apply(FLUXCOM_TER_TX,1,FUN=index_fun)
FLUXCOM_TER_NoTX  <- apply(FLUXCOM_TER_NoTX,1,FUN=index_fun)
FLUXCOM_NEE_TX    <- apply(FLUXCOM_NEE_TX,1,FUN=index_fun)
FLUXCOM_NEE_NoTX  <- apply(FLUXCOM_NEE_NoTX,1,FUN=index_fun)

ESMs_GPP_TX       <- apply(ESMs_GPP_TX,1,FUN=index_fun)
ESMs_GPP_NoTX     <- apply(ESMs_GPP_NoTX,1,FUN=index_fun)
ESMs_TER_TX       <- apply(ESMs_TER_TX,1,FUN=index_fun)
ESMs_TER_NoTX     <- apply(ESMs_TER_NoTX,1,FUN=index_fun)
ESMs_NEE_TX       <- apply(ESMs_NEE_TX,1,FUN=index_fun)
ESMs_NEE_NoTX     <- apply(ESMs_NEE_NoTX,1,FUN=index_fun)
#########----------------------------------------------------------------------------GPP_field_sig_fluxcom
RF_Tx_p       <- read.table("F:/Flux_paper/Manuscript/Nature_Co_EV/Revision/dataframe/hossein/climate_pcor_field_sig/FieldSig/FieldSig/FLUXCOM/RF/GPP_P_tx_corrected.txt",head=F,sep='\t')
RF_NoTx_p     <- read.table("F:/Flux_paper/Manuscript/Nature_Co_EV/Revision/dataframe/hossein/climate_pcor_field_sig/FieldSig/FieldSig/FLUXCOM/RF/GPP_P_notx_corrected.txt",head=F,sep='\t')
MAR_Tx_p      <- read.table("F:/Flux_paper/Manuscript/Nature_Co_EV/Revision/dataframe/hossein/climate_pcor_field_sig/FieldSig/FieldSig/FLUXCOM/MAR/GPP_P_tx_corrected.txt",head=F,sep='\t')
MAR_NoTx_p    <- read.table("F:/Flux_paper/Manuscript/Nature_Co_EV/Revision/dataframe/hossein/climate_pcor_field_sig/FieldSig/FieldSig/FLUXCOM/MAR/GPP_P_notx_corrected.txt",head=F,sep='\t')
ANN_Tx_p      <- read.table("F:/Flux_paper/Manuscript/Nature_Co_EV/Revision/dataframe/hossein/climate_pcor_field_sig/FieldSig/FieldSig/FLUXCOM/ANN/GPP_P_tx_corrected.txt",head=F,sep='\t')
ANN_NoTx_p    <- read.table("F:/Flux_paper/Manuscript/Nature_Co_EV/Revision/dataframe/hossein/climate_pcor_field_sig/FieldSig/FieldSig/FLUXCOM/ANN/GPP_P_notx_corrected.txt",head=F,sep='\t')

RF_Tx_p       <- cbind(RF_Tx_p,1)
RF_NoTx_p     <- cbind(RF_NoTx_p,1)
MAR_Tx_p      <- cbind(MAR_Tx_p,1)
MAR_NoTx_p    <- cbind(MAR_NoTx_p,1)
ANN_Tx_p      <- cbind(ANN_Tx_p,1)
ANN_NoTx_p    <- cbind(ANN_NoTx_p,1)

FLUXCOM_GPP_TX[which(is.na(FLUXCOM_GPP_TX))]      <- 5
FLUXCOM_GPP_NoTX[which(is.na(FLUXCOM_GPP_NoTX))]  <- 5
FLUXCOM_TER_TX[which(is.na(FLUXCOM_TER_TX))]      <- 5
FLUXCOM_TER_NoTX[which(is.na(FLUXCOM_TER_NoTX))]  <- 5
FLUXCOM_NEE_TX[which(is.na(FLUXCOM_NEE_TX))]      <- 5
FLUXCOM_NEE_NoTX[which(is.na(FLUXCOM_NEE_NoTX))]  <- 5

find_p <- function(x,y){
    df_tmp <- c()
for (i in 1:length(x)){
    p_tmp   <- y[i,x[i]]
    df_tmp  <- rbind(df_tmp,p_tmp)
      }
     return(df_tmp)
}

RF_GPP_tx    <- find_p(FLUXCOM_GPP_TX,RF_Tx_p)
MAR_GPP_tx   <- find_p(FLUXCOM_GPP_TX,MAR_Tx_p) 
ANN_GPP_tx   <- find_p(FLUXCOM_GPP_TX,ANN_Tx_p)

RF_GPP_notx    <- find_p(FLUXCOM_GPP_NoTX,RF_NoTx_p)
MAR_GPP_notx   <- find_p(FLUXCOM_GPP_NoTX,MAR_NoTx_p) 
ANN_GPP_notx   <- find_p(FLUXCOM_GPP_NoTX,ANN_NoTx_p)

GPP_tx_p    <- cbind(RF_GPP_tx,MAR_GPP_tx,ANN_GPP_tx)
GPP_notx_p  <- cbind(RF_GPP_notx,MAR_GPP_notx,ANN_GPP_notx) 

FUN_p_num <- function(x){
   y <- length(which(x<0.05))
   return(y)
    }
FLUXCOM_GPP_P_tx_No <- apply(GPP_tx_p,1,FUN_p_num)
FLUXCOM_GPP_P_notx_No <- apply(GPP_notx_p,1,FUN_p_num)

tmp <- data.frame(x=FLUXCOM_GPP_TX,y=FLUXCOM_GPP_P_tx_No)
tmp$x[which(tmp$x==5)] <- 5
tmp$x[which(tmp$y<1)] <- 5 
FLUXCOM_GPP_P_tx_No <- tmp$x

tmp <- data.frame(x=FLUXCOM_GPP_NoTX,y=FLUXCOM_GPP_P_notx_No)
tmp$x[which(tmp$x==5)] <- 5
tmp$x[which(tmp$y<1)] <- 5 
FLUXCOM_GPP_P_notx_No <- tmp$x

#####################################################################################
#########----------------------------------------------------------------------------TER_field_sig_fluxcom
RF_Tx_p       <- read.table("F:/Flux_paper/Manuscript/Nature_Co_EV/Revision/dataframe/hossein/climate_pcor_field_sig/FieldSig/FieldSig/FLUXCOM/RF/TER_P_tx_corrected.txt",head=F,sep='\t')
RF_NoTx_p     <- read.table("F:/Flux_paper/Manuscript/Nature_Co_EV/Revision/dataframe/hossein/climate_pcor_field_sig/FieldSig/FieldSig/FLUXCOM/RF/TER_P_notx_corrected.txt",head=F,sep='\t')
MAR_Tx_p      <- read.table("F:/Flux_paper/Manuscript/Nature_Co_EV/Revision/dataframe/hossein/climate_pcor_field_sig/FieldSig/FieldSig/FLUXCOM/MAR/TER_P_tx_corrected.txt",head=F,sep='\t')
MAR_NoTx_p    <- read.table("F:/Flux_paper/Manuscript/Nature_Co_EV/Revision/dataframe/hossein/climate_pcor_field_sig/FieldSig/FieldSig/FLUXCOM/MAR/TER_P_notx_corrected.txt",head=F,sep='\t')
ANN_Tx_p      <- read.table("F:/Flux_paper/Manuscript/Nature_Co_EV/Revision/dataframe/hossein/climate_pcor_field_sig/FieldSig/FieldSig/FLUXCOM/ANN/TER_P_tx_corrected.txt",head=F,sep='\t')
ANN_NoTx_p    <- read.table("F:/Flux_paper/Manuscript/Nature_Co_EV/Revision/dataframe/hossein/climate_pcor_field_sig/FieldSig/FieldSig/FLUXCOM/ANN/TER_P_notx_corrected.txt",head=F,sep='\t')

RF_Tx_p       <- cbind(RF_Tx_p,1)
RF_NoTx_p     <- cbind(RF_NoTx_p,1)
MAR_Tx_p      <- cbind(MAR_Tx_p,1)
MAR_NoTx_p    <- cbind(MAR_NoTx_p,1)
ANN_Tx_p      <- cbind(ANN_Tx_p,1)
ANN_NoTx_p    <- cbind(ANN_NoTx_p,1)

RF_GPP_tx    <- find_p(FLUXCOM_TER_TX,RF_Tx_p)
MAR_GPP_tx   <- find_p(FLUXCOM_TER_TX,MAR_Tx_p) 
ANN_GPP_tx   <- find_p(FLUXCOM_TER_TX,ANN_Tx_p)

RF_GPP_notx    <- find_p(FLUXCOM_TER_NoTX,RF_NoTx_p)
MAR_GPP_notx   <- find_p(FLUXCOM_TER_NoTX,MAR_NoTx_p) 
ANN_GPP_notx   <- find_p(FLUXCOM_TER_NoTX,ANN_NoTx_p)

GPP_tx_p    <- cbind(RF_GPP_tx,MAR_GPP_tx,ANN_GPP_tx)
GPP_notx_p  <- cbind(RF_GPP_notx,MAR_GPP_notx,ANN_GPP_notx) 


FLUXCOM_TER_P_tx_No <- apply(GPP_tx_p,1,FUN_p_num)
FLUXCOM_TER_P_notx_No <- apply(GPP_notx_p,1,FUN_p_num)

tmp <- data.frame(x=FLUXCOM_TER_TX,y=FLUXCOM_TER_P_tx_No)
tmp$x[which(tmp$x==5)] <- 5
tmp$x[which(tmp$y<1)] <- 5 
FLUXCOM_TER_P_tx_No <- tmp$x


tmp <- data.frame(x=FLUXCOM_TER_NoTX,y=FLUXCOM_TER_P_notx_No)
tmp$x[which(tmp$x==5)] <- 5
tmp$x[which(tmp$y<1)] <- 5 
FLUXCOM_TER_P_notx_No <- tmp$x

#########----------------------------------------------------------------------------NEE_field_sig_fluxcom
RF_Tx_p       <- read.table("F:/Flux_paper/Manuscript/Nature_Co_EV/Revision/dataframe/hossein/climate_pcor_field_sig/FieldSig/FieldSig/FLUXCOM/RF/NEE_P_tx_corrected.txt",head=F,sep='\t')
RF_NoTx_p     <- read.table("F:/Flux_paper/Manuscript/Nature_Co_EV/Revision/dataframe/hossein/climate_pcor_field_sig/FieldSig/FieldSig/FLUXCOM/RF/NEE_P_notx_corrected.txt",head=F,sep='\t')
MAR_Tx_p      <- read.table("F:/Flux_paper/Manuscript/Nature_Co_EV/Revision/dataframe/hossein/climate_pcor_field_sig/FieldSig/FieldSig/FLUXCOM/MAR/NEE_P_tx_corrected.txt",head=F,sep='\t')
MAR_NoTx_p    <- read.table("F:/Flux_paper/Manuscript/Nature_Co_EV/Revision/dataframe/hossein/climate_pcor_field_sig/FieldSig/FieldSig/FLUXCOM/MAR/NEE_P_notx_corrected.txt",head=F,sep='\t')
ANN_Tx_p      <- read.table("F:/Flux_paper/Manuscript/Nature_Co_EV/Revision/dataframe/hossein/climate_pcor_field_sig/FieldSig/FieldSig/FLUXCOM/ANN/NEE_P_tx_corrected.txt",head=F,sep='\t')
ANN_NoTx_p    <- read.table("F:/Flux_paper/Manuscript/Nature_Co_EV/Revision/dataframe/hossein/climate_pcor_field_sig/FieldSig/FieldSig/FLUXCOM/ANN/NEE_P_notx_corrected.txt",head=F,sep='\t')

RF_Tx_p       <- cbind(RF_Tx_p,1)
RF_NoTx_p     <- cbind(RF_NoTx_p,1)
MAR_Tx_p      <- cbind(MAR_Tx_p,1)
MAR_NoTx_p    <- cbind(MAR_NoTx_p,1)
ANN_Tx_p      <- cbind(ANN_Tx_p,1)
ANN_NoTx_p    <- cbind(ANN_NoTx_p,1)

RF_GPP_tx    <- find_p(FLUXCOM_NEE_TX,RF_Tx_p)
MAR_GPP_tx   <- find_p(FLUXCOM_NEE_TX,MAR_Tx_p) 
ANN_GPP_tx   <- find_p(FLUXCOM_NEE_TX,ANN_Tx_p)

RF_GPP_notx    <- find_p(FLUXCOM_NEE_NoTX,RF_NoTx_p)
MAR_GPP_notx   <- find_p(FLUXCOM_NEE_NoTX,MAR_NoTx_p) 
ANN_GPP_notx   <- find_p(FLUXCOM_NEE_NoTX,ANN_NoTx_p)

GPP_tx_p    <- cbind(RF_GPP_tx,MAR_GPP_tx,ANN_GPP_tx)
GPP_notx_p  <- cbind(RF_GPP_notx,MAR_GPP_notx,ANN_GPP_notx) 


FLUXCOM_NEE_P_tx_No <- apply(GPP_tx_p,1,FUN_p_num)
FLUXCOM_NEE_P_notx_No <- apply(GPP_notx_p,1,FUN_p_num)

tmp <- data.frame(x=FLUXCOM_NEE_TX,y=FLUXCOM_NEE_P_tx_No)
tmp$x[which(tmp$x==5)] <- 5
tmp$x[which(tmp$y<1)] <- 5 
FLUXCOM_NEE_P_tx_No <- tmp$x

tmp <- data.frame(x=FLUXCOM_TER_NoTX,y=FLUXCOM_NEE_P_notx_No)
tmp$x[which(tmp$x==5)] <- 5
tmp$x[which(tmp$y<1)] <- 5
FLUXCOM_NEE_P_notx_No <- tmp$x

#########################################################################################ESMs_GPP_field_sig

CM_Tx_p       <- read.table("F:/Flux_paper/Manuscript/Nature_Co_EV/Revision/dataframe/hossein/climate_pcor_field_sig/FieldSig/FieldSig/ESM/CMCC_CM/GPP_P_tx_corrected.txt",head=F,sep='\t')
CM_NoTx_p     <- read.table("F:/Flux_paper/Manuscript/Nature_Co_EV/Revision/dataframe/hossein/climate_pcor_field_sig/FieldSig/FieldSig/ESM/CMCC_CM/GPP_P_notx_corrected.txt",head=F,sep='\t')
ES_Tx_p       <- read.table("F:/Flux_paper/Manuscript/Nature_Co_EV/Revision/dataframe/hossein/climate_pcor_field_sig/FieldSig/FieldSig/ESM/CMCC_ES/GPP_P_tx_corrected.txt",head=F,sep='\t')
ES_NoTx_p     <- read.table("F:/Flux_paper/Manuscript/Nature_Co_EV/Revision/dataframe/hossein/climate_pcor_field_sig/FieldSig/FieldSig/ESM/CMCC_ES/GPP_P_notx_corrected.txt",head=F,sep='\t')
LM_Tx_p       <- read.table("F:/Flux_paper/Manuscript/Nature_Co_EV/Revision/dataframe/hossein/climate_pcor_field_sig/FieldSig/FieldSig/ESM/Nor_LM/GPP_P_tx_corrected.txt",head=F,sep='\t')
LM_NoTx_p     <- read.table("F:/Flux_paper/Manuscript/Nature_Co_EV/Revision/dataframe/hossein/climate_pcor_field_sig/FieldSig/FieldSig/ESM/Nor_LM/GPP_P_notx_corrected.txt",head=F,sep='\t')
MM_Tx_p       <- read.table("F:/Flux_paper/Manuscript/Nature_Co_EV/Revision/dataframe/hossein/climate_pcor_field_sig/FieldSig/FieldSig/ESM/Nor_MM/GPP_P_tx_corrected.txt",head=F,sep='\t')
MM_NoTx_p     <- read.table("F:/Flux_paper/Manuscript/Nature_Co_EV/Revision/dataframe/hossein/climate_pcor_field_sig/FieldSig/FieldSig/ESM/Nor_MM/GPP_P_notx_corrected.txt",head=F,sep='\t')


CM_Tx_p       <- cbind(CM_Tx_p,1)
CM_NoTx_p     <- cbind(CM_NoTx_p,1)
ES_Tx_p      <- cbind(ES_Tx_p,1)
ES_NoTx_p    <- cbind(ES_NoTx_p,1)
LM_Tx_p      <- cbind(LM_Tx_p,1)
LM_NoTx_p    <- cbind(LM_NoTx_p,1)
MM_Tx_p      <- cbind(MM_Tx_p,1)
MM_NoTx_p    <- cbind(MM_NoTx_p,1)





ESMs_GPP_TX[which(is.na(ESMs_GPP_TX))]      <- 5
ESMs_GPP_NoTX[which(is.na(ESMs_GPP_NoTX))]  <- 5
ESMs_TER_TX[which(is.na(ESMs_TER_TX))]      <- 5
ESMs_TER_NoTX[which(is.na(ESMs_TER_NoTX))]  <- 5
ESMs_NEE_TX[which(is.na(ESMs_NEE_TX))]      <- 5
ESMs_NEE_NoTX[which(is.na(ESMs_NEE_NoTX))]  <- 5

CM_GPP_tx    <- find_p(ESMs_GPP_TX,CM_Tx_p)
ES_GPP_tx   <- find_p(ESMs_GPP_TX,ES_Tx_p) 
LM_GPP_tx   <- find_p(ESMs_GPP_TX,LM_Tx_p)
MM_GPP_tx   <- find_p(ESMs_GPP_TX,MM_Tx_p)

CM_GPP_notx    <- find_p(ESMs_GPP_NoTX,CM_NoTx_p)
ES_GPP_notx   <- find_p(ESMs_GPP_NoTX,ES_NoTx_p) 
LM_GPP_notx   <- find_p(ESMs_GPP_NoTX,LM_NoTx_p)
MM_GPP_notx   <- find_p(ESMs_GPP_NoTX,MM_NoTx_p)


GPP_tx_p    <- cbind(CM_GPP_tx,ES_GPP_tx,LM_GPP_tx,MM_GPP_tx)
GPP_notx_p  <- cbind(CM_GPP_notx,ES_GPP_notx,LM_GPP_notx,MM_GPP_notx) 

FUN_p_num <- function(x){
   y <- length(which(x<0.05))
   return(y)
    }
ESMs_GPP_P_tx_No <- apply(GPP_tx_p,1,FUN_p_num)
ESMs_GPP_P_notx_No <- apply(GPP_notx_p,1,FUN_p_num)


tmp <- data.frame(x=ESMs_GPP_TX,y=ESMs_GPP_P_tx_No)
tmp$x[which(tmp$x==5)] <- 5
tmp$x[which(tmp$y<1)] <- 5 
ESMs_GPP_P_tx_No <- tmp$x


tmp <- data.frame(x=ESMs_GPP_NoTX,y=ESMs_GPP_P_notx_No)
tmp$x[which(tmp$x==5)] <- 5
tmp$x[which(tmp$y<1)] <- 5 
ESMs_GPP_P_notx_No <- tmp$x

###############################################################
#########################################################################################ESMs_TER_field_sig

CM_Tx_p       <- read.table("F:/Flux_paper/Manuscript/Nature_Co_EV/Revision/dataframe/hossein/climate_pcor_field_sig/FieldSig/FieldSig/ESM/CMCC_CM/TER_P_tx_corrected.txt",head=F,sep='\t')
CM_NoTx_p     <- read.table("F:/Flux_paper/Manuscript/Nature_Co_EV/Revision/dataframe/hossein/climate_pcor_field_sig/FieldSig/FieldSig/ESM/CMCC_CM/TER_P_notx_corrected.txt",head=F,sep='\t')
ES_Tx_p       <- read.table("F:/Flux_paper/Manuscript/Nature_Co_EV/Revision/dataframe/hossein/climate_pcor_field_sig/FieldSig/FieldSig/ESM/CMCC_ES/TER_P_tx_corrected.txt",head=F,sep='\t')
ES_NoTx_p     <- read.table("F:/Flux_paper/Manuscript/Nature_Co_EV/Revision/dataframe/hossein/climate_pcor_field_sig/FieldSig/FieldSig/ESM/CMCC_ES/TER_P_notx_corrected.txt",head=F,sep='\t')
LM_Tx_p       <- read.table("F:/Flux_paper/Manuscript/Nature_Co_EV/Revision/dataframe/hossein/climate_pcor_field_sig/FieldSig/FieldSig/ESM/Nor_LM/TER_P_tx_corrected.txt",head=F,sep='\t')
LM_NoTx_p     <- read.table("F:/Flux_paper/Manuscript/Nature_Co_EV/Revision/dataframe/hossein/climate_pcor_field_sig/FieldSig/FieldSig/ESM/Nor_LM/TER_P_notx_corrected.txt",head=F,sep='\t')
MM_Tx_p       <- read.table("F:/Flux_paper/Manuscript/Nature_Co_EV/Revision/dataframe/hossein/climate_pcor_field_sig/FieldSig/FieldSig/ESM/Nor_MM/TER_P_tx_corrected.txt",head=F,sep='\t')
MM_NoTx_p     <- read.table("F:/Flux_paper/Manuscript/Nature_Co_EV/Revision/dataframe/hossein/climate_pcor_field_sig/FieldSig/FieldSig/ESM/Nor_MM/TER_P_notx_corrected.txt",head=F,sep='\t')


CM_Tx_p       <- cbind(CM_Tx_p,1)
CM_NoTx_p     <- cbind(CM_NoTx_p,1)
ES_Tx_p      <- cbind(ES_Tx_p,1)
ES_NoTx_p    <- cbind(ES_NoTx_p,1)
LM_Tx_p      <- cbind(LM_Tx_p,1)
LM_NoTx_p    <- cbind(LM_NoTx_p,1)
MM_Tx_p      <- cbind(MM_Tx_p,1)
MM_NoTx_p    <- cbind(MM_NoTx_p,1)

CM_GPP_tx    <- find_p(ESMs_TER_TX,CM_Tx_p)
ES_GPP_tx   <- find_p(ESMs_TER_TX,ES_Tx_p) 
LM_GPP_tx   <- find_p(ESMs_TER_TX,LM_Tx_p)
MM_GPP_tx   <- find_p(ESMs_TER_TX,MM_Tx_p)

CM_GPP_notx    <- find_p(ESMs_TER_NoTX,CM_NoTx_p)
ES_GPP_notx   <- find_p(ESMs_TER_NoTX,ES_NoTx_p) 
LM_GPP_notx   <- find_p(ESMs_TER_NoTX,LM_NoTx_p)
MM_GPP_notx   <- find_p(ESMs_TER_NoTX,MM_NoTx_p)


GPP_tx_p    <- cbind(CM_GPP_tx,ES_GPP_tx,LM_GPP_tx,MM_GPP_tx)
GPP_notx_p  <- cbind(CM_GPP_notx,ES_GPP_notx,LM_GPP_notx,MM_GPP_notx) 


ESMs_TER_P_tx_No <- apply(GPP_tx_p,1,FUN_p_num)
ESMs_TER_P_notx_No <- apply(GPP_notx_p,1,FUN_p_num)

tmp <- data.frame(x=ESMs_TER_TX,y=ESMs_TER_P_tx_No)
tmp$x[which(tmp$x==5)] <- 5
tmp$x[which(tmp$y<1)] <- 5 
ESMs_TER_P_tx_No <- tmp$x


tmp <- data.frame(x=ESMs_TER_NoTX,y=ESMs_TER_P_notx_No)
tmp$x[which(tmp$x==5)] <- 5
tmp$x[which(tmp$y<1)] <- 5 
ESMs_TER_P_notx_No <- tmp$x


#########################################################################################ESMs_NEE_field_sig

CM_Tx_p       <- read.table("F:/Flux_paper/Manuscript/Nature_Co_EV/Revision/dataframe/hossein/climate_pcor_field_sig/FieldSig/FieldSig/ESM/CMCC_CM/NEE_P_tx_corrected.txt",head=F,sep='\t')
CM_NoTx_p     <- read.table("F:/Flux_paper/Manuscript/Nature_Co_EV/Revision/dataframe/hossein/climate_pcor_field_sig/FieldSig/FieldSig/ESM/CMCC_CM/NEE_P_notx_corrected.txt",head=F,sep='\t')
ES_Tx_p       <- read.table("F:/Flux_paper/Manuscript/Nature_Co_EV/Revision/dataframe/hossein/climate_pcor_field_sig/FieldSig/FieldSig/ESM/CMCC_ES/NEE_P_tx_corrected.txt",head=F,sep='\t')
ES_NoTx_p     <- read.table("F:/Flux_paper/Manuscript/Nature_Co_EV/Revision/dataframe/hossein/climate_pcor_field_sig/FieldSig/FieldSig/ESM/CMCC_ES/NEE_P_notx_corrected.txt",head=F,sep='\t')
LM_Tx_p       <- read.table("F:/Flux_paper/Manuscript/Nature_Co_EV/Revision/dataframe/hossein/climate_pcor_field_sig/FieldSig/FieldSig/ESM/Nor_LM/NEE_P_tx_corrected.txt",head=F,sep='\t')
LM_NoTx_p     <- read.table("F:/Flux_paper/Manuscript/Nature_Co_EV/Revision/dataframe/hossein/climate_pcor_field_sig/FieldSig/FieldSig/ESM/Nor_LM/NEE_P_notx_corrected.txt",head=F,sep='\t')
MM_Tx_p       <- read.table("F:/Flux_paper/Manuscript/Nature_Co_EV/Revision/dataframe/hossein/climate_pcor_field_sig/FieldSig/FieldSig/ESM/Nor_MM/NEE_P_tx_corrected.txt",head=F,sep='\t')
MM_NoTx_p     <- read.table("F:/Flux_paper/Manuscript/Nature_Co_EV/Revision/dataframe/hossein/climate_pcor_field_sig/FieldSig/FieldSig/ESM/Nor_MM/NEE_P_notx_corrected.txt",head=F,sep='\t')


CM_Tx_p       <- cbind(CM_Tx_p,1)
CM_NoTx_p     <- cbind(CM_NoTx_p,1)
ES_Tx_p      <- cbind(ES_Tx_p,1)
ES_NoTx_p    <- cbind(ES_NoTx_p,1)
LM_Tx_p      <- cbind(LM_Tx_p,1)
LM_NoTx_p    <- cbind(LM_NoTx_p,1)
MM_Tx_p      <- cbind(MM_Tx_p,1)
MM_NoTx_p    <- cbind(MM_NoTx_p,1)






CM_GPP_tx    <- find_p(ESMs_NEE_TX,CM_Tx_p)
ES_GPP_tx   <- find_p(ESMs_NEE_TX,ES_Tx_p) 
LM_GPP_tx   <- find_p(ESMs_NEE_TX,LM_Tx_p)
MM_GPP_tx   <- find_p(ESMs_NEE_TX,MM_Tx_p)

CM_GPP_notx    <- find_p(ESMs_NEE_NoTX,CM_NoTx_p)
ES_GPP_notx   <- find_p(ESMs_NEE_NoTX,ES_NoTx_p) 
LM_GPP_notx   <- find_p(ESMs_NEE_NoTX,LM_NoTx_p)
MM_GPP_notx   <- find_p(ESMs_NEE_NoTX,MM_NoTx_p)


GPP_tx_p    <- cbind(CM_GPP_tx,ES_GPP_tx,LM_GPP_tx,MM_GPP_tx)
GPP_notx_p  <- cbind(CM_GPP_notx,ES_GPP_notx,LM_GPP_notx,MM_GPP_notx) 


ESMs_NEE_P_tx_No <- apply(GPP_tx_p,1,FUN_p_num)
ESMs_NEE_P_notx_No <- apply(GPP_notx_p,1,FUN_p_num)


tmp <- data.frame(x=ESMs_NEE_TX,y=ESMs_NEE_P_tx_No)
tmp$x[which(tmp$x==5)] <- 5
tmp$x[which(tmp$y<1)] <- 5 
ESMs_NEE_P_tx_No <- tmp$x


tmp <- data.frame(x=ESMs_NEE_NoTX,y=ESMs_NEE_P_notx_No)
tmp$x[which(tmp$x==5)] <- 5
tmp$x[which(tmp$y<1)] <- 5 
ESMs_NEE_P_notx_No <- tmp$x




###########################################################################Inv40
#tmp_Inv40     <- data.frame(Value=Diff_Inv_40,Lat=index[,2],Type="Inv2020")
#tmp_Inv33     <- data.frame(Value=Diff_Inv_33,Lat=index[,2],Type="Inv2013")
#tmp_FLUXCOM   <- data.frame(Value=Diff_FLUXCOM,Lat=index[,2],Type="FLUXCOM")
#tmp_ESMs      <- data.frame(Value=Diff_ESMs,Lat=index[,2],Type="ESMs")
#Lat_range     <- seq(-54.5,83.5,2)
#tmp_data      <- rbind(tmp_Inv40,tmp_Inv33,tmp_FLUXCOM,tmp_ESMs)
#in_val        <- findInterval(tmp_data$Lat,Lat_range)
#tmp_1         <- data.frame(tmp_data,range=in_val)
#df_tmp        <- ddply(tmp_1,.(Type,range),summarize,value=mean(Value),sd=sd(Value),Lat=mean(Lat))
############################--------------------------------------
#C_range      <- c(-60,-23.5,23.5,89)
#in_val       <- findInterval(tmp_data$Lat,C_range)
#tmp_2        <- data.frame(tmp_data,range=in_val)
#df_tmp       <- ddply(tmp_2,.(Type,range),summarize,value=sum(Value))



###################################################################################
tmp_data       <- data.frame(x=index[,1],y=index[,2],z=FLUXCOM_GPP_P_tx_No)
tmp_raster     <- rasterFromXYZ(tmp_data)
projection(tmp_raster) <- c("+proj=longlat +ellps=WGS84 +no_defs")
tmp_raster     <- raster::projectRaster(tmp_raster,crs="+proj=robin",method="ngb")
df_F_GPP_TX_no    <- data.frame(rasterToPoints(tmp_raster),Type="FLUXCOM GPP",Class="FLUXCOM WE")
###################################################################################
tmp_data       <- data.frame(x=index[,1],y=index[,2],z=FLUXCOM_GPP_P_notx_No)
tmp_raster     <- rasterFromXYZ(tmp_data)
projection(tmp_raster) <- c("+proj=longlat +ellps=WGS84 +no_defs")
tmp_raster     <- raster::projectRaster(tmp_raster,crs="+proj=robin",method="ngb")
df_F_GPP_NoTX_no    <- data.frame(rasterToPoints(tmp_raster),Type="FLUXCOM GPP",Class="FLUXCOM NonWE")
###################################################################################
tmp_data       <- data.frame(x=index[,1],y=index[,2],z=FLUXCOM_TER_P_tx_No)
tmp_raster     <- rasterFromXYZ(tmp_data)
projection(tmp_raster) <- c("+proj=longlat +ellps=WGS84 +no_defs")
tmp_raster     <- raster::projectRaster(tmp_raster,crs="+proj=robin",method="ngb")
df_F_TER_TX_no    <- data.frame(rasterToPoints(tmp_raster),Type="FLUXCOM TER",Class="FLUXCOM WE")
###################################################################################
tmp_data       <- data.frame(x=index[,1],y=index[,2],z=FLUXCOM_TER_P_notx_No)
tmp_raster     <- rasterFromXYZ(tmp_data)
projection(tmp_raster) <- c("+proj=longlat +ellps=WGS84 +no_defs")
tmp_raster     <- raster::projectRaster(tmp_raster,crs="+proj=robin",method="ngb")
df_F_TER_NoTX_no    <- data.frame(rasterToPoints(tmp_raster),Type="FLUXCOM TER",Class="FLUXCOM NonWE")
###################################################################################
tmp_data       <- data.frame(x=index[,1],y=index[,2],z=FLUXCOM_NEE_P_tx_No)
tmp_raster     <- rasterFromXYZ(tmp_data)
projection(tmp_raster) <- c("+proj=longlat +ellps=WGS84 +no_defs")
tmp_raster     <- raster::projectRaster(tmp_raster,crs="+proj=robin",method="ngb")
df_F_NEE_TX_no    <- data.frame(rasterToPoints(tmp_raster),Type="FLUXCOM NEE",Class="FLUXCOM WE")
###################################################################################
tmp_data       <- data.frame(x=index[,1],y=index[,2],z=FLUXCOM_NEE_P_notx_No)
tmp_raster     <- rasterFromXYZ(tmp_data)
projection(tmp_raster) <- c("+proj=longlat +ellps=WGS84 +no_defs")
tmp_raster     <- raster::projectRaster(tmp_raster,crs="+proj=robin",method="ngb")
df_F_NEE_NoTX_no    <- data.frame(rasterToPoints(tmp_raster),Type="FLUXCOM NEE",Class="FLUXCOM NonWE")
###################################################################################
tmp_data       <- data.frame(x=index[,1],y=index[,2],z=ESMs_GPP_P_tx_No)
tmp_raster     <- rasterFromXYZ(tmp_data)
projection(tmp_raster) <- c("+proj=longlat +ellps=WGS84 +no_defs")
tmp_raster     <- raster::projectRaster(tmp_raster,crs="+proj=robin",method="ngb")
df_E_GPP_TX_no    <- data.frame(rasterToPoints(tmp_raster),Type="ESMs GPP",Class="ESMs WE")
###################################################################################
tmp_data       <- data.frame(x=index[,1],y=index[,2],z=ESMs_GPP_P_notx_No)
tmp_raster     <- rasterFromXYZ(tmp_data)
projection(tmp_raster) <- c("+proj=longlat +ellps=WGS84 +no_defs")
tmp_raster     <- raster::projectRaster(tmp_raster,crs="+proj=robin",method="ngb")
df_E_GPP_NoTX_no    <- data.frame(rasterToPoints(tmp_raster),Type="ESMs GPP",Class="ESMs NonWE")
###################################################################################
tmp_data       <- data.frame(x=index[,1],y=index[,2],z=ESMs_TER_P_tx_No)
tmp_raster     <- rasterFromXYZ(tmp_data)
projection(tmp_raster) <- c("+proj=longlat +ellps=WGS84 +no_defs")
tmp_raster     <- raster::projectRaster(tmp_raster,crs="+proj=robin",method="ngb")
df_E_TER_TX_no    <- data.frame(rasterToPoints(tmp_raster),Type="ESMs TER",Class="ESMs WE")
###################################################################################
tmp_data       <- data.frame(x=index[,1],y=index[,2],z=ESMs_TER_P_notx_No)
tmp_raster     <- rasterFromXYZ(tmp_data)
projection(tmp_raster) <- c("+proj=longlat +ellps=WGS84 +no_defs")
tmp_raster     <- raster::projectRaster(tmp_raster,crs="+proj=robin",method="ngb")
df_E_TER_NoTX_no    <- data.frame(rasterToPoints(tmp_raster),Type="ESMs TER",Class="ESMs NonWE")
###################################################################################
tmp_data       <- data.frame(x=index[,1],y=index[,2],z=ESMs_NEE_P_tx_No)
tmp_raster     <- rasterFromXYZ(tmp_data)
projection(tmp_raster) <- c("+proj=longlat +ellps=WGS84 +no_defs")
tmp_raster     <- raster::projectRaster(tmp_raster,crs="+proj=robin",method="ngb")
df_E_NEE_TX_no    <- data.frame(rasterToPoints(tmp_raster),Type="ESMs NEE",Class="ESMs WE")
###################################################################################
tmp_data       <- data.frame(x=index[,1],y=index[,2],z=ESMs_NEE_P_notx_No)
tmp_raster     <- rasterFromXYZ(tmp_data)
projection(tmp_raster) <- c("+proj=longlat +ellps=WGS84 +no_defs")
tmp_raster     <- raster::projectRaster(tmp_raster,crs="+proj=robin",method="ngb")
df_E_NEE_NoTX_no    <- data.frame(rasterToPoints(tmp_raster),Type="ESMs NEE",Class="ESMs NonWE")
##########------------------------------------------------------------------------ 


###################################################################################
tmp_data       <- data.frame(x=index[,1],y=index[,2],z=FLUXCOM_GPP_TX)
tmp_raster     <- rasterFromXYZ(tmp_data)
projection(tmp_raster) <- c("+proj=longlat +ellps=WGS84 +no_defs")
tmp_raster     <- raster::projectRaster(tmp_raster,crs="+proj=robin",method="ngb")
df_F_GPP_TX    <- data.frame(rasterToPoints(tmp_raster),Type="FLUXCOM GPP",Class="WE")
###################################################################################
tmp_data       <- data.frame(x=index[,1],y=index[,2],z=FLUXCOM_GPP_NoTX)
tmp_raster     <- rasterFromXYZ(tmp_data)
projection(tmp_raster) <- c("+proj=longlat +ellps=WGS84 +no_defs")
tmp_raster     <- raster::projectRaster(tmp_raster,crs="+proj=robin",method="ngb")
df_F_GPP_NoTX    <- data.frame(rasterToPoints(tmp_raster),Type="FLUXCOM GPP",Class="NonWE")
###################################################################################
tmp_data       <- data.frame(x=index[,1],y=index[,2],z=FLUXCOM_TER_TX)
tmp_raster     <- rasterFromXYZ(tmp_data)
projection(tmp_raster) <- c("+proj=longlat +ellps=WGS84 +no_defs")
tmp_raster     <- raster::projectRaster(tmp_raster,crs="+proj=robin",method="ngb")
df_F_TER_TX    <- data.frame(rasterToPoints(tmp_raster),Type="FLUXCOM TER",Class="WE")
###################################################################################
tmp_data       <- data.frame(x=index[,1],y=index[,2],z=FLUXCOM_TER_NoTX)
tmp_raster     <- rasterFromXYZ(tmp_data)
projection(tmp_raster) <- c("+proj=longlat +ellps=WGS84 +no_defs")
tmp_raster     <- raster::projectRaster(tmp_raster,crs="+proj=robin",method="ngb")
df_F_TER_NoTX    <- data.frame(rasterToPoints(tmp_raster),Type="FLUXCOM TER",Class="NonWE")
###################################################################################
tmp_data       <- data.frame(x=index[,1],y=index[,2],z=FLUXCOM_NEE_TX)
tmp_raster     <- rasterFromXYZ(tmp_data)
projection(tmp_raster) <- c("+proj=longlat +ellps=WGS84 +no_defs")
tmp_raster     <- raster::projectRaster(tmp_raster,crs="+proj=robin",method="ngb")
df_F_NEE_TX    <- data.frame(rasterToPoints(tmp_raster),Type="FLUXCOM NEE", Class="WE")
###################################################################################
tmp_data       <- data.frame(x=index[,1],y=index[,2],z=FLUXCOM_NEE_NoTX)
tmp_raster     <- rasterFromXYZ(tmp_data)
projection(tmp_raster) <- c("+proj=longlat +ellps=WGS84 +no_defs")
tmp_raster     <- raster::projectRaster(tmp_raster,crs="+proj=robin",method="ngb")
df_F_NEE_NoTX    <- data.frame(rasterToPoints(tmp_raster),Type="FLUXCOM NEE",Class="NonWE")
###################################################################################
tmp_data       <- data.frame(x=index[,1],y=index[,2],z=ESMs_GPP_TX)
tmp_raster     <- rasterFromXYZ(tmp_data)
projection(tmp_raster) <- c("+proj=longlat +ellps=WGS84 +no_defs")
tmp_raster     <- raster::projectRaster(tmp_raster,crs="+proj=robin",method="ngb")
df_E_GPP_TX    <- data.frame(rasterToPoints(tmp_raster),Type="ESMs GPP",Class="WE")
###################################################################################
tmp_data       <- data.frame(x=index[,1],y=index[,2],z=ESMs_GPP_NoTX)
tmp_raster     <- rasterFromXYZ(tmp_data)
projection(tmp_raster) <- c("+proj=longlat +ellps=WGS84 +no_defs")
tmp_raster     <- raster::projectRaster(tmp_raster,crs="+proj=robin",method="ngb")
df_E_GPP_NoTX    <- data.frame(rasterToPoints(tmp_raster),Type="ESMs GPP",Class="NonWE")
###################################################################################
tmp_data       <- data.frame(x=index[,1],y=index[,2],z=ESMs_TER_TX)
tmp_raster     <- rasterFromXYZ(tmp_data)
projection(tmp_raster) <- c("+proj=longlat +ellps=WGS84 +no_defs")
tmp_raster     <- raster::projectRaster(tmp_raster,crs="+proj=robin",method="ngb")
df_E_TER_TX    <- data.frame(rasterToPoints(tmp_raster),Type="ESMs TER",Class="WE")
###################################################################################
tmp_data       <- data.frame(x=index[,1],y=index[,2],z=ESMs_TER_NoTX)
tmp_raster     <- rasterFromXYZ(tmp_data)
projection(tmp_raster) <- c("+proj=longlat +ellps=WGS84 +no_defs")
tmp_raster     <- raster::projectRaster(tmp_raster,crs="+proj=robin",method="ngb")
df_E_TER_NoTX    <- data.frame(rasterToPoints(tmp_raster),Type="ESMs TER",Class="NonWE")
###################################################################################
tmp_data       <- data.frame(x=index[,1],y=index[,2],z=ESMs_NEE_TX)
tmp_raster     <- rasterFromXYZ(tmp_data)
projection(tmp_raster) <- c("+proj=longlat +ellps=WGS84 +no_defs")
tmp_raster     <- raster::projectRaster(tmp_raster,crs="+proj=robin",method="ngb")
df_E_NEE_TX    <- data.frame(rasterToPoints(tmp_raster),Type="ESMs NEE",Class="WE")
###################################################################################
tmp_data       <- data.frame(x=index[,1],y=index[,2],z=ESMs_NEE_NoTX)
tmp_raster     <- rasterFromXYZ(tmp_data)
projection(tmp_raster) <- c("+proj=longlat +ellps=WGS84 +no_defs")
tmp_raster     <- raster::projectRaster(tmp_raster,crs="+proj=robin",method="ngb")
df_E_NEE_NoTX    <- data.frame(rasterToPoints(tmp_raster),Type="ESMs NEE",Class="NonWE")
##########------------------------------------------------------------------------


#df_data      <- rbind(df_F_GPP_TX,df_F_GPP_NoTX,df_F_TER_TX,df_F_TER_NoTX,df_F_NEE_TX,df_F_NEE_NoTX,
#                      df_E_GPP_TX,df_E_GPP_NoTX,df_E_TER_TX,df_E_TER_NoTX,df_E_NEE_TX,df_E_NEE_NoTX)

#df_data$Type <- factor(df_data$Type,levels=c("FLUXCOM_GPP_TX","FLUXCOM_TER_TX","FLUXCOM_NEE_TX","FLUXCOM_GPP_NoTX","FLUXCOM_TER_NoTX","FLUXCOM_NEE_NoTX",
#                        "ESMs_GPP_TX","ESMs_TER_TX","ESMs_NEE_TX","ESMs_GPP_NoTX","ESMs_TER_NoTX","ESMs_NEE_NoTX")) 

#df_data$z[which(df_data$z < -2.5)]=-2.49
#df_data$z[which(df_data$z > 2.5)] =2.49
#######################_--------------------------------------------------------------------------------


#df_data$z <- factor(df_data$z,levels=c("1","2","3","4"))
#################################################################
df_data_p_no_FLUXCOM      <- rbind(df_F_GPP_TX_no,df_F_GPP_NoTX_no,df_F_TER_TX_no,df_F_TER_NoTX_no,df_F_NEE_TX_no,df_F_NEE_NoTX_no)

df_data_p_no_ESMs      <- rbind(df_E_GPP_TX_no,df_E_GPP_NoTX_no,df_E_TER_TX_no,df_E_TER_NoTX_no,df_E_NEE_TX_no,df_E_NEE_NoTX_no)

df_data_p_no_FLUXCOM$Type <- factor(df_data_p_no_FLUXCOM$Type,levels=c("FLUXCOM GPP","FLUXCOM TER","FLUXCOM NEE"))
df_data_p_no_ESMs$Type <- factor(df_data_p_no_ESMs$Type,levels=c("ESMs GPP","ESMs TER","ESMs NEE"))


df_data_p_no_FLUXCOM$Class <- factor(df_data_p_no_FLUXCOM$Class,levels=c("FLUXCOM WE","FLUXCOM NonWE")) 
df_data_p_no_ESMs$Class <- factor(df_data_p_no_ESMs$Class,levels=c("ESMs WE","ESMs NonWE")) 


#df_data$z[which(df_data$z < -2.5)]=-2.49
#df_data$z[which(df_data$z > 2.5)] =2.49
#######################_--------------------------------------------------------------------------------
df_data_p_no_FLUXCOM$z[which(df_data_p_no_FLUXCOM$z==5)] <- NA 
df_data_p_no_ESMs$z[which(df_data_p_no_ESMs$z==5)] <- NA 

#df_data$z <- factor(df_data$z,levels=c("1","2","3","4"))

label_data_FLUXCOM <- data.frame(x=rep(-13000000,times=6),y=rep(900000,times=6),lab=c("85%","80%","71%","70%","76%","50%"),Type=rep(unique(df_data_p_no_FLUXCOM$Type),times=2),
                                 Class=rep(c("FLUXCOM WE", "FLUXCOM NonWE"),each=3))
label_data_ESMs <- data.frame(x=rep(-13000000,times=6),y=rep(900000,times=6),lab=c("47%","58%","34%","12%","17%","0%"),Type=rep(unique(df_data_p_no_ESMs$Type),times=2),
                                 Class=rep(c("ESMs WE", "ESMs NonWE"),each=3))

label_data_FLUXCOM_ <- data.frame(x=rep(7000000,times=6),y=rep(-4000000,times=6),lab=c("a","b","c","d","e","f"),Type=rep(unique(df_data_p_no_FLUXCOM$Type),times=2),
                            Class=rep(c("FLUXCOM WE", "FLUXCOM NonWE"),each=3))

label_data_ESMs_ <- data.frame(x=rep(7000000,times=6),y=rep(-4000000,times=6),lab=c("g","h","i","j","k","l"),Type=rep(unique(df_data_p_no_ESMs$Type),times=2),
                            Class=rep(c("ESMs WE", "ESMs NonWE"),each=3))

label_data_FLUXCOM$Class=factor(label_data_FLUXCOM$Class,levels=c("FLUXCOM WE", "FLUXCOM NonWE"))
label_data_ESMs$Class=factor(label_data_ESMs$Class,levels=c("ESMs WE", "ESMs NonWE"))

label_data_FLUXCOM_$Class=factor(label_data_FLUXCOM_$Class,levels=c("FLUXCOM WE", "FLUXCOM NonWE"))
label_data_ESMs_$Class=factor(label_data_ESMs_$Class,levels=c("ESMs WE", "ESMs NonWE"))


##########_--------------------------------------------------------------------------------------------
pdf(file = "F:/Flux_paper/Manuscript/Nature_Co_EV/Revision/third_round/Figures/Figure4.pdf", width=6.6, height=5.)   
pushViewport(viewport(layout=grid.layout(100,100)))
       
#windowsFonts(Times_New_Roman = windowsFont("Times New Roman"))

world <- shapefile("F:/Flux_paper/dataframe/world_shape/World_Continents.shp")
world     <- spTransform(world, CRS("+proj=robin"))
world <- fortify(world)
box <- shapefile("F:/Flux_paper/dataframe/world_shape/ne_110m_populated_places/ne_110m_wgs84_bounding_box.shp")
box <- crop(box,c(-180,180,-60,90))
box     <- spTransform(box, CRS("+proj=robin"))
box <- fortify(box)
theme_opts <- list(theme(
                      #  axis.text.y=element_text( size=6,family="Times"),
                      #  axis.text.x = element_text( size=6,family="Times"),
                        axis.text = element_blank(),
                     #   axis.title.y=element_text( size=6,family="Times"),
                    #    strip.text=element_blank(),
                     strip.text=element_text( size=7,family="Times"),
                        axis.ticks = element_line(size=0.0),
                        axis.ticks.length = unit(.00, "cm"),
                        legend.key.height=unit(0.5,"lines"),
                        legend.key.width=unit(1.5,"lines"),
                        legend.spacing = unit(-0.4, "cm"),
                        legend.position="bottom",
                      #  legend.direction="vertical",
                         plot.margin = unit(c(0.1,0.1,0.0,0.1),units="lines"),
                        panel.border = element_rect( colour = NA,fill=NA, size=0.),

                        panel.grid=element_blank(),
#legend.key = element_rect(colour = "black", fill = "transparent"),

                        strip.background = element_blank(),
                        axis.title.x=element_text( size=6.5,family="Times")

                      ))
df_data_p_no_FLUXCOM$z=factor(df_data_p_no_FLUXCOM$z)
h1 <-   ggplot(df_data_p_no_FLUXCOM,aes(x=x,y=y)) + theme_bw()+
        geom_tile(aes(x=x,y=y,fill=z))+
        geom_path(data=world, aes(long, lat, group=group, fill=NULL), linetype="solid", color="black",size = 0.2) +
        geom_path(data=box, aes(long, lat, group=group, fill=NULL), linetype="solid", color="black",size = 0.2) +
      #  scale_color_distiller(breaks=seq(1,4,1),palette="RdBu", direction=-1)+
        scale_fill_manual(values = c("#97A1FC","yellow","red", "#70D3E0"),na.value="gray80",labels=c("TEMP","SM","SW","VPD"))+
        scale_y_continuous(expand = c(0,0),limits=c(-6336039,8625155))+
        scale_x_continuous(expand = c(0,0))+
        xlab(NULL)+ylab(NULL)+
#     scale_color_startrek(breaks=seq(1,4,1),labels=c("TEMP","SM","SW","VPD"))+
#     scale_fill_startrek(breaks=seq(1,4,1),labels=c("TEMP","SM","SW","VPD"))+
  guides(fill= F)+  
        geom_text(data=label_data_FLUXCOM,aes(x=x,y=y,label=lab),family="Times",size=7/.pt,check_overlap = TRUE)+
        geom_text(data=label_data_FLUXCOM_,aes(x=x,y=y,label=lab),family="Times",size=7/.pt,check_overlap = TRUE,fontface="bold")+

   #     guides(fill= guide_legend(label.position = "bottom",title.vjust=-0.4,title=NULL,label=T,label.theme=element_text(size=6.5,colour="black",angle=0,family="Times")))+  
       facet_grid(Class~Type)+coord_equal() + 
        theme_opts

print(h1, vp=viewport(layout.pos.row=1:45, layout.pos.col=1:100))
df_data_p_no_ESMs$z=factor(df_data_p_no_ESMs$z)
h1 <-   ggplot(df_data_p_no_ESMs,aes(x=x,y=y)) + theme_bw()+
        geom_tile(aes(x=x,y=y,fill=z))+
        geom_path(data=world, aes(long, lat, group=group, fill=NULL), linetype="solid", color="black",size = 0.2) +
        geom_path(data=box, aes(long, lat, group=group, fill=NULL), linetype="solid", color="black",size = 0.2) +
      #  scale_color_distiller(breaks=seq(1,4,1),palette="RdBu", direction=-1)+
        scale_fill_manual(values = c("#97A1FC","yellow","red", "#70D3E0"),na.value="gray80",labels=c("TEMP","SM","SW","VPD"))+
        scale_y_continuous(expand = c(0,0),limits=c(-6336039,8625155))+
        scale_x_continuous(expand = c(0,0))+
        xlab(NULL)+ylab(NULL)+
#     scale_color_startrek(breaks=seq(1,4,1),labels=c("TEMP","SM","SW","VPD"))+
#     scale_fill_startrek(breaks=seq(1,4,1),labels=c("TEMP","SM","SW","VPD"))+
  guides(fill= guide_legend(byrow = TRUE,ticks = TRUE, even.steps = FALSE,frame.linewidth = 0.55, frame.colour = "black", ticks.colour = "black",
        ticks.linewidth = 0.3,nrow=1,title=NULL,label.position = "bottom",label=T,label.theme=element_text(size=6.5,colour="black",angle=0,family="Times")))+  
        geom_text(data=label_data_ESMs,aes(x=x,y=y,label=lab),family="Times",size=7/.pt,check_overlap = TRUE)+
        geom_text(data=label_data_ESMs_,aes(x=x,y=y,label=lab),family="Times",size=7/.pt,check_overlap = TRUE,fontface="bold")+

   #     guides(fill= guide_legend(label.position = "bottom",title.vjust=-0.4,title=NULL,label=T,label.theme=element_text(size=6.5,colour="black",angle=0,family="Times")))+  
       facet_grid(Class~Type)+coord_equal() + 
        theme_opts

print(h1, vp=viewport(layout.pos.row=46:100, layout.pos.col=1:100))

dev.off()





