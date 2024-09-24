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

##########################################FLUXCOM_GPP
RF_WE       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/GPP/RF/df_Trend_WE.txt",head=F,sep=' ')
RF_NoWE       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/GPP/RF/df_Trend_NoWE.txt",head=F,sep=' ')
MAR_WE       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/GPP/MAR/df_Trend_WE.txt",head=F,sep=' ')
MAR_NoWE       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/GPP/MAR/df_Trend_NoWE.txt",head=F,sep=' ')
ANN_WE       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/GPP/ANN/df_Trend_WE.txt",head=F,sep=' ')
ANN_NoWE       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/GPP/ANN/df_Trend_NoWE.txt",head=F,sep=' ')

p_RF_tmp <- cbind(RF_WE,RF_NoWE)
p_ANN_tmp <- cbind(ANN_WE,ANN_NoWE)
p_MAR_tmp <- cbind(MAR_WE,MAR_NoWE)

FUN_p   <- function(x){
              y <- x[1:19]
              z <- x[20:38]
              Res <- t.test(y,z)
              p_value <- Res$p.value 
              return(p_value)
            }

p_RF_GPP     <- apply(p_RF_tmp,1,FUN_p)
p_MAR_GPP     <- apply(p_MAR_tmp,1,FUN_p)
p_ANN_GPP     <- apply(p_ANN_tmp,1,FUN_p)
######------------------------------------------------------------------------------TER
RF_WE       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/TER/RF/df_Trend_WE.txt",head=F,sep=' ')
RF_NoWE       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/TER/RF/df_Trend_NoWE.txt",head=F,sep=' ')
MAR_WE       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/TER/MAR/df_Trend_WE.txt",head=F,sep=' ')
MAR_NoWE       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/TER/MAR/df_Trend_NoWE.txt",head=F,sep=' ')
ANN_WE       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/TER/ANN/df_Trend_WE.txt",head=F,sep=' ')
ANN_NoWE       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/TER/ANN/df_Trend_NoWE.txt",head=F,sep=' ')

p_RF_tmp <- cbind(RF_WE,RF_NoWE)
p_ANN_tmp <- cbind(ANN_WE,ANN_NoWE)
p_MAR_tmp <- cbind(MAR_WE,MAR_NoWE)
FUN_p   <- function(x){
              y <- x[1:19]
              z <- x[20:38]
              Res <- t.test(y,z)
              p_value <- Res$p.value 
              return(p_value)
            }
p_RF_TER     <- apply(p_RF_tmp,1,FUN_p)
p_MAR_TER     <- apply(p_MAR_tmp,1,FUN_p)
p_ANN_TER     <- apply(p_ANN_tmp,1,FUN_p)

######------------------------------------------------------------------------------GPP___ESMs
CM_WE       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/GPP/CM/df_Trend_WE.txt",head=F,sep=' ')
CM_NoWE       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/GPP/CM/df_Trend_NoWE.txt",head=F,sep=' ')
ES_WE       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/GPP/ES/df_Trend_WE.txt",head=F,sep=' ')
ES_NoWE       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/GPP/ES/df_Trend_NoWE.txt",head=F,sep=' ')
LM_WE       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/GPP/LM/df_Trend_WE.txt",head=F,sep=' ')
LM_NoWE       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/GPP/LM/df_Trend_NoWE.txt",head=F,sep=' ')
MM_WE       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/GPP/MM/df_Trend_WE.txt",head=F,sep=' ')
MM_NoWE       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/GPP/MM/df_Trend_NoWE.txt",head=F,sep=' ')

p_CM_tmp <- cbind(CM_WE,CM_NoWE)
p_ES_tmp <- cbind(ES_WE,ES_NoWE)
p_LM_tmp <- cbind(LM_WE,LM_NoWE)
p_MM_tmp <- cbind(MM_WE,MM_NoWE)

FUN_p   <- function(x){
              y <- x[1:20]
              z <- x[21:40]
              Res <- t.test(y,z)
              p_value <- Res$p.value 
              return(p_value)
            }
p_CM_GPP     <- apply(p_CM_tmp,1,FUN_p)
p_ES_GPP     <- apply(p_ES_tmp,1,FUN_p)
p_LM_GPP     <- apply(p_LM_tmp,1,FUN_p)
p_MM_GPP     <- apply(p_MM_tmp,1,FUN_p)

length(which(p_MM_GPP<0.05))
######------------------------------------------------------------------------------TER___ESMs
CM_WE       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/TER/CM/df_Trend_WE.txt",head=F,sep=' ')
CM_NoWE       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/TER/CM/df_Trend_NoWE.txt",head=F,sep=' ')
ES_WE       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/TER/ES/df_Trend_WE.txt",head=F,sep=' ')
ES_NoWE       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/TER/ES/df_Trend_NoWE.txt",head=F,sep=' ')
LM_WE       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/TER/LM/df_Trend_WE.txt",head=F,sep=' ')
LM_NoWE       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/TER/LM/df_Trend_NoWE.txt",head=F,sep=' ')
MM_WE       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/TER/MM/df_Trend_WE.txt",head=F,sep=' ')
MM_NoWE       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/TER/MM/df_Trend_NoWE.txt",head=F,sep=' ')

p_CM_tmp <- cbind(CM_WE,CM_NoWE)
p_ES_tmp <- cbind(ES_WE,ES_NoWE)
p_LM_tmp <- cbind(LM_WE,LM_NoWE)
p_MM_tmp <- cbind(MM_WE,MM_NoWE)

FUN_p   <- function(x){
              y <- x[1:20]
              z <- x[21:40]
              Res <- t.test(y,z)
              p_value <- Res$p.value 
              return(p_value)
            }
p_CM_TER     <- apply(p_CM_tmp,1,FUN_p)
p_ES_TER     <- apply(p_ES_tmp,1,FUN_p)
p_LM_TER     <- apply(p_LM_tmp,1,FUN_p)
p_MM_TER     <- apply(p_MM_tmp,1,FUN_p)


ESMs_GPP <- cbind(p_CM_GPP,p_ES_GPP,p_LM_GPP,p_MM_GPP)
ESMs_TER <- cbind(p_CM_TER,p_ES_TER,p_LM_TER,p_MM_TER)
FLUXCOM_GPP <- cbind(p_ANN_GPP,p_MAR_GPP,p_RF_GPP)
FLUXCOM_TER <- cbind(p_ANN_TER,p_MAR_TER,p_RF_TER)

FUN_p_num <- function(x){
   y <- length(which(x<0.05))
   return(y)
    }

ESM_no_GPP     <- apply(ESMs_GPP,1,FUN_p_num)
ESM_no_TER     <- apply(ESMs_TER,1,FUN_p_num)
FLUXCOM_no_GPP <- apply(FLUXCOM_GPP,1,FUN_p_num)
FLUXCOM_no_TER <- apply(FLUXCOM_TER,1,FUN_p_num)


###############_-----------------------------------------------------------------
tmp_data       <- data.frame(x=index[,1],y=index[,2],z=ESM_no_GPP)
tmp_raster     <- rasterFromXYZ(tmp_data)
projection(tmp_raster) <- c("+proj=longlat +ellps=WGS84 +no_defs")
tmp_raster     <- raster::projectRaster(tmp_raster,crs="+proj=robin",method="ngb")
ESMs_p_GPP    <- data.frame(rasterToPoints(tmp_raster),Type="ESMs_p_GPP")
###############_-----------------------------------------------------------------
tmp_data       <- data.frame(x=index[,1],y=index[,2],z=ESM_no_TER)
tmp_raster     <- rasterFromXYZ(tmp_data)
projection(tmp_raster) <- c("+proj=longlat +ellps=WGS84 +no_defs")
tmp_raster     <- raster::projectRaster(tmp_raster,crs="+proj=robin",method="ngb")
ESMs_p_TER    <- data.frame(rasterToPoints(tmp_raster),Type="ESMs_p_TER")
###############_-----------------------------------------------------------------
tmp_data       <- data.frame(x=index[,1],y=index[,2],z=FLUXCOM_no_GPP)
tmp_raster     <- rasterFromXYZ(tmp_data)
projection(tmp_raster) <- c("+proj=longlat +ellps=WGS84 +no_defs")
tmp_raster     <- raster::projectRaster(tmp_raster,crs="+proj=robin",method="ngb")
FLUXCOM_p_GPP    <- data.frame(rasterToPoints(tmp_raster),Type="FLUXCOM_p_GPP")
###############_-----------------------------------------------------------------
tmp_data       <- data.frame(x=index[,1],y=index[,2],z=FLUXCOM_no_TER)
tmp_raster     <- rasterFromXYZ(tmp_data)
projection(tmp_raster) <- c("+proj=longlat +ellps=WGS84 +no_defs")
tmp_raster     <- raster::projectRaster(tmp_raster,crs="+proj=robin",method="ngb")
FLUXCOM_p_TER    <- data.frame(rasterToPoints(tmp_raster),Type="FLUXCOM_p_TER")


##########################################FLUXCOM_TER
RF_GPP       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/GPP/RF/df_Trend.txt",head=F,sep=' ')
RF_TER       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/TER/RF/df_Trend.txt",head=F,sep=' ')

MAR_GPP       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/GPP/MAR/df_Trend.txt",head=F,sep=' ')
MAR_TER       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/TER/MAR/df_Trend.txt",head=F,sep=' ')

ANN_GPP       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/GPP/ANN/df_Trend.txt",head=F,sep=' ')
ANN_TER       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/TER/ANN/df_Trend.txt",head=F,sep=' ')

CM_GPP       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/GPP/CM/df_Trend.txt",head=F,sep=' ')
CM_TER       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/TER/CM/df_Trend.txt",head=F,sep=' ')
ES_GPP       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/GPP/ES/df_Trend.txt",head=F,sep=' ')
ES_TER       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/TER/ES/df_Trend.txt",head=F,sep=' ')
LM_GPP       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/GPP/LM/df_Trend.txt",head=F,sep=' ')
LM_TER       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/TER/LM/df_Trend.txt",head=F,sep=' ')
MM_GPP       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/GPP/MM/df_Trend.txt",head=F,sep=' ')
MM_TER       <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/TER/MM/df_Trend.txt",head=F,sep=' ')

FLUXCOM_GPP  <- ((RF_GPP+MAR_GPP+ANN_GPP))/3
FLUXCOM_TER  <- ((RF_TER+MAR_TER+ANN_TER))/3
ESMs_GPP     <- (CM_GPP+ES_GPP+LM_GPP+MM_GPP)/4
ESMs_TER     <- (CM_TER+ES_TER+LM_TER+MM_TER)/4

FLUXCOM_GPP  <- apply(FLUXCOM_GPP,1,mean)
FLUXCOM_TER  <- apply(FLUXCOM_TER,1,mean)
ESMs_GPP  <- apply(ESMs_GPP,1,mean)
ESMs_TER  <- apply(ESMs_TER,1,mean)
###########################################################################Inv40
##################################################################
tmp_data    <- data.frame(x=index[,1],y=index[,2],z=FLUXCOM_GPP)
tmp_raster  <- rasterFromXYZ(tmp_data)
projection(tmp_raster) <- c("+proj=longlat +ellps=WGS84 +no_defs")
tmp_raster  <- raster::projectRaster(tmp_raster,crs="+proj=robin")
df_FLUXCOM_GPP    <- data.frame(rasterToPoints(tmp_raster),Type="FLUXCOM GPP")
###########################################################################Inv33
tmp_data    <- data.frame(x=index[,1],y=index[,2],z=FLUXCOM_TER)
tmp_raster  <- rasterFromXYZ(tmp_data)
projection(tmp_raster) <- c("+proj=longlat +ellps=WGS84 +no_defs")
tmp_raster  <- raster::projectRaster(tmp_raster,crs="+proj=robin")
df_FLUXCOM_TER    <- data.frame(rasterToPoints(tmp_raster),Type="FLUXCOM TER")
###########################################################################FLUXCOM
tmp_data    <- data.frame(x=index[,1],y=index[,2],z=ESMs_GPP)
tmp_raster  <- rasterFromXYZ(tmp_data)
projection(tmp_raster) <- c("+proj=longlat +ellps=WGS84 +no_defs")
tmp_raster  <- raster::projectRaster(tmp_raster,crs="+proj=robin")
df_ESMs_GPP    <- data.frame(rasterToPoints(tmp_raster),Type="ESMs GPP")
###########################################################################ESMs
tmp_data    <- data.frame(x=index[,1],y=index[,2],z=ESMs_TER)
tmp_raster  <- rasterFromXYZ(tmp_data)
projection(tmp_raster) <- c("+proj=longlat +ellps=WGS84 +no_defs")
tmp_raster  <- raster::projectRaster(tmp_raster,crs="+proj=robin")
df_ESMs_TER     <- data.frame(rasterToPoints(tmp_raster),Type="ESMs TER")
##########------------------------------------------------------------------------
df_data      <- rbind(df_FLUXCOM_GPP,df_FLUXCOM_TER,df_ESMs_GPP,df_ESMs_TER)
df_data$Type <- factor(df_data$Type,levels=c("FLUXCOM GPP","FLUXCOM TER","ESMs GPP","ESMs TER")) 
df_data$z[which(df_data$z < -4)]=-4.01
df_data$z[which(df_data$z > 4)] =4.01
#######################_--------------------------------------------------------------------------------
CM_GPP    <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/GPP/CM_15_year_trend_GPP.txt",head=T,sep=' ')
ES_GPP   <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/GPP/ES_15_year_trend_GPP.txt",head=T,sep=' ')
LM_GPP   <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/GPP/LM_15_year_trend_GPP.txt",head=T,sep=' ')
MM_GPP   <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/GPP/MM_15_year_trend_GPP.txt",head=T,sep=' ')

CM_TER    <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/GPP/CM_15_year_trend_TER.txt",head=T,sep=' ')
ES_TER   <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/GPP/ES_15_year_trend_TER.txt",head=T,sep=' ')
LM_TER   <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/GPP/LM_15_year_trend_TER.txt",head=T,sep=' ')
MM_TER   <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/GPP/MM_15_year_trend_TER.txt",head=T,sep=' ')

ANN_GPP  <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/GPP/ANN_15_year_trend_GPP.txt",head=T,sep=' ')
MAR_GPP  <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/GPP/MAR_15_year_trend_GPP.txt",head=T,sep=' ')
RF_GPP   <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/GPP/RF_15_year_trend_GPP.txt",head=T,sep=' ')

ANN_TER  <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/GPP/ANN_15_year_trend_TER.txt",head=T,sep=' ')
MAR_TER  <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/GPP/MAR_15_year_trend_TER.txt",head=T,sep=' ')
RF_TER   <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/GPP/RF_15_year_trend_TER.txt",head=T,sep=' ')



########----------------------------------------------------------------------------------------------

FLUX_GPP      <- data.frame(Value=c(ANN_GPP$WE,MAR_GPP$WE,RF_GPP$WE,ANN_GPP$NoWE,MAR_GPP$NoWE,RF_GPP$NoWE),Type=rep(c("WE","NonWE"),each=19*3),Class="GPP",sub="FLUXCOM")
FLUX_TER      <- data.frame(Value=c(ANN_TER$WE,MAR_TER$WE,RF_TER$WE,ANN_TER$NoWE,MAR_TER$NoWE,RF_TER$NoWE),Type=rep(c("WE","NonWE"),each=19*3),Class="TER",sub="FLUXCOM")
ESM_GPP       <- data.frame(Value=c(CM_GPP$WE,ES_GPP$WE,LM_GPP$WE,MM_GPP$WE,CM_GPP$NoWE,ES_GPP$NoWE,LM_GPP$NoWE,MM_GPP$NoWE),Type=rep(c("WE","NonWE"),each=20*4),Class="GPP",sub="ESMs")
ESM_TER       <- data.frame(Value=c(CM_TER$WE,ES_TER$WE,LM_TER$WE,MM_TER$WE,CM_TER$NoWE,ES_TER$NoWE,LM_TER$NoWE,MM_TER$NoWE),Type=rep(c("WE","NonWE"),each=20*4),Class="TER",sub="ESMs")
TX             <- rbind(FLUX_GPP,FLUX_TER,ESM_GPP,ESM_TER)
df_TX_Bar <- ddply(TX,.(Class,Type,sub),summarize,Mean=mean(Value),Ymax=max(Value),Ymin=min(Value),Upper=quantile(Value,0.75),Lower=quantile(Value,0.25))
df_TX          <- TX


label_data <- data.frame(x=rep(-15000000,times=4),y=rep(8000000,times=4),lab=c("a","b","c","d"),Type=unique(df_data$Type))
##########_--------------------------------------------------------------------------------------------
pdf(file = "F:/Figure3.pdf", width=6.5, height=6.2)  
pushViewport(viewport(layout=grid.layout(100,100)))

world <- shapefile("F:/Flux_paper/dataframe/world_shape/World_Continents.shp")
world     <- spTransform(world, CRS("+proj=robin"))
world <- fortify(world)
box <- shapefile("F:/Flux_paper/dataframe/world_shape/ne_110m_populated_places/ne_110m_wgs84_bounding_box.shp")
box <- crop(box,c(-180,180,-60,90))
box     <- spTransform(box, CRS("+proj=robin"))
box <- fortify(box)
theme_opts <- list(theme(

                         axis.text = element_blank(),
                         strip.text=element_text( size=7,family="Times"),
                         axis.ticks = element_line(size=0.0),
                         axis.ticks.length = unit(.00, "cm"),
                         legend.key.height=unit(1.2,"lines"),
                         legend.key.width=unit(0.4,"lines"),
                         legend.spacing = unit(-0.5, "cm"),
                         legend.direction="vertical",
                         plot.margin = unit(c(-0.2,0.1,0.0,0.1),units="lines"),
                         panel.border = element_rect( colour = NA,fill=NA, size=0.),
                         panel.spacing.y = unit(2.5, "lines"),
                         panel.grid=element_blank(),

                        strip.background = element_blank(),
                        axis.title.x=element_text( size=6.5,family="Times")

                      ))

h1 <-   ggplot(data=df_data,aes(x=x,y=y)) + theme_bw()+
        geom_tile(aes(x=x,y=y,fill=z))+
        geom_path(data=world, aes(long, lat, group=group, fill=NULL), linetype="solid", color="black",size = 0.2) +
        geom_path(data=box, aes(long, lat, group=group, fill=NULL), linetype="solid", color="black",size = 0.2) +
        scale_fill_distiller(breaks=seq(-3.0,3.0,1.),labels=seq(-3,3,1),palette="RdBu", direction=-1)+
        scale_y_continuous(expand = c(0,0),limits=c(-6336039,8625155))+
        scale_x_continuous(expand = c(0,0))+
        geom_text(data=label_data,aes(x=x,y=y,label=lab),family="Times",size=7/.pt,fontface="bold")+

        xlab(NULL)+ylab(NULL)+
        guides(fill= guide_colorbar(byrow = TRUE,ticks = TRUE, even.steps = FALSE,frame.linewidth = 0.55, frame.colour = "black", ticks.colour = "black",
        ticks.linewidth = 0.3,nrow=1,title.position="left",title=expression(Delta~Trend[Flux]~(WE-NonWE)~(g~C~Year^-1)),label.position = "right",title.theme=element_text(size=6.5,colour="black",angle=90,family="Times"),label=T,label.theme=element_text(size=6.5,colour="black",angle=0,family="Times")))+  
        facet_wrap( ~Type, ncol=2)+coord_equal() + 
        theme_opts
print(h1, vp=viewport(layout.pos.row=1:70, layout.pos.col=1:100))

######_----------------------------------------------------------------------
theme_opts <- list(theme(

                        axis.text = element_blank(),
                     #   axis.title.y=element_text( size=6,family="Times"),
                        strip.text=element_blank(),

                        axis.ticks = element_line(size=0.0),
                        axis.ticks.length = unit(.00, "cm"),
                        legend.key.height=unit(0.2,"lines"),
                        legend.key.width=unit(0.6,"lines"),
                        legend.spacing.x = unit(0.05, "cm"),
                        legend.position=c(0.05,0.4),
                        legend.direction="vertical",
                        plot.margin = unit(c(0.1,0.1,0.0,0.1),units="lines"),
                        panel.border = element_rect( colour = NA,fill=NA, size=0.),

                        panel.grid=element_blank(),
legend.key = element_rect(colour = "transparent", fill = "transparent"),
   legend.background = element_rect(fill='transparent'), #transparent legend bg
                        strip.background = element_blank(),
                        axis.title.x=element_text( size=6.5,family="Times")

                      ))
FLUXCOM_p_GPP$z <- factor(FLUXCOM_p_GPP$z)
h1 <-   ggplot(data=FLUXCOM_p_GPP,aes(x=x,y=y,fill=z)) + theme_bw()+
        geom_tile(aes(x=x,y=y))+
        geom_path(data=world, aes(long, lat, group=group, fill=NULL), linetype="solid", color="black",size = 0.2) +
        geom_path(data=box, aes(long, lat, group=group, fill=NULL), linetype="solid", color="black",size = 0.2) +
       scale_fill_manual(values = c("gray","yellow","red", "#70D3E0", "#97A1FC"))+
        scale_y_continuous(expand = c(0,0),limits=c(-6336039,8625155))+
        scale_x_continuous(expand = c(0,0))+
        xlab(NULL)+ylab(NULL)+
        geom_text(aes(x=7000000,y=-4300000),label="43%",family="Times",size=7/.pt,check_overlap = TRUE)+

        guides(fill= guide_legend(title.vjust=-0.4,title=NULL,label.hjust=-1,label=T,label.theme=element_text(size=6.5,colour="black",angle=0,family="Times")))+  
        facet_wrap( ~Type, ncol=2)+coord_equal() + 
        theme_opts
sbvp1 <- viewport(width=0.2,height=0.4,x=0.274,y=0.68)
print(h1,vp=sbvp1,more=TRUE)
#######################################################
FLUXCOM_p_TER$z <- factor(FLUXCOM_p_TER$z)
h1 <-   ggplot(data=FLUXCOM_p_TER,aes(x=x,y=y,fill=z)) + theme_bw()+
        geom_tile(aes(x=x,y=y))+
        geom_path(data=world, aes(long, lat, group=group, fill=NULL), linetype="solid", color="black",size = 0.2) +
        geom_path(data=box, aes(long, lat, group=group, fill=NULL), linetype="solid", color="black",size = 0.2) +
       scale_fill_manual(values = c("gray","yellow","red", "#70D3E0", "#97A1FC"))+
        scale_y_continuous(expand = c(0,0),limits=c(-6336039,8625155))+
        scale_x_continuous(expand = c(0,0))+
        xlab(NULL)+ylab(NULL)+
        geom_text(aes(x=7000000,y=-4300000),label="38%",family="Times",size=7/.pt,check_overlap = TRUE)+

        guides(fill= guide_legend(title.vjust=-0.4,title=NULL,label.hjust=-1,label=T,label.theme=element_text(size=6.5,colour="black",angle=0,family="Times")))+  
        facet_wrap( ~Type, ncol=2)+coord_equal() + 
        theme_opts
sbvp1 <- viewport(width=0.2,height=0.4,x=0.73,y=0.68)
print(h1,vp=sbvp1,more=TRUE)
##################################################################
ESMs_p_GPP$z <- factor(ESMs_p_GPP$z)
h1 <-   ggplot(data=ESMs_p_GPP,aes(x=x,y=y,fill=z)) + theme_bw()+
        geom_tile(aes(x=x,y=y))+
        geom_path(data=world, aes(long, lat, group=group, fill=NULL), linetype="solid", color="black",size = 0.2) +
        geom_path(data=box, aes(long, lat, group=group, fill=NULL), linetype="solid", color="black",size = 0.2) +
       scale_fill_manual(values = c("gray","yellow","red", "#70D3E0", "#97A1FC"))+
        scale_y_continuous(expand = c(0,0),limits=c(-6336039,8625155))+
        scale_x_continuous(expand = c(0,0))+
        xlab(NULL)+ylab(NULL)+
        geom_text(aes(x=7000000,y=-4300000),label="41%",family="Times",size=7/.pt,check_overlap = TRUE)+

        guides(fill= guide_legend(title.vjust=-0.4,title=NULL,label.hjust=-1,label=T,label.theme=element_text(size=6.5,colour="black",angle=0,family="Times")))+  
        facet_wrap( ~Type, ncol=2)+coord_equal() + 
        theme_opts
sbvp1 <- viewport(width=0.2,height=0.4,x=0.274,y=0.36)
print(h1,vp=sbvp1,more=TRUE)
##################################################################
ESMs_p_TER$z <- factor(ESMs_p_TER$z)
h1 <-   ggplot(data=ESMs_p_TER,aes(x=x,y=y,fill=z)) + theme_bw()+
        geom_tile(aes(x=x,y=y))+
        geom_path(data=world, aes(long, lat, group=group, fill=NULL), linetype="solid", color="black",size = 0.2) +
        geom_path(data=box, aes(long, lat, group=group, fill=NULL), linetype="solid", color="black",size = 0.2) +
       scale_fill_manual(values = c("gray","yellow","red", "#70D3E0", "#97A1FC"))+
        scale_y_continuous(expand = c(0,0),limits=c(-6336039,8625155))+
        scale_x_continuous(expand = c(0,0))+
        xlab(NULL)+ylab(NULL)+
        geom_text(aes(x=7000000,y=-4300000),label="39%",family="Times",size=7/.pt,check_overlap = TRUE)+

        guides(fill= guide_legend(title.vjust=-0.4,title=NULL,label.hjust=-1,label=T,label.theme=element_text(size=6.5,colour="black",angle=0,family="Times")))+  
        facet_wrap( ~Type, ncol=2)+coord_equal() + 
        theme_opts
sbvp1 <- viewport(width=0.2,height=0.4,x=0.73,y=0.36)
print(h1,vp=sbvp1,more=TRUE)

####----------------------------------------------------------------------
theme_opts <- list(theme(axis.text.x = element_text(size=7,family='Times',colour = "black"),
                         axis.text.y = element_text(size=7,family='Times',colour = "black"),
                        axis.title.x = element_text(size=7,family='Times',colour = "black"),
                        axis.title.y =element_text(size=7,family='Times',colour = "black"),
                        axis.ticks = element_line(size=0.5),
                        axis.ticks.length = unit(.04, "cm"),
                        panel.grid=element_blank(),
                        legend.spacing = unit(-0.05, "cm"),
                       legend.direction="vertical",
                       strip.text= element_text(size=7,family='Times',colour = "black"),

                        legend.background = element_rect(fill='transparent'),
                        legend.key.height=unit(0.3,"lines"),
                        legend.key.width=unit(0.5,"lines"),
                         plot.margin = unit(c(0.1,0.1,0.0,0.1),units="lines"),
axis.line = element_line(colour = "black",size=0.),
       panel.border = element_blank(),
                        legend.key.size=unit(10,"cm"),
                        strip.background = element_blank(),
                        plot.title = element_text(size=22)))

H <-   ggplot(data=subset(df_TX_Bar,sub=="FLUXCOM"),aes(x=Class,col=Type)) + theme_bw()+ 
       geom_boxplot(aes(lower=Lower,upper=Upper,middle=Mean,ymin=Ymin,ymax=Ymax),stat = "identity",show.legend=F)+
       geom_point(data=subset(df_TX,sub=="FLUXCOM"),aes(x=Class,y=Value),position = position_jitterdodge(jitter.width = 0.25),alpha=0.5,size=1/.pt,show.legend=F)+
       ylab(expression(Trend[Flux]~(Pg~C~Year^-1)))+xlab("")+
       geom_hline(yintercept=0,size=0.2,linetype = "longdash")+
       geom_text(x=1,y=0.18,label="*(3/3)",size=6.9/.pt,family="Times",col="black",check_overlap = TRUE)+
       geom_text(x=2,y=0.18,label="*(3/3)",size=6.9/.pt,family="Times",col="black",check_overlap = TRUE)+
       scale_colour_manual(values = c("red4",  "#0066cc"))+
        geom_text(x=0.5,y=0.1,label="e",family="Times",size=7/.pt,show.legend=F,col="black",fontface="bold")+

      guides( col=guide_legend(title="",label=T,label.theme=element_text(size=5.2,colour="black",angle=0,family="Times")),
              fill=guide_legend(title="",label=T,label.theme=element_text(size=5.2,colour="black",angle=0,family="Times")))+
              facet_wrap( ~sub, ncol=1)+
       theme_opts+theme(     legend.position=c(0.6,0.3))

print(H, vp=viewport(layout.pos.row=75:100, layout.pos.col=1:38))

H <-   ggplot(data=subset(df_TX_Bar,sub=="ESMs"),aes(x=Class,col=Type)) + theme_bw()+ 
       geom_boxplot(aes(lower=Lower,upper=Upper,middle=Mean,ymin=Ymin,ymax=Ymax),stat = "identity")+
       geom_point(data=subset(df_TX,sub=="ESMs"),aes(x=Class,y=Value),position = position_jitterdodge(jitter.width = 0.25),alpha=0.5,size=1/.pt)+
       ylab(expression(Trend[Flux]~(Pg~C~Year^-1)))+xlab("")+
       geom_hline(yintercept=0,size=0.2,linetype = "longdash")+
       geom_text(x=1,y=0.55,label="*(2/4)",size=6.9/.pt,family="Times",col="black",check_overlap = TRUE)+
       geom_text(x=2,y=0.55,label="*(1/4)",size=6.9/.pt,family="Times",col="black",check_overlap = TRUE)+
       scale_colour_manual(values = c("red4",  "#0066cc"))+
        geom_text(x=0.5,y=0.35,label="f",family="Times",size=7/.pt,show.legend=F,col="black",facefont="bold")+

      scale_x_discrete(labels=c("ESMs_GPP"="GPP","ESMs_TER"="TER"))+
      guides( col=guide_legend(title="",label=T,label.theme=element_text(size=6.5,colour="black",angle=0,family="Times")),
              fill=guide_legend(title="",label=T,label.theme=element_text(size=6.5,colour="black",angle=0,family="Times")))+
              facet_wrap( ~sub, ncol=1)+
       theme_opts+theme( )
print(H, vp=viewport(layout.pos.row=75:100, layout.pos.col=45:98))

dev.off()
##################




