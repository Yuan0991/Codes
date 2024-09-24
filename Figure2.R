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
##########################################Inv2020
Jena_WE     <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/Jena/df_Trend_WE.txt",head=F,sep=' ')
Jena_NoWE     <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/Jena/df_Trend_NoWE.txt",head=F,sep=' ')

Jena_T_WE     <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/Jena_T/df_Trend_WE.txt",head=F,sep=' ')
Jena_T_NoWE     <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/Jena_T/df_Trend_NoWE.txt",head=F,sep=' ')

CAMs_WE     <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/CAMs/df_Trend_WE.txt",head=F,sep=' ')
CAMs_NoWE     <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/CAMs/df_Trend_NoWE.txt",head=F,sep=' ')


P_cal   <- cbind(Jena_WE,Jena_NoWE,index)
P_cal   <- na.omit(P_cal)
FUN_p   <- function(x){
              y <- x[1:26]
              z <- x[27:52]
              Res <- t.test(y,z)
              p_value <- Res$p.value 
              return(p_value)
            }

p_Jena_2020  <- apply(P_cal,1,FUN=FUN_p)

P_cal        <- cbind(CAMs_WE,CAMs_NoWE,index)
P_cal        <- na.omit(P_cal)
p_CAMS_2020  <- apply(P_cal,1,FUN=FUN_p)

P_cal        <- cbind(Jena_T_WE,Jena_T_NoWE,index)
P_cal        <- na.omit(P_cal)
p_Jena_T_2020  <- apply(P_cal,1,FUN=FUN_p)
############_-----------------------------------------------------------------------Inv2013
P_cal   <- cbind(Jena_WE[,1:19],Jena_NoWE[,1:19],index)
P_cal   <- na.omit(P_cal)
FUN_p   <- function(x){
              y <- x[1:19]
              z <- x[20:38]
              Res <- t.test(y,z)
              p_value <- Res$p.value 
              return(p_value)
            }

p_Jena_2013  <- apply(P_cal,1,FUN=FUN_p)

P_cal   <- cbind(CAMs_WE[,1:19],CAMs_NoWE[,1:19],index)
P_cal   <- na.omit(P_cal)
p_CAMS_2013  <- apply(P_cal,1,FUN=FUN_p)

P_cal   <- cbind(Jena_T_WE[,1:19],Jena_T_NoWE[,1:19],index)
P_cal   <- na.omit(P_cal)
p_Jena_T_2013  <- apply(P_cal,1,FUN=FUN_p)

##############----------------------------------------------------------------------


ANN_WE     <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/FLUXCOM/ANN/df_Trend_WE.txt",head=F,sep=' ')
ANN_NoWE     <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/FLUXCOM/ANN/df_Trend_NoWE.txt",head=F,sep=' ')

MAR_WE     <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/FLUXCOM/MAR/df_Trend_WE.txt",head=F,sep=' ')
MAR_NoWE     <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/FLUXCOM/MAR/df_Trend_NoWE.txt",head=F,sep=' ')

RF_WE      <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/FLUXCOM/RF/df_Trend_WE.txt",head=F,sep=' ')
RF_NoWE      <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/FLUXCOM/RF/df_Trend_NoWE.txt",head=F,sep=' ')

P_cal_ANN  <- cbind(ANN_WE,ANN_NoWE,index)
P_cal_MAR  <- cbind(MAR_WE,MAR_NoWE,index)
P_cal_RF  <- cbind(RF_WE,RF_NoWE,index)
P_cal_ANN <- na.omit(P_cal_ANN)
P_cal_MAR <- na.omit(P_cal_MAR)
P_cal_RF <- na.omit(P_cal_RF)
FUN_p   <- function(x){
              y <- x[1:19]
              z <- x[20:38]
              Res <- t.test(y,z)
              p_value <- Res$p.value 
              return(p_value)
            }
p_ANN  <- apply(P_cal_ANN,1,FUN=FUN_p)
p_MAR  <- apply(P_cal_MAR,1,FUN=FUN_p)
p_RF  <- apply(P_cal_RF,1,FUN=FUN_p)




CM_WE     <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/CMIP6/CMCC_CM/df_Trend_WE.txt",head=F,sep=' ')
CM_NoWE     <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/CMIP6/CMCC_CM/df_Trend_NoWE.txt",head=F,sep=' ')

ES_WE     <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/CMIP6/CMCC_ES/df_Trend_WE.txt",head=F,sep=' ')
ES_NoWE     <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/CMIP6/CMCC_ES/df_Trend_NoWE.txt",head=F,sep=' ')

LM_WE     <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/CMIP6/Nor_LM/df_Trend_WE.txt",head=F,sep=' ')
LM_NoWE     <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/CMIP6/Nor_LM/df_Trend_NoWE.txt",head=F,sep=' ')

MM_WE     <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/CMIP6/Nor_MM/df_Trend_WE.txt",head=F,sep=' ')
MM_NoWE     <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/CMIP6/Nor_MM/df_Trend_NoWE.txt",head=F,sep=' ')

P_cal_CM  <- cbind(CM_WE[,1:19],CM_NoWE[,1:19],index)
P_cal_ES  <- cbind(ES_WE[,1:19],ES_NoWE[,1:19],index)
P_cal_LM  <- cbind(LM_WE[,1:19],LM_NoWE[,1:19],index)
P_cal_MM  <- cbind(MM_WE[,1:19],MM_NoWE[,1:19],index)

P_cal_CM <- na.omit(P_cal_CM)
P_cal_ES <- na.omit(P_cal_ES)
P_cal_LM <- na.omit(P_cal_LM)
P_cal_MM <- na.omit(P_cal_MM)

FUN_p   <- function(x){
              y <- x[1:19]
              z <- x[20:38]
              Res <- t.test(y,z)
              p_value <- Res$p.value 
              return(p_value)
            }
p_CM  <- apply(P_cal_CM,1,FUN=FUN_p)
p_ES  <- apply(P_cal_ES,1,FUN=FUN_p)
p_LM  <- apply(P_cal_LM,1,FUN=FUN_p)
p_MM  <- apply(P_cal_MM,1,FUN=FUN_p)

P_ESM     <- cbind(p_CM,p_ES,p_LM,p_MM)
P_FLUXCOM <- cbind(p_ANN,p_MAR,p_RF)
P_INV_2020     <- cbind(p_Jena_2020,p_CAMS_2020,p_Jena_T_2020)
P_INV_2013     <- cbind(p_Jena_2013,p_CAMS_2013,p_Jena_T_2013)

FUN_p_num <- function(x){
   y <- length(which(x<0.05))
   return(y)
    }
ESM_no     <- apply(P_ESM,1,FUN_p_num)
FLUXCOM_no <- apply(P_FLUXCOM,1,FUN_p_num)
INV_2020_no     <- apply(P_INV_2020,1,FUN_p_num)
INV_2013_no     <- apply(P_INV_2013,1,FUN_p_num)


ESM_no     <- data.frame(Value=ESM_no,x=P_cal_CM$x,y=P_cal_CM$y)
FLUXCOM_no <- data.frame(Value=FLUXCOM_no,x=P_cal_RF$x,y=P_cal_RF$y)
INV_2020_no     <- data.frame(Value=INV_2020_no,x=P_cal$x,y=P_cal$y)
INV_2013_no     <- data.frame(Value=INV_2013_no,x=P_cal$x,y=P_cal$y)

################################################################################

Jena     <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/Jena/df_Trend.txt",head=F,sep=' ')
CAMs     <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/CAMs/df_Trend.txt",head=F,sep=' ')
Jena_T   <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/Jena_T/df_Trend.txt",head=F,sep=' ')

ANN     <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/FLUXCOM/ANN/df_Trend.txt",head=F,sep=' ')
MAR     <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/FLUXCOM/MAR/df_Trend.txt",head=F,sep=' ')
RF      <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/FLUXCOM/RF/df_Trend.txt",head=F,sep=' ')

CM     <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/CMIP6/CMCC_CM/df_Trend.txt",head=F,sep=' ')
ES     <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/CMIP6/CMCC_ES/df_Trend.txt",head=F,sep=' ')
LM     <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/CMIP6/Nor_LM/df_Trend.txt",head=F,sep=' ')
MM     <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/CMIP6/Nor_MM/df_Trend.txt",head=F,sep=' ')


Jena_2020 <- apply(Jena,1,mean)
CAMs_2020 <- apply(CAMs,1,mean)
Jena_T_2020 <- apply(Jena_T,1,mean)

Jena_2013 <- apply(Jena[,1:19],1,mean)
CAMs_2013 <- apply(CAMs[,1:19],1,mean)
Jena_T_2013 <- apply(Jena_T[,1:19],1,mean)

ANN  <- apply(ANN,1,mean)
MAR  <- apply(MAR,1,mean)
RF   <- apply(RF,1,mean)

CM   <- apply(CM[,1:19],1,mean)
ES   <- apply(ES[,1:19],1,mean)
LM   <- apply(LM[,1:19],1,mean)
MM   <- apply(MM[,1:19],1,mean)


Inv_2020     <- (Jena_2020+CAMs_2020+Jena_T_2020)/3
Inv_2013     <- (Jena_2013+CAMs_2013+Jena_T_2013)/3

FLUXCOM <- (ANN+MAR+RF)/3
ESMs    <- (CM + ES + LM + MM)/4

#######################################



#############################
tmp_Inv40     <- data.frame(Value=Inv_2020,Lat=index[,2],Type="Inv20")
tmp_Inv33     <- data.frame(Value=Inv_2013,Lat=index[,2],Type="Inv13")
tmp_FLUXCOM   <- data.frame(Value=FLUXCOM,Lat=index[,2],Type="FLUXCOM")
tmp_ESMs      <- data.frame(Value=ESMs,Lat=index[,2],Type="ESMs")
Lat_range     <- seq(-54.5,83.5,2)
tmp_data      <- rbind(tmp_Inv40,tmp_Inv33,tmp_FLUXCOM,tmp_ESMs)
in_val        <- findInterval(tmp_data$Lat,Lat_range)
tmp_1         <- data.frame(tmp_data,range=in_val)
df_tmp        <- ddply(tmp_1,.(Type,range),summarize,value=mean(Value),sd=sd(Value),Lat=mean(Lat))

#########--------------------------------

###################################################################################
tmp_data       <- data.frame(x=index[,1],y=index[,2],z=Inv_2020 )
tmp_raster     <- rasterFromXYZ(tmp_data)
projection(tmp_raster) <- c("+proj=longlat +ellps=WGS84 +no_defs")
tmp_raster     <- raster::projectRaster(tmp_raster,crs="+proj=robin",method="ngb")
Inv_F_2020    <- data.frame(rasterToPoints(tmp_raster),Type="Inv20")


###################################################################################Inv
tmp_data       <- data.frame(x=index[,1],y=index[,2],z=INV_2020_no[,1])
tmp_raster     <- rasterFromXYZ(tmp_data)
projection(tmp_raster) <- c("+proj=longlat +ellps=WGS84 +no_defs")
tmp_raster     <- raster::projectRaster(tmp_raster,crs="+proj=robin",method="ngb")
Inv_p_2020    <- data.frame(rasterToPoints(tmp_raster),Type="Inv_p_2020")

###################################################################################InvP
tmp_data       <- data.frame(x=index[,1],y=index[,2],z=Inv_2013 )
tmp_raster     <- rasterFromXYZ(tmp_data)
projection(tmp_raster) <- c("+proj=longlat +ellps=WGS84 +no_defs")
tmp_raster     <- raster::projectRaster(tmp_raster,crs="+proj=robin",method="ngb")
Inv_F_2013    <- data.frame(rasterToPoints(tmp_raster),Type="Inv13")


###################################################################################InvP
tmp_data       <- data.frame(x=index[,1],y=index[,2],z=INV_2013_no[,1])
tmp_raster     <- rasterFromXYZ(tmp_data)
projection(tmp_raster) <- c("+proj=longlat +ellps=WGS84 +no_defs")
tmp_raster     <- raster::projectRaster(tmp_raster,crs="+proj=robin",method="ngb")
Inv_p_2013    <- data.frame(rasterToPoints(tmp_raster),Type="Inv_p_2013")

###################################################################################
tmp_data       <- data.frame(x=index[,1],y=index[,2],z=FLUXCOM)
tmp_raster     <- rasterFromXYZ(tmp_data)
projection(tmp_raster) <- c("+proj=longlat +ellps=WGS84 +no_defs")
tmp_raster     <- raster::projectRaster(tmp_raster,crs="+proj=robin",method="ngb")
FLUXCOM_F    <- data.frame(rasterToPoints(tmp_raster),Type="FLUXCOM")
###################################################################################
tmp_data       <- data.frame(x=index[,1],y=index[,2],z=FLUXCOM_no[,1])
tmp_raster     <- rasterFromXYZ(tmp_data)
projection(tmp_raster) <- c("+proj=longlat +ellps=WGS84 +no_defs")
tmp_raster     <- raster::projectRaster(tmp_raster,crs="+proj=robin",method="ngb")
FLUXCOM_p    <- data.frame(rasterToPoints(tmp_raster),Type="FLUXCOM_p")
###################################################################################
tmp_data       <- data.frame(x=index[,1],y=index[,2],z=ESMs)
tmp_raster     <- rasterFromXYZ(tmp_data)
projection(tmp_raster) <- c("+proj=longlat +ellps=WGS84 +no_defs")
tmp_raster     <- raster::projectRaster(tmp_raster,crs="+proj=robin",method="ngb")
ESMs_F    <- data.frame(rasterToPoints(tmp_raster),Type="ESMs")
###################################################################################
tmp_data       <- data.frame(x=index[,1],y=index[,2],z=ESM_no[,1])
tmp_raster     <- rasterFromXYZ(tmp_data)
projection(tmp_raster) <- c("+proj=longlat +ellps=WGS84 +no_defs")
tmp_raster     <- raster::projectRaster(tmp_raster,crs="+proj=robin",method="ngb")
ESMs_p    <- data.frame(rasterToPoints(tmp_raster),Type="ESMs_p")
##########------------------------------------------------------------------------
df_data      <- rbind(Inv_F_2020,Inv_F_2013,FLUXCOM_F,ESMs_F)
df_data$Type <- factor(df_data$Type,levels=c("Inv20","Inv13","FLUXCOM","ESMs")) 

df_data$z[which(df_data$z < -4)]=-4.01
df_data$z[which(df_data$z > 4)] =4.01
#######################_--------------------------------------------------------------------------------
Jena_G  <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/Jena_15_year_trend_nee.txt",head=T,sep='')
CAMS_G  <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/CAMS_15_year_trend_nee.txt",head=T,sep='')
Jena_T_G  <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/Jena_T_15_year_trend_nee.txt",head=T,sep='')

ANN_G  <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/ANN_15_year_trend_nee.txt",head=T,sep='')
MAR_G  <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/MAR_15_year_trend_nee.txt",head=T,sep='')
RF_G  <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/RF_15_year_trend_nee.txt",head=T,sep='')

CM_G  <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/CM_15_year_trend_nee.txt",head=T,sep='')
ES_G  <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/ES_15_year_trend_nee.txt",head=T,sep='')
LM_G  <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/LM_15_year_trend_nee.txt",head=T,sep='')
MM_G  <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/Sensitivity_WEI_WEF/MM_15_year_trend_nee.txt",head=T,sep='')


Inv_2020 <- data.frame(Value=c(Jena_G$WE,CAMS_G$WE,Jena_T_G$WE,Jena_G$NoWE,CAMS_G$NoWE,Jena_T_G$NoWE),Type=rep(c("WE","NonWE"),each=26*3),Class="Inv20")
Inv_2013 <- data.frame(Value=c(Jena_G$WE[1:19],CAMS_G$WE[1:19],Jena_T_G$WE[1:19],Jena_G$NoWE[1:19],CAMS_G$NoWE[1:19],Jena_T_G$NoWE[1:19]),Type=rep(c("WE","NonWE"),each=19*3),Class="Inv13")
FLUXCOM  <- data.frame(Value=c(ANN_G$WE,MAR_G$WE,RF_G$WE,ANN_G$NoWE,MAR_G$NoWE,RF_G$NoWE),Type=rep(c("WE","NonWE"),each=19*3),Class="FLUXCOM")
ESMs  <- data.frame(Value=c(CM_G$WE[1:19],ES_G$WE[1:19],LM_G$WE[1:19],MM_G$WE[1:19],CM_G$NoWE[1:19],ES_G$NoWE[1:19],LM_G$NoWE[1:19],MM_G$NoWE[1:19]),Type=rep(c("WE","NonWE"),each=19*4),Class="ESMs")

df_Pcor <- rbind(Inv_2020,Inv_2013,FLUXCOM,ESMs)
df_Pcor_bar <- ddply(df_Pcor,.(Class,Type),summarize,Mean=mean(Value),Ymax=max(Value),Ymin=min(Value),Upper=quantile(Value,0.75),Lower=quantile(Value,0.25))
df_Pcor_bar$Class <- factor(df_Pcor_bar$Class,levels=c("Inv20","Inv13","FLUXCOM","ESMs"))

df_Pcor$Class <- factor(df_Pcor$Class,levels=c("Inv20","Inv13","FLUXCOM","ESMs"))

label_data <- data.frame(x=rep(-15000000,times=4),y=rep(8000000,times=4),lab=c("a","b","c","d"),Type=unique(df_data$Type))



##########_--------------------------------------------------------------------------------------------
pdf(file = "F:/Figure2.pdf", width=6.5, height=6.2)   
pushViewport(viewport(layout=grid.layout(100,100)))
       
#windowsFonts(Times = windowsFont("Times New Roman"))

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
                        legend.key.height=unit(1.5,"lines"),
                        legend.key.width=unit(0.5,"lines"),
                        legend.spacing = unit(-0.5, "cm"),
                        #legend.position="bottom",
                        legend.direction="vertical",
                         plot.margin = unit(c(0.1,0.1,0.0,0.1),units="lines"),
                        panel.border = element_rect( colour = NA,fill=NA, size=0.),
                         panel.spacing.y = unit(2.5, "lines"),

                        panel.grid=element_blank(),

                        strip.background = element_blank(),
                        axis.title.x=element_text( size=6.5,family="Times")

                      ))

h1 <-   ggplot(data=df_data,aes(x=x,y=y)) + theme_bw()+
        geom_tile(aes(x=x,y=y,fill=z),show.legend=T)+
        geom_path(data=world, aes(long, lat, group=group, fill=NULL), linetype="solid", color="black",size = 0.2) +
        geom_path(data=box, aes(long, lat, group=group, fill=NULL), linetype="solid", color="black",size = 0.2) +
        scale_fill_distiller(palette="RdBu", direction=-1,breaks=seq(-3,3,1))+
        scale_y_continuous(expand = c(0,0),limits=c(-6336039,8625155))+
        scale_x_continuous(expand = c(0,0))+
        geom_text(data=label_data,aes(x=x,y=y,label=lab),family="Times",size=6.9/.pt,fontface="bold")+

        xlab(NULL)+ylab(NULL)+
        guides(fill= guide_colorbar(byrow = TRUE,ticks = TRUE, even.steps = FALSE,frame.linewidth = 0.55, frame.colour = "black", ticks.colour = "black",
        ticks.linewidth = 0.3,nrow=1,title.position="left",title=expression(Delta~Trend[NEE]~(WE-NonWE)~(g~C~Year^-1)),label.position = "right",title.theme=element_text(size=6.5,colour="black",angle=90,family="Times"),label=T,label.theme=element_text(size=6.5,colour="black",angle=0,family="Times")))+  
         facet_wrap( ~Type, ncol=2)+coord_equal() +   
        theme_opts
print(h1, vp=viewport(layout.pos.row=1:70, layout.pos.col=1:100))
#########################
theme_opts <- list(theme(
                        axis.text = element_blank(),
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
Inv_p_2020$z <- factor(Inv_p_2020$z)
h1 <-   ggplot(data=Inv_p_2020,aes(x=x,y=y,fill=z)) + theme_bw()+
        geom_tile(aes(x=x,y=y))+
        geom_path(data=world, aes(long, lat, group=group, fill=NULL), linetype="solid", color="black",size = 0.2) +
        geom_path(data=box, aes(long, lat, group=group, fill=NULL), linetype="solid", color="black",size = 0.2) +
       scale_fill_manual(values = c("gray80","yellow","red", "#70D3E0", "#97A1FC"))+
        scale_y_continuous(expand = c(0,0),limits=c(-6336039,8625155))+
        scale_x_continuous(expand = c(0,0))+
        xlab(NULL)+ylab(NULL)+
        geom_text(aes(x=7000000,y=-4300000),label="38%",family="Times",size=6.9/.pt,check_overlap = TRUE)+
        guides(fill= guide_legend(title.vjust=-0.4,title=NULL,label.hjust=-1,label=T,label.theme=element_text(size=6.5,colour="black",angle=0,family="Times")))+  
        facet_wrap( ~Type, ncol=2)+coord_equal() + 
        theme_opts
sbvp1 <- viewport(width=0.2,height=0.4,x=0.274,y=0.67)
print(h1,vp=sbvp1,more=TRUE)
########################################
Inv_p_2013$z <- factor(Inv_p_2013$z)
h1 <-   ggplot(data=Inv_p_2013,aes(x=x,y=y,fill=z)) + theme_bw()+
        geom_tile(aes(x=x,y=y))+
        geom_path(data=world, aes(long, lat, group=group, fill=NULL), linetype="solid", color="black",size = 0.2) +
        geom_path(data=box, aes(long, lat, group=group, fill=NULL), linetype="solid", color="black",size = 0.2) +
       scale_fill_manual(values = c("gray80","yellow","red", "#70D3E0", "#97A1FC"))+
        scale_y_continuous(expand = c(0,0),limits=c(-6336039,8625155))+
        scale_x_continuous(expand = c(0,0))+
        xlab(NULL)+ylab(NULL)+
        geom_text(aes(x=7000000,y=-4300000),label="47%",family="Times",size=6.9/.pt,check_overlap = TRUE)+

        guides(fill= guide_legend(title.vjust=-0.4,title=NULL,label.hjust=-1,label=T,label.theme=element_text(size=6.5,colour="black",angle=0,family="Times")))+  
        facet_wrap( ~Type, ncol=2)+coord_equal() + 
        theme_opts
sbvp1 <- viewport(width=0.2,height=0.4,x=0.70,y=0.67)
print(h1,vp=sbvp1,more=TRUE)
######################################
##################################

FLUXCOM_p$z <- factor(FLUXCOM_p$z)
h1 <-   ggplot(data=FLUXCOM_p,aes(x=x,y=y,fill=z)) + theme_bw()+
        geom_tile(aes(x=x,y=y))+
        geom_path(data=world, aes(long, lat, group=group, fill=NULL), linetype="solid", color="black",size = 0.2) +
        geom_path(data=box, aes(long, lat, group=group, fill=NULL), linetype="solid", color="black",size = 0.2) +
     #   scale_fill_distiller(breaks=seq(-12,12,4),palette="RdBu", direction=-1)+
       scale_fill_manual(values = c("gray80","yellow","red", "#70D3E0", "#97A1FC"))+
        scale_y_continuous(expand = c(0,0),limits=c(-6336039,8625155))+
        scale_x_continuous(expand = c(0,0))+

        xlab(NULL)+ylab(NULL)+
        geom_text(aes(x=7000000,y=-4300000),label="44%",family="Times",size=6.9/.pt,check_overlap = TRUE)+

        guides(fill= guide_legend(title.vjust=-0.4,title=NULL,label.hjust=-1,label=T,label.theme=element_text(size=6.5,colour="black",angle=0,family="Times")))+  
        facet_wrap( ~Type, ncol=2)+coord_equal() + 
        theme_opts
sbvp1 <- viewport(width=0.2,height=0.4,x=0.274,y=0.36)
print(h1,vp=sbvp1,more=TRUE)
#########################################################################################

ESMs_p$z <- factor(ESMs_p$z)
h1 <-   ggplot(data=ESMs_p,aes(x=x,y=y,fill=z)) + theme_bw()+
        geom_tile(aes(x=x,y=y))+
        geom_path(data=world, aes(long, lat, group=group, fill=NULL), linetype="solid", color="black",size = 0.2) +
        geom_path(data=box, aes(long, lat, group=group, fill=NULL), linetype="solid", color="black",size = 0.2) +
     #   scale_fill_distiller(breaks=seq(-12,12,4),palette="RdBu", direction=-1)+
       scale_fill_manual(values = c("gray80","yellow","red", "#70D3E0", "#97A1FC"))+
        scale_y_continuous(expand = c(0,0),limits=c(-6336039,8625155))+
        scale_x_continuous(expand = c(0,0))+

        xlab(NULL)+ylab(NULL)+
        geom_text(aes(x=7000000,y=-4300000),label="53%",family="Times",size=6.9/.pt,check_overlap = TRUE)+

        guides(fill= guide_legend(title.vjust=-0.4,title=NULL,label.hjust=-1,label=T,label.theme=element_text(size=6.5,colour="black",angle=0,family="Times")))+  
        facet_wrap( ~Type, ncol=2)+coord_equal() + 
        theme_opts
sbvp1 <- viewport(width=0.2,height=0.4,x=0.70,y=0.36)
print(h1,vp=sbvp1,more=TRUE)


#########_--------------------------------------------------------
theme_opts <- list(theme(axis.text.x = element_text(size=7,family='Times',colour = "black"),
                         axis.text.y = element_text(size=7,family='Times',colour = "black"),
                        axis.title.x = element_text(size=7,family='Times',colour = "black"),
                        axis.title.y =element_text(size=7,family='Times',colour = "black"),
                        axis.ticks = element_line(size=0.5),
                        axis.ticks.length = unit(.04, "cm"),
                        panel.grid=element_blank(),
                        legend.spacing = unit(5, "cm"),
                         legend.margin=margin(t=-1),
                        legend.position=c(0.7,0.05),
                       legend.direction="horizontal",
                     #   strip.text.x=element_text( size=9,family="Times",margin = margin(0.12,0,0.12,0, "cm")),
                       strip.text=element_blank(),
                        legend.background = element_rect(fill='transparent'),
                        legend.key.height=unit(0.3,"lines"),
                        legend.key.width=unit(0.5,"lines"),
                      #  panel.border = element_rect(fill=NA, colour = "black", size=0.25),
                         plot.margin = unit(c(0.1,0.1,0.0,0.1),units="lines"),
axis.line = element_line(colour = "black",size=0.),
       panel.border = element_blank(),
                        legend.key.size=unit(10,"cm"),
                        strip.background = element_blank(),
                        plot.title = element_text(size=22)))

H <-   ggplot(data=df_Pcor_bar,aes(x=Class,col=Type)) + theme_bw()+ 
       geom_boxplot(aes(lower=Lower,upper=Upper,middle=Mean,ymin=Ymin,ymax=Ymax),stat = "identity")+
       geom_point(data=df_Pcor,aes(x=Class,y=Value),position = position_jitterdodge(jitter.width = 0.25),alpha=0.5,size=1/.pt)+
       ylab(expression(Trend[NEE]~(Pg~C~year^-1)))+xlab("")+
       geom_hline(yintercept=0,size=0.2,linetype = "longdash")+
       geom_text(aes(label="*(1/3)",x=1,y=0.17),size=6.9/.pt,family="Times",col="black",check_overlap = TRUE)+
       geom_text(aes(label="*(1/3)",x=2,y=0.18),size=6.9/.pt,family="Times",col="black",check_overlap = TRUE)+
       geom_text(aes(label="*(3/3)",x=3,y=0.16),size=6.9/.pt,family="Times",col="black",check_overlap = TRUE)+
       geom_text(aes(label="*(2/4)",x=4,y=0.13),size=6.9/.pt,family="Times",col="black",check_overlap = TRUE)+
       scale_colour_manual(values = c("red4",  "#0066cc"))+

      scale_x_discrete(labels=c("Inversions_2020"="Inv","Inversions_2013"="Inv","FLUXCOM"="FLUXCOM","ESMs"="ESMs"))+
      guides( col=guide_legend(title="",label=T,label.theme=element_text(size=6.5,colour="black",angle=0,family="Times")),
              fill=guide_legend(title="",label=T,label.theme=element_text(size=6.5,colour="black",angle=0,family="Times")))+
       theme_opts
#print(H)
print(H, vp=viewport(layout.pos.row=75:100, layout.pos.col=1:41))
########################################################


theme_opts <- list(theme(axis.text.x = element_text(size=6.9,family='Times',color="black"),
                         axis.text.y = element_text(size=6.9,family='Times',color="black"),
                        axis.title.x = element_text(size=6.9,family='Times'),
                        axis.title.y =element_text(size=6.9,family='Times'),
                        axis.ticks = element_line(size=0.5),
                        axis.ticks.length = unit(.04, "cm"),
                        legend.spacing.y = unit(0.08, "cm"),
                        legend.position=c(0.78,0.95),
                      #  strip.text.x=element_text( size=9,family="Times",margin = margin(0.12,0,0.12,0, "cm")),
                        strip.text=element_blank(),
                         plot.margin = unit(c(0.1,0.1,0.0,-0.9),units="lines"),
axis.line = element_line(colour = "black",size=0.),
       panel.border = element_blank(),
                        legend.background = element_rect(fill='transparent'),
                        legend.key.height=unit(0.3,"lines"),
                        legend.key.width=unit(0.5,"lines"),
                        legend.key.size=unit(10,"cm"),
                        panel.grid=element_blank(),
                        plot.title = element_text(size=22)))

df_tmp <- subset(df_tmp,Lat>=-51.5)
H <-   ggplot(data=df_tmp,aes(x=Lat,y=value,col=Type,fill=Type)) + theme_bw()+
       geom_hline(yintercept=0,size=0.2,col="gray",linetype="longdash")+

       geom_line(size=0.35)+
        geom_text(aes(x=-40,y=4,label="e  f"),family="Times",size=6.9/.pt,show.legend=F,col="black",check_overlap = TRUE,fontface="bold")+

       ylab(expression(Delta~Trend[NEE]~(g~Year^-1)))+xlab("Latitude[o]")+
      scale_color_aaas()+
      scale_fill_aaas(alpha=0.4)+
     scale_x_continuous(breaks=seq(-50,80,25))+
      guides( col=guide_legend(title="",label=T,label.theme=element_text(size=6.5,angle=0,family="Times")),
              fill=guide_legend(title="",label=T,label.theme=element_text(size=6.5,angle=0,family="Times")))+
           #   facet_wrap( ~Clas, ncol=2,scales = "free_y")+
       theme_opts
print(H, vp=viewport(layout.pos.row=74:100, layout.pos.col=51:85))


#########################################################################
theme_opts <- list(theme(axis.text.x = element_text(size=6,family='Times',colour = "black",angle=45),
                         axis.text.y = element_text(size=6,family='Times',colour = "black"),
                        axis.title.x = element_text(size=6,family='Times',colour = "black"),
                        axis.title.y =element_text(size=6,family='Times',colour = "black"),
                        axis.ticks = element_line(size=0.2),
                        axis.ticks.length = unit(.02, "cm"),
                        panel.grid=element_blank(),
                        legend.spacing = unit(0.08, "cm"),
                        legend.position="bottom",
            #            strip.text.x=element_text( size=6,family="Times",margin = margin(0.12,0,0.12,0, "cm")),
                        strip.text=element_blank(),
                        legend.background = element_rect(fill='transparent'),
                        legend.key.height=unit(0.4,"lines"),
                        legend.key.width=unit(0.5,"lines"),
                    #    panel.border = element_rect(fill='transparent', colour = "black", size=0.28),
                        legend.key.size=unit(10,"cm"),
                       plot.background =element_rect(fill='transparent'),
                        plot.margin = unit(c(0.,0.,0.0,0.),units="lines"),
   axis.line = element_line(colour = "black",size=0.),
       panel.border = element_blank(),
                        plot.title = element_text(size=22)))

df_trend_S      <- data.frame(Contributions=c(1.336,-0.336),Type=c("Tro.","Non-Tro."))
df_trend_S$Type <- factor(df_trend_S$Type,levels=c("Tro.","Non-Tro."))
inset_1 <-   ggplot(data=df_trend_S,aes(x=Type,y=Contributions*100,col=Type,fill=Type)) + theme_bw()+ 
       geom_bar(size=0.2,stat="identity",alpha=0.85,width=0.25,,show.legend=F,col="black")+ 
       ylab("Contribution (%)")+xlab(NULL)+
       geom_hline(yintercept=0,size=0.01)+


     scale_color_npg(alpha=0.6)+
      scale_fill_npg(alpha=0.6)+
     scale_y_continuous(expand = c(0.,0.0),limits=c(-40,140),breaks=c(0,50,100))+
     scale_x_discrete(expand = c(0.1,0.1))+theme_opts+theme(axis.title.y = element_text(vjust = -1.5))

sbvp1 <- viewport(width=0.10,height=0.12,x=0.053,y=0.72)
print(inset_1,vp=sbvp1,more=TRUE)
########################################
df_trend_S      <- data.frame(Contributions=c(0.934,0.066),Type=c("Tro.","Non-Tro."))
df_trend_S$Type <- factor(df_trend_S$Type,levels=c("Tro.","Non-Tro."))
inset_1 <-   ggplot(data=df_trend_S,aes(x=Type,y=Contributions*100,col=Type,fill=Type)) + theme_bw()+ 
       geom_bar(size=0.2,stat="identity",alpha=0.85,width=0.25,,show.legend=F,col="black")+ 
       ylab("Contribution (%)")+xlab(NULL)+
       geom_hline(yintercept=0,size=0.01)+

     scale_color_npg(alpha=0.6)+
      scale_fill_npg(alpha=0.6)+
     scale_y_continuous(expand = c(0.,0.0),limits=c(-10,110),breaks=c(0,50,100))+
     scale_x_discrete(expand = c(0.1,0.1))+theme_opts+theme(axis.title.y = element_text(vjust = -1.5))

sbvp1 <- viewport(width=0.10,height=0.12,x=0.50,y=0.72)
print(inset_1,vp=sbvp1,more=TRUE)

#####################################
df_trend_S      <- data.frame(Contributions=c(0.767,0.233),Type=c("Tro.","Non-Tro."))
df_trend_S$Type <- factor(df_trend_S$Type,levels=c("Tro.","Non-Tro."))
inset_1 <-   ggplot(data=df_trend_S,aes(x=Type,y=Contributions*100,col=Type,fill=Type)) + theme_bw()+ 
       geom_bar(size=0.2,stat="identity",alpha=0.85,width=0.25,,show.legend=F,col="black")+ 
       ylab("Contribution (%)")+xlab(NULL)+
       geom_hline(yintercept=0,size=0.2)+

     scale_color_npg(alpha=0.6)+
      scale_fill_npg(alpha=0.6)+
     scale_y_continuous(expand = c(0.,0.0),limits=c(00,100),breaks=c(0,100,50))+
     scale_x_discrete(expand = c(0.1,0.1))+theme_opts+theme(axis.title.y = element_text(vjust = -1.5))


sbvp1 <- viewport(width=0.10,height=0.12,x=0.053,y=0.41)
print(inset_1,vp=sbvp1,more=TRUE)
#########################################################
df_trend_S      <- data.frame(Contributions=c(0.189,0.811),Type=c("Tro.","Non-Tro."))
df_trend_S$Type <- factor(df_trend_S$Type,levels=c("Tro.","Non-Tro."))
inset_1 <-   ggplot(data=df_trend_S,aes(x=Type,y=Contributions*100,col=Type,fill=Type)) + theme_bw()+ 
       geom_bar(size=0.2,stat="identity",alpha=0.85,width=0.25,,show.legend=F,col="black")+ 
       ylab("Contribution (%)")+xlab(NULL)+
       geom_hline(yintercept=0,size=0.2)+

     scale_color_npg(alpha=0.6)+
      scale_fill_npg(alpha=0.6)+
     scale_y_continuous(expand = c(0.,0.0),limits=c(00,100),breaks=c(0,100,50))+
     scale_x_discrete(expand = c(0.1,0.1))+theme_opts+theme(axis.title.y = element_text(vjust = -1.5))


sbvp1 <- viewport(width=0.10,height=0.12,x=0.50,y=0.41)
print(inset_1,vp=sbvp1,more=TRUE)




dev.off()





