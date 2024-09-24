rm(list=ls())
library(colorRamps)
library(ggplot2)
library("viridis") 
library(ggsci)
library(grid)
library(raster)
library(extrafont)
library(gridExtra)
library(plyr)
library(rasterVis)
library(showtext)

font_add(family='Times',regular='C:/Windows/Fonts/times.ttf')
showtext_auto()
############################################--------------------------------------------
get_area_weighted_Tem <- function(input_data){
df_nee <- c()
for(i in 1:(ncol(input_data)-2)){
    tmp_data   <- data.frame(x=input_data$x,y=input_data$y,nee=input_data[,i])
    tmp_image  <- rasterFromXYZ(tmp_data)
    crs(tmp_image) <- '+proj=longlat +ellps=WGS84 +no_defs'
    tmp_area   <- raster::area(tmp_image) *1000^2
    nee_spo    <- tmp_area*tmp_image
    a          <- !!tmp_image
    a_area     <- tmp_area*a
    tmp_nee    <- data.frame(nee=sum(nee_spo[],na.rm=T)/sum(a_area[],na.rm=T))
    df_nee     <- rbind(df_nee,tmp_nee)
    }
    return(df_nee)
}
get_Year_mean <- function(x,tmp_ind){
    x_tmp <- split(x,tmp_ind)
    x_tmp_length <- sapply(x_tmp,FUN=function(x){sum(x,na.rm=T)})
    return(x_tmp_length)
    }
get_V_mean <- function(x,tmp_ind){
    x_tmp <- split(x,tmp_ind)
    x_tmp_length <- sapply(x_tmp,FUN=function(x){mean(x,na.rm=T)})
    return(x_tmp_length)
    }
#####-----------------------------------------
##########------------------------------------------------------------------------------------------------
tx90p_L <- read.table("F:/Flux_paper/R/Analyse/Revise_R/Stations/Station_month_length.txt",head=F,sep=' ')
tx90p_V <- read.table("F:/Flux_paper/R/Analyse/Revise_R/Stations/Station_month_value.txt",head=F,sep=' ')

nNA_length <- apply(tx90p_L,1,FUN=function(x){length(which(!is.na(x)))})
index      <- which(nNA_length>(35*12))
tx90p_V    <- tx90p_V[index,]
tx90p_L    <- tx90p_L[index,]
tmp_year   <- rep(1980:2021,each=12)
tx_v       <- tx90p_L*tx90p_V
sum_v      <- t(apply(tx_v,1,get_Year_mean,tmp_year)[-c(1,42),])
sum_l      <- t(apply(tx90p_L,1,get_Year_mean,tmp_year)[-c(1,42),])
tx_v       <- sum_v/sum_l
tmp        <- apply(tx_v,2,mean,na.rm=T)
#########------------------------------------------------
#tx90d    <- data.frame(tx90d)
df_Obs  <- apply(sum_l,2,mean,na.rm=T)
df_Obs  <- data.frame(WE =df_Obs,Year=1981:2020,Type="GSOD")

######-------------------------------------------------------------------CRU
tx90p_L <- read.table("F:/Flux_paper/R/Analyse/Revise_R/CRUNCEP/CRUNCEP_month_length.txt",head=F,sep=' ')
tx90p_V <- read.table("F:/Flux_paper/R/Analyse/Revise_R/CRUNCEP/CRUNCEP_month_value.txt",head=F,sep=' ')
index   <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/index.txt",head=T,sep=' ')

tmp_year  <- rep(1980:2016,each=12)

tx_v       <- tx90p_L*tx90p_V
sum_v      <- t(apply(tx_v,1,get_Year_mean,tmp_year)[-c(1),])
sum_l      <- t(apply(tx90p_L,1,get_Year_mean,tmp_year)[-c(1),])
tx_v       <- sum_v/sum_l
tx90v     <- data.frame(tx_v,x=index[,1],y=index[,2])
tx90v     <- get_area_weighted_Tem(tx90v)


df_CRU    <- apply(sum_l,2,mean,na.rm=T)

df_CRU    <- data.frame(WE =df_CRU,Year=1981:2016,Type="CRUNCEP")




############---------------------------------------------------------------------ERA5
tx90p_L <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/ERA5_month_length.txt",head=F,sep=' ')
tx90p_V <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/ERA5_month_value.txt",head=F,sep=' ')
tmp_year  <- rep(1980:2021,each=12)

tx_v       <- tx90p_L*tx90p_V
sum_v      <- t(apply(tx_v,1,get_Year_mean,tmp_year)[-c(1,42),])
sum_l      <- t(apply(tx90p_L,1,get_Year_mean,tmp_year)[-c(1,42),])
tx_v       <- sum_v/sum_l
tx90v     <- data.frame(tx_v,x=index[,1],y=index[,2])
tx90v     <- get_area_weighted_Tem(tx90v)


df_ERA   <- apply(sum_l,2,mean,na.rm=T)

df_ERA    <- data.frame(WE =df_ERA,Year=1981:2020,Type="ERA5")

df_data    <- data.frame(rbind(df_Obs,df_CRU,df_ERA))
##############------------------------------------------------------
df_trend      <- data.frame(Trend=c(2.4274,2.3515,2.2839),Type=c("ERA5","CRUNCEP","GSOD"))
df_trend$Type <- factor(df_trend$Type,levels=c("ERA5","CRUNCEP","GSOD"))
#########--------------------------------------------------------------------------------
tx90p_L <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/ERA5_month_length.txt",head=F,sep=' ')
tx90p_V <- read.table("F:/Flux_paper/R/Analyse/Revise_R/dataframe/ERA5_month_value.txt",head=F,sep=' ')
tmp_year  <- rep(1980:2021,each=12)
tx_v       <- tx90p_L*tx90p_V
sum_v      <- t(apply(tx_v,1,get_Year_mean,tmp_year)[-c(1,42),])
sum_l      <- t(apply(tx90p_L,1,get_Year_mean,tmp_year)[-c(1,42),])
tx_v       <- sum_v/sum_l

Spo_L     <- t(apply(tx90p_L,1,get_Year_mean,tmp_year)[-c(1,42),])

tx90p_L    <- data.frame(Spo_L,x=index[,1],y=index[,2])
tx90p_V    <- data.frame(tx_v,x=index[,1],y=index[,2])


year    <- 1981:2020
tx90d_L_trend <- apply(tx90p_L[,1:40],1,FUN=function(x,year){summary(lm(x~year))$coefficient[2]},year)
tx90d_L_p     <- apply(tx90p_L[,1:40],1,FUN=function(x,year){summary(lm(x~year))$coefficient[8]},year)
tx90p_V_trend <- apply(tx90p_V[,1:40],1,FUN=function(x,year){summary(lm(x~year))$coefficient[2]},year)
tx90p_V_p     <- apply(tx90p_V[,1:40],1,FUN=function(x,year){summary(lm(x~year))$coefficient[8]},year)


df_Spo        <- data.frame(Value=c(tx90d_L_trend,tx90p_V_trend),p=c(tx90d_L_p,tx90p_V_p),Type=rep(c("Days","Value"),each=length(tx90d_L_p)),
                            x=tx90p_L[,41],y=tx90p_L[,42])


L_trend       <- subset(df_Spo,Type=="Days")
L_trend       <- data.frame(x=L_trend$x,y=L_trend$y,z=L_trend$Value)
L_image       <- rasterFromXYZ(L_trend)
projection(L_image) <- c("+proj=longlat +ellps=WGS84 +no_defs")
L_proj             <- raster::projectRaster(L_image,crs="+proj=robin")
df_L     <- data.frame(rasterToPoints(L_proj),Type="Occurences")


L_p       <- subset(df_Spo,Type=="Days")
L_p       <- data.frame(x=L_p$x,y=L_p$y,z=L_p$p)
L_image       <- rasterFromXYZ(L_p)
projection(L_image) <- c("+proj=longlat +ellps=WGS84 +no_defs")
L_proj             <- raster::projectRaster(L_image,crs="+proj=robin")
df_L_p     <- data.frame(rasterToPoints(L_proj),Type="Occurences")




V_trend       <- subset(df_Spo,Type=="Value")
V_trend       <- data.frame(x=V_trend$x,y=V_trend$y,z=V_trend$Value)
V_image       <- rasterFromXYZ(V_trend)
projection(V_image) <- c("+proj=longlat +ellps=WGS84 +no_defs")
V_proj             <- raster::projectRaster(V_image,crs="+proj=robin")
df_V     <- data.frame(rasterToPoints(V_proj),Type="Trend")

V_p       <- subset(df_Spo,Type=="Value")
V_p       <- data.frame(x=V_p$x,y=V_p$y,z=V_p$p)
V_image       <- rasterFromXYZ(V_p)
projection(V_image) <- c("+proj=longlat +ellps=WGS84 +no_defs")
V_proj             <- raster::projectRaster(V_image,crs="+proj=robin")
df_V_p     <- data.frame(rasterToPoints(V_proj),Type="Trend")

#df_Spo    <- rbind(df_L,df_V)

df_L$z[which(df_L$z < -1.8)]=-1.79
df_L$z[which(df_L$z > 1.8)] =1.79
df_V$z <- df_V$z*100
df_V$z[which(df_V$z < -12)]=-11.9
df_V$z[which(df_V$z > 12)] =11.9
######################################################################################################


pdf(file = "F:/Figure1.pdf",width=6.2, height=4.2,family="Times")          
anno_data <- data.frame(lab=c("0.76~days~year^-1","0.97~days~year^-1","0.73~days~year^-1"),x=c(1999.99,1999.99,1999.99),y=c(69,63.5,58),Type=c("CRUNCEP","ERA5","GSOD"))
anno_data$Type <- factor(anno_data$Type,levels=c("CRUNCEP","ERA5","GSOD"))
#windowsFonts(Times = windowsFont("Times New Roman"))

pushViewport(viewport(layout=grid.layout(100,100)))

theme_opts <- list(theme(axis.text.x = element_text(size=7,family='Times',color="black"),
                         axis.text.y = element_text(size=7,family='Times',color="black"),
                        axis.title.x = element_text(size=7,family='Times'),
                        axis.title.y =element_text(size=7,family='Times'),
                        axis.ticks = element_line(size=0.3),
                        axis.ticks.length = unit(.04, "cm"),
                        legend.spacing.y = unit(0.08, "cm"),
                        legend.position=c(0.18,0.85),
                      #  strip.text.x=element_text( size=9,family="Times",margin = margin(0.12,0,0.12,0, "cm")),
                        strip.text=element_blank(),

                        legend.background = element_rect(fill='transparent'),
                        legend.key.height=unit(0.7,"lines"),
                        legend.key.width=unit(0.5,"lines"),
                        legend.key.size=unit(10,"cm"),
                        panel.border = element_rect(fill=NA, colour = "black", size=0.28),
                        panel.grid=element_blank(),
                        plot.title = element_text(size=22)))


H <-   ggplot(data=df_data,aes(x=Year,y=WE,col=Type,fill=Type)) + theme_bw()+
       geom_point(size=0.4)+
       geom_line(size=0.35)+
       ylab(expression(WEF~(days~year^-1)))+xlab("Year")+
       geom_text(data=anno_data,aes(label=lab,x=x,y=y),size=6.5/.pt,family="Times",parse=T,show.legend=F)+
       scale_color_aaas()+
       scale_fill_aaas(alpha=0.4)+
       scale_y_continuous(expand = c(0,0),limits=c(20.5,75),breaks=seq(0,75,10))+
       guides( col=guide_legend(title="",label=T,label.theme=element_text(size=6.5,angle=0,family="Times")),
              fill=guide_legend(title="",label=T,label.theme=element_text(size=6.5,angle=0,family="Times")))+
              theme_opts
print(H, vp=viewport(layout.pos.row=1:42, layout.pos.col=1:50))


#####---------------------------------------------------------------------------------------------------------------
theme_opts <- list(theme(axis.text.x = element_text(size=7,family='Times',colour = "black"),
                         axis.text.y = element_text(size=7,family='Times',colour = "black"),
                        axis.title.x = element_text(size=7,family='Times',colour = "black"),
                        axis.title.y =element_text(size=7,family='Times',colour = "black"),
                        axis.ticks = element_line(size=0.5),
                        axis.ticks.length = unit(.04, "cm"),
                        panel.grid=element_blank(),
                        legend.spacing = unit(0.08, "cm"),
                        legend.position="bottom",
                        strip.text=element_blank(),
                        legend.background = element_rect(fill='transparent'),
                        legend.key.height=unit(0.4,"lines"),
                        legend.key.width=unit(0.5,"lines"),
                        panel.border = element_rect(fill=NA, colour = "black", size=0.28),
                        legend.key.size=unit(10,"cm"),
                        strip.background = element_blank(),
                        plot.title = element_text(size=22)))


H <-   ggplot(data=df_trend,aes(x=Type,y=Trend,col=Type,fill=Type)) + theme_bw()+ 
       geom_bar(position=position_dodge(0.5),size=0.2,stat="identity",alpha=0.85,width=0.4,,show.legend=F,col="black")+ 
        ylab(expression(WEI~trend~(10^-2~degree~C~year^-1)))+xlab("Datasets")+
       geom_text(data=subset(df_trend,Type=="ERA5"),aes(label="**",x=Type,y=Trend+0.1),size=6.5/.pt,family="Times",col="black")+
       geom_text(data=subset(df_trend,Type=="CRUNCEP" | Type=="GSOD"),aes(label="*",x=Type,y=Trend+0.1),size=6.5/.pt,family="Times",col="black")+
       geom_text(data=subset(df_trend,Type=="ERA5"),aes(label="p = 0.001",x=Type,y=Trend+0.35),size=6.5/.pt,family="Times",col="black")+
       geom_text(data=subset(df_trend,Type=="CRUNCEP" ),aes(label="p = 0.017",x=Type,y=Trend+0.35),size=6.5/.pt,family="Times",col="black")+
       geom_text(data=subset(df_trend,Type== "GSOD"),aes(label="p = 0.025",x=Type,y=Trend+0.35),size=6.5/.pt,family="Times",col="black")+

      scale_color_aaas(alpha=0.3)+
      scale_fill_aaas(alpha=0.3)+
      scale_y_continuous(expand = c(0,0),limits=c(0,2.9))+
      guides( col=guide_legend(title="",label=T,label.theme=element_text(size=6.5,colour="black",angle=0,family="Times")),
              fill=guide_legend(title="",label=T,label.theme=element_text(size=6.5,colour="black",angle=0,family="Times")))+

       theme_opts


print(H, vp=viewport(layout.pos.row=1:42, layout.pos.col=51:100))
######
    

#######¡ª¡ª----------------------------------------------------------------------------------
world <- shapefile("F:/Flux_paper/dataframe/world_shape/World_Continents.shp")
world     <- spTransform(world, CRS("+proj=robin"))
world <- fortify(world)
box <- shapefile("F:/Flux_paper/dataframe/world_shape/ne_110m_populated_places/ne_110m_wgs84_bounding_box.shp")
box <- crop(box,c(-180,180,-60,90))
box     <- spTransform(box, CRS("+proj=robin"))
box <- fortify(box)
theme_opts <- list(theme(

                        axis.text = element_blank(),
                        strip.text=element_blank(),
                        axis.ticks = element_line(size=0.0),
                        axis.ticks.length = unit(.00, "cm"),
                        legend.key.height=unit(0.3,"lines"),
                        legend.key.width=unit(1.8,"lines"),
                        legend.spacing = unit(-5.5, "cm"),
legend.margin=margin(20,0,0,0),
                        legend.position="bottom",
                        legend.direction="horizontal",
                         plot.margin = unit(c(0.1,0.1,-0.8,0.1),units="lines"),
                        panel.border = element_rect( colour = NA,fill=NA, size=0.),

                        panel.grid=element_blank(),

                        strip.background = element_blank(),
                        axis.title.x=element_text( size=6.5,family="Times")

                      ))


h1 <-   ggplot(data=df_L,aes(x=x,y=y,fill=z)) + theme_bw()+
        geom_tile(aes(x=x,y=y))+
        geom_path(data=world, aes(long, lat, group=group, fill=NULL), linetype="solid", color="black",size = 0.2) +
        geom_path(data=box, aes(long, lat, group=group, fill=NULL), linetype="solid", color="black",size = 0.2) +
        scale_fill_distiller(breaks=seq(-1.5,1.5,0.5),palette="RdBu", direction=-1)+
        scale_y_continuous(expand = c(0,0),limits=c(-6336039,8625155))+
        scale_x_continuous(expand = c(0,0))+
        xlab(NULL)+ylab(NULL)+
        geom_text(aes(x=-15000000,y=8000000),label="c",family="Times",size=6.9/.pt,fontface='bold',check_overlap = TRUE)+
        geom_text(aes(x=-16000000,y=6300000),label="a",family="Times",size=6.9/.pt,fontface='bold',check_overlap = TRUE)+

        guides(fill= guide_colorbar(byrow = TRUE,ticks = TRUE, even.steps = FALSE,frame.linewidth = 0.55, frame.colour = "black", ticks.colour = "black",
        ticks.linewidth = 0.3,nrow=1,title.hjust=-0.5,title.position="top",title=expression(WEF~trend~(days~year^-1)),label.position = "bottom",title.theme=element_text(size=6.5,colour="black",angle=0,family="Times"),label=T,label.vjust = -1,label.theme=element_text(size=6.5,colour="black",angle=0,family="Times")))+  
        facet_wrap( ~Type, ncol=2)+coord_equal() + 
        theme_opts

print(h1, vp=viewport(layout.pos.row=35:100, layout.pos.col=1:50))


########---------------------------------------------

inset_L_p <-   ggplot(data=df_L_p,aes(x=x,y=y,fill=z)) + theme_bw()+
        geom_tile(data=subset(df_L_p,z<0.05),aes(x=x,y=y),fill="gray50",show.legend=F)+
     #   geom_density_2d(data=subset(df_L_p,z<0.05),bins=20)+
       geom_path(data=world, aes(long, lat, group=group, fill=NULL), linetype="solid", color="black",size = 0.2) +
        geom_path(data=box, aes(long, lat, group=group, fill=NULL), linetype="solid", color="black",size = 0.2) +
    #    scale_fill_distiller(breaks=seq(-1.5,1.5,0.5),palette="RdBu", direction=-1)+
        scale_y_continuous(expand = c(0,0),limits=c(-6336039,8625155))+
        scale_x_continuous(expand = c(0,0))+
        xlab(NULL)+ylab(NULL)+
        geom_text(aes(x=-13000000,y=900000),label="81%",family="Times",size=6.9/.pt,fontface='plain',check_overlap = TRUE)+

        guides(fill= guide_colorbar(byrow = TRUE,ticks = TRUE, even.steps = FALSE,frame.linewidth = 0.55, frame.colour = "black", ticks.colour = "black",
        ticks.linewidth = 0.3,nrow=1,title.hjust=-0.4,title.position="top",title=expression(WEF~trend~(days~year^-1)),label.position = "bottom",title.theme=element_text(size=5,colour="black",angle=0,family="Times"),label=T,label.vjust = -1,label.theme=element_text(size=5,colour="black",angle=0,family="Times")))+  
        facet_wrap( ~Type, ncol=2)+coord_equal() + 
        theme_opts+theme( plot.background =element_rect(fill='transparent'), panel.border = element_rect(fill='transparent'))



sbvp1 <- viewport(width=0.25,height=0.47,x=0.32,y=0.22)
print(inset_L_p,vp=sbvp1,more=TRUE)

##########---------------------------------------
h1 <-   ggplot(data=df_V,aes(x=x,y=y,fill=z)) + theme_bw()+
        geom_tile(aes(x=x,y=y))+
        geom_path(data=world, aes(long, lat, group=group, fill=NULL), linetype="solid", color="black",size = 0.2) +
        geom_path(data=box, aes(long, lat, group=group, fill=NULL), linetype="solid", color="black",size = 0.2) +
        scale_fill_distiller(breaks=seq(-12,12,4),palette="RdBu", direction=-1)+
        scale_y_continuous(expand = c(0,0),limits=c(-6336039,8625155))+
        scale_x_continuous(expand = c(0,0))+
        xlab(NULL)+ylab(NULL)+
        geom_text(aes(x=-15000000,y=8000000),label="d",family="Times",size=6.9/.pt,fontface='bold',check_overlap = TRUE)+
        geom_text(aes(x=-16000000,y=6300000),label="b",family="Times",size=6.9/.pt,fontface='bold',check_overlap = TRUE)+

        guides(fill= guide_colorbar(byrow = TRUE,ticks = TRUE, even.steps = FALSE,frame.linewidth = 0.55, frame.colour = "black", ticks.colour = "black",
        ticks.linewidth = 0.3,nrow=1,title.hjust=-0.85,title.position="top",title=expression(WEI~trend~(10^-2~degree~C~year^-1)),label.position = "bottom",title.theme=element_text(size=6.5,colour="black",angle=0,family="Times"),label=T,label.vjust = -1,label.theme=element_text(size=6.5,colour="black",angle=0,family="Times")))+  
        facet_wrap( ~Type, ncol=2)+coord_equal() + 
        theme_opts
print(h1, vp=viewport(layout.pos.row=35:100, layout.pos.col=51:100))
#################------------------------------------------------------
inset_V_p <-   ggplot(data=df_V_p,aes(x=x,y=y,fill=z)) + theme_bw()+
        geom_tile(data=subset(df_V_p,z<0.05),aes(x=x,y=y),fill="gray50",show.legend=F)+
     #   geom_density_2d(data=subset(df_L_p,z<0.05),bins=20)+
       geom_path(data=world, aes(long, lat, group=group, fill=NULL), linetype="solid", color="black",size = 0.2) +
        geom_path(data=box, aes(long, lat, group=group, fill=NULL), linetype="solid", color="black",size = 0.2) +
    #    scale_fill_distiller(breaks=seq(-1.5,1.5,0.5),palette="RdBu", direction=-1)+
        scale_y_continuous(expand = c(0,0),limits=c(-6336039,8625155))+
        scale_x_continuous(expand = c(0,0))+
        geom_text(aes(x=-13000000,y=900000),label="21%",family="Times",size=6.9/.pt,fontface='plain',check_overlap = TRUE)+

        xlab(NULL)+ylab(NULL)+
        guides(fill= guide_colorbar(byrow = TRUE,ticks = TRUE, even.steps = FALSE,frame.linewidth = 0.55, frame.colour = "black", ticks.colour = "black",
        ticks.linewidth = 0.3,nrow=1,title.position="top",title=expression(WEF~trend~(days~year^-1)),label.position = "bottom",title.theme=element_text(size=5,colour="black",angle=0,family="Times"),label=T,label.vjust = -1,label.theme=element_text(size=5,colour="black",angle=0,family="Times")))+  
        facet_wrap( ~Type, ncol=2)+coord_equal() + 
        theme_opts+theme( plot.background =element_rect(fill='transparent'), panel.border = element_rect(fill='transparent'))



sbvp1 <- viewport(width=0.25,height=0.47,x=0.82,y=0.22)
print(inset_V_p,vp=sbvp1,more=TRUE)
#####
dev.off()

































