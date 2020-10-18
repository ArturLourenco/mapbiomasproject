#####################################
#### 1 Limpar Console e Memória #####
#####################################

gc(TRUE) #garbage colector da RAM
rm(list = ls()) #limpar memoria das variáveis globais 
dev.off() # limpar os plots
cat("\014") #limpar console

################################################################################
##### 2 Carregar bibliotecas, arquivos externos e definir pasta de trabalho ####
################################################################################

list.of.packages <- c("colorRamps","ggplot2","zoo","RColorBrewer", "ggrepel", "sf", "rgeos",
                      "rworldmap", "rworldxtra","scales","openair","raster","rgdal","rasterVis",
                      "ggspatial", "cowplot", "googleway","rayshader") # lista de pacotes utilizados

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])] # checar se há algum pacote novo

if(length(new.packages)) install.packages(new.packages) # instala os pacotes novos

lapply(list.of.packages, require, character.only = TRUE) # carrega os pacotes necessários

# setwd("G:\\My Drive\\Pesquisa\\Coordenador de Projetos Pesquisa 2018")
# source("G:\\My Drive\\Pesquisa\\Coordenador de Projetos Pesquisa 2018\\windplot.R")
# source("G:/My Drive/Pesquisa/Coordenador de Projetos Pesquisa/Pré-tratamento dos dados/calendarplot.R")

setwd("G:\\My Drive\\Pesquisa\\Coordenador de Projetos Pesquisa 2018\\CAATINGA") #define a pasta de trabalho

####################################
#### 3 Pré tratamento dos dados ####
####################################

raster::raster("G:\\My Drive\\São Francisco\\Serra_Teixeira_12m.tif") -> localtif

# bacia_jatoba<- readOGR("G:\\My Drive\\Pesquisa\\Projeto Ibiapina\\Geo Data\\Bacia_Jatoba.shp") # carrega o shape da bacia

limitecampus<- readOGR("G:\\My Drive\\Ensino\\Disciplinas\\Zoneamento Ambiental\\Trabalhos 2017.2\\Limite_Campus_IFPB_PI_Line.shp") # carrega o shape da bacia

# cropjatoba <- crop(localtif, extent(bacia_jatoba)) #cortar a extensão do raster pela bacia
# 
# maskjatoba <- mask(cropjatoba, bacia_jatoba) # fazer a máscara pelo vetor da bacia

# elmat = matrix(raster::extract(maskjatoba,raster::extent(maskjatoba),buffer=1000),
               # nrow=ncol(maskjatoba),ncol=nrow(maskjatoba))

lim_raster <- raster(ncol=ncol(localtif), nrow=nrow(localtif))
lim_raster <- setExtent(lim_raster, extent(localtif))
lim_raster <- rasterize(limitecampus, lim_raster, 1, background=0)
lim_mat <- matrix(extract(lim_raster, extent(lim_raster)),
                   nrow=nrow(lim_raster),
                   ncol=ncol(lim_raster))
lim_mat <- t(lim_mat)

elmat = matrix(raster::extract(localtif,raster::extent(localtif),buffer=1000),
               nrow=ncol(localtif),ncol=nrow(localtif))

elmat %>%
  sphere_shade(texture = "desert") %>%
  plot_map()

elmat %>%
  sphere_shade(sunangle = 45, texture = "desert") %>%
  plot_map()

model<- elmat %>%
  sphere_shade(texture = "desert") %>%
  # add_water(detect_water(elmat, zscale = 9), color="desert") %>%
  add_water(lim_mat) # for subway routes

model %>% plot_map()

raymat = ray_shade(elmat,lambert)

elmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat), color="desert") %>%
  add_shadow(raymat,0.7) %>%
  plot_map()

ambmat = ambient_shade(elmat)

elmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat), color="desert") %>%
  add_shadow(raymat,0.7) %>%
  # add_shadow(ambmat,0.7) %>%
  plot_map()

elmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat), color="desert") %>%
  add_shadow(ray_shade(elmat,zscale=3,maxsearch = 300),0.9) %>%
  add_shadow(ambmat,0.7) %>%
  plot_3d(elmat,zscale=10,fov=0,theta=135,zoom=0.75,phi=45, windowsize = c(1000,800))

elmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat), color="desert") %>%
  add_shadow(raymat,0.5) %>%
  add_shadow(ambmat,0.5) %>%
  plot_3d(elmat,zscale=10,fov=30,theta=-225,phi=25,windowsize=c(1000,800),zoom=0.3)
render_depth(focus=0.6,focallength = 200)

mat_ray <- ray_shade(elmat, zscale=3,maxsearch = 300)
mat_amb <- ambient_shade(elmat)

elmat %>%
  sphere_shade(texture = "imhof1") %>%
  add_water(detect_water(elmat, min_area = 150, zscale = 25), color = "desert") %>%
  add_shadow(mat_ray, 0.7) %>%
  add_shadow(mat_amb, 0.7) %>%
  #plot_map()
  plot_3d(elmat,zscale=3,fov=0,theta=135,zoom=0.75,phi=45, windowsize = c(800,600),
          water=TRUE, waterdepth=160, wateralpha=0.5, watercolor="lightblue",
          waterlinecolor = "white", waterlinealpha = 0.3)

elmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat), color="desert") %>%
  add_shadow(ray_shade(elmat,zscale=3,maxsearch = 300),0.5) %>%
  add_shadow(ambmat,0.8) %>%
  plot_3d(elmat,zscale=15,fov=0,theta=135,zoom=0.75,phi=45, windowsize = c(1000,800))

save_3dprint("teste.stl",maxwidth = 4, "mm")
