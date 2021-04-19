#### 1 Limpar Console e Memória #####


gc(TRUE) #garbage colector da RAM
rm(list = ls()) #limpar memoria das variáveis globais
dev.off() # limpar os plots
cat("\014") #limpar console


##### 2 Carregar bibliotecas, arquivos externos e definir pasta de trabalho ####


list.of.packages <-
  c(
    "colorRamps",
    "ggplot2",
    "zoo",
    "RColorBrewer",
    "ggrepel",
    "sf",
    "rgeos",
    "ggforce",
    "rworldmap",
    "rworldxtra",
    "scales",
    "openair",
    "raster",
    "rgdal",
    "rasterVis",
    "ggspatial",
    "reshape2",
    "cowplot",
    "googleway",
    "networkD3",
    "tidyverse",
    "rayshader",
    "ggpmisc",
    "magrittr",
    "ggmap"
  ) # lista de pacotes utilizados

new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])] # checar se há algum pacote novo

if (length(new.packages))
  install.packages(new.packages) # instala os pacotes novos

lapply(list.of.packages, require, character.only = TRUE) # carrega os pacotes necessários

# setwd("G:\\My Drive\\Pesquisa\\Coordenador de Projetos Pesquisa 2018")
# source("G:\\My Drive\\Pesquisa\\Coordenador de Projetos Pesquisa 2018\\windplot.R")
# source("G:/My Drive/Pesquisa/Coordenador de Projetos Pesquisa/Pré-tratamento dos dados/calendarplot.R")

setwd("G:\\My Drive\\Pesquisa\\Coordenador de Projetos Pesquisa 2018\\Dados Caatinga") #define a pasta de trabalho

# load(
#   "G:/My Drive/Pesquisa/Coordenador de Projetos Pesquisa 2018/Dados Caatinga/dataanalisys.RData"
# )

prepGradient <- function(x, y, spacing = max(y) / 100) {
  stopifnot(length(x) == length(y))
  df <- data.frame(x = x, y = y)
  newDf = data.frame(x = NULL, y = NULL, z = NULL)
  for (r in 1:nrow(df)) {
    n <- floor(df[r, "y"] / spacing)
    for (s in c(1:n)) {
      tmp <- data.frame(x = df[r, "x"],
                        y = spacing,
                        z = s * spacing)
      newDf <- rbind(newDf, tmp)
    }
    tmp <- data.frame(x = df[r, "x"],
                      y = df[r, "y"] %% spacing,
                      z = df[r, "y"])
    newDf <- rbind(newDf, tmp)
  }
  return(newDf)
}


#### 3 Pré tratamento dos dados ####

# Analisar e eficiência do reservatório

princesapop <-
  read.table(
    "popprincesa.csv",
    header = T,
    sep = ",",
    quote = "",
    encoding = "UTF-8",
    col.names = c("data", "pop")
  )

classcodes <- read_excel('MAPBIOMAS_Col5_Legenda_Cores.xlsx',n_max = 30)[,-c(6:9)]

names(classcodes)<- c('type','classe','class','id','color_hex')

classcodes$color_hex<- paste('#',classcodes$color_hex,sep = '')

legend_colors <- setNames(classcodes$color_hex, classcodes$id)

legend_labels <- setNames(classcodes$classe, classcodes$id)

legend_classe <- setNames(classcodes$color_hex, classcodes$classe)

# colnames(classcodes) <- c("uso_do_solo", "land_use", "id")

# bacia_jatoba2<- read_sf("G:\\My Drive\\Pesquisa\\Projeto Ibiapina\\Geo Data\\Bacia_Jatoba.shp") # carrega o shape da bacia

bacia_jatoba <-
  readOGR("G:\\My Drive\\Pesquisa\\Projeto Ibiapina\\Geo Data\\Bacia_Jatoba_WGS84.shp") # carrega o shape da bacia

dem_jatoba_II_area<-raster('G:/My Drive/Pesquisa/Coordenador de Projetos Pesquisa 2018/JATOBA/DemjatobaWgs84.tif')

dem_jatoba_II<- dem_jatoba_II_area %>% 
  crop(., bacia_jatoba) %>% 
  mask(.,bacia_jatoba)

raster_jatoba_II_list<- paste('G:\\My Drive\\MAPBIOMAS-EXPORT\\mapbiomas-brazil-collection-50-',seq(1985,2019,1),'.tif',sep = '')

raster_jatoba_II <- stack(raster_jatoba_II_list)

for (i in 1:dim(raster_jatoba_II)[3]) {
  
  raster_jatoba_II[[i]]<- reclassify(raster_jatoba_II[[i]], cbind(0, NA))

}

raster_years <- 1985:2019 # vetor com a sequência dos anos

raster_trans_jatoba_II <-
  raster("G:\\My Drive\\MAPBIOMAS-EXPORT\\mapbiomas-brazil-collection-50-1985_2019.tif") # raster com todas as bandas

raster_trans_jatoba_II<- reclassify(raster_trans_jatoba_II, cbind(0, NA))

raster_trans_jatoba_II@data@names <- raster_years

rasternbands<- dim(raster_jatoba_II)[3]

#### 4 Análise dos Dados ####


classareaszones <-
  list(NULL) # cria uma lista aonde será colocado as tabelas de todos os anos

for (i in 1:rasternbands) {
  classareas <-
    area(raster_jatoba_II[[i]]) # cria um raster e calcula área para cada pixel (diferente por está em angular, WGS84)
  classareaszones[[i]] <-
    as.data.frame(zonal(classareas, raster_jatoba_II[[i]], 'sum')) # soma todos os pixels pelas classes/zonas
  
}


classareastrans <-
  area(raster_trans_jatoba_II) # cria um raster e calcula área para cada pixel (diferente por está em angular, WGS84)
classareaszonestrans <-
  as.data.frame(zonal(classareastrans, raster_trans_jatoba_II, 'sum')) # soma todos os pixels pelas classes/zonas

# dividir as classes
classareaszonestrans <-
  cbind(classareaszonestrans,
        source = ((classareaszonestrans[, 1] %/% 1e2) %% 1e2),
        target = ((classareaszonestrans[, 1] %/% 1e0) %% 1e2))

#merge para o source

zonetransmerged <-
  merge(classareaszonestrans,
        classcodes,
        by.x = "source",
        by.y = "id")

# merge para o target
zonetransmerged <-
  merge(zonetransmerged, classcodes, by.x = "target", by.y = "id")

# cria string para legenda

zonetransmerged <-
  cbind(zonetransmerged,
        trans = paste(zonetransmerged[, 'classe.x'], " -> ", zonetransmerged[, 'classe.y']))

zonetransmerged <- cbind(
  zonetransmerged,
  mudanca = ifelse(
    zonetransmerged[, 'source'] == zonetransmerged[, 'target'],
    "Não Houve Mudança",
    "Houve Mudança"
  ),
  cormudanca = ifelse(zonetransmerged[, 'source'] == zonetransmerged[, 'target'], "#247503", "#8C8A04")
)

# colnames(zonetransmerged[, c(1, 2)]) <- c("source", "target")

# zonetransmerged[unique(zonetransmerged$source),] temp

# mescla todas as tabela em uma só

classareasfull <-
  Reduce(function(dtf1, dtf2)
    merge(dtf1, dtf2, by = "zone", all = TRUE),
    classareaszones) 

# classareaszones %>% reduce(full_join, by = "zone") # mescla todas as tabelas em uma só

row.names(classareasfull) <-
  classareasfull[, 'zone'] # coloca o nome das linhas utilizando os valores da coluna 1

classareasfull[is.na(classareasfull)] <- 0

classareasfull <-
  classareasfull[2:(rasternbands + 1)] # retira a coluna 1 (zonas)

colnames(classareasfull) <-
  as.character(raster_years) # coloca os números dos anos nas colunas


# Terrain Analysis  -------------------------------------------------------

dem_jatoba_II_slope_aspect<- dem_jatoba_II %>% 
  terrain(.,opt = c('slope','aspect'))

hillshade_jatoba_II<- hillShade(dem_jatoba_II_slope_aspect$slope,dem_jatoba_II_slope_aspect$aspect)

hillshade_jatoba_II_spatial<- hillshade_jatoba_II %>% 
  as(.,'SpatialPixelsDataFrame') %>% 
  as.data.frame(.,xy = TRUE)

dem_jatoba_II_spatial<- dem_jatoba_II %>% 
  as(.,'SpatialPixelsDataFrame') %>% 
  as.data.frame(.,xy = TRUE)

ggplot() +
  geom_raster(data = hillshade_jatoba_II_spatial, aes(x = x, y = y, fill = layer),show.legend = FALSE) +
  scale_fill_distiller(palette = "Greys") +
  new_scale_fill() +
  geom_raster(data = dem_jatoba_II_spatial, aes(x = x, y = y, fill = DemjatobaWgs84),alpha = .5) +
  scale_fill_gradientn('name',colors = grDevices::terrain.colors(10)) +
  theme_bw()

coords<- c(left = dem_jatoba_II@extent@xmin, 
           bottom = dem_jatoba_II@extent@ymin, 
           right = dem_jatoba_II@extent@xmax,
           top = dem_jatoba_II@extent@ymax)

# map <- ggmap(get_map(location = coords,zoom = 13, source ="google",maptype = "satellite"))

map <- ggmap(get_googlemap(center = c(-37.960817,-7.718334),zoom = 12, source ="google",maptype = "satellite"))

dem_jatoba_II_spatial$region <- ifelse(dem_jatoba_II_spatial$x > -37.9 & dem_jatoba_II_spatial$y > -7.70, 1, 2)

map + geom_raster(data = dem_jatoba_II_spatial, aes(x = x, y = y, fill = DemjatobaWgs84),alpha = .5) +
  scale_fill_gradientn('name',colors = grDevices::terrain.colors(10)) +
  scale_x_continuous(limits = coords[c('left','right')], expand = c(0, 0)) +
  scale_y_continuous(limits = coords[c('bottom','top')], expand = c(0, 0)) +
  coord_cartesian() +
  facet_zoom(xy = c(-37.8, -37.875) ,ylim = c(-7.700,-7.675)) +
  theme_bw()


# zoom em mapa
ggplot() +
  ggmap(get_googlemap(center = c(-37.960817,-7.718334),zoom = 12, source ="google",maptype = "satellite")) +
  geom_raster(data = dem_jatoba_II_spatial, aes(x = x, y = y, fill = DemjatobaWgs84),alpha = .5) +
  scale_fill_gradientn('name',colors = grDevices::terrain.colors(10)) +
  # scale_x_continuous(limits = coords[c('left','right')], expand = c(0, 0)) +
  # scale_y_continuous(limits = coords[c('bottom','top')], expand = c(0, 0)) +
  coord_cartesian() +
  facet_zoom(xy = c(-37.8, -37.875) ,ylim = c(-7.700,-7.675)) +
  theme_bw()

# Criar data frames spaciais  ---------------------------------------------

# cobertura

df_jatoba_II_spatial <-
  as(raster_jatoba_II[['mapbiomas.brazil.collection.50.1985']], "SpatialPixelsDataFrame") # transformar para tabela de dados espaciais

df_jatoba_II_spatial <-
  as.data.frame(df_jatoba_II_spatial, xy = TRUE) # transformar para data frame

colnames(df_jatoba_II_spatial) <-
  c("classe", "x", "y") # dar nomes as colunas

df_jatoba_II_spatial<- merge(df_jatoba_II_spatial, classcodes, 
                             by.x = "classe", by.y = "id")

df_jatoba_II_spatial_table<- df_jatoba_II_spatial %>% 
  group_by(classe) %>% 
  summarise(count = n()) %>% 
  mutate(per = (count*100)/sum(count))

# transição

df_trans_jatoba_II_spatial <-
  as(raster_trans_jatoba_II, "SpatialPixelsDataFrame") # transformar para tabela de dados espaciais

df_trans_jatoba_II_spatial <-
  as.data.frame(df_trans_jatoba_II_spatial, xy = TRUE) # transformar para data frame

colnames(df_trans_jatoba_II_spatial) <-
  c("classe", "x", "y") # dar nomes as colunas

df_trans_jatoba_II_spatial <-
  cbind(df_trans_jatoba_II_spatial,
        source = ((df_trans_jatoba_II_spatial[, 1] %/% 1e2) %% 1e2),
        target = ((df_trans_jatoba_II_spatial[, 1] %/% 1e0) %% 1e2))

df_trans_jatoba_II_spatial <- cbind(
  df_trans_jatoba_II_spatial,
  mudanca = ifelse(
    df_trans_jatoba_II_spatial[, 'source'] == df_trans_jatoba_II_spatial[, 'target'],
    "Não Houve Mudança",
    "Houve Mudança"
  ),
  cormudanca = ifelse(
    df_trans_jatoba_II_spatial[, 'source'] == df_trans_jatoba_II_spatial[, 'target'],
    "#34c410",
    "#ff0000"
  )
)


# criar data set para plotar ----------------------------------------------


names(classareaszones)<- raster_years

zonemerged <-
  cbind(zones = rownames(classareasfull), classareasfull)

zonemerged <-
  merge(zonemerged,
        classcodes,
        by.x = "zones",
        by.y = "id")

classareasfullmelt <-
  cbind(zones = rownames(classareasfull), classareasfull)

classareasfullmelt$zones <- factor(classareasfullmelt$zones, levels = unique(classareasfullmelt$zones))


classareasfullmelt <- melt(classareasfullmelt, id = "zones")

classareasfullmeltmerge <-merge(classareasfullmelt,
                                classcodes,
                                by.x = "zones",
                                by.y = "id")

# classareasfullmeltmerge$zones <-
#   as.numeric(classareasfullmeltmerge$zones)

# classareasfullmeltmerge <-
#   classareasfullmeltmerge[order(as.numeric(classareasfullmeltmerge$zones)), ]
# colnames(classareasfullmeltmerge[1,])<- "classes"
classareasfullmeltmerge[is.na(classareasfullmeltmerge)] <- 0

# 
# nodes <- zonemerged
# nodes <- nodes[, c(-2, -4, -5)]
# colnames(nodes) <- c("zone", "name")
# nodes$zone <- nodes$zone - 2
# ids <- data.frame(zone = seq(max(nodes$zone)))
# nodes <- merge(ids, nodes, all.x = T)
# nodes <-
#   data.frame(name = as.character(nodes$name), stringsAsFactors = F)
# 
# links <- data.frame(zonetransmerged[, c(5, 8, 4)])
# 
# links$sum <- as.double(links$sum)
# colnames(links) <- c("source", "target", "value")
# 
# 
# landuse <- list(nodes = nodes, links = links)
# 
# tidy_landuse <- landuse$links %>%
#   gather_set_data(1:3)

#### Hydro data ####

monthnames <-
  c('Jan',
    'Fev',
    'Mar',
    'Abr',
    'Mai',
    'Jun',
    'Jul',
    'Ago',
    'Set',
    'Out',
    'Nov',
    'Dez')

Evpt_pianco_basin<- data.frame("month"=1:12,"val"=c(207,183,191,169,152,132,143,167,185,208,210,215),stringsAsFactors = F)
Evpt_sousa<- data.frame("month"=1:12,"val"=c(173.6,134.4,117.8,114,108.5,108,120.9,145.7,159,176.7,174,182.9),stringsAsFactors = F)

vol_jatoba_II<- read_csv("G:/My Drive/Pesquisa/Coordenador de Projetos Pesquisa 2018/Dados Caatinga/volumejatoba2021.csv", locale = locale("br",decimal_mark = ',',date_format = "%d/%m/%Y"))

vol_jatoba_II_annual<- vol_jatoba_II %>% 
  group_by(ano=format(`Data do registro`, "%Y")) %>%
  top_n(1, `Data do registro`)

landuse <- classareasfull %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column(.,var = 'ano')

p_annual_df<- p_annual %>%
  fortify() %>% 
  set_colnames(c("ano","valor"))

datasetplot <- princesapop %>%
  mutate(ano = as.character(data)) %>% 
  inner_join(.,landuse) %>% 
  inner_join(.,select(vol_jatoba_II_annual,ano,`Volume (m³)`)) %>% 
  inner_join(.,p_annual_df) %>%
  set_rownames(as.character(.$ano)) %>% 
  select(-data,-ano) %>% 
  rename(v = `Volume (m³)`,p = valor)


datasetplot <- datasetplot %>% 
  filter( row.names(.) > "1996")

datasetplotscaled <-
  data.frame("data" = datasetplot[, 1], scale(datasetplot[, -1]))

datasetplotmelted <- melt(datasetplot, id.vars = "data")

datasetplotscaledmelted <- melt(datasetplotscaled, id.vars = "data")

# tratmento precipitação --------------------------------------------------


daily_mean_pi <-
  aggregate(pi_plu_ser_1911_2019_full,
            format(time(pi_plu_ser_1911_2019_full), "%d/%m"),
            mean,
            na.rm = TRUE) # Calcular a médias dos dias por mês
a <- as.data.frame(daily_mean_pi)
a$names <- rownames(a)
a <- cbind(a, substr(a[, 2], 4, 5), substr(a[, 2], 1, 2),as.Date.character(paste("2020",substr(a[, 2], 4, 5), substr(a[, 2], 1, 2),sep = "-") ))
colnames(a) <- c("Valor", "Dia_Mes", "Mes", "Dia","Data")
a<- a[order(a$Data),]

monthly_mean_pi <-
  aggregate(pi_plu_ser_1911_2019_full, format(time(pi_plu_ser_1911_2019_full), "%m/%Y"), sum, na.rm = TRUE) # Calcular a chuva mensal
a <- as.data.frame(monthly_mean_pi)
a$names <- rownames(a)
a <- cbind(a, substr(a[, 2], 4, 7), substr(a[, 2], 1, 2))
colnames(a) <- c("Valor", "Dia_Ano", "Ano", "Mes")
b <- dcast(a, Ano ~ Mes, value.var = "Valor")
c <- b[, 2:ncol(b)]
colnames(c) <- monthnames
rownames(c) <- as.vector(unique(a$Ano))
d <- colMeans(c)
d <- data.frame(d, "m" = names(d))


#### Mapas e Gráficos ####

load("G:/My Drive/Pesquisa/Coordenador de Projetos Pesquisa 2018/Dados Caatinga/enviroment01.RData")

col <-
  colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

M <- cor(datasetplotscaled[, -1])
colnames(M) <- c("V", "P", "Pop", rownames(classareasfull))
rownames(M) <- c("V", "P", "Pop", rownames(classareasfull))

res1 <- cor.mtest(datasetplot, conf.level = .95)

M <- cor(datasetplot)


#### Correlation Plot ####

corrplot(
  M,
  p.mat = res1$p,
  method = "color",
  col = col(200),
  type = "upper",
  order = "hclust",
  # number.cex = 0.4,
  addCoef.col = "black",
  # Add coefficient of correlation
  tl.col = "black",
  tl.srt = 45,
  sig.level = 0.05,
  insig = "pch",
  addrect = 3,
  # hide correlation coefficient on the principal diagonal
  diag = FALSE
)

#### Alll Variables line plot ####

ggplot(datasetplotscaledmelted, aes(x = data, y = value)) +
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(
    "Legenda",
    labels = c(
      "Volume",
      "Precipitação","População",
      as.character(zonemerged$classe)
    ) ,
    values = c("blue", "black","red", zonemerged$color_hex)
  ) +
  theme_minimal()


#### Selected Variables line plot ####

ggplot(
  datasetplotscaledmelted %>% filter(variable == "X33" |
                                       variable == "X15"),
  aes(x = data, y = value)
) +
  geom_line(aes(color = variable), size = 1) +
  scale_fill_manual(
    "Legenda",
    labels = c("População", "Volume", zonemerged$classe) ,
    values = c("blue", "black", zonemerged$color_hex)
  ) +
  theme_minimal()

#### Volume area plot ####

ggplot(data = v_annual, aes(x = data, y = v)) +
  geom_area(
    color = "darkblue",
    fill = "lightblue", size = 1,
    # alpha = 0.5,
    position = position_dodge(0.8)
  ) + 
  geom_hline(aes(yintercept=7229180),
             color="red", linetype="dashed", size=1) +
  geom_hline(aes(yintercept=6487200),
             color="blue", linetype="dashed", size=1) +
  labs(
    title = "Volume do Reservatório Jatobá II entre 1995-2019",
    subtitle = "Valor referente a última leitura da cota do açude",
    y = "Volume (m³)",
    x = "Tempo (anos)",
    caption = "Fonte: ANA/AESA, 2020"
  ) +  
  theme_minimal()

#### Volume area plot 2 ####

ggplot(data = v_serie_1995_2020, aes(x = data, y = valor)) +
  geom_area(
    color = "darkblue",
    fill = "lightblue", size = 1,
    # alpha = 0.5,
    position = position_dodge(0.8)
  ) + 
  geom_hline(aes(yintercept=7229180),
             color="red", linetype="dashed", size=1) +
  geom_hline(aes(yintercept=6487200),
             color="blue", linetype="dashed", size=1) +
  labs(
    title = "Volume do Reservatório Jatobá II entre 1995-2019",
    subtitle = "Valor referente a última leitura da cota do açude",
    y = "Volume (m³)",
    x = "Tempo (anos)",
    caption = "Fonte: ANA/AESA, 2020"
  ) +  
  theme_minimal()

#### Annual precipitation plot ####

ggplot(data = p_annual3, aes(
  x = data,
  y = p,
  color = p,
  label = p
)) +
  geom_point(aes(shape = tipo), size = 3) +
  labs(
    title = "Precipitação Acumulada Anual na Estação Pluviométrica Princesa Isabel 1911 - 2019",
    subtitle = "Divisão entre anos secos, normais e úmidos",
    y = "Precipitação (mm)",
    x = "Tempo (anos)",
    caption = "Fonte dos Dados: ANA/AESA, 2020", shape = "Tipo"
  ) +  # title and caption
  scale_color_gradientn(colours = rev(matlab.like2(5)), name = 'P(mm)') +
  geom_hline(aes(yintercept=mean(groups$`1`),linetype="Seco"),
             color="red", size=1) +
  geom_hline(aes(yintercept=mean(groups$`2`),linetype="Normal"),
             color="green", size=1) +
  geom_hline(aes(yintercept=mean(groups$`3`),linetype="Úmido"),
             color="blue", size=1) +  
  # geom_text(aes(label=ifelse(temp<=coredata(day$temp[which.min(day$temp)]),as.character(temp),'')),hjust=0,vjust=1) +
  scale_linetype_manual(name = "Médias", values = c(3,3,3), 
                        guide = guide_legend(override.aes = list(color = c("red","green","blue")))) +
  theme_bw()

# Matrix plot ---------------------------------

mtplot<- ggplot(data = a, aes(x = Mes, y = Dia, fill = Valor)) +
  geom_tile(color = "Black", size = .7) +
  scale_y_discrete(name = "Dias", breaks = unique(a$Dia)[seq(1, 31, 2)]) +
  scale_x_discrete(name = "Meses", labels = monthnames) +
  # geom_text(aes(Mes, Dia, label = Valor), color = "black", size = 4) +
  scale_fill_gradientn(colours = rev(matlab.like2(5)), name = 'P(mm)') +
  theme_minimal() +
  theme(
    # text=element_text(size=16, family="A"),
    text=element_text(size=12),
    # axis.title.x = element_blank(),
    # axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank()
  ) +
  coord_equal(ratio = 0.2) +
  labs(title = "Matriz de Precipitação na Estação Pluviométrica Princesa Isabel",
       subtitle = "Valor médio diário da série histórica 1911-2019",
       caption = "Fonte dos dados: AESA, 2019.")

par(mfrow = c(1, 2))
plot_gg(mtplot, width = 6, raytrace = FALSE, preview = TRUE)

plot_gg(mtplot, height = 3, width = 3, units = "cm", raytrace = T, multicore = T,  
        zoom = 0.6, phi = 25, theta = -10, sunangle = 225)
Sys.sleep(0.2)
render_snapshot(clear = TRUE)

save_3dprint("c://temp//plotmatrix.stl",maxwidth = 100,unit = "mm")

# Barplot ---------------------------------

df2<- d

df3<- d

df3$m<-1:12

df2$m<-1:12

df2 <- prepGradient(df2$m, df2$d)

ggplot(df2, aes(x = as.factor(x), y = y, fill = z)) +
  geom_bar(stat = "identity") +
  geom_bar(df3,mapping = aes(x = as.factor(m), y = d, fill = d), alpha = 0.1, stat = "identity",color = "black") +
  scale_fill_gradientn(colours = rev(matlab.like2(5)), name = 'P(mm)') +
  scale_y_continuous(name = "Precipitação (mm)") +
  scale_x_discrete(name = "Meses",
                   breaks = 1:12,
                   labels = monthnames) +
  theme_minimal() +
  labs(title = "Precipitação na Estação Pluviométrica Princesa Isabel",
       subtitle = "Valor médio mensal da série histórica 1911-2019",
       caption = "Fonte dos dados: AESA/ANA, 2019.")

ggplot(Evpt_pianco_basin, aes(x = as.factor(month), y = val, fill = val)) +
  geom_bar(stat = "identity") +
  scale_fill_gradientn(colours = matlab.like2(5), name = 'Evpt(mm)') +
  scale_y_continuous(name = "Evapotranspiração (mm)") +
  scale_x_discrete(name = "Meses",labels = monthnames) +
  theme_minimal() +
  labs(title = "Precipitação na Estação Pluviométrica Princesa Isabel",
       subtitle = "Valor médio mensal da série histórica 1911-2019",
       caption = "Fonte dos dados: AESA, 2019.")

### Population plot Princesa ####

ggplot(princesapop, aes(x = data, y = pop)) +
  geom_point(size = 1) +
  geom_line() +
  annotate("curve", x = 2001, xend = 1996, y = 25000,curvature = .2, 
           yend = 24500, colour = "black", size=1, alpha=0.8, arrow=arrow(length = unit(6, "mm"))) +
  annotate("text", x = 2007, y = 25000,
           label = "Início do desmenbramento territorial" , color="black",
           size=4 , angle=0, fontface="bold") +
  geom_vline(xintercept = 1996,
             color = "red",
             size = 1.5) +
  # geom_smooth()+
  scale_x_continuous(breaks = seq(1980, 2019, 3)) +
  scale_color_manual("Legenda",
                     labels = c("População") ,
                     values = c("blue")) +
  labs(
    title = "Série Histórica População Princesa Isabel",
    subtitle = "Dinâmica populacional via censo, contagem e estimativa",
    caption = "Fonte dos Dados: RIPSA, MS, IBGE e TCU.",
    x = "Tempo(anos)",
    y = "População (N.º habitantes)"
  ) +
  theme_minimal()

#### Mapas ####


# Gráfico alluvial --------------------------------------------------------

df_plot_al<- zonetransmerged[,c('source','target','color_hex.y','sum')]

names(df_plot_al)<- c('a1985','a2019','cor','area')

df_plot_al$a1985<- as.factor(df_plot_al$a1985)

df_plot_al$a2019<- as.factor(df_plot_al$a2019)

df_plot_al$area<- as.numeric(df_plot_al$area)

df_alluvial <- reshape2::melt(df_plot_al)
df_alluvial <- gather_set_data(df_alluvial, 1:2)

tbl_perc<- classareasfull %>%
  mutate(Classe = rownames(.)) %>% 
  select(Classe,`1985`,`2019`) %>%
  mutate('1985 (%)' = round((`1985`/sum(`1985`))*100,2)) %>% 
  mutate('2019 (%)' = round((`2019`/sum(`2019`))*100,2)) %>% 
  select(Classe,`1985 (%)`,`2019 (%)`) 

# ggplot(df_alluvial,
#        aes(y = value, axis1 = a1985, axis2 = a2017)) +
#   geom_alluvium(aes(fill = a2017), width = 1/12) +
#   geom_stratum(width = 1/12, fill = "black", color = "grey") +
#   # geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
#   scale_x_discrete(limits = c("a1987", "a2017"), expand = c(.05, .05)) +
#   scale_fill_brewer(type = "qual", palette = "Set1") +
#   ggtitle("UC Berkeley admissions and rejections, by sex and department")

ggplot(df_alluvial, aes(x, id = id, split = y, value = value)) +
  geom_parallel_sets(aes(fill = a2019), alpha = 0.8, axis.width = 0.1) + 
  geom_parallel_sets_axes(fill = 'grey', colour = 'black',axis.width = 0.1) +
  geom_parallel_sets_labels(colour = 'black',angle = 0, size = 5) +
  # annotate(geom = 'text', x=c('a1985','a1985'), y=c(96,50), label=c("teste 96",'teste'), hjust = 1) + 
  scale_x_discrete(labels = c("1985", "2019"),limits = c("a1985", "a2019"),
                   expand = c(0.05, 0.15)
  ) +
  scale_fill_manual("Classes",labels = setNames(paste(names(legend_labels),':',sep = '',gsub("[0-9,.]", "",names(legend_classe))),names(legend_labels))
                    ,values = legend_colors) +
  annotate(geom = "table", x = 'a2019', y = 20, label = list(tbl_perc), 
           vjust = 1, hjust = -1) +
  theme_minimal() +
  theme(axis.ticks = element_blank(),axis.text.y = element_blank()) 


# Slope plot --------------------------------------------------------------


ggplot(
  data = classareasfullmeltmerge %>% filter(variable == "1985" |
                                              variable == "2012" |
                                              variable == "2019"),
  aes(x = variable, y = value, group = classe)
) +
  geom_line(aes(color = classe, alpha = 1), size = 2) +
  geom_point(aes(color = classe, alpha = 1),  size = 4) +
  geom_text_repel(
    data = classareasfullmeltmerge %>% filter(variable == "1985"),
    aes(label = paste0(signif(value, 2), " km²")) ,
    hjust = "right",
    fontface = "bold",
    size = 4,
    nudge_x = -.5,
    direction = "y"
  ) +
  geom_text_repel(
    data = classareasfullmeltmerge %>% filter(variable == "2019"),
    aes(label = paste0(signif(value, 2), " km²")) ,
    hjust = "left",
    fontface = "bold",
    size = 4,
    nudge_x = .5,
    direction = "y"
  ) +
  # move the x axis labels up top
  scale_color_manual("Legenda", values = legend_classe) +
  scale_x_discrete(position = "top") +
  theme_bw() +
  # Format tweaks
  # Remove the legend
  theme(legend.position = "bottom") +
  # Remove the panel border
  theme(panel.border     = element_blank()) +
  # Remove just about everything from the y axis
  theme(axis.title.y     = element_blank()) +
  theme(axis.text.y      = element_blank()) +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.minor.y = element_blank()) +
  # Remove a few things from the x axis and increase font size
  theme(axis.title.x     = element_blank()) +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.text.x.top      = element_text(size = 12)) +
  # Remove x & y tick marks
  theme(axis.ticks       = element_blank()) +
  # Format title & subtitle
  theme(plot.title       = element_text(size = 14, face = "bold", hjust = 0.5)) +
  theme(plot.subtitle    = element_text(hjust = 0.5)) +
  #  Labelling as desired
  labs(title = "Evolução do Uso e Ocupração do Solo na Bacia de Drenagem do Jatobá II",
       # subtitle = "Valor da Área em Km²",
       caption = "Fonte dos Dados: Autor/MapBiomas(2020).")


# Mapa de Uso de algum ano ------------------------------------------------

ggplot() +
  geom_raster(data = df_jatoba_II_spatial, aes(
    x = x,
    y = y,
    fill = as.factor(classe)
  )) +
  geom_sf(data = st_as_sf(bacia_jatoba),
          colour = "black",
          fill = NA) +
  labs(
    title = "Uso e Ocupação do Solo da Bacia de Drenagem do Açude Jatobá II",
    subtitle = "Ano: 1985",
    caption = "Fonte dos Dados: Autor/MAPBIOMAS (2020)",
    y = "Latitude",
    x = "Longitude"
  ) +
  scale_fill_manual("Classes",
                    labels = legend_labels,
                    values = legend_colors)  +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tl") +
  coord_sf() +
  facet_zoom(xy = c(250,366) ,ylim = c(0,20000),horizontal = FALSE) +
  theme(
    panel.grid.major = element_line(
      color = gray(.5),
      linetype = "dashed",
      size = 0.5
    ),
    #legend.position = c(.1, .5),#
    panel.background = element_rect(fill = "aliceblue"),
    text = element_text(size = 12),
    plot.title = element_text(
      hjust = 0,
      vjust = 0,
      face = "bold"
    )
  )


# validação areas ---------------------------------------------------------

t1<- t0[t0@data$mapbiomas.brazil.collection.50.1985 == 33,]


#para trabalhar como simple feature
# t2<- t0 %>%
#   disaggregate() %>%
#   st_as_sf() %>% 
#   mutate(area = st_area(.)) %>% 
#   group_by(mapbiomas.brazil.collection.50.1985) %>% 
#   filter(area == max(area))


# Mapa tipo de uso natural e antrópico ------------------------------------------------

ggplot() +
  geom_raster(data = df_jatoba_II_spatial, aes(
    x = x,
    y = y,
    fill = as.factor(type)
  )) +
  geom_sf(data = st_as_sf(bacia_jatoba),
          colour = "black",
          fill = NA) +
  labs(
    title = "Tipo de Uso do solo na Bacia de Drenagem do Açude Jatobá II",
    subtitle = "Ano: 2019",
    caption = "Fonte dos Dados: Autor/MAPBIOMAS (2020)",
    y = "Latitude",
    x = "Longitude"
  ) +
  scale_fill_manual("Tipo de Uso",
                    labels = c('Antrópico','Mosaico','Natural'),
                    values = c('Antrópico'='#2E4057','Mosaico'='#CAFFB9','Natural'='#C0D461'))  +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tl") +
  coord_sf() +
  theme(
    panel.grid.major = element_line(
      color = gray(.5),
      linetype = "dashed",
      size = 0.5
    ),
    #legend.position = c(.1, .5),#
    panel.background = element_rect(fill = "aliceblue"),
    text = element_text(size = 12),
    plot.title = element_text(
      hjust = 0,
      vjust = 0,
      face = "bold"
    )
  )

# Mapa de transição  ---------------------------------------------------

ggplot() +
  geom_raster(data = df_trans_jatoba_II_spatial, aes(
    x = x,
    y = y,
    fill = as.factor(mudanca)
  )) +
  geom_sf(data = st_as_sf(bacia_jatoba),
          colour = "black",
          fill = NA) +
  labs(
    title = "Transição de Uso do Solo da Bacia de Drenagem do Açude Jatobá",
    subtitle = "Ano: 1985/2019",
    caption = "Fonte dos Dados: Autor/MAPBIOMAS(2020)",
    y = "Latitude",
    x = "Longitude"
  ) +
  # scale_fill_discrete("Classes")+
  scale_fill_manual("Classes",
                    values = unique(df_trans_jatoba_II_spatial$cormudanca), labels = unique(df_trans_jatoba_II_spatial$mudanca)) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tl") +
  coord_sf() +
  theme(
    panel.grid.major = element_line(
      color = gray(.5),
      linetype = "dashed",
      size = 0.5
    ),
    legend.position = "bottom",
    panel.background = element_rect(fill = "aliceblue"),
    text = element_text(size = 12),
    plot.title = element_text(
      hjust = 0,
      vjust = 0,
      face = "bold"
    )
  )


# gráfico de evolução área ------------------------------------------------

# classareasfullmeltmerge$zones<- as.factor(classareasfullmeltmerge$zones)
# 
# myLevels <- levels(classareasfullmeltmerge$zones)
# 
# classareasfullmeltmerge$zones <- factor(classareasfullmeltmerge$zones , levels=sort(myLevels, decreasing = T))

ggplot(classareasfullmeltmerge, aes(x = variable,y = value, group = zones, fill = zones)) +
  geom_area(colour = "black", size = .3,show.legend = FALSE) +
  labs(
    title = "Evolução de Uso e Ocupração do Solo da Bacia de Drenagem do Açude Jatobá II",
    subtitle = "Ano: 1985-2019",
    caption = "Fonte dos Dados: MAPBIOMAS (2020)",
    y = "Área(km²)",
    x = "Tempo(anos)"
  ) +
  scale_x_discrete(breaks = seq(1985, 2019, 8)) +
  scale_fill_manual("Classes",
                    labels = legend_labels,
                    values = legend_colors) + 
  facet_wrap(
    ~ classe,scales = "free_y") +
  theme_bw()

# Gráfico de linha evolução dos usos --------------------------------------


ggplot(classareasfullmeltmerge, aes(x = variable,y = value, group = classe)) +
  geom_line(aes(color = zones)) + 
  labs(
    title = "Evolução de Uso do Solo da Bacia de Drenagem do Açude Jatobá",
    subtitle = "Ano: 1985/2019",
    caption = "Fonte dos Dados: Autor/MAPBIOMAS",
    y = "Área(km²)",
    x = "Tempo(anos)"
  ) +
  scale_x_discrete(breaks = seq(1985, 2019, 2)) +
  scale_color_manual("Classes",
                    labels = legend_labels,
                    values = legend_colors) + 
  facet_wrap(
    ~ classe,scales = "free") +
  labs(x = "", y = "") +
  theme_bw()

