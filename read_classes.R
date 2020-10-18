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
    "rayshader"
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

load(
  "G:/My Drive/Pesquisa/Coordenador de Projetos Pesquisa 2018/Dados Caatinga/dataanalisys.RData"
)

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


princesapop <-
  read.table(
    "popprincesa.csv",
    header = T,
    sep = ",",
    quote = "",
    encoding = "UTF-8",
    col.names = c("data", "pop")
  )

classcodes <-
  read.table(
    "classcodes4.csv",
    header = T,
    sep = ",",
    quote = "",
    encoding = "UTF-8"
  )

colnames(classcodes) <- c("uso_do_solo", "land_use", "id")
colfunc <- colorRampPalette(c("#174B02", "#44D808"))
floresta <- colfunc(10)
colfunc <- colorRampPalette(c("#787603", "#E0DD08"))
agropecuaria <- colfunc(6)
semvegetacao <- "#FA3613"
praia <- "#C70039"
infra <- "#747572"
rocha <- "#000000"
mineracao <- "#663302"
outravegetacao <- "#BEFA13"
corposdagua <- "#0F2CB0"
rioslagooceano <- "#16BFDE"
aquicultura <- "#6116DE"
outra <- "#6B3A31"
semobs <- "#8844DF"
classcodes$colors <-
  c(
    floresta,
    rocha,
    outravegetacao,
    agropecuaria,
    semvegetacao,
    praia,
    infra ,
    mineracao,
    outra,
    corposdagua,
    rioslagooceano,
    aquicultura,
    semobs
  )

# bacia_jatoba2<- read_sf("G:\\My Drive\\Pesquisa\\Projeto Ibiapina\\Geo Data\\Bacia_Jatoba.shp") # carrega o shape da bacia

bacia_jatoba <-
  readOGR("G:\\My Drive\\Pesquisa\\Projeto Ibiapina\\Geo Data\\Bacia_Jatoba_WGS84.shp") # carrega o shape da bacia

rastercaatinga <-
  raster("CAATINGA4.tif", band = 33) # carrega banda específica

rasternbands <-
  nbands(raster("CAATINGA4.tif")) # número de bandas do raster

years <- 1985:2018 # vetor com a sequência dos anos

classcaatinga <- stack("CAATINGA4.tif") # raster com todas as bandas

transcaatinga <-
  raster("G:\\My Drive\\MAPBIOMAS-EXPORT\\0000000000-0000000000 (2).tif") # raster com todas as bandas

cropjatoba <-
  crop(classcaatinga, bacia_jatoba) #cortar a extensão do raster pela bacia

cropjatobatrans <-
  crop(transcaatinga, bacia_jatoba) #cortar a extensão do raster pela bacia

maskjatoba <-
  mask(cropjatoba, bacia_jatoba) # fazer a máscara pelo vetor da bacia

maskjatobatrans <-
  mask(cropjatobatrans, bacia_jatoba) # fazer a máscara pelo vetor da bacia

maskjatoba@data@names <- years


#### 4 Análise dos Dados ####


classareaszones <-
  list(NULL) # cria uma lista aonde será colocado as tabelas de todos os anos

for (i in 1:rasternbands) {
  classareas <-
    area(maskjatoba[[i]]) # cria um raster e calcula área para cada pixel (diferente por está em angular, WGS84)
  classareaszones[[i]] <-
    as.data.frame(zonal(classareas, maskjatoba[[i]], 'sum')) # soma todos os pixels pelas classes/zonas
  
}


classareastrans <-
  area(maskjatobatrans) # cria um raster e calcula área para cada pixel (diferente por está em angular, WGS84)
classareaszonestrans <-
  as.data.frame(zonal(classareastrans, maskjatobatrans, 'sum')) # soma todos os pixels pelas classes/zonas
classareaszonestrans <-
  cbind(classareaszonestrans,
        source = ((classareaszonestrans[, 1] %/% 1e2) %% 1e2),
        target = ((classareaszonestrans[, 1] %/% 1e0) %% 1e2))
zonetransmerged <-
  merge(classareaszonestrans,
        classcodes,
        by.x = "source",
        by.y = "id")
zonetransmerged <-
  merge(zonetransmerged, classcodes, by.x = "target", by.y = "id")
zonetransmerged <-
  cbind(zonetransmerged,
        trans = paste(zonetransmerged[, 8], " -> ", zonetransmerged[, 5]))
zonetransmerged <- cbind(
  zonetransmerged,
  mudanca = ifelse(
    zonetransmerged[, 1] == zonetransmerged[, 2],
    "Não Houve Mudança",
    "Houve Mudança"
  ),
  cormudanca = ifelse(zonetransmerged[, 1] == zonetransmerged[, 2], "#247503", "#8C8A04")
)
colnames(zonetransmerged[, c(1, 2)]) <- c("source", "target")

# zonetransmerged[unique(zonetransmerged$source),] temp

classareasfull <-
  Reduce(function(dtf1, dtf2)
    merge(dtf1, dtf2, by = "zone", all = TRUE),
    classareaszones) # mescla todas as tabela em uma só

# classareaszones %>% reduce(full_join, by = "zone") # mescla todas as tabelas em uma só

row.names(classareasfull) <-
  classareasfull[, 1] # coloca o nome das linhas utilizando os valores da coluna 1

classareasfull[is.na(classareasfull)] <- 0

classareasfull <-
  classareasfull[2:(rasternbands + 1)] # retira a coluna 1 (zonas)

colnames(classareasfull) <-
  as.character(years) # coloca os números dos anos nas colunas

maskjatobaDFSpatial <-
  as(maskjatoba[[which(maskjatoba@data@names == 2018)]], "SpatialPixelsDataFrame") # transformar para tabela de dados espaciais
maskjatobaDFSpatial <-
  as.data.frame(maskjatobaDFSpatial, xy = TRUE) # transformar para data frame
colnames(maskjatobaDFSpatial) <-
  c("classe", "x", "y") # dar nomes as colunas

maskjatobatransDFSpatial <-
  as(maskjatobatrans, "SpatialPixelsDataFrame") # transformar para tabela de dados espaciais
maskjatobatransDFSpatial <-
  as.data.frame(maskjatobatransDFSpatial, xy = TRUE) # transformar para data frame
colnames(maskjatobatransDFSpatial) <-
  c("classe", "x", "y") # dar nomes as colunas
maskjatobatransDFSpatial <-
  cbind(maskjatobatransDFSpatial,
        source = ((maskjatobatransDFSpatial[, 1] %/% 1e2) %% 1e2),
        target = ((maskjatobatransDFSpatial[, 1] %/% 1e0) %% 1e2))
maskjatobatransDFSpatial <- cbind(
  maskjatobatransDFSpatial,
  mudanca = ifelse(
    maskjatobatransDFSpatial[, 4] == maskjatobatransDFSpatial[, 5],
    "Não Houve Mudança",
    "Houve Mudança"
  ),
  cormudanca = ifelse(
    maskjatobatransDFSpatial[, 4] == maskjatobatransDFSpatial[, 5],
    "#247503",
    "#8C8A04"
  )
)

zonemerged <-
  merge(classareaszones[[which(maskjatoba@data@names == 2018)]],
        classcodes,
        by.x = "zone",
        by.y = "id")

classareasfullmelt <-
  cbind(zones = rownames(classareasfull), classareasfull)
classareasfullmelt <- melt(classareasfullmelt, id = "zones")
classareasfullmeltmerge <-
  merge(classareasfullmelt,
        classcodes,
        by.x = "zones",
        by.y = "id")
classareasfullmeltmerge$zones <-
  as.numeric(levels(classareasfullmeltmerge$zones))[classareasfullmeltmerge$zones]
classareasfullmeltmerge <-
  classareasfullmeltmerge[order(classareasfullmeltmerge$zones), ]
# colnames(classareasfullmeltmerge[1,])<- "classes"
classareasfullmeltmerge[is.na(classareasfullmeltmerge)] <- 0


nodes <- zonemerged
nodes <- nodes[, c(-2, -4, -5)]
colnames(nodes) <- c("zone", "name")
nodes$zone <- nodes$zone - 2
ids <- data.frame(zone = seq(max(nodes$zone)))
nodes <- merge(ids, nodes, all.x = T)
nodes <-
  data.frame(name = as.character(nodes$name), stringsAsFactors = F)

links <- data.frame(zonetransmerged[, c(5, 8, 4)])

links$sum <- as.double(links$sum)
colnames(links) <- c("source", "target", "value")


landuse <- list(nodes = nodes, links = links)

tidy_landuse <- landuse$links %>%
  gather_set_data(1:3)

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


v_serie_1995_2020<- fortify.zoo(v_jatobaii_1995_2020,names = "data")
names(v_serie_1995_2020)[2]<- "valor"

v_maxdf <- as.data.frame(cbind(index(v_max), coredata(v_max)))
v_maxdf2 <-
  cbind(v_maxdf,
        "mes" = substr(v_maxdf[, 1], 1, 2),
        "ano" = substr(v_maxdf[, 1], 4, 7))
v_maxdf3 <- dcast(v_maxdf2, mes ~ ano, value.var = 'V2')
v_annual <-
  sapply(v_maxdf3[, -1], function(x)
    x[max(which(!is.na(x)))])
v_annual <-
  data.frame(
    "data" = as.Date(paste(names(v_annual), 12, 31, sep = "-")),
    "v" = as.numeric(v_annual),
    row.names = NULL
  )

p_annual['1992']<- NA
p_annual['1993']<- NA

p_annual2 <-
  data.frame("data" = as.Date(paste(index(p_annual), 12, 31, sep = "-")), "p" =
               coredata(p_annual))

landuse <- t(classareasfull)



p_annual3 <- p_annual2[-88, ]

groups<-split(sort(p_annual3$p), ceiling(seq_along(sort(p_annual3$p))/36))

p_annual3$tipo[p_annual3$p<=max(groups$`1`)]<- "Seco"
p_annual3$tipo[p_annual3$p>max(groups$`1`) & p_annual3$p<=max(groups$`2`)]<- "Normal"
p_annual3$tipo[p_annual3$p>max(groups$`2`) & p_annual3$p<=max(groups$`3`)]<- "Úmido"

landuse <- landuse[-14, ]

princesapop <- princesapop[-19, ]

datasetplot <-
  cbind(v_annual[1:23, ], "p" = p_annual3[85:107, ]$p, "pop" = princesapop$pop[16:38], landuse[11:33, ])

datasetplot <- datasetplot %>% filter(as.numeric(data) < "1996")

datasetplotscaled <-
  data.frame("data" = datasetplot[, 1], scale(datasetplot[, -1]))

datasetplotmelted <- melt(datasetplot, id.vars = "data")

datasetplotscaledmelted <- melt(datasetplotscaled, id.vars = "data")

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

col <-
  colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

M <- cor(datasetplotscaled[, -1])
colnames(M) <- c("V", "P", "Pop", as.character(zonemerged$zone))
rownames(M) <- c("V", "P", "Pop", as.character(zonemerged$zone))

#### Correlation Plot ####

corrplot(
  M,
  method = "color",
  col = col(200),
  type = "upper",
  order = "FPC",
  # number.cex = 0.4,
  addCoef.col = "black",
  # Add coefficient of correlation
  tl.col = "black",
  tl.srt = 45,
  sig.level = 0.01,
  insig = "blank",
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
      as.character(zonemerged$"uso_do_solo")
    ) ,
    values = c("blue", "black","red", zonemerged$colors)
  ) +
  theme_minimal()


#### Selected Variables line plot ####

ggplot(
  datasetplotscaledmelted %>% filter(variable == "pop" |
                                       variable == "v"),
  aes(x = data, y = value)
) +
  geom_line(aes(color = variable), size = 1) +
  scale_fill_manual(
    "Legenda",
    labels = c("População", "Volume", zonemerged$"uso_do_solo") ,
    values = c("blue", "black", zonemerged$colors)
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



ggplot(tidy_landuse, aes(
  x,
  id = id,
  split = y,
  value = value
)) +
  # geom_parallel_sets(aes(fill = id), alpha = 0.3, axis.width = 0.1) +
  geom_parallel_sets(aes(fill = factor(source)), alpha = 1, axis.width = 0.1) +
  geom_parallel_sets_axes(axis.width = 0.1) +
  # geom_parallel_sets_labels(colour = 'white',angle = 360, position = "identity") +
  scale_x_discrete(
    labels = c("1987", "2017"),
    limits = c("source", "target"),
    expand = c(0.05, 0.15)
  ) +
  scale_fill_manual("Classes",
                    labels = zonemerged$uso_do_solo ,
                    values = zonemerged$colors)

ggplot(
  data = classareasfullmeltmerge %>% filter(variable == "1987" |
                                              variable == "2001" |
                                              variable == "2018"),
  aes(x = variable, y = value, group = uso_do_solo)
) +
  geom_line(aes(color = uso_do_solo, alpha = 1), size = 2) +
  geom_point(aes(color = uso_do_solo, alpha = 1), size = 4) +
  geom_text_repel(
    data = classareasfullmeltmerge %>% filter(variable == "1987"),
    aes(label = paste0(signif(value, 2), " km²")) ,
    hjust = "right",
    fontface = "bold",
    size = 4,
    nudge_x = -.5,
    direction = "y"
  ) +
  geom_text_repel(
    data = classareasfullmeltmerge %>% filter(variable == "2018"),
    aes(label = paste0(signif(value, 2), " km²")) ,
    hjust = "left",
    fontface = "bold",
    size = 4,
    nudge_x = .5,
    direction = "y"
  ) +
  # move the x axis labels up top
  scale_color_manual("Legenda", values = zonemerged$colors) +
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
  labs(title = "Evolução do Uso do Solo na Bacia do Jatobá II",
       # subtitle = "Valor da Área em Km²",
       caption = "Fonte dos Dados: Autor/MapBiomas,2017.")

newggslopegraph(
  z,
  variable,
  value,
  uso_do_solo,
  Title = "Evolução do Uso do Solo na Bacia do Jatobá II",
  CaptionJustify = "R",
  SubTitle = "Valor da Área em Km²",
  DataTextSize = 5,
  DataLabelPadding = 0.01,
  Caption = "Fonte:MapBiomas",
  WiderLabels = T,
  YTextSize = 3,
  XTextSize = 14,
  LineThickness = 1,
  LineColor = zonemerged$colors
)

ggplot() +
  geom_raster(data = maskjatobaDFSpatial, aes(
    x = x,
    y = y,
    fill = as.factor(classe)
  )) +
  geom_sf(data = st_as_sf(bacia_jatoba),
          colour = "black",
          fill = NA) +
  labs(
    title = "Uso do Solo da Bacia de Drenagem do Açude Jatobá",
    subtitle = "Ano: 2017",
    caption = "Fonte dos Dados: Autor/MAPBIOMAS",
    y = "Latitude",
    x = "Longitude"
  ) +
  scale_fill_manual("Classes",
                    labels = zonemerged$"uso_do_solo" ,
                    values = zonemerged$colors) +
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

ggplot() +
  geom_raster(data = maskjatobatransDFSpatial, aes(
    x = x,
    y = y,
    fill = as.factor(mudanca)
  )) +
  geom_sf(data = st_as_sf(bacia_jatoba),
          colour = "black",
          fill = NA) +
  labs(
    title = "Transição de Uso do Solo da Bacia de Drenagem do Açude Jatobá",
    subtitle = "Ano: 1985/2017",
    caption = "Fonte dos Dados: Autor/MAPBIOMAS",
    y = "Latitude",
    x = "Longitude"
  ) +
  # scale_fill_discrete("Classes")+
  scale_fill_manual("Classes",
                    values = c("Não Houve Mudança" = "green", "Houve Mudança" = "red")) +
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

ggplot(
  classareasfullmeltmerge,
  aes(
    x = as.numeric(levels(classareasfullmeltmerge$variable))[classareasfullmeltmerge$variable],
    y = classareasfullmeltmerge$value,
    fill = as.factor(classareasfullmeltmerge$zones)
  )
) +
  geom_area(colour = "black", size = .3) +
  labs(
    title = "Evolução de Uso do Solo da Bacia de Drenagem do Açude Jatobá",
    subtitle = "Ano: 1985/2017",
    caption = "Fonte dos Dados: Autor/MAPBIOMAS",
    y = "Área(km²)",
    x = "Tempo(anos)"
  ) +
  scale_x_continuous(breaks = seq(1985, 2017, 2)) +
  scale_fill_manual("Classes",
                    labels = zonemerged$uso_do_solo ,
                    values = zonemerged$colors)


ggplot(classareasfullmeltmerge, aes(x = as.numeric(
  levels(classareasfullmeltmerge$variable)
)[classareasfullmeltmerge$variable], y = value)) +
  geom_line() +
  labs(
    title = "Evolução de Uso do Solo da Bacia de Drenagem do Açude Jatobá",
    subtitle = "Ano: 1985/2017",
    caption = "Fonte dos Dados: Autor/MAPBIOMAS",
    y = "Área(km²)",
    x = "Tempo(anos)"
  ) +
  facet_wrap(
    ~ zones,
    scales = 'free_y',
    ncol = 1,
    labeller = unique(as.character(classareasfullmeltmerge$uso_do_solo))
  ) +
  labs(x = "", y = "") +
  theme_bw()

scale_fill_manual("Classes",
                  labels = zonemerged$uso_do_solo ,
                  values = zonemerged$colors)


ggplot(classareasfullmeltmerge, aes(x = variable, y = value)) +
  geom_line(aes(color = as.factor(uso_do_solo)), size = 1) +
  theme_minimal()


ggplot() +
  geom_raster(
    data = a,
    aes(
      x = variable,
      y = a$'rownames(classareasfull)',
      fill = value
    ),
    interpolate = F
  )
