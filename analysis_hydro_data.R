# fazer a análise da eficiencia do reservatório 
# calcular a demanda pelos dados de evapo e defluencia
# sum(jatoba_II_bh$diff)/12 idéia final 

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
    "magrittr",
    "readxl",
    "timeDate",
    "fuzzyjoin",
    "lubridate",
    "readxl"
  ) # lista de pacotes utilizados

new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])] # checar se há algum pacote novo

if (length(new.packages))
  install.packages(new.packages) # instala os pacotes novos

lapply(list.of.packages, require, character.only = TRUE) # carrega os pacotes necessários

setwd("G:/My Drive/Pesquisa/Coordenador de Projetos Pesquisa 2017/Pré-tratamento dos dados") ## mudar a o local aonde estão os arquivos

load("Dados_Zoo.RData") ## Carregar dados gerados pelo hydroweb e aesa

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

setAs("character","myDate", function(from) as.Date(from, format="%d/%m/%Y") )


#### 3 Pré tratamento dos dados ####


# 3.1 Precipitation Data --------------------------------------------------


princesa2016<- read.csv2("princesa2016.csv",header = T,
                         sep = ",",dec = ",",
                         colClasses=c("character","character","myDate","character"))

princesa2016$PrecipitaÃ.Ã.o..mm.<- as.numeric(gsub(",",".",princesa2016$PrecipitaÃ.Ã.o..mm.))


princesa2017<- read.csv2("princesa2017.csv",header = T,
                         sep = ",",dec = ",",
                         colClasses=c("character","character","myDate","character"))

princesa2017$PrecipitaÃ.Ã.o..mm.<- as.numeric(gsub(",",".",princesa2017$PrecipitaÃ.Ã.o..mm.))

princesa2018<- read.csv2("princesa2018.csv",header = T,
                         sep = ",",dec = ",",
                         colClasses=c("character","character","myDate","character"))

princesa2018$PrecipitaÃ.Ã.o..mm.<- as.numeric(gsub(",",".",princesa2018$PrecipitaÃ.Ã.o..mm.))

princesa2019<- read.csv2("princesa2019.csv",header = T,
                         sep = ",",dec = ",",
                         colClasses=c("character","character","myDate","character"))

princesa2019$PrecipitaÃ.Ã.o..mm.<- as.numeric(gsub(",",".",princesa2019$PrecipitaÃ.Ã.o..mm.))

princesa_mensal_2020<- read_excel('G:/My Drive/Pesquisa/Coordenador de Projetos Pesquisa 2018/Dados Caatinga/relatorio-pluviometria.xlsx')

princesa_mensal_2020_long<- princesa_mensal_2020 %>%
  pivot_longer(cols = starts_with('dia '), 
               values_drop_na = FALSE) %>% 
  mutate(name = stringr::str_remove_all(name,"dia.")) %>%
  unite(Date, Ano, Mês, name, sep = '-') %>%
  mutate(Date = as.Date(Date))  %>%
  filter(!is.na(Date))

a2016<-zoo(princesa2017$PrecipitaÃ.Ã.o..mm.,princesa2017$Data.do.registro)

a2017<-zoo(princesa2017$PrecipitaÃ.Ã.o..mm.,princesa2017$Data.do.registro)

a2018<-zoo(princesa2018$PrecipitaÃ.Ã.o..mm.,princesa2018$Data.do.registro)

a2019<-zoo(princesa2019$PrecipitaÃ.Ã.o..mm.,princesa2019$Data.do.registro)

a2020<-zoo(princesa_mensal_2020_long$value,princesa_mensal_2020_long$Date)

pi_plu_ser_1911_2020<- rbind.zoo(a1911_1991,a1994_2016,a2017,a2018,a2019,a2020)

pi_plu_ser_1911_2020_full <- merge(pi_plu_ser_1911_2020,zoo(,seq(start(pi_plu_ser_1911_2020),end(pi_plu_ser_1911_2020),by="day")), all=TRUE)

pi_plu_ser_1911_2020_full<- na.fill(pi_plu_ser_1911_2020_full,0)

pi_plu_ser_1911_2020_full[as.Date("2016-06-30")]<- 18.9

pi_plu_ser_1911_2020_full<- pi_plu_ser_1911_2020_full[!year(time(pi_plu_ser_1911_2020_full)) %in% c(1911,1992,1993)]

p_annual<- aggregate(pi_plu_ser_1911_2020_full, format(time(pi_plu_ser_1911_2020_full), "%Y"), sum, na.rm = TRUE) 

# 3.2 Volume Reservoir Data -----------------------------------------------

vol_jatoba_II<- read_csv("G:/My Drive/Pesquisa/Coordenador de Projetos Pesquisa 2018/Dados Caatinga/volumejatoba2021.csv", locale = locale("br",decimal_mark = ',',date_format = "%d/%m/%Y"))

vol_jatoba_II_annual<- vol_jatoba_II %>% 
  group_by(ano=format(`Data do registro`, "%Y")) %>%
  top_n(1, `Data do registro`)
  # summarise(volume_m3 = max(`Data do registro`)) %>% 

# v_jatobaii_1995_2021<- zoo(volumejatoba2$'Volume..mÂ³.',volumejatoba2$Data.do.registro)
# v_jatobaii_1995_2020full<- merge(v_jatobaii_1995_2020,zoo(,seq(start(v_jatobaii_1995_2020),end(v_jatobaii_1995_2020),by="month")), all=TRUE)

# v_max<- aggregate(v_jatobaii_1995_2021, format(time(v_jatobaii_1995_2021), "%m/%Y"), max, na.rm = TRUE) # Calcular a médias dos dias por mês

cav_jatoba_II <- 
  read_excel('G:/My Drive/Pesquisa/Coordenador de Projetos Pesquisa 2018/Dados Caatinga/CAVL_JatobaII.xlsx')[,1:3] %>% 
  set_colnames(c('cota','area_m2','vol_m3'))

monthly_sum_precip <- 
  fortify(pi_plu_ser_1911_2020_full) %>%
  set_colnames(c('mes_ano','valor')) %>%
  group_by(mes_ano=format(mes_ano, "%Y-%m")) %>% # group by the day column
  summarise(sum=sum(valor)) %>%
  mutate(data=as.Date.character(timeLastDayInMonth(paste(mes_ano,'01',sep = "-")))) %>% 
  # add_row(mes_ano = str_sub(princesa_mensal_2020$`Data do registro`,1,-4), sum = princesa_mensal_2020$`Precipitação (mm)`, data = princesa_mensal_2020$`Data do registro`) %>% 
  # add_row(mes_ano = '2020-08',sum = 0,data = as.Date('2020-08-31')) %>% 
  arrange(data)

jatoba_II_bh<- vol_jatoba_II %>%  
  # fortify(v_jatobaii_1995_2021) %>%
  set_colnames(c('nome','data','vol_perc','vol_m3','aflu_deflu_m3')) %>%
  arrange(data) %>% 
  # filter(row_number() <= n()-2) %>%
  mutate(mes_ano = str_sub(data,1,-4)) %>%
  mutate(mes = as.integer(str_sub(data,6,7))) %>%
  mutate(ano = as.integer(str_sub(data,1,4))) %>%
  left_join(.,dplyr::select(monthly_sum_precip,mes_ano,sum),by = 'mes_ano') %>% 
  left_join(.,Evpt_pianco_basin,by = 'mes') %>%
  dplyr::rename(p = sum, evpt = val) %>% 
  filter( ano < '2021' & ano > '2019') %>%
  difference_join(.,cav_jatoba_II,'vol_m3', max_dist = 1000, mode = 'left') %>% 
  distinct(., mes_ano, .keep_all = TRUE) 


# 3.3 Evapotranspiration Data -------------------------------------------------

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

Evpt_pianco_basin<- data.frame("mes"=1:12,"val"=c(207,183,191,169,152,132,143,167,185,208,210,215),stringsAsFactors = F)
Evpt_sousa<- data.frame("month"=1:12,"val"=c(173.6,134.4,117.8,114,108.5,108,120.9,145.7,159,176.7,174,182.9),stringsAsFactors = F)


# 4 Plots -----------------------------------------------------------------

# 4.1 Daily Preciptation Matrix Plot --------------------------------------


# Média -------------------------------------------------------------------

daily_mean_pi <-
  aggregate(pi_plu_ser_1911_2020_full,
            format(time(pi_plu_ser_1911_2020_full), "%d/%m"),
            mean,
            na.rm = TRUE)
daily_mean_pi <- as.data.frame(daily_mean_pi)
daily_mean_pi$names <- rownames(daily_mean_pi)
daily_mean_pi <- cbind(daily_mean_pi, substr(daily_mean_pi[, 2], 4, 5), substr(daily_mean_pi[, 2], 1, 2),as.Date.character(paste("2020",substr(daily_mean_pi[, 2], 4, 5), substr(daily_mean_pi[, 2], 1, 2),sep = "-") ))
colnames(daily_mean_pi) <- c("Valor", "Dia_Mes", "Mes", "Dia","Data")
daily_mean_pi<- daily_mean_pi[order(daily_mean_pi$Data),]

ggplot(data = daily_mean_pi, aes(x = Mes, y = Dia, fill = Valor)) +
  geom_tile(color = "Black", size = .7) +
  scale_y_discrete(name = "Dias", breaks = unique(daily_mean_pi$Dia)[seq(1, 31, 2)]) +
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
       caption = "Fonte dos dados: AESA/ANA, 2019.")

ggsave('p_media.png',width = 11,height = 8.5,units = 'in',path = "G:/My Drive/Pesquisa/Coordenador de Projetos Pesquisa 2018/Dados Caatinga",device = "png",dpi = 320)


# Max ---------------------------------------------------------------------


daily_sum_precip <- 
  fortify(pi_plu_ser_1911_2020_full) %>%
  set_colnames(c('dia_mes','valor')) %>%
  group_by(dia_mes=format(dia_mes, "%m-%d")) %>% # group by the day column
  summarise(max=max(valor)) %>%
  mutate(mes=substr(.$dia_mes, 1, 2),dia=as.factor(substr(.$dia_mes, 4, 5))) %>% 
  mutate(data=as.Date.character(paste("2020",mes, dia,sep = "-") )) %>% 
  arrange(data)

ggplot(data = daily_sum_precip, aes(x = mes, y = dia, fill = max)) +
  geom_tile(color = "Black", size = .7) +
  scale_y_discrete(name = "Dias", breaks = unique(daily_sum_precip$dia)[seq(1, 31, 2)]) +
  scale_x_discrete(name = "Meses", labels = monthnames) +
  # geom_text(aes(Mes, Dia, label = Valor), color = "black", size = 4) +
  scale_fill_gradientn(colours = brewer.pal(5,'Blues'), name = 'P(mm)') +
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
       subtitle = "Valor máximo diário da série histórica 1911-2019",
       caption = "Fonte dos dados: AESA/ANA, 2019.")

# Dias com Chuva ----------------------------------------------------------

daily_count_precip <- 
  fortify(pi_plu_ser_1911_2020_full) %>%
  set_colnames(c('dia_mes','valor')) %>%
  filter(valor>0) %>% 
  group_by(dia_mes=format(dia_mes, "%m-%d")) %>% # group by the day column
  summarise(days_p = n()) %>%
  mutate(mes=substr(.$dia_mes, 1, 2),dia=as.factor(substr(.$dia_mes, 4, 5))) %>% 
  mutate(data=as.Date.character(paste("2020",mes, dia,sep = "-") )) %>% 
  arrange(data)

ggplot(data = daily_count_precip, aes(x = mes, y = dia, fill = days_p)) +
  geom_tile(color = "Black", size = .7) +
  scale_y_discrete(name = "Dias", breaks = unique(daily_sum_precip$dia)[seq(1, 31, 2)]) +
  scale_x_discrete(name = "Meses", labels = monthnames) +
  # geom_text(aes(Mes, Dia, label = Valor), color = "black", size = 4) +
  scale_fill_viridis_c(name = 'Nº (dias)') +
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
       subtitle = "Número de dias com chuva na série histórica 1911-2019",
       caption = "Fonte dos dados: AESA/ANA, 2019.")


# 4.2 Monthly Mean Precipitation ----------------------------------------------

monthly_mean_pi <-
  aggregate(pi_plu_ser_1911_2020_full, format(time(pi_plu_ser_1911_2020_full), "%m/%Y"), sum, na.rm = TRUE) # Calcular a chuva mensal

monthly_mean_pi <- as.data.frame(monthly_mean_pi)

monthly_mean_pi$names <- rownames(monthly_mean_pi)

monthly_mean_pi <- cbind(monthly_mean_pi, substr(monthly_mean_pi[, 2], 4, 7), substr(monthly_mean_pi[, 2], 1, 2))

colnames(monthly_mean_pi) <- c("Valor", "Dia_Ano", "Ano", "Mes")

monthly_mean_pi <- dcast(monthly_mean_pi, Ano ~ Mes, value.var = "Valor")

monthly_mean_pi <- monthly_mean_pi[, 2:ncol(monthly_mean_pi)]

colnames(monthly_mean_pi) <- monthnames

rownames(monthly_mean_pi) <- as.vector(unique(monthly_mean_pi$Ano))

monthly_mean_pi <- colMeans(monthly_mean_pi)

monthly_mean_pi <- data.frame('value'=monthly_mean_pi, "m" = names(monthly_mean_pi))

df2<- monthly_mean_pi

df3<- monthly_mean_pi

df3$m<-1:12

df2$m<-1:12

df2 <- prepGradient(df2$m, df2$value)

ggplot(df2, aes(x = as.factor(x), y = y, fill = z)) +
  geom_bar(stat = "identity") +
  geom_bar(df3,mapping = aes(x = as.factor(m), y = value, fill = value), alpha = 0.1, stat = "identity",color = "black") +
  scale_fill_gradientn(colours = rev(matlab.like2(5)), name = 'P(mm)') +
  scale_y_continuous(name = "Precipitação (mm)") +
  scale_x_discrete(name = "Meses",
                   breaks = 1:12,
                   labels = monthnames) +
  theme_minimal() +
  labs(title = "Precipitação na Estação Pluviométrica Princesa Isabel",
       subtitle = "Valor médio mensal da série histórica 1911-2019",
       caption = "Fonte dos dados: AESA/ANA, 2019.")


# 4.3 Annual Precipitation Plot -------------------------------------------

annual_precip <- 
  fortify(pi_plu_ser_1911_2020_full) %>%
  set_colnames(c('data','valor')) %>%
  group_by(ano=as.numeric(format(data, "%Y"))) %>% # group by the day column
  summarise(max=max(valor),sum=sum(valor)) %>%
  mutate(tipo = if_else(sum<mean(sum),'Abaixo da Média','Acima da Média'))


ggplot(data = annual_precip, aes(
  x = ano,
  y = sum,
  color = sum,
  label = sum
)) +
  geom_point(aes(shape = tipo), size = 3) +
  labs(
    title = "Precipitação Acumulada Anual na Estação Pluviométrica Princesa Isabel 1912 - 2019",
    subtitle = "",
    y = "Precipitação (mm)",
    x = "Tempo (anos)",
    caption = "Fonte dos Dados: ANA/AESA, 2020.\nFonte: Silva, 2021.", shape = "Tipo"
  ) +  # title and caption
  scale_color_gradientn(colours = rev(matlab.like2(5)), name = 'P(mm)') +
  # scale_x_continuous(breaks = annual_precip$ano[seq(1,106,5)]) +
  geom_hline(aes(yintercept=mean(sum),linetype="Média"),
             color="red", size=1) +
  # geom_text(aes(label=ifelse(temp<=coredata(day$temp[which.min(day$temp)]),as.character(temp),'')),hjust=0,vjust=1) +
  scale_linetype_manual(name = '',values = c(3,3,3),  
                        guide = guide_legend(override.aes = list(color = c("red")))) +
  theme_bw()
