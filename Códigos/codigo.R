require(tidyverse)

datos_main = rio::import("Datos/EvolutionPopUSA_MainData.csv")
datos_genero = rio::import("Datos/EvolutionPopUSA_TagData.tsv")

datos_main = datos_main %>% 
  select(recording_id, artist_name_clean, track_name, first_entry, quarter,
         year, fiveyear, decade)

a = datos_main %>% 
  mutate(repet = NA, repet_dec = NA) # crear columnas de  n° repeticion
cuenta = plyr::count(a[2])

for (i in 1:length(a$artist_name_clean)){
  pos = which((a$artist_name_clean[i]) == cuenta$artist_name_clean)
  a$repet[i] = cuenta$freq[pos]
}

decadas = seq(1960, 2010, by=10) # Vector de los años
for (decada in decadas){
  b = a %>% filter(decade == decada)
  cuenta = plyr::count(b[2])
  for (i in 1:length(b$artist_name_clean)){
    pos = which((b$artist_name_clean[i]) == cuenta$artist_name_clean)
    j = which((b$recording_id[i]) == a$recording_id)
    a$repet_dec[j] = cuenta$freq[pos]
  }
}
View(a)


a %>% filter(decade == 1960) %>% filter(repet_dec > 40) %>%
  ggplot(aes(x = artist_name_clean)) +
  geom_bar(stat = "count", fill = "#a580f8", col = "black") +
  labs(x = "Artistas", y = "Entradas en el TOP 100",
         title = "Top 3 artistas más repetidos en la década del 60")

a %>% filter(decade == 1970) %>% filter(repet_dec > 24) %>%
  ggplot(aes(x = artist_name_clean)) +
  geom_bar(stat = "count", fill = "peru", col = "black") +
  labs(x = "Artistas", y = "Entradas en el TOP 100",
       title = "Top 3 artistas más repetidos en la década del 70")

a %>% filter(decade == 1980) %>% filter(repet_dec > 15) %>%
  ggplot(aes(x = artist_name_clean)) +
  geom_bar(stat = "count", fill = "#66cdaa", col = "black") +
  labs(x = "Artistas", y = "Entradas en el TOP 100",
       title = "Top 3 artistas más repetidos en la década del 80")

a %>% filter(decade == 1990) %>% filter(repet_dec > 13) %>%
  ggplot(aes(x = artist_name_clean)) +
  geom_bar(stat = "count", fill = "#a5fcc2", col = "black") +
  labs(x = "Artistas", y = "Entradas en el TOP 100",
       title = "Top 3 artistas más repetidos en la década del 90")

a %>% filter(decade == 2000) %>% filter(repet_dec > 22) %>%
  ggplot(aes(x = artist_name_clean)) +
  geom_bar(stat = "count", fill = "#66bdcd", col = "black") +
  labs(x = "Artistas", y = "Entradas en el TOP 100",
       title = "Top 3 artistas más repetidos en la década del 2000")


