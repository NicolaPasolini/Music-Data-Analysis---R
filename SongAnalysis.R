install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyverse")
library(ggplot2)
library(dplyr)
library(tidyverse)



# pulizia dati

# tolgo canzoni con popolarità = 0 per non sporcare i dati
nrow(music)
music <- subset(music, pop != 0)
# quante canzoni ho per anno? 
distribuzione_anni <- table(music$year)
print(distribuzione_anni)
barplot(distribuzione_anni, main = "Distribuzione delle Canzoni per Anno", xlab = "Anno", ylab = "Numero di Canzoni")
# scelgo il minimo e rendo omogeneo il numero 
music_top31 <- music %>%
  group_by(year) %>%
  arrange(desc(pop)) %>%
  slice(1:31) %>%
  ungroup()
distribuzione_top31 <- table(music_top31$year)
print(distribuzione_top31)
barplot(distribuzione_top31, main = "Distribuzione delle Canzoni per Anno", xlab = "Anno", ylab = "Numero di Canzoni")









#analisi generale

# hall of fame artisti
top_artist <- music_top31 %>%
  group_by(artist) %>%
  summarize(
    total_pop = sum(pop),  # Somma della popolarità per artista
    song_count = n()       # Conta il numero di canzoni per ogni artista
  ) %>%
  arrange(desc(song_count)) %>%
  slice_head(n = 6)

ggplot(top_artist, aes (reorder (artist, total_pop), total_pop))+
  geom_col(aes (fill=total_pop))+
  geom_point ()+
  labs(title = "Popolarità Artisti",
       x = "Artisti",
       y = "Valore Medio di Popolarità") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Ruota le etichette sull'asse x leggibilità


ggplot(top_artist, aes (reorder (artist, song_count), song_count))+
  geom_col(aes (fill=song_count))+
  geom_point ()+
  labs(title = "Popolarità Artisti",
       x = "Artisti",
       y = "Frequenza canzoni") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# hall of fame generi
top_genre <- music_top31 %>%
  group_by(top.genre) %>%
  summarize (sum_pop_genre = sum (pop)) %>%
  arrange (desc (sum_pop_genre))%>%
  head()
top_genre

ggplot(top_genre, aes (reorder (`top.genre`, sum_pop_genre), sum_pop_genre))+
  geom_col(aes (fill = sum_pop_genre))+
  geom_point()+
  labs(title = "Popolarità genere",
     x = "Genere",
     y = "Valore Medio di Popolarità") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Ruota le etichette sull'asse x leggibilità


# Filtrare le canzoni del 2019 e ordinarle per popolarità decrescente
top_songs_2019 <- music_top31 %>%
  filter(year == 2019) %>%
  arrange(desc(pop)) %>%
  slice_head(n = 10)  # Seleziona solo le top 10 canzoni più popolari

# Visualizzare i risultati
print(top_songs_2019)

# Creare un grafico a barre per visualizzare i risultati
ggplot(top_songs_2019, aes(x = reorder(title, pop), y = pop, fill = artist)) +
  geom_col() +
  labs(title = "Top 10 Most Listened Songs in 2019",
       x = "Song Title",
       y = "Popularity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rende i titoli delle canzoni leggibili


#analisi durata e bpm
music <- music %>%
  mutate(genre_group = case_when(
    grepl("country", tolower(`top.genre`)) ~ "Country",
    grepl("hip hop|rap", tolower(`top.genre`)) ~ "Hip Hop",
    `top.genre` %in% c("edm", "electro house", "big room", "dubstep", "belgian edm", "electronic trap", "brostep", "complextro") ~ "EDM",
    grepl("pop", tolower(`top.genre`)) & !grepl("hip hop|rap|latin", tolower(`top.genre`)) ~ "Pop",
    grepl("indie", tolower(`top.genre`)) | `top.genre` == "alaska indie" ~ "Indie",
    grepl("latin", tolower(`top.genre`)) | `top.genre` %in% c("reggaeton", "tropical house", "canadian latin") ~ "Latin",
    grepl("r&b", tolower(`top.genre`)) | `top.genre` == "alternative r&b" ~ "R&B",
    `top.genre` %in% c("chicago rap", "detroit hip hop", "canadian hip hop", "barbadian pop") ~ "Rap",
    TRUE ~ "Other"
  ))



table(music$genre_group)

duration <-music%>%
  ggplot(aes(x=reorder(genre_group,dur),y=dur))+geom_boxplot(fill="skyblue")+ggtitle("Durata canzoni")+theme_light()
duration

max_pop <-music%>%filter(dur>=400)%>%select(artist,dur,title)
max_pop
min_pop <-music%>%filter(dur<=150)%>%select(artist,dur,title)
min_pop

bpm <- music%>%
  ggplot(aes(x=reorder(genre_group,bpm),y=bpm))+geom_boxplot(fill="skyblue")+ggtitle("Distribuzione bpm")+theme_light()
bpm

bpm_most <-music%>%filter(bpm>=200)%>%group_by(artist)%>%summarise(title,bpm)
bpm_most




# analisi andamento del valore medio di popolarità delle canzoni nei vari anni per valutare trend 
media_pop_per_anno <- aggregate(pop ~ year, data = music, FUN = mean)
print(media_pop_per_anno)
# Crea un grafico a dispersione con linea di tendenza
ggplot(media_pop_per_anno, aes(x = year, y = pop)) +
  geom_point(color = "dodgerblue3", size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "darkorange", fill = "lightgoldenrodyellow", alpha = 0.3) +
  labs(title = "Valore Medio di Popolarità delle Canzoni per Anno",
       x = "Anno",
       y = "Valore Medio di Popolarità") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "gray"),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "none")


# analisi popolarità generi nel 2019
music_2019 <- subset(music, year == 2019)
# Calcola il valore medio di popolarità per ogni genere
media_pop_per_genere <- aggregate(pop ~ top.genre, data = music_2019, FUN = mean)
# Crea un grafico a barre con legenda
ggplot(media_pop_per_genere, aes(x = top.genre, y = pop, fill = top.genre)) +
  geom_bar(stat = "identity") +
  labs(title = "Popolarità per Genere nel 2019",
       x = "Genere",
       y = "Valore Medio di Popolarità") +
  scale_fill_brewer(palette = "Set3") + 
  theme_minimal() +
  theme(axis.text.x = element_blank())  # Nasconde le etichette sull'asse x













# analisi correlazione tra popolarità e vari indici 

# pop - live
correlazione <- cor(music_top31$pop, music_top31$live)
print(paste("Correlazione tra popolarità e live:", correlazione))
# andamento
media_live_anno <- aggregate(live ~ year, data = music_top31, FUN = mean)
media_totale_anno <- merge(media_pop_per_anno, media_live_anno, by = "year")
ggplot(media_totale_anno, aes(x = year)) +
  geom_line(aes(y = pop, color = "Popolarità"), size = 1.5) +
  geom_line(aes(y = live, color = "Live"), size = 1.5) +
  labs(title = "Andamento della Popolarità Media e del Valore Live Medio Negli Anni",
       x = "Anno",
       y = "Media") +
  scale_color_manual(values = c("Popolarità" = "blue", "Live" = "red")) +
  theme_minimal()
# dispersione
ggplot(music_top31, aes(x = pop, y = live)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Correlazione tra Popolarità e Valore Live",
       x = "Popolarità",
       y = "Valore Live") +
  theme_minimal() +
  coord_fixed(ratio = 1) +
  xlim(1, 100) +
  ylim(1, 100)


# pop - acous
correlazione <- cor(music_top31$pop, music_top31$acous)
print(paste("Correlazione tra popolarità e acusticità:", correlazione))
# andamento
media_acous_anno <- aggregate(acous ~ year, data = music_top31, FUN = mean)
media_totale_anno <- merge(media_pop_per_anno, media_acous_anno, by = "year")
ggplot(media_totale_anno, aes(x = year)) +
  geom_line(aes(y = pop, color = "Popolarità"), size = 1.5) +
  geom_line(aes(y = acous, color = "Acusticità"), size = 1.5) +
  labs(title = "Andamento della Popolarità Media e del Valore acustico Medio Negli Anni",
       x = "Anno",
       y = "Media") +
  scale_color_manual(values = c("Popolarità" = "blue", "Acusticità" = "red")) +
  theme_minimal()
# dispersione
ggplot(music_top31, aes(x = pop, y = acous)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Correlazione tra Popolarità e Acusticità",
       x = "Popolarità",
       y = "Acusticità") +
  theme_minimal() +
  coord_fixed(ratio = 1) +
  xlim(1, 100) +
  ylim(1, 100)


# pop - dnce
correlazione <- cor(music_top31$pop, music_top31$dnce)
print(paste("Correlazione tra popolarità e ballabilità:", correlazione))
# andamento
media_dnce_anno <- aggregate(dnce ~ year, data = music_top31, FUN = mean)
media_totale_anno <- merge(media_pop_per_anno, media_dnce_anno, by = "year")
ggplot(media_totale_anno, aes(x = year)) +
  geom_line(aes(y = pop, color = "Popolarità"), size = 1.5) +
  geom_line(aes(y = dnce, color = "Ballabilità"), size = 1.5) +
  labs(title = "Andamento della Popolarità Media e della Ballabilità Media Negli Anni",
       x = "Anno",
       y = "Media") +
  scale_color_manual(values = c("Popolarità" = "blue", "Ballabilità" = "red")) +
  theme_minimal()
# dispersione
ggplot(music_top31, aes(x = pop, y = dnce)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Correlazione tra Popolarità e Ballabilità",
       x = "Popolarità",
       y = "Ballabilità") +
  theme_minimal() +
  coord_fixed(ratio = 1) +
  xlim(1, 100) +
  ylim(1, 100)


# pop - nrgy
correlazione <- cor(music_top31$pop, music_top31$nrgy)
print(paste("Correlazione tra popolarità e energia:", correlazione))
# andamento
media_nrgy_anno <- aggregate(nrgy ~ year, data = music_top31, FUN = mean)
media_totale_anno <- merge(media_pop_per_anno, media_nrgy_anno, by = "year")
ggplot(media_totale_anno, aes(x = year)) +
  geom_line(aes(y = pop, color = "Popolarità"), size = 1.5) +
  geom_line(aes(y = nrgy, color = "Energia"), size = 1.5) +
  labs(title = "Andamento della Popolarità Media e della Energia Media Negli Anni",
       x = "Anno",
       y = "Media") +
  scale_color_manual(values = c("Popolarità" = "blue", "Energia" = "red")) +
  theme_minimal()
# dispersione
ggplot(music_top31, aes(x = pop, y = nrgy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Correlazione tra Popolarità e Energia",
       x = "Popolarità",
       y = "Energia") +
  theme_minimal() +
  coord_fixed(ratio = 1) +
  xlim(1, 100) +
  ylim(1, 100)


# pop - spch
correlazione <- cor(music_top31$pop, music_top31$spch)
print(paste("Correlazione tra popolarità e parole:", correlazione))
# andamento
media_spch_anno <- aggregate(spch ~ year, data = music_top31, FUN = mean)
media_totale_anno <- merge(media_pop_per_anno, media_spch_anno, by = "year")
ggplot(media_totale_anno, aes(x = year)) +
  geom_line(aes(y = pop, color = "Popolarità"), size = 1.5) +
  geom_line(aes(y = spch, color = "Parole"), size = 1.5) +
  labs(title = "Andamento della Popolarità Media e del n. di Parole Medie Negli Anni",
       x = "Anno",
       y = "Media") +
  scale_color_manual(values = c("Popolarità" = "blue", "Parole" = "red")) +
  theme_minimal()
# dispersione
ggplot(music_top31, aes(x = pop, y = spch)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Correlazione tra Popolarità e Parole",
       x = "Popolarità",
       y = "Parole") +
  theme_minimal() +
  coord_fixed(ratio = 1) +
  xlim(1, 100) +
  ylim(1, 100)



#val
correlazione <- cor(music_top31$pop, music_top31$val)
print(paste("Correlazione tra popolarità e valenza:", correlazione))
# andamento
media_val_anno <- aggregate(val ~ year, data = music_top31, FUN = mean)
media_totale_anno <- merge(media_pop_per_anno, media_val_anno, by = "year")
ggplot(media_totale_anno, aes(x = year)) +
  geom_line(aes(y = pop, color = "Popolarità"), size = 1.5) +
  geom_line(aes(y = val, color = "Valenza"), size = 1.5) +
  labs(title = "Andamento della Popolarità Media e della Valenza Negli Anni",
       x = "Anno",
       y = "Media") +
  scale_color_manual(values = c("Popolarità" = "blue", "Valenza" = "red")) +
  theme_minimal()
# dispersione
ggplot(music_top31, aes(x = pop, y = val)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Correlazione tra Popolarità e Valenza",
       x = "Popolarità",
       y = "Valenza") +
  theme_minimal() +
  coord_fixed(ratio = 1) +
  xlim(1, 100) +
  ylim(1, 100)




val_nrgy <-music%>%
  ggplot(aes(x=val,y=nrgy))+geom_point()+ggtitle("Positività/Energia")+theme_light()+geom_smooth(se=FALSE)
val_nrgy
cor(music$val,music$nrgy)

val_nrgy <-music%>%ggplot(aes(x=val,y=nrgy))+geom_point()+ggtitle("Positività/Energia")+theme_light()+geom_smooth(se=FALSE)+facet_wrap(~genre_group)
val_nrgy



acous_nrgy <-music%>%ggplot(aes(x=acous,y=nrgy))+geom_point()+ggtitle("Acousticità/Energia")+theme_light()+geom_smooth(se=FALSE)
acous_nrgy
cor(music$acous,music$nrgy)
