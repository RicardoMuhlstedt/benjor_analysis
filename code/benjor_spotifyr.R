library(tidyverse)
library(spotifyr)
library(lubridate)
library(modelr)

## spotifyr
Sys.setenv(SPOTIFY_CLIENT_ID = "xxxxxxxxxxxxxxxxxx")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "xxxxxxxxxxxxxxxxxxxxxxx")
access_token <- get_spotify_access_token()



## geting artist features with spotifyr
benjor <- get_artist_audio_features("Jorge Ben Jor") %>%
  mutate(time_signature = as.factor(time_signature))



## 5 tracks with most valence
benjor %>%
  arrange(-valence) %>%
  select(track_name, valence, album_name) %>%
  head(5) %>%
  knitr::kable()


## 5 track with most danceability
benjor %>%
  arrange(-danceability) %>%
  select(track_name, danceability) %>%
  head(5) %>%
  knitr::kable()


## 5 tracks with most energy
benjor %>%
  arrange(-energy) %>%
  select(track_name, energy) %>%
  head(5) %>%
  knitr::kable()


## features by album
album_mean <- benjor %>%
  group_by(album_name) %>%
  summarise(mean_energy = mean(energy),
            mean_danceability = mean(danceability),
            mean_valence = mean(valence),
            album_release_year = album_release_year) %>%
  ungroup() %>%
  arrange(-mean_valence)

## removing repetition
sorted_mean <- album_mean %>%
  group_by(album_name) %>%
  distinct(album_name, .keep_all = TRUE) %>%
  arrange(-mean_valence)


  

## relation between danceability and valence
sorted_mean %>%
  ggplot(aes(mean_danceability, mean_valence)) +
  geom_point() +
  geom_smooth()



## relation between energy and valence   
sorted_mean %>%
  ggplot(aes(mean_energy, mean_valence)) +
  geom_point(aes()) +
  geom_smooth()



## relation between loudness and valence
benjor %>%
  ggplot(aes(valence, loudness)) +
  geom_point(aes(color = key_name)) +
  geom_smooth()

## between energy and valence
benjor %>%
  ggplot(aes(energy, valence)) +
  geom_point(aes()) +
  geom_smooth()

## removed relation 
mod1 <- lm(energy ~ valence, benjor)

resid_benjor <- benjor %>%
  select(track_name, valence, energy, danceability, loudness) %>%
  add_residuals(mod1) %>%
  mutate(resid = exp(resid))

## ploting energy with relation removed
resid_benjor %>%
  ggplot(aes(energy, resid)) +
  geom_point(aes())


## remove relation
benjor %>%
  ggplot(aes(danceability, valence)) +
  geom_point() +
  geom_smooth()


## relation between acousticness and valence

## creating model
mod2 <- lm(valence ~ splines::ns(acousticness, 5) , benjor)

## adding predictions
grid <- benjor %>%
  data_grid(acousticness) %>%
  add_predictions(mod2)

## adding residuals
grid2 <- benjor %>%
  add_residuals(mod2)

## fitting the model
benjor %>%
  ggplot(aes(acousticness, valence)) +
  geom_point(size = 2.5, color = "#5ab4ac") +
  geom_line(aes(y = pred), data = grid, size = 1.5, color  = "#756bb1")
  

## checking for a pattern on the residuals
grid2 %>%
  ggplot(aes(acousticness, resid)) +
  geom_point(aes(color = key_name), size = 2.5)

## ploting valence and acousticness
benjor %>%
  ggplot(aes(acousticness, valence)) +
  geom_line(aes(y = pred), data = grid, size = 1, color  = "#756bb1") +
  labs(x = "Acousticness",
       y = "Valence",
       title = "Relation between acousticness and valence") +
  theme_minimal()

## most used key   
benjor %>%
  count(key_name, sort = TRUE) %>%
  ggplot(aes(reorder(key_name, n), n)) +
  geom_bar(stat = "identity", fill  = "#a6bddb") +
  geom_text(aes(label = n)) +
  scale_y_continuous(labels = NULL) +
  labs(x = "Key name",
       y = "Count",
       title = "Ratio of used keys") +
  theme_minimal()

## valence  
benjor %>%
  ggplot(aes(valence)) +
  geom_histogram(binwidth = .03, fill = "#dd1c77") +
  labs(x = "Valence",
       y = "Count",
       title = "Ratio of valence") +
  theme_minimal()
  

## tempo
benjor %>%
  ggplot(aes(tempo)) +
  geom_histogram(binwidth = 4, fill = "#c994c7") +
  labs(x = "Tempo",
       y = "Count",
       title = "Ratio of time signature") +
  theme_minimal()

## energy
benjor %>%
  ggplot(aes(energy)) +
  geom_histogram(binwidth = .07, fill = "#e7e1ef") +
  labs(x = "Energy",
       y = "Count",
       title = "Ratio of  musical energy") +
  theme_minimal()

## valence and time_signature
benjor %>%
  ggplot(aes(valence)) +
  geom_freqpoly(aes(color = time_signature)) +
  viridis::scale_color_viridis(discrete = TRUE) +
  scale_color_discrete(name = "Time signature") +
  labs(x = "Valence",
       y = "Count",
       title = "Frequency of valence by time signature") +
  theme_minimal()






