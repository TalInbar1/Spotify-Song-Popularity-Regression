library(readxl)
dataset <- read_excel(file.choose())
head(dataset)
names(dataset)

##3##

plot(x=dataset$energy ,y=dataset$danceability ,xlab="Energy (%)",ylab="Danceability (%)")
#Correlation Coefficient Calculation
cor(dataset$energy, dataset$danceability, method = "pearson", use = "complete.obs")


plot(x=dataset$acousticness  ,y=dataset$energy ,xlab="Acousticness (%)",ylab="Energy (%)")
#Correlation Coefficient Calculation
cor(dataset$acousticness, dataset$energy, method = "pearson", use = "complete.obs")


plot(x=dataset$tempo  ,y=dataset$danceability ,xlab="Tempo (BPM)",ylab="Danceability (%)")
#Correlation Coefficient Calculation
cor(dataset$tempo, dataset$danceability, method = "pearson", use = "complete.obs")


plot(x=dataset$energy ,y=dataset$audio_valence ,xlab="Energy (%)",ylab="Audio Valence (%)")
#Correlation Coefficient Calculation
cor(dataset$energy, dataset$audio_valence, method = "pearson", use = "complete.obs")


plot(x=dataset$instrumentalness ,y=dataset$speechiness ,xlab="Instrumentalness (%)",ylab="Speechiness (%)")
#Correlation Coefficient Calculation
cor(dataset$instrumentalness, dataset$speechiness, method = "pearson", use = "complete.obs")

##4##

mean(dataset$song_duration_ms)
median(dataset$song_duration_ms)
sd(dataset$song_duration_ms)
IQR(dataset$song_duration_ms)
install.packages("e1071")
library(e1071)
skewness(dataset$song_duration_ms)

mean(dataset$acousticness)
median(dataset$acousticness)
sd(dataset$acousticness)
IQR(dataset$acousticness)
skewness(dataset$acousticness)

mean(dataset$danceability)
median(dataset$danceability)
sd(dataset$danceability)
IQR(dataset$danceability)
skewness(dataset$danceability)

mean(dataset$energy)
median(dataset$energy)
sd(dataset$energy)
IQR(dataset$energy)
skewness(dataset$energy)

mean(dataset$instrumentalness)
median(dataset$instrumentalness)
sd(dataset$instrumentalness)
IQR(dataset$instrumentalness)
skewness(dataset$instrumentalness)

mean(dataset$liveness)
median(dataset$liveness)
sd(dataset$liveness)
IQR(dataset$liveness)
skewness(dataset$liveness)

mean(dataset$speechiness)
median(dataset$speechiness)
sd(dataset$speechiness)
IQR(dataset$speechiness)
skewness(dataset$speechiness)

mean(dataset$tempo)
median(dataset$tempo)
sd(dataset$tempo)
IQR(dataset$tempo)
skewness(dataset$tempo)

mean(dataset$audio_valence)
median(dataset$audio_valence)
sd(dataset$audio_valence)
IQR(dataset$audio_valence)
skewness(dataset$audio_valence)

mean(dataset$song_popularity)
median(dataset$song_popularity)
sd(dataset$song_popularity)
IQR(dataset$song_popularity)
skewness(dataset$song_popularity)



###Categorical variable
audio_mode_1 <- dataset[dataset$audio_mode == 1, ]
audio_mode_0 <- dataset[dataset$audio_mode == 0, ]

mean(audio_mode_1$song_popularity)
median(audio_mode_1$song_popularity)
sd(audio_mode_1$song_popularity)
IQR(audio_mode_1$song_popularity)
skewness(audio_mode_1$song_popularity)

mean(audio_mode_0$song_popularity)
median(audio_mode_0$song_popularity)
sd(audio_mode_0$song_popularity)
IQR(audio_mode_0$song_popularity)
skewness(audio_mode_0$song_popularity)




##5##
bp_song_duration <- boxplot(dataset$song_duration_ms, main="Song Duration (ms)")
is_outlier <- dataset$song_duration_ms %in% bp$out
outliers_info <- dataset[is_outlier, c("song_name", "song_duration_ms")]
print(outliers_info)
#Remove outliers above 500000
threshold_ms <- 500000
outliers_to_remove_count <- sum(dataset$song_duration_ms > threshold_ms)
print(paste("Number of songs over 500,000 ms to be removed:", outliers_to_remove_count))
newDataset <- dataset[dataset$song_duration_ms <= threshold_ms, ] #Create new dataset, excluding outliers.


bp_acousticness <- boxplot(dataset$acousticness, main="Acousticness (%)")
is_outlier_acoustic <- dataset$acousticness %in% bp_acoustic$out
outliers_acoustic_info <- dataset[is_outlier_acoustic, c("song_name", "acousticness")]
print(outliers_acoustic_info)

bp_danceability <- boxplot(dataset$danceability, main="Danceability (%)")
is_outlier_dance <- dataset$danceability %in% bp_dance$out
outliers_dance_info <- dataset[is_outlier_dance, c("song_name", "danceability")]
print(outliers_dance_info)

bp_energy <- boxplot(dataset$energy, main="Energy (%)")
is_outlier_energy <- dataset$energy %in% bp_energy$out
outliers_energy_info <- dataset[is_outlier_energy, c("song_name", "energy")]
print(outliers_energy_info)

bp_instrumentalness <- boxplot(dataset$instrumentalness, main="Instrumentalness (%)")
is_outlier_instrumentalness <- dataset$instrumentalness %in% bp_instrumentalness$out
outliers_instrumentalness_info <- dataset[is_outlier_instrumentalness, c("song_name", "instrumentalness")]
print(outliers_instrumentalness_info)

bp_liveness <- boxplot(dataset$liveness, main="Liveness (%)")
is_outlier_liveness <- newDataset$liveness %in% bp_liveness$out
outliers_liveness_info <- newDataset[is_outlier_liveness, c("song_name", "liveness")]
print(outliers_liveness_info)

bp_speechiness <- boxplot(dataset$speechiness, main="Speechiness (%)")
is_outlier_speechiness <- newDataset$speechiness %in% bp_speechiness$out
outliers_speechiness_info <- newDataset[is_outlier_speechiness, c("song_name", "speechiness")]
print(outliers_speechiness_info)

bp_tempo <- boxplot(dataset$tempo, main="Tempo (BPM)")
is_outlier_tempo <- newDataset$tempo %in% bp_tempo$out
outliers_tempo_info <- newDataset[is_outlier_tempo, c("song_name", "tempo")]
print(outliers_tempo_info)
bp_tempo$out
# We will remove the outlier of a song with a tempo of 0
zero_tempo_count <- sum(newDataset$tempo == 0)
print(paste("Number of songs with 0 BPM to be removed:", zero_tempo_count))
old_size <- nrow(newDataset)
newDataset <- newDataset[newDataset$tempo > 0, ]#Update newDataset after outlier removal.


bp_audio_valence <- boxplot(dataset$audio_valence, main="Audio Valence (%)")
is_outlier_audio_valence <- newDataset$audio_valence %in% bp_audio_valence$out
outliers_audio_valence_info <- newDataset[is_outlier_audio_valence, c("song_name", "audio_valence")]
print(outliers_audio_valence_info)

bp_song_popularity <- boxplot(dataset$song_popularity, main="Song Popularity (1-100)")
is_outlier_popularity <- newDataset$song_popularity %in% bp_song_popularity$out
outliers_popularity_info <- newDataset[is_outlier_popularity, c("song_name", "song_popularity")]
print(outliers_popularity_info)



##6##
##danceability

hist(
  newDataset$danceability,
  probability = TRUE,     col = "grey",  main = "PDF of Danceability",  xlab = "Danceability"
)
lines(density(newDataset$danceability), col = "red", lwd = 2)


ecdf_danceability <- ecdf(newDataset$danceability)
plot(
  ecdf_danceability,
  main = "CDF of Danceability",
  xlab = "Danceability",
  ylab = "Cumulative Probability",
  col = "red",
  lwd = 2
)


##energy
hist(
  newDataset$energy,
  probability = TRUE,     col = "grey",  main = "PDF of Energy",  xlab = "Energy"
)
lines(density(newDataset$energy), col = "red", lwd = 2)


ecdf_energy <- ecdf(newDataset$energy)
plot(
  ecdf_energy,
  main = "CDF of Energy",
  xlab = "Energy",
  ylab = "Cumulative Probability",
  col = "red",
  lwd = 2
)


##speechiness
hist(
  newDataset$speechiness,
  probability = TRUE,     col = "grey",  main = "PDF of Speechiness",  xlab = "Speechiness"
)
lines(density(newDataset$speechiness), col = "red", lwd = 2)


ecdf_speechiness <- ecdf(newDataset$speechiness)
plot(
  ecdf_speechiness,
  main = "CDF of Speechiness",
  xlab = "Speechiness",
  ylab = "Cumulative Probability",
  col = "red",
  lwd = 2
)



##7##

colors_vector <- ifelse(
  newDataset$audio_mode == 1,
  rgb(0, 0, 1, alpha = 0.2),  #Blue for major (1)
  rgb(1, 0, 0, alpha = 0.2)   #Red for minor (0)
)

plot(
  x = newDataset$energy,
  y = newDataset$audio_valence,
  
  main = "Valence vs. Energy (by Audio Mode)",
  xlab = "Energy (%)",
  ylab = "Audio Valence (%)",
  
  col = colors_vector,  
  pch = 19              
)

legend(
  "topleft",
  legend = c("Minor (0)", "Major (1)"),
  col = c(rgb(1, 0, 0), rgb(0, 0, 1)), 
  pch = 10
)


##8##

##Violin Plot##
install.packages("ggplot2")
install.packages("scales")

library(ggplot2)
library(scales)

ggplot(newDataset, aes(x = factor(time_signature), y = song_popularity, fill = factor(time_signature))) +
  geom_violin(alpha = 0.7, scale = "area", trim = TRUE) +
  geom_boxplot(width=0.1, outlier.shape = NA, fill="white") +
  
  scale_y_continuous(limits = c(0, 100)) + 
  
  labs(
    title = "Distribution of Song Popularity by Time Signature",
    x = "Time Signature",
    y = "Song Popularity (0-100)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_line(color = "gray", size = 0.5)
  ) +
  scale_fill_brewer(palette = "Set3")




##Hexbin Plot##

install.packages("hexbin")

library(ggplot2)

ggplot(newDataset, aes(x = tempo, y = energy)) +
  
  geom_hex(bins = 40) +
  
  scale_fill_gradient(low = "blue", high = "red") +
  
  labs(
    title = "Hexbin Plot: Concentration of Energy vs. Tempo",
    x = "Tempo (BPM)",
    y = "Energy",
    fill = "Song Count" 
  ) +
  theme_minimal()


install.packages("hexbin")

library(ggplot2)

ggplot(newDataset, aes(x = tempo, y = energy)) +
  
  geom_hex(bins = 40) +
  
  scale_fill_gradient(low = "green", high = "red") +
  
  labs(
    title = "Hexbin Plot: Concentration of Energy vs. Tempo",
    x = "Tempo (BPM)",
    y = "Energy",
    fill = "Song Count" 
  ) +
  theme_minimal()



##9##


## table 1:
categorical_var <- newDataset$audio_mode

absolute_frequency <- table(categorical_var)

relative_frequency <- prop.table(absolute_frequency) * 100

frequency_table <- data.frame(
  Category_Audio_Mode = names(absolute_frequency),
  Absolute_Frequency = as.vector(absolute_frequency),
  Relative_Frequency_Percent = round(as.vector(relative_frequency), 2)
)

frequency_table$Category_Audio_Mode <- ifelse(
  frequency_table$Category_Audio_Mode == "0", 
  "Minor (0)", 
  "Major (1)"
)
print(frequency_table)


## table 2:
categorical_var <- newDataset$time_signature

absolute_frequency <- table(categorical_var)

relative_frequency <- prop.table(absolute_frequency) * 100

frequency_table_time_sig <- data.frame(
  Category_Time_Signature = names(absolute_frequency),
  Absolute_Frequency = as.vector(absolute_frequency),
  Relative_Frequency_Percent = round(as.vector(relative_frequency), 2)
)
print(frequency_table_time_sig)



## table 3: 
cross_tab <- table(newDataset$time_signature, newDataset$audio_mode)

cross_tab_df <- as.data.frame.matrix(cross_tab)

names(cross_tab_df) <- c("Count_Minor_0", "Count_Major_1")

cross_tab_df$Total <- cross_tab_df$Count_Minor_0 + cross_tab_df$Count_Major_1

cross_tab_df$Percent_Minor <- (cross_tab_df$Count_Minor_0 / cross_tab_df$Total) * 100

cross_tab_df$Percent_Major <- (cross_tab_df$Count_Major_1 / cross_tab_df$Total) * 100

cross_tab_df$Percent_Minor <- round(cross_tab_df$Percent_Minor, 2)
cross_tab_df$Percent_Major <- round(cross_tab_df$Percent_Major, 2)

cross_tab_df <- cross_tab_df[, c("Count_Minor_0", "Count_Major_1", "Percent_Minor", "Percent_Major", "Total")]

print(cross_tab_df)

