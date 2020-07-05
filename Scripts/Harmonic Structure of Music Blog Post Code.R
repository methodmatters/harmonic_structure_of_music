
library(plyr); library(dplyr)
library(ggplot2)
library(tidyverse)
library(gplots) 
library(RColorBrewer)
library(kableExtra)

# set the directory for the data file
in_dir <- 'D:\\Directory\\'

# read in the raw data 
raw_data <- read.csv(paste0(in_dir, 'song_level_data_genre.csv'), stringsAsFactors = FALSE)

# missing values - none!
sapply(raw_data, function(x) sum(is.na(x)))

### Counts of Songs per Genre
### Counts of Songs per Genre
### Counts of Songs per Genre
### Counts of Songs per Genre
### Counts of Songs per Genre

raw_data %>% 
  group_by(genre_clean) %>%
  summarise(num_songs=n()) %>%
  ggplot(aes(x = reorder(genre_clean, num_songs), 
             y = num_songs, fill = genre_clean)) +
  geom_bar(stat = 'identity') + 
  geom_text(aes(label = num_songs), 
            size = 4, hjust = -0.15) + 
  coord_flip(ylim = c(0,3500)) + 
  labs(x = "Genre", y = "Number of Songs", 
       title = 'Number of Songs Per Genre' ) +
  theme(legend.position = "none")


### Mode Analysis
### Mode Analysis
### Mode Analysis
### Mode Analysis
### Mode Analysis

# barplot of mode across songs
# all songs included
raw_data %>% 
  select(genre_clean, mode_clean) %>% 
  group_by(mode_clean) %>%
  # counts of songs per mode
  summarise(Percentage=n())  %>%
  # calculate the % of songs per mode
  mutate(Percentage=Percentage/sum(Percentage)*100,
         mode_clean = recode(mode_clean, 'maj' = 'Major',
                             'min' = 'Minor'))  %>%  
  # pass to ggplot
  ggplot(aes(x = reorder(mode_clean, Percentage) , y = Percentage, fill = mode_clean)) +
  geom_bar(stat = 'identity') + 
  # specify the colors
  scale_fill_manual(name = "Mode", values = c('maroon', 'darkgrey')) + 
  # add the value labels above the bars
  geom_text(aes(label = paste(round(Percentage, 0), "%", sep = '')), hjust = -0.1) +
  # flip the axes
  coord_flip(ylim = c(0,71)) + 
  # add the titles
  labs(x = "Mode", y = "Percentage", 
       title = 'Song Modes Across Music Collection (8503 Songs)' ) 


# barplot of modes by genre
raw_data %>% 
  group_by(genre_clean) %>% 
  # count the number of songs per genre
  # and include that in our genre text
  mutate(num_per_genre = n(),
         master_genre = paste(genre_clean, " (N = ", num_per_genre, ")", sep = '')) %>% 
  # select genres with 200+ songs and remove rap songs
  filter(num_per_genre > 200 & genre_clean != "Rap") %>% 
  select(master_genre, mode_clean) %>%  
  # group by genre and mode
  group_by(master_genre, mode_clean) %>%
  # calculate the number per mode per genre
  summarise(Percentage=n()) %>%  
  # group by genre
  group_by(master_genre) %>% 
  # and calculate the % per mode per genre
  # order the factor for the plot
  # (ordered by % major mode)
  mutate(Percentage=Percentage/sum(Percentage)*100,
         genre_clean_factor = factor(master_genre, 
                                     levels = c("Country (N = 551)", 
                                                "Pop (N = 607)", 
                                                "Rock (N = 3426)",
                                                "World (N = 319)", 
                                                "Jazz (N = 1141)",
                                                "Soul / R&B (N = 205)")),
         mode_clean = recode(mode_clean, 'maj' = 'Major',
                             'min' = 'Minor')) %>%    
  # pass to ggplot
  ggplot(aes(x = mode_clean, y = Percentage, fill = mode_clean)) +
  # we want a bar plot
  geom_bar(stat = 'identity')  +
  # add the value labels to the bars
  geom_text(aes(label = paste(round(Percentage, 0), "%", sep = '')), 
            hjust = .5, vjust =-.3, size = 3)  + 
  # add the labels
  labs(x = "Mode", y = "Percentage", 
       title = 'Song Mode Distributions By Music Genre' ) +
  # facet per genre
  facet_grid(. ~ genre_clean_factor) + 
  # specify the colors
  scale_fill_manual(name = "Mode", values = c('maroon', 'darkgrey')) +
  theme(strip.text.x = element_text(size = 8))

### Song Key Analysis
### Song Key Analysis
### Song Key Analysis
### Song Key Analysis
### Song Key Analysis
### Song Key Analysis

# percentage of keys across all songs
raw_data %>% 
  select(genre_clean, master_key, mode_clean) %>% 
  group_by(master_key) %>%
  # calculate the number of songs for each key
  # hang on to the mode info - we'll use that
  # in our plot
  summarise(Percentage=n(), 
            mode_clean = unique(mode_clean))  %>%  
  # calculate the percentage of songs per key
  # recode the mode variable to make it clean
  # for the plot
  mutate(Percentage=Percentage/sum(Percentage)*100,
         mode_clean = recode(mode_clean, 'maj' = 'Major',
                             'min' = 'Minor'))  %>% 
  # pass the data on to ggplot
  ggplot(aes(x = reorder(master_key, Percentage) , y = Percentage, fill = mode_clean)) +
  # we want a bar plot
  geom_bar(stat = 'identity') + 
  # specify the colors
  scale_fill_manual(name = "Mode", values = c('maroon', 'darkgrey')) + 
  # add the value labels 
  geom_text(aes(label = paste(round(Percentage, 1), "%", sep = '')), hjust = -0.1, size = 3.5) + 
  # flip the chart
  coord_flip(ylim = c(0, 11.5)) + 
  # add the labels
  labs(x = "Key", y = "Percentage", 
       title = 'Song Keys Across Music Collection (8503 Songs)' ) 

# percentage of keys, separate per genre
raw_data %>% 
  group_by(genre_clean) %>% 
  mutate(num_per_genre = n(),
         master_genre = paste(genre_clean, " \n(N = ", num_per_genre, ")", sep = '')) %>%  
  filter(num_per_genre > 200 & genre_clean != "Rap") %>% 
  select(master_genre, master_key) %>% 
  group_by(master_genre, master_key) %>%
  summarise(Percentage=n()) %>%
  group_by(master_genre) %>% 
  mutate(Percentage=Percentage/sum(Percentage)*100) %>% 
  ggplot(aes(x = master_key, y = Percentage, fill = master_key)) +
  geom_bar(stat = 'identity') +
  # add the value labels above the bars
  geom_text(aes(label = paste(round(Percentage, 0), "%", sep = '')), 
            hjust = .5, vjust =-.3, size = 2.5) + 
  # rotate the x axis labels 90 degrees so they're horizontal
  # and hide the legend
  theme(axis.text.x = element_text(angle = 90, vjust = .3, hjust=1),
        legend.position = "none" , strip.text.x = element_text(size = 100)) +
  labs(x = "Key", y = "Percentage", 
       title = 'Song Key Distributions By Music Genre' ) +
  coord_cartesian(ylim = c(0,16)) + 
  facet_grid(master_genre ~ .) +
  theme(strip.text.y = element_text(size = 9, angle = 0))


### Cluster Genres by Song Key 
### Cluster Genres by Song Key
### Cluster Genres by Song Key
### Cluster Genres by Song Key
### Cluster Genres by Song Key


# make the cluster data
# use tidyverse here - column to rownames
cluster_data <- raw_data %>% 
  group_by(genre_clean) %>% 
  mutate(num_per_genre = n()) %>% 
  filter(num_per_genre > 200 & genre_clean != "Rap") %>% 
  select(genre_clean, master_key) %>% 
  group_by(genre_clean, master_key) %>%
  summarise(Percentage=n()) %>%
  group_by(genre_clean) %>% 
  mutate(Percentage=Percentage/sum(Percentage)*100) %>%
  spread(master_key, Percentage) %>%
  replace(is.na(.), 0) %>%
  column_to_rownames(var = "genre_clean")

# what does it look like?
cluster_data

# scale the data
cluster_data_scaled <- scale(cluster_data)

# what does it look like?
round(cluster_data_scaled,2)


# heatmap with heatmap.2 library
# red-blue color palette
# red is high, blue is low
hmcol = rev(colorRampPalette(brewer.pal(9, "RdBu"))(10))
heatmap.2(cluster_data_scaled, 
          # we've already scaled the data above
          # so we turn of scaling here
          scale = c("none"), 
          # show histogram on color key
          density.info=c("histogram"),
          # turn off tracing in the plot
          trace=c("none"), 
          # specify our color palette
          # (defined above)
          col = hmcol,
          # set the font size for
          # row labels
          cexRow=1.3,
          # set the margins so we see
          # all axis labels
          margin=c(5, 7), 
          # set the plot title
          main = 'Clustering Genres by Song Key')

