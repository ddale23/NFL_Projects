# load in libraries
library(tidyverse)
library(gridExtra)

#base data set (what we created in Python)
base_data = read.csv("C:/Users/drez_/Desktop/merged_csv.csv")


#adding in colors for each color to the data set
data_for_graphs =
base_data %>%
  mutate(colors = case_when(
    cluster == 0 ~ '#730791',
    cluster == 1 ~ '#910707',
    cluster == 2 ~ '#ed8115',
    cluster == 3 ~ '#076b18',
    cluster == 4 ~ '#0a53c9',
    cluster == 5 ~ '#000000'
  )) 

#grouping by each cluster and summarizing the data
data_for_graphs =
data_for_graphs %>%
  group_by(cluster, colors) %>%
  summarize(inches = mean(inch_height),
            weight = mean(weight),
            tar = mean(tar_gm),
            rec = mean(rec_gm),
            car_gm = mean(car_gm),
            ay = mean(ay_per_targ),
            count = n_distinct(full_name)) 

#changing cluster to factor and color to character
data_for_graphs =
data_for_graphs %>%
  mutate(
    cluster = as.factor(cluster),
    colors = as.character(colors))


#creating a new reference for the colors (this will come in handy when making graphs)
col = as.character(data_for_graphs$colors)

## making graphs for each data point

#height(inches)
height = data_for_graphs %>%
  ggplot(aes(inches, cluster)) + geom_col(aes(fill = cluster)) +
  scale_fill_manual(values = col) +
  theme_bw() +
  labs(x = '',
       y = 'Cluster',
       title = 'Height') +
  theme(legend.position = "none",
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12))
  
#weight(pounds)
weight = data_for_graphs %>%
  ggplot(aes(weight, cluster)) + geom_col(aes(fill = cluster)) +
  scale_fill_manual(values = col) +
  theme_bw() +
  labs(x = '',
       y = 'Cluster',
       title = 'Weight') +
  theme(legend.position = "none",
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12))

#targets per game
targets = data_for_graphs %>%
  ggplot(aes(tar, cluster)) + geom_col(aes(fill = cluster)) +
  scale_fill_manual(values = col) +
  theme_bw() +
  labs(x = '',
       y = '',
       title = 'Targets per Game') +
  theme(legend.position = "none",
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.text = element_text(size = 12)) 

#receptions per game
receptions = data_for_graphs %>%
  ggplot(aes(rec, cluster)) + geom_col(aes(fill = cluster)) +
  scale_fill_manual(values = col) +
  theme_bw() +
  labs(x = '',
       y = '',
       title = 'Receptions per Game') +
  theme(legend.position = "none",
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.text = element_text(size = 12))

#carries per game
carries = data_for_graphs %>%
  ggplot(aes(car_gm, cluster)) + geom_col(aes(fill = cluster)) +
  scale_fill_manual(values = col) +
  theme_bw() +
  labs(x = '',
       y = '',
       title = 'Carries per Game') +
  theme(legend.position = "none",
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.text = element_text(size = 12)) 

#air yards
air_yards = data_for_graphs %>%
  ggplot(aes(ay, cluster)) + geom_col(aes(fill = cluster)) +
  scale_fill_manual(values = col) +
  theme_bw() +
  labs(x = '',
       y = '',
       title = 'Air Yards per Target') +
  theme(legend.position = "none",
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.text = element_text(size = 12))
        

#arranging graphs onto one grid --> this is final product
grid.arrange(height, air_yards, receptions,
             weight, carries, targets, nrow = 2)















