library(tidyverse)
library(ggplot2)

nationalities <- ggplot(data=players_df) + 
  geom_bar(mapping = aes(x=nationality))

nationalities