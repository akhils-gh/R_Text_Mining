library(tidyverse)
library(tidytext)

data("sentiments")
sentiments

get_sentiments("afinn")

get_sentiments("")