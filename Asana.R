# install packages
#install.packages("remotes")
#remotes::install_github("UrbanInstitute/urbnthemes")

# load packages
library(tidyverse)
library(dplyr)
library(tidyr)
library(readxl)
library(magrittr)
library(urbnthemes)
library(ggplot2)
library(openxlsx)
library(lubridate)
library(janitor)
library(naniar)
library(scales)
library(ggrepel) 
library(stringr)
library(utils)
library(tidylog)
library(rrapply)
library(mdthemes)
library(skimr)
library(gghighlight)
library(kableExtra)
library(ggimage)
library(formattable)
library(ggtext)

# set options
options(scipen = 100)
set_urbn_defaults(style = "print")

# load asana data and convert to story points 
asana_current <- read.csv("All_Projects.csv") %>%
  janitor::clean_names() %>%
  # create points in separate column
  mutate(points = gsub("\\([^)]+\\)", "", effort_level),
         points = as.numeric(points),
         points = replace_na(points, 0.5)) %>%
  # 1 hour = 0.5 points, so make the adjustment by dividing by 2 
  mutate(points = points / 2) %>%
  # remove rows with an empty section column 
  filter(section_column != "")




