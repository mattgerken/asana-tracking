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
asana <- read.csv("All_Projects.csv") %>%
  janitor::clean_names() %>%
  # create points in separate column
  mutate(points = gsub("\\([^)]+\\)", "", effort_level),
         points = as.numeric(points),
         points = replace_na(points, 0.5)) %>%
  # 1 hour = 0.5 points, so make the adjustment by dividing by 2 
  mutate(points = points / 2) %>%
  # remove rows with an empty section column 
  filter(section_column != "")

# load meeting time
meeting_time <- read.xlsx("meeting tracking.xlsx",
                          sheet = "Sheet1") %>%
  janitor::clean_names() %>%
  rename(assignee = assignee_nickname) %>%
  # 1 hour = 0.5 points, so make the adjustment by dividing by 2
  mutate(points = hours/2) %>%
  select(-hours)

#----- Current week (March 11-15) completed tasks

# this week's meetings
this_week_meeting <- meeting_time %>%
  # filter for the current week
  filter(section_column == "Mar 11-15") %>%
  select(-section_column)

# this week's completed tasks
this_week <- asana %>%
  # filter Asana for this week's tasks
  filter(section_column == "Mar 11-15") %>%
  # filter for completed tasks (tasks with a non-missing completed_at date)
  filter(!is.na(completed_at) & completed_at != "") %>%
  # get total story points by team member by workstream
  group_by(assignee, workstream) %>%
  summarize(points = sum(points, na.rm = TRUE)) %>%
  ungroup() %>%
  rbind(this_week_meeting)

# create a figure showing LOE by team member by workstream
this_week %>% 
  mutate(workstream = factor(workstream, levels = c("Workstream 5", "Workstream 4", "Workstream 3",
                                                    "Workstream 2", "Workstream 1", "Meetings")),
         assignee = factor(assignee, levels = c("Team Member 5", "Team Member 4", "Team Member 3",
                                                "Team Member 2", "Team Member 1"))) %>%
  ggplot() +
  geom_col(mapping = aes(x = assignee, y = points, fill = workstream)) +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1)),
                     limits = c(0, 25),
                     breaks = 0:5 * 5) +
  labs(x = NULL,
       y = NULL) +  
  coord_flip() + 
  guides(color = FALSE,
         fill = guide_legend(reverse = TRUE)) + 
  scale_fill_manual(values = c("#0a4c6a" ,"#d2d2d2", "#fdbf11", "#ec008b",
                               "#55b748", "#1696d2")) + 
  theme(legend.box = "vertical", legend.position = "right",
        legend.direction = "vertical",
        plot.caption = element_markdown(hjust = 0, size = 8),
        plot.title = element_markdown(size = 12),
        legend.text = element_markdown(size = 8)) + 
  geom_hline(yintercept=as.numeric(c(20)),
             linetype=2, colour="black") +
  geom_text(aes(y = 23.5, x = 5.2, label = "20 points = 40 hours"), size = 3, color = "black", family = "Lato") +
  labs(caption = paste0("**Note:** 1 hour = 0.5 points. Tasks with a missing level of effort were assigned 0.25 points.")) +
  ggtitle("Tasks Completed by Team Member and Workstream (Week of March 11-15)") 

ggsave("completed_this_week.png", bg="white", height = 6, width = 8)

#----- Current week (March 11-15) total allocation

# pull in this week's meeting time
summarized_this_week_meeting <- this_week_meeting %>%
  select(-workstream)

# allocation by team member as points (all assigned tasks - those completed and not completed)
this_week_all_tasks <- asana %>%
  filter(section_column == "Mar 11-15") %>%
  group_by(assignee) %>%
  summarize(points = sum(points)) %>%
  ungroup() %>%
  rbind(summarized_this_week_meeting) %>%
  group_by(assignee) %>%
  summarize(points = sum(points, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(percent = points / 20,
         image_path = case_when(assignee == "Team Member 1" ~ "pics/alien.png",
                                assignee == "Team Member 2" ~ "pics/ghost.png",
                                assignee == "Team Member 3" ~ "pics/pumpkin.png",
                                assignee == "Team Member 4" ~ "pics/star.png",
                                assignee == "Team Member 5" ~ "pics/tree.png"))

# create allocation figure
this_week_all_tasks %>%
  arrange(desc(points)) %>%
  filter(!is.na(assignee)) %>%
  mutate(assignee = factor(assignee, levels = .$assignee),
         percent = scales::percent(round(percent, 2))) %>%
  ggplot(aes(points, assignee)) +
  geom_rect(aes(xmin = 15, xmax = 20, ymin = -Inf, ymax = Inf),
            fill = "lightgreen", alpha = 0.1, linetype = 0) + 
  geom_rect(aes(xmin = 20, xmax = 40, ymin = -Inf, ymax = Inf),
            fill = "lightcoral", alpha = 0.1, linetype = 0) + 
  geom_rect(aes(xmin = 0, xmax = 15, ymin = -Inf, ymax = Inf),
            fill = "#FFDAB9", alpha = 0.1, linetype = 0) + 
  geom_image(aes(image = image_path), size = 0.08) +
  geom_text(aes(label = percent), vjust = 2.75, hjust = 0.5, size = 3) +
  geom_text(aes(x = 7.5, y = 5.3, label = "Below Target\n(0-75%)"), size = 3, color = "black", family = "Lato") +
  geom_text(aes(x = 17.5, y = 5.3, label = "On Target\n(75-100%)"), size = 3, color = "black", family = "Lato") +
  geom_text(aes(x =30, y = 5.3, label = "Above Target\n(>100%)"), size = 3, color = "black", family = "Lato") +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), limits = c(0, 40), breaks = 0:8*5) +
  geom_vline(xintercept=as.numeric(c(15, 20)),
             linetype=2, colour="black") + 
  labs(x = "Number of points", 
       y = NULL,
       caption = paste0("**Note:** 1 hour = 0.5 points. Tasks with a missing level of effort were assigned 0.25 points."),
       subtitle = "Includes all tasks assigned (completed and uncompleted) as well as meeting time.") +
  theme(plot.caption = element_markdown(hjust = 0, size = 8),
        plot.title = element_markdown(size = 12),
        plot.subtitle = element_markdown(size = 9)) + 
  ggtitle("Team Allocation for Week of March 11-15")

ggsave("allocated_this_week.png", bg="white", height = 6, width = 8)

#----- Current week (March 11-15) uncompleted tasks

# all uncompleted tasks
this_week_uncompleted <- asana %>%
  filter(section_column == "Mar 11-15") %>%
  # uncompleted tasks = missing completed_at date
  filter(completed_at == "") %>%
  select(assignee, workstream, effort_level, name, due_date) %>%
  arrange(assignee, workstream, due_date) %>%
  rename("Name" = 1,
         "Workstream" = 2,
         "Effort Level" = 3,
         "Task" = 4,
         "Due Date" = 5)

# turn into table
knitr::kable(this_week_uncompleted, align = "c", booktabs = TRUE,
             caption = '<b>Tasks Yet to Be Completed for Week of March 11 - 15</b>', format = 'html') %>% 
  kable_styling(font_size = 11, html_font = "Lato") %>%
  save_kable(file = "uncompleted_this_week.png",
             zoom = 1.5)

