<h1 align="center"> Project Management Reporting in R Following Agile Methodology </h1>

Are you a data analyst seeking to support your team's project management tracking? The following guide provides a walkthrough for taking tasks from a project management platform (in this example, Asana) and creating a report for your team using R.

# Agile Methodology

[Agile methodology](https://asana.com/resources/agile-methodology) is a project management framework that is iterative and that breaks projects down into "sprints," or phases. Common Agile ceremonies include:

- **Daily stand-up** for each team member to share progress on tasks, discuss their plans for the day, and any task blockers they are encountering
- **Sprint planning** to meet as a team to discuss tasks and priorities for the next sprint
- **Weekly retros** to reflect on the previous sprint (which can be scheduled at the end of the work week on a Friday)

A project management platform such as Asana pairs well with Agile methodology. In Asana, you can add tasks and assign them to team members, add the task's level of effort as a [custom field](https://help.asana.com/s/article/organize-projects-with-custom-fields) (e.g., 30 minutes to complete, 1 hour, 2 hours, etc.), and the project ("workstream") the task is associated with as a custom field. 

For a team engaged in multiple projects, there are many ways to set up an Asana board. One way is to add each work week as a column and organize tasks underneath those columns. 

<p align="center">
  <img src="https://github.com/mattgerken/asana-tracking/blob/main/pics/asana.PNG?raw=true" width="80%">
</p>

Level of effort can be assigned using units of time (e.g., 30 minutes, 1 hour, 2 hours, 3 hours, etc.), which I do in this example, or using a different methodology, such as [story points](https://www.simplilearn.com/story-points-in-agile-article).

# Reporting in R

Below, I discuss weekly reporting in R using (hypothetical) task-level data exported from Asana to inform team project management. Such a report could time well with the weekly retro, providing the team an opportunity to reflect on, for example:

1. Tasks Completed
2. Team Allocation
3. Uncompleted Tasks
4. Workstreams Over Time

## Export Data and Load

You can export data from your Asana project as a CSV file following the clicks below. Any custom tags you create will also show up in this data extract (two examples below). Fields in my extract include:

- Unique task ID
- Date task was created
- Task completion date (empty cell = open task)
- Task description
- Section/column task was organized underneath
- Task assignee
- Due date
- (Custom field) Effort level
- (Custom field) Workstream

<p align="center">
  <img src="https://github.com/mattgerken/asana-tracking/blob/main/pics/export.PNG?raw=true" width="40%">
</p>

In creating your team's Agile norms, decide how to treat meeting time. Meeting time could be added as "tasks" in Asana (a potentially laborious undertaking for everyone across the team). Alternatively, each team member's time spent in meetings can be tracked in a separate Excel spreadsheet (which I have done for this example) using Outlook calendars ("meetings" included work meetings, sick leave, vacation time, and other appointments) or other more direct methods.

Reporting starts by loading both the Asana data extract and meetings spreadsheet into R. For the sake of this walkthrough, I created fake data for a hypothetical team with the following characteristics:

- 5 team members
- 5 workstreams with meeting time tracked separately

The custom **"Effort level"** field tracked level of effort (in hours) for each task. I converted these task-hours into points using the conversion **1 hour = 0.5 points**.

```{r}
# load libraries
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
```

## Tasks Completed

The end of the work week provides an opportunity to reflect on tasks completed and total meeting time over the past work week, either formally through a practice like a **Team Retro** or informally through a report.  Level of effort can be visualized across the team by workstream through simple stacked bar charts. Since 1 hour = 0.5 points, 20 points is the equivalent of a 40-hr workweek. 

Using the fake data I created for the sake of this walkthrough, the visual reveals that during the week of March 11th - 15th:

- Team members contributed the most to workstreams 1 and 4
- Team members 1 and 2 spent the most time in meetings

```{r2}
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
```

<p align="center">
  <img src="https://github.com/mattgerken/asana-tracking/blob/main/completed_this_week.png?raw=true" width="80%">
</p>

## Team Allocation

You can also consider total team allocation for the week; that is, all tasks assigned (both completed tasks and tasks that had not been completed as of the end of the week) plus meeting time. Tallying total points (using 1 hour = 0.5 points) for each team member and then dividing by 20 (20 points = 40 hours a week) creates each team member's allocation percentage for the week. Tips for visualizing:

- Make it fun! Use the **ggimage** package to assign team members avatars or profile pictures.
- Use **geom_rect** to shade your visual into sections. I created a "Below Target" section (0%-75%), an "On Target" section (75%-100%), and an "Above Target" section (>100%). These cutoffs can be determined with your team.

```{r3}
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
```
<p align="center">
  <img src="https://github.com/mattgerken/asana-tracking/blob/main/allocated_this_week.png?raw=true" width="80%">
</p>

## Uncompleted Tasks

If reporting is built into a recurring end-of-week team meeting, like a Weekly Team Retro, there is an opportunity to review uncompleted tasks: task status updates, any blockers, and updated timelines. These outstanding tasks can be consolidated into a table to facilitate open team discussion.

```{r4}
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
```
<p align="center">
  <img src="https://github.com/mattgerken/asana-tracking/blob/main/uncompleted_this_week.png?raw=true" width="80%">
</p>

## Workstreams Over Time


# Lessons Learned



