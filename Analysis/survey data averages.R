


library(readxl)
library(tidyverse)

survey_res <- read_excel("results-survey421291 (1).xlsx")

###### for grid point and heat
# load data
df <- survey_res[,6:(ncol(survey_res)-9)]  # -9 because those are the area columns

df_long <- df %>%
  pivot_longer(cols = everything(), names_to = "question", values_to = "answer")

# extract column names and determine which data type (e.g. CL for clustered) and map type (e.g. P for point)
df_long <- df_long %>%
  filter(!is.na(answer)) %>%
  mutate(
    type = str_sub(question, 1, 4),   # e.g. SC, CL, RD
    options = str_extract_all(substring(question, 5), "[GVRW]"), # answer options
    answer = toupper(answer)
  )

# Step 3: Expand rows to include shown options for each column
# This gives one row per answer per option shown
df_expanded <- df_long %>%
  unnest(options)

# indicate whether the shown option was picked
df_expanded <- df_expanded %>%
  mutate(was_picked = (answer == options) * 1)

# win rate per column per question option
win_rates <- df_expanded %>%
  group_by(type, question, options) %>%
  summarise(
    times_shown = n(),
    times_picked = sum(was_picked),
    win_rate = times_picked / times_shown,
    .groups = "drop"
  )

# average win rate per type and answer
avg_win_rates <- win_rates %>%
  group_by(type, options) %>%
  summarise(
    avg_win_rate = mean(win_rate),
    .groups = "drop"
  )

avg_win_rate_wide <- avg_win_rates %>%
  pivot_wider(
    names_from = options,
    values_from = avg_win_rate
  )

print(avg_win_rate_wide)


avg_win_rate_wide = avg_win_rate_wide[,c(1,3,2,4,5)] # columns in correct order

# defining for the points data
survey_point = avg_win_rate_wide[grepl("P", avg_win_rate_wide$type),]
survey_point = c(as.matrix(t(survey_point[c(3,1,4,5,2),2:5])))


survey_grid = avg_win_rate_wide[grepl("G", avg_win_rate_wide$type),]
survey_grid = c(as.matrix(t(survey_grid[c(3,1,4,5,2),2:5])))

survey_heat = avg_win_rate_wide[grepl("H", avg_win_rate_wide$type),]
survey_heat = c(as.matrix(t(survey_heat[c(3,1,4,5,2),2:5])))


rowSums(survey_point[,2:5])


########################## 
### for area

###### for grid point and heat
# Load your data
df <- survey_res[,(107-8):ncol(survey_res)]  # -9 because those are the area values

# Step 1: Pivot to long format
df_long <- df %>%
  pivot_longer(cols = everything(), names_to = "question", values_to = "answer")

# Step 2: Clean column names to extract metadata
df_long <- df_long %>%
  filter(!is.na(answer)) %>%
  mutate(
    type = str_sub(question, 1, 4),   # 
    options = str_extract_all(question, "[MSL]"),
    answer = toupper(answer)
  )

# Step 3: Expand rows to include shown options for each column
# This gives one row per answer per option shown
df_expanded <- df_long %>%
  unnest(options)

# Step 4: Mark whether the shown option was picked
df_expanded <- df_expanded %>%
  mutate(was_picked = (answer == options) * 1)

# Step 5: Calculate win rate per column per option
win_rates <- df_expanded %>%
  group_by(type, question, options) %>%
  summarise(
    times_shown = n(),
    times_picked = sum(was_picked),
    win_rate = times_picked / times_shown,
    .groups = "drop"
  )

# Step 6: Average win rate per type and answer
avg_win_rates <- win_rates %>%
  group_by(type, options) %>%
  summarise(
    avg_win_rate = mean(win_rate),
    .groups = "drop"
  )

avg_win_rate_wide <- avg_win_rates %>%
  pivot_wider(
    names_from = options,
    values_from = avg_win_rate
  )

print(avg_win_rate_wide)


avg_win_rate_wide = avg_win_rate_wide[,c(1,4,3,2)] # columns in correct order

survey_area = c(t(as.matrix(avg_win_rate_wide[c(3,2,1),2:4])))


rowSums(as.data.frame(avg_win_rate_wide)[,2:4])

3/2




