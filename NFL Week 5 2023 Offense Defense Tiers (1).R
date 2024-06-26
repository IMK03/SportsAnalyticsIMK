
# Load R and Import necessary Libraries
remove.packages("rlang")
remove.packages("tidyverse")
remove.packages("nflreadr")
remove.packages("nflplotR")
install.packages("rlang")
install.packages("tidyverse")
install.packages("nflreadr")
install.packages("nflplotR")
install.packages("ggplot2")
#install.packages("nflfastr")

library(tidyverse)
library(nflreadr)
library(nflplotR)



# Loading in the Data and Taking a Look

pbp_22 = load_pbp(2022)

pbp_22
colnames(pbp_22)
View(pbp_22)


pbp_22 |> 
  filter(pass == 1) |> 
  group_by(passer) |> 
  summarise(ypa = mean(yards_gained, na.rm = TRUE),
            plays = n()) |> 
  arrange(-plays) |> 
  filter(plays > 100) |> 
  mutate(yards_gained = ypa * plays) |> 
  select(passer, yards_gained)




# RBs
pbp_22 |> 
  filter(posteam == "CAR", rush == 1) |> 
  group_by(rusher) |> 
  summarise(mean_epa = mean(epa),
            success_rate = mean(success),
            ypc = mean(yards_gained),
            plays = n()) |> 
  arrange(-mean_epa) |> 
  filter(plays >= 20)






# Offensive performances

pbp_23 = load_pbp(2023)

wk5_off = pbp_23 |> 
  filter(week == 5, !is.na(yards_gained), (pass == 1 | rush == 1)) |> 
  group_by(posteam) |> 
  summarise(mean_epa = mean(epa),
            success_rate = mean(success),
            ypa = mean(yards_gained),
            plays = n()) |> 
  arrange(-mean_epa)
wk5_off

ggplot(wk5_off, aes(x = mean_epa, y = success_rate)) +
  labs(
    x = "EPA per play (explosiveness)",
    y = "Success Rate (consistency)",
    title = "NFL Offensive Performances (Week 5)",
    caption = "By: Idrees Muhammad Kudaimi  |  Data @nflfastR"
  ) +
  stat_smooth(formula = y ~ x, method = "lm", geom = "line", se = FALSE, color = "gray") +
  nflplotR::geom_mean_lines(aes(x0 = mean_epa, y0 = success_rate)) +
  nflplotR::geom_nfl_logos(aes(team_abbr = posteam), width = .07, alpha = .7)

ggsave("off_performance_w5.png", width = 16, height = 9, units = "cm")



# Defensive Performances

wk5_def = pbp_23 |> 
  filter(week == 5, !is.na(yards_gained), (pass == 1 | rush == 1)) |> 
  group_by(defteam) |> 
  summarise(mean_epa = mean(epa),
            success_rate = mean(success),
            ypa = mean(yards_gained),
            plays = n()) |> 
  arrange(mean_epa)
wk5_def


# Final Plots

ggplot(wk5_def, aes(x = mean_epa, y = success_rate)) +
  labs(
    x = "EPA per play Allowed (explosiveness)",
    y = "Success Rate Allowed (consistency)",
    title = "NFL Defensive Performances (Week 5)",
    caption = "By: Idrees Muhammad Kudaimi  |  Data @nflfastR"
  ) +
  scale_x_reverse() +
  scale_y_reverse() +
  stat_smooth(formula = y ~ x, method = 'lm', geom = 'line', se = FALSE, color = "gray") +
  nflplotR::geom_mean_lines(aes(x0 = mean_epa, y0 = success_rate)) +
  nflplotR::geom_nfl_logos(aes(team_abbr = defteam), width = .07, alpha = .7)


ggsave("def_performance_w5.png", width = 16, height = 9, units = "cm")

# Performances week 10 on 

wk10_on_def = pbp_23 |>
  filter(week %in% c(10, 11, 12, 13, 14, 15, 16, 17, 18), !is.na(yards_gained), (pass == 1 | rush == 1)) |> 
  group_by(defteam) |> 
  summarise(mean_epa = mean(epa),
            success_rate = mean(success),
            ypa = mean(yards_gained),
            plays = n()) |> 
  arrange(mean_epa)


# Final Plots

ggplot(wk10_on_def, aes(x = mean_epa, y = success_rate)) +
  labs(
    x = "EPA per play Allowed (explosiveness)",
    y = "Success Rate Allowed (consistency)",
    title = "NFL Defensive Performances (Week 10 on)",
    caption = "By: Idrees Muhammad Kudaimi  |  Data @nflfastR"
  ) +
  scale_x_reverse() +
  scale_y_reverse() +
  stat_smooth(formula = y ~ x, method = 'lm', geom = 'line', se = FALSE, color = "gray") +
  nflplotR::geom_mean_lines(aes(x0 = mean_epa, y0 = success_rate)) +
  nflplotR::geom_nfl_logos(aes(team_abbr = defteam), width = .07, alpha = .7)


# Performances week 10 on 

wk10_on_off = pbp_23 |>
  filter(week %in% c(10, 11, 12, 13, 14, 15, 16, 17, 18), !is.na(yards_gained), (pass == 1 | rush == 1)) |> 
  group_by(posteam) |> 
  summarise(mean_epa = mean(epa),
            success_rate = mean(success),
            ypa = mean(yards_gained),
            plays = n()) |> 
  arrange(mean_epa)


# Final Plots

ggplot(wk10_on_off, aes(x = mean_epa, y = success_rate)) +
  labs(
    x = "EPA per play (explosiveness)",
    y = "Success Rate (consistency)",
    title = "NFL Offensive Performances (Week 10 on)",
    caption = "By: Idrees Muhammad Kudaimi  |  Data @nflfastR"
  ) +
  stat_smooth(formula = y ~ x, method = 'lm', geom = 'line', se = FALSE, color = "gray") +
  nflplotR::geom_mean_lines(aes(x0 = mean_epa, y0 = success_rate)) +
  nflplotR::geom_nfl_logos(aes(team_abbr = posteam), width = .07, alpha = .7)


#https://x.com/TotallyREALSpo1/status/1795281662995046529

