library(ggplot2)
library(dplyr)

## Read in RE24 --------------------------------------------------------
re24 <- read.csv("RE24.csv")
re24 <- re24[-1]
re24 %>%
  filter(PA >= 300) -> re24_300



## Scatterplot RE24 w/Risp vs w/o RISP ---------------------------------
ggplot(re24_300, aes(RE24_no_risp, RE24_risp)) +
  geom_point() + 
  geom_hline(yintercept = 0, color = "blue") +
  geom_vline(xintercept = 0, color = "blue") -> risp_plot
cor(re24_300$RE24_risp, re24_300$RE24_no_risp)
cor(re24_300$RE24_risp, re24_300$Runs_Start_risp)
cor(re24_300$RE24_no_risp, re24_300$Runs_Start_no_risp)
cor(re24_300$RE24, re24_300$Runs.Start)
cor(re24_300$Runs.Start, re24_300$PA)
cor(re24_300$Runs_Start_risp, re24_300$PA_risp)
cor(re24_300$Runs_Start_no_risp, re24_300$PA_no_risp)


re24_300 %>%
  mutate(RE24_difference = RE24_risp - RE24_no_risp) -> re24_300





## Examining the Data -------------------------------------------------
re24_300 %>%
  arrange(desc(RE24_difference)) %>%
  head()

re24_300 %>%
  arrange(RE24_difference) %>%
  head()

re24_300 %>%
  arrange(desc(RE24_no_risp)) %>%
  head()

re24_300 %>%
  arrange(desc(RE24_risp)) %>%
  head()

re24_300 %>%
  filter(nameLast == "Urshela")



## Adding in Batting Orders ----------------------------------------- 
data2019 %>% 
  group_by(BAT_ID, BAT_LINEUP_ID) %>%  
  summarize(N = n()) %>%  
  arrange(desc(N)) %>%  
  mutate(Batting_order = first(BAT_LINEUP_ID)) -> positions

positions %>%
  select(BAT_ID, Batting_order) %>%
  unique() -> batting_orders

re24_300 %>%
  inner_join(batting_orders, by = "BAT_ID") -> re24_300


re24_300 %>%
  group_by(Batting_order) %>%
  mutate(avg_RE24 = mean(RE24), 
         avg_PA = mean(PA), 
         avg_Runs_Start = mean(Runs.Start),
         avg_RE24_risp = mean(RE24_risp), 
         avg_PA_risp = mean(PA_risp), 
         avg_Runs_Start_risp = mean(Runs_Start_risp),
         avg_RE24_no_risp = mean(RE24_no_risp), 
         avg_PA_no_risp = mean(PA_no_risp), 
         avg_Runs_Start_no_risp = mean(Runs_Start_no_risp),
         avg_RE24_diff = mean(RE24_difference)) %>%
  summarize(N = n(), avg_RE24, avg_PA, avg_Runs_Start, 
            avg_RE24_risp, avg_PA_risp, avg_Runs_Start_risp, 
            avg_RE24_no_risp, avg_PA_no_risp, avg_Runs_Start_no_risp,
            avg_RE24_diff) %>%
  unique() %>%
  as.data.frame() %>%
  mutate_if(is.numeric, round, digits = 2) -> averages_by_batting_order

cor(averages_by_batting_order$avg_RE24, 
    averages_by_batting_order$avg_PA)
cor(averages_by_batting_order$avg_Runs_Start, 
    averages_by_batting_order$avg_PA)
cor(averages_by_batting_order$avg_RE24_risp, 
    averages_by_batting_order$avg_PA_risp)
cor(averages_by_batting_order$avg_Runs_Start_risp, 
    averages_by_batting_order$avg_PA_risp)
cor(averages_by_batting_order$avg_Runs_Start_no_risp, 
    averages_by_batting_order$avg_PA_no_risp)
cor(averages_by_batting_order$avg_RE24_no_risp, 
    averages_by_batting_order$avg_PA_no_risp)
cor(averages_by_batting_order$avg_RE24_diff, 
    averages_by_batting_order$avg_RE24_no_risp)


averages_by_batting_order %>%
  group_by(Batting_order) %>%
  mutate(RE24_per_RS = avg_RE24/avg_Runs_Start,
         RE24_risp_per_RS = avg_RE24_risp/avg_Runs_Start_risp,
         RE24_no_risp_per_RS = avg_RE24_no_risp/avg_Runs_Start_no_risp) %>%
  select(Batting_order, RE24_per_RS, RE24_risp_per_RS, RE24_no_risp_per_RS) %>%
  as.data.frame() %>%
  mutate_if(is.numeric, round, digits = 4) -> re24_rates
  


ggplot(averages_by_batting_order, aes(Batting_order, avg_RE24)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") -> batting_order_plot





## Plotting with Batting Orders ---------------------------------------
ggplot(re24_300, aes(RE24_no_risp, RE24_risp, 
                     color = as.factor(Batting_order))) +
  geom_point() + 
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  scale_color_manual(breaks = c("1", "2", "3", 
                                "4", "5", "6", 
                                "7", "8", "9"),
                     values=c("red", "orange", "darkmagenta", 
                              "darkgreen", "blue", "turquoise3", 
                              "violet", "sienna", "black")) -> risp_plot
risp_plot

risp_plot %+% subset(re24_300, Batting_order %in% c("3","4"))




## Getting Teams ------------------------------------------------------
Batting %>% filter(yearID == 2019) -> batters_2019

batters_2019 %>%  
  group_by(playerID, stint, teamID, G) %>%  
  summarize() %>% 
  mutate(Team = first(teamID)) %>%
  as.data.frame() -> batters_2019 

batters_2019 %>%
  filter(Team == "NYA", G > 40)

batters_2019 %>%
  filter(playerID == "encared01")



## Batting Order Multipliers ------------------------------------------
avg_rs <- mean(re24_300$Runs.Start)
avg_rs_risp <- mean(re24_300$Runs_Start_risp)
avg_rs_no_risp <- mean(re24_300$Runs_Start_no_risp)

averages_by_batting_order %>%
  group_by(Batting_order) %>%
  mutate(rs_mult = avg_Runs_Start/avg_rs,
         rs_mult_risp = avg_Runs_Start_risp/avg_rs_risp,
         rs_mult_no_risp = avg_Runs_Start_no_risp/avg_rs_no_risp) %>%
  select(Batting_order, rs_mult, rs_mult_risp, rs_mult_no_risp) %>%
  as.data.frame() %>%
  mutate_if(is.numeric, round, digits = 4) -> batting_order_mults








## Function for best batting order -----------------------------------
batters <- re24_300$BAT_ID
for (b in batters) {
  maximize_re24(b)
}



maximize_re24 <- function(batter) {
  
}

test_id <- "urshg001"
orders <- c(1:9)

batter_re24_risp <- re24_300 %>%
  filter(BAT_ID == test_id) %>%
  select(RE24_risp) %>%
  as.numeric()
batter_re24_no_risp <- re24_300 %>%
  filter(BAT_ID == test_id) %>%
  select(RE24_no_risp) %>%
  as.numeric()
batter_rs_risp <- re24_300 %>%
  filter(BAT_ID == test_id) %>%
  select(Runs_Start_risp) %>%
  as.numeric()
batter_rs_no_risp <- re24_300 %>%
  filter(BAT_ID == test_id) %>%
  select(Runs_Start_no_risp) %>%
  as.numeric()
batter_order <- re24_300 %>%
  filter(BAT_ID == test_id) %>%
  select(Batting_order) %>%
  as.numeric()
batter_pa_risp <- re24_300 %>%
  filter(BAT_ID == test_id) %>%
  select(PA_risp) %>%
  as.numeric()
batter_pa_no_risp <- re24_300 %>%
  filter(BAT_ID == test_id) %>%
  select(PA_no_risp) %>%
  as.numeric()

df9 <- data.frame(matrix(nrow = 9, ncol = 2))
for (i in orders){
  df9$X1[i] <- i
  df9$X2[i] <- assign(paste0("new_re24_",i),(batter_re24_risp/batter_rs_risp)*
           (batting_order_mults$rs_mult_risp[i]/batting_order_mults$rs_mult_risp[batter_order])*
           (averages_by_batting_order$avg_Runs_Start_risp[i])*
           (batter_pa_risp/averages_by_batting_order$avg_PA_risp[i]) +
           (batter_re24_no_risp/batter_rs_no_risp)*
           (batting_order_mults$rs_mult_no_risp[i]/batting_order_mults$rs_mult_no_risp[batter_order])*
           (averages_by_batting_order$avg_Runs_Start_no_risp[i])*
           (batter_pa_no_risp/averages_by_batting_order$avg_PA_no_risp[i]))
  df9 <- df9 %>%
    arrange(desc(X2))
}









