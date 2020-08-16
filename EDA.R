library(ggplot2)

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
  filter(nameFirst == "DJ")



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
         avg_Runs_Start_no_risp = mean(Runs_Start_no_risp)) %>%
  summarize(N = n(), avg_RE24, avg_PA, avg_Runs_Start, 
            avg_RE24_risp, avg_PA_risp, avg_Runs_Start_risp, 
            avg_RE24_no_risp, avg_PA_no_risp, avg_Runs_Start_no_risp) %>%
  unique() %>%
  as.data.frame() -> averages_by_batting_order




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
  summarize(N = n()) %>%  
  arrange(desc(N)) %>%  
  mutate(Team = first(teamID)) -> batters_2019 

batters_2019 %>%
  filter(Team == "NYA", G > 40)

batters_2019 %>%
  filter(playerID == "encared01")











