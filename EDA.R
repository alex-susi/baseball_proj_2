library(ggplot2)
library(dplyr)
library(Lahman)
library(nnet)

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
risp_plot
cor(re24_300$RE24_risp, re24_300$RE24_no_risp)
cor(re24_300$RE24_risp, re24_300$Runs_Start_risp)
cor(re24_300$RE24_no_risp, re24_300$Runs_Start_no_risp)
cor(re24_300$RE24, re24_300$Runs.Start)
cor(re24_300$Runs.Start, re24_300$PA)
cor(re24_300$Runs_Start_risp, re24_300$PA_risp)
cor(re24_300$Runs_Start_no_risp, re24_300$PA_no_risp)








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
  arrange(desc(RE24_no_risp_per_RS)) %>%
  head()

re24_300 %>%
  filter(nameLast == "Encarnacion")








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
  mutate(RE24_difference = RE24_risp - RE24_no_risp,
         RE24_risp_per_RS = RE24_risp/Runs_Start_risp,
         RE24_no_risp_per_RS = RE24_no_risp/Runs_Start_no_risp) %>%
  relocate(Batting_order, .after = RE24_no_risp_per_RS) %>% 
  mutate_if(is.numeric, round, digits = 4) -> re24_300

# Averages by Batting Order
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

# Correlations
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
  select(Batting_order, RE24_per_RS, 
         RE24_risp_per_RS, RE24_no_risp_per_RS) %>%
  as.data.frame() %>%
  mutate_if(is.numeric, round, digits = 4) -> re24_rates
  

# Plot
ggplot(averages_by_batting_order, aes(Batting_order, avg_RE24)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") -> batting_order_plot

averages_by_batting_order %>%
  select(Batting_order, avg_RE24, avg_RE24_risp, avg_RE24_no_risp) ->
  averages_by_batting_order_sub

library(reshape2)
avgs_by_bat <- melt(averages_by_batting_order_sub, id.vars = "Batting_order")

ggplot(avgs_by_bat) +
  geom_line(aes(x = Batting_order, 
            y =  value,
            colour = variable)) +
  scale_colour_manual(values = c("red", "darkgreen", "blue")) +
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  scale_x_continuous(breaks = c(1,2,3,
                     4,5,6,
                     7,8,9))-> batting_order_plot2








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

risp_plot %+% subset(re24_300, Batting_order %in% c("2","8"))








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

test_id <- "lemad001"
#test_id <- 
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
df9







## Predicting Batting Order ---------------------------------------
re24_300$Batting_order <- as.factor(re24_300$Batting_order) 
re24_300$Batting_order <- relevel(re24_300$Batting_order, ref = "8")
fit <- multinom(Batting_order ~ RE24_risp_per_RS + RE24_no_risp_per_RS, 
           data = re24_300)
summary(fit)
predict(fit)
predict_fit <- as.vector(predict(fit))


probs <- round(predict(fit, type = "prob"), digits = 4)
error <- table(predict(fit), re24_300$Batting_order)
sum(diag(error))/sum(error)

# P-values
z <- summary(fit)$coefficients/summary(fit)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2

# Adding results to RE24_300
ids <- re24_300$BAT_ID
results <- as.data.frame(predict_fit, ids, col.names = headers)
results$BAT_ID <- ids
names(results)[1] <- "Predicted_order"
re24_300_predict <- merge(re24_300, results, by = "BAT_ID")

player_probs <- as.data.frame(probs)
player_probs$BAT_ID <- ids
player_probs <- player_probs[,c("BAT_ID","1","2","3",
                                "4","5",'6','7','8','9')]

player_probs %>%
  filter(BAT_ID == "sancg002")

re24_300_predict %>%
  filter(BAT_ID == "sancg002")

re24_300_predict %>%
  arrange(desc(RE24_risp_per_RS)) %>%
  head()

re24_300_predict %>%
  arrange((Predicted_order)) %>%
  head()


re24_300_predict %>%
  group_by(Predicted_order) %>%
  mutate(avg_RE24 = mean(RE24), 
         avg_PA = mean(PA), 
         avg_Runs_Start = mean(Runs.Start),
         avg_RE24_risp = mean(RE24_risp), 
         avg_PA_risp = mean(PA_risp), 
         avg_Runs_Start_risp = mean(Runs_Start_risp),
         avg_RE24_no_risp = mean(RE24_no_risp), 
         avg_PA_no_risp = mean(PA_no_risp), 
         avg_Runs_Start_no_risp = mean(Runs_Start_no_risp),
         avg_RE24_diff = mean(RE24_difference),
         avg_RE24_risp_per_RS = mean(RE24_risp_per_RS),
         avg_RE24_no_risp_per_RS = mean(RE24_no_risp_per_RS)) %>%
  summarize(N = n(), avg_RE24, avg_PA, avg_Runs_Start, 
            avg_RE24_risp, avg_PA_risp, avg_Runs_Start_risp, 
            avg_RE24_no_risp, avg_PA_no_risp, avg_Runs_Start_no_risp,
            avg_RE24_diff, avg_RE24_risp_per_RS,
            avg_RE24_no_risp_per_RS) %>%
  unique() %>%
  as.data.frame() %>%
  mutate_if(is.numeric, round, digits = 4) -> averages_by_batting_order






## MACHINE LEARNING --------------------------------------------------
head(re24_300)

re24_300 %>%
  ggvis(~RE24_no_risp, ~RE24_risp, fill = ~Batting_order) %>%
  layer_points()

re24_300 %>%
  select(nameFirst, nameLast, BAT_ID, PA, RE24_risp, 
         RE24_no_risp, Batting_order) -> re24_300_short

re24_300 %>%
  select(PA, RE24_risp, RE24_no_risp, Batting_order) -> re24_knn
  
set.seed(1234)
ind <- sample(2, nrow(re24_knn), replace = TRUE, prob = c(0.67, 0.33))

re24.training <- re24_knn[ind==1, 1:3]
re24.test <- re24_knn[ind==2, 1:3]

re24.trainLabels <- re24_knn[ind==1, 4]
re24.testLabels <- re24_knn[ind==2, 4]


re24_pred <- knn(train = re24.training, test = re24.test, 
                 cl = re24.trainLabels, k = 3)

re24TestLabels <- data.frame(re24.testLabels)
merge <- data.frame(re24_pred, re24TestLabels)
names(merge) <- c("Predicted Order",
                  "Observed Order")
merge
merge[1]


CrossTable(re24.testLabels, 
           re24_pred, 
           prop.chisq = FALSE)

re24_test_results <- data.frame(re24.test, merge)
re24_test_results <- inner_join(re24_test_results, re24_300_short)
re24_test_results %>%
  select(nameFirst, nameLast, BAT_ID, PA, 
         RE24_risp, RE24_no_risp, Predicted.Order,
         Observed.Order) -> re24_test_results
re24_test_results

