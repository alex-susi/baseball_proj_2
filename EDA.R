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
  filter(nameFirst == "DJ")





