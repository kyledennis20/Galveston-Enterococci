library(ggplot2)
library(dplyr)
exceedance_beach_monthly = read.csv("C:/Users/Kyle/Desktop/Galveston Consulting/Past Work/Task 2/Data/exceedance_beach_monthly.csv")
exceedance_beach_monthly = exceedance_beach_monthly %>% mutate(date_num = seq(1,12))

ggplot(data = exceedance_beach_monthly, aes(x = date_num, y = TX163187)) +
  geom_point() +
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth")

