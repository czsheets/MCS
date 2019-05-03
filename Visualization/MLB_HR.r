library(Lahman)
library(dplyr)

hr_teams <- Teams %>% 
  filter((lgID == 'NL' | lgID == 'AL') & yearID > 1900) %>%
  group_by(lgID, yearID) %>%
  mutate(HR_tot = sum(HR)) %>%
  select(yearID, teamID, HR, lgID, HR_tot)
head(hr_teams)

years <- c(0,1920, 1942, 1973, 1996, max(hr_teams$yearID))

ggplot(hr_teams) + 
  geom_point(aes(yearID, HR, col = lgID),alpha = .35) + 
  geom_smooth(aes(yearID, HR, group = lgID, col = lgID)) + 
  ylab("Total Home Runs") + 
  xlab("Year") + 
  labs(col = "League") +
  ggtitle("Major League Baseball home run trends") +
  geom_vline(xintercept = years[-length(years)], linetype = 2, col = 'grey') + 
  scale_x_discrete(name = 'Year', limits = years) +
  theme_tufte()
