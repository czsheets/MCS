library(Lahman)
library(dplyr)
library(ggraph)
library(tidygraph)
hof <- Lahman::HallOfFame %>% filter(inducted == 'Y' & category == 'Player' & votedBy == 'BBWAA') %>% select(playerID) %>% 
  inner_join(Pitching %>% select(playerID, yearID, teamID), by = 'playerID') %>%
full_join(Lahman::HallOfFame %>% filter(inducted == 'Y' & category == 'Player' & votedBy == 'BBWAA') %>% select(playerID) %>% 
  inner_join(Batting %>% select(playerID, yearID, teamID), by = 'playerID'),
  by = c('playerID','yearID','teamID')) %>% 
  inner_join(Lahman::Master %>% select(playerID, nameFirst, nameLast), by = 'playerID') %>%
  mutate(team_year = paste0(yearID, teamID))
  
cross_hof <- (hof %>% mutate(join_var = 1)) %>% 
  full_join(hof %>% mutate(join_var = 1), by = 'join_var') %>% 
  select(-join_var) %>%
  filter(playerID.x != playerID.y) %>%
  mutate(same_team = ifelse(team_year.x == team_year.y, 1, 0)) %>%
  ungroup() %>%
  select(playerID.x, playerID.y, same_team) %>%
  group_by(playerID.x, playerID.y) %>%
  mutate(total = sum(same_team)) %>%
  select(-same_team) %>%
  distinct()

cross_hof_name <- cross_hof %>% 
  inner_join(Lahman::Master %>% 
                           select(playerID, nameFirst, nameLast) %>%
                           mutate(name = paste(nameFirst, nameLast)) %>%
                           select(playerID, name), by = c('playerID.x' = 'playerID')) %>% 
  inner_join(Lahman::Master %>% 
               select(playerID, nameFirst, nameLast) %>%
               mutate(name = paste(nameFirst, nameLast)) %>%
               select(playerID, name), by = c('playerID.y' = 'playerID')) %>%
  ungroup() %>%
  select(name.x, name.y, total) %>%
  distinct() %>%
  filter(name.x < name.y) 

hof_graph <- as_tbl_graph(hof_no_zero, directed = FALSE)

ggraph(hof_graph) + 
  geom_edge_link(alpha = .20) + 
  geom_node_point(alpha = .20) +
  geom_node_text(
    aes(label = name), size = 4, col = 'maroon', repel = T) +
  theme_graph() +
  ggtitle('Baseball Hall of Fame Relationships')
