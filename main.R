library(data.table)
library(lpSolve)

# max budget of 100
# 2 GKP 5 DEF 5 MID 3 FWD
# maximize
# max 3 from 1 club
# captaincy

data <- fread("data/players_raw.csv")[, .(web_name, team, total_points, 
                                          now_cost, position = element_type)]
data[, `:=`(team = as.factor(team),
            position = as.factor(position))]

team_constr <- c()
for(team in unique(data$team)){
  team_constr <- c(team_constr, data$team == team)
}
pos_constr <- c()
for(pos in unique(data$position)){
  pos_constr <- c(pos_constr, data$position == pos)
}

squad <- rep(1, nrow(data))
captain <- rep(1, nrow(data))
obj <- data$total_points

constr <- matrix(
  c(data$now_cost,                  # budget
    squad > 0,             # teamsize
    team_constr,                    # 20x club constraint
    pos_constr,                     # 4 position
    pos_constr                     # 4 position
    ),                   # captain
  byrow = TRUE, nrow = 30
)
const_direction <- c("<=",                        #budget
                     "==",                        # teamsize 
                     rep("<=", 20),               # 20x club constr
                     "<=", "<=","<=","<=",        # 4 position less than
                     ">=", ">=", ">=", "=="      # 4 position at least
                     )                        # captain
rhs <- c(830, 
         11, 
         rep(3, 20), 
         5, 5, 3, 1, 
         2, 3, 1, 1
         )

result <- lp("max", obj, constr,const_direction, rhs, all.bin = TRUE)

data$result <- result$solution
answer <- data[result != 0 , .(web_name, total_points, team, position, 
                              now_cost = now_cost/10)]

