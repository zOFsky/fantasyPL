library(data.table)
library(lpSolve)

# max budget of 100
# 2 GKP 5 DEF 5 MID 3 FWD
# maximize
# max 3 from 1 club
# captaincy

data <- fread("data/players_raw22.csv")[, .(web_name, team, total_points, 
                                            now_cost, position = element_type)]
#data[433, now_cost := 150]
data[, `:=`(team = as.factor(team),
            position = as.factor(position))]

team_constr <- c()
for(team in unique(data$team)){
  team_constr <- c(team_constr, data$team == team)
}
pos_constr <- c()
for(pos in levels(data$position)){
  pos_constr <- c(pos_constr, data$position == pos)
}

squad <- rep(1, nrow(data))
captain <- rep(1, nrow(data))
#obj <- data$total_points

constr <- matrix(
  c(data$now_cost,                  # budget
    squad,             # teamsize
    team_constr,                    # 20x club constraint
    pos_constr,                     # 4 position
    pos_constr                     # 4 position
  ),                   # captain
  byrow = TRUE, nrow = 30
)
const_direction <- c("<=",                        #budget
                     "==",                        # teamsize 
                     rep("<=", 20),               # 20x club constr
                     "==", "<=","<=","<=",        # 4 position less than
                     "==", ">=", ">=", ">="      # 4 position at least
)                        # captain
rhs <- c(830, 
         11, 
         rep(3, 20), 
         1, 5, 5, 3, 
         1, 3, 2, 1
)


res <- vector("list", nrow(data))
for(i in seq_len(nrow(data))){
  
  #cat("iter:", i, "\n")
  
  obj <- data$total_points
  obj[[i]] <- obj[[i]] * 2
  
  res[[i]] <- lp("max", obj, constr, const_direction, rhs, all.bin = TRUE)
}

captain_index <- which.max(unlist(lapply(res, function(x) x$objval)))
print(data[res[[captain_index]]$solution == 1,])
print(sum(data[res[[captain_index]]$solution == 1,]$total_points))
answer <- data[res[[captain_index]]$solution == 1,]









