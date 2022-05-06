library(data.table)
library(lpSolve)

# max budget of 100
# 2 GKP 5 DEF 5 MID 3 FWD
# maximize
# max 3 from 1 club
# captaincy

data <- fread("data/cleaned_players.csv")
obj <- data$total_points
constr <- matrix(
  c(data$now_cost, rep(1, nrow(data))), byrow = TRUE, nrow = 2
)
const_direction <- c("<=", "==")
rhs <- c(1000, 15)

result <- lp("max", obj, constr,const_direction, rhs, all.bin = TRUE)

data$result <- result$solution
data[result == 1, .(second_name, total_points, element_type, now_cost = now_cost/10)]
