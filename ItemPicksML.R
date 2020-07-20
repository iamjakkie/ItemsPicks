library("tidyverse")

picks <- read.csv('Data/picks.csv')

glimpse(picks)

clean_picks <- select(picks, -RET_TO_PICK, -SUP_TO_PICK, -DURATION)

model <- lm(log(SECONDS_PER_PICK) ~ ., clean_picks)

view(picks)
model
summary(model)
coeffs = coef(model)
# f <- function(floor, side, alley, turns, distance, ret_loc, sup_loc){
#   coeffs[1] + coeffs[2]*floor + coeffs[3]*side + coeffs[4]*alley +
#     coeffs[5]*turns + coeffs[6]*distance + coeffs[7]*ret_loc +
#     coeffs[8]*sup_loc
# }
f <- function(x){
  coeffs[1] + coeffs[2]*x[1] + coeffs[3]*x[2] + coeffs[4]*x[3] +
    coeffs[5]*x[4] + coeffs[6]*x[5] + coeffs[7]*x[6] +
    coeffs[8]*x[7]
}
res <- optim(rep(5,7),f, method="L-BFGS-B", lower=rep(0,7), upper=rep(10,7))


res <- optimize(f, lower = 0, upper = 5, maximum = FALSE)


hist(picks$SECONDS_PER_PICK)

car::vif(model)
