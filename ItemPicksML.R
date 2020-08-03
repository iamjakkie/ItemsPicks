library("tidyverse")
library("ggthemr")
picks <- read.csv('Data/picks.csv')

glimpse(picks)

clean_picks <- select(picks, -RET_TO_PICK, -SUP_TO_PICK, -DURATION)

# data cleanup
# 1. Distance - greater than 0

clean_picks[clean_picks$DISTANCE<=0]

ggplot(clean_picks, aes(DISTANCE, SECONDS_PER_PICK)) +
  geom_point()

# two weird outliers
clean_picks <- clean_picks %>% filter(SECONDS_PER_PICK <= 1000)

# 2. Floor - categorical variable [0,3]
unique(round(clean_picks$FLOOR))
clean_picks$FLOOR <- as_factor(round(clean_picks$FLOOR))
unique(clean_picks$FLOOR)

ggplot(clean_picks, aes(FLOOR, fill = SECONDS_PER_PICK)) +
  geom_bar()

clean_picks %>% group_by(FLOOR) %>%
  summarise(avg_picks = mean(SECONDS_PER_PICK)) %>%
  ggplot(aes(x = reorder(FLOOR, -avg_picks), avg_picks)) +
  geom_bar(stat='identity') +
  labs(x = "FLOOR", y = "SECONDS_PER_PICK",
       title = "AVG SECONDS_PER_PICK PER FLOOR",
       subtitle = "Getting items from 3rd floor takes longer")

# 3. SIDE - categorical [0,1]
unique(clean_picks$SIDE)
clean_picks$SIDE <- as_factor(round(clean_picks$SIDE))
unique(clean_picks$SIDE)

ggplot(clean_picks, aes(SIDE)) +
  geom_bar()

clean_picks %>% group_by(SIDE) %>%
  summarise(avg_picks = mean(SECONDS_PER_PICK)) %>%
  ggplot(aes(SIDE, avg_picks)) +
  geom_bar(stat='identity') +
  geom_text(aes(label=avg_picks), position=position_dodge(width=0.9), vjust=-0.25)

# 4. LR_ALLEY - categorical [0,1]
unique(clean_picks$LR_ALLEY)

clean_picks %>% group_by(LR_ALLEY) %>%
  summarise(count = n()) %>%
  arrange(-count) %>%
  head()

clean_picks %>% filter(LR_ALLEY != 1 & LR_ALLEY != 2) %>%
  count()

clean_picks <- clean_picks %>% filter(LR_ALLEY ==1 | LR_ALLEY ==2)
#deleted 6988 rows

clean_picks$LR_ALLEY <- as_factor(clean_picks$LR_ALLEY)
unique(clean_picks$LR_ALLEY)

# 5. TURNS - I'd assume this is a categorical variable

unique(clean_picks$TURNS)

clean_picks$TURNS <- as_factor(clean_picks$TURNS)


# 6. Distance - continuous

summary(clean_picks$DISTANCE)

ggplot(clean_picks, aes(DISTANCE, SECONDS_PER_PICK)) +
  geom_point()

clean_picks %>% filter(DISTANCE > 400000) %>%
  ggplot(aes(DISTANCE, SECONDS_PER_PICK)) +
  geom_point()

# get rid of outliers (5)

nrow(clean_picks)
clean_picks <- clean_picks %>% filter(DISTANCE <= 400000)
nrow(clean_picks)


# 7. RET_LOC

summary(clean_picks$RET_LOC)

ggplot(clean_picks, aes(RET_LOC, SECONDS_PER_PICK)) + 
  geom_point()

# 8. SUP_LOC

summary(clean_picks$SUP_LOC)

ggplot(clean_picks, aes(SUP_LOC, SECONDS_PER_PICK)) +
  geom_point()


## STATISTICAL PART

# 1. Distribution of continuous values

## a. Distance
hist(clean_picks$DISTANCE)
hist(log(clean_picks$DISTANCE)) #better
clean_picks$DISTANCE <- log(clean_picks$DISTANCE)
## b. RET_LOC
hist(clean_picks$RET_LOC)
hist(log(clean_picks$RET_LOC))
## c. SUP_LOC
hist(clean_picks$SUP_LOC)
hist(log(clean_picks$SUP_LOC))
## d. SECONDS_PER_PICK
hist(clean_picks$SECONDS_PER_PICK)
hist(log(clean_picks$SECONDS_PER_PICK)) #better

clean_picks <- clean_picks %>% filter(DISTANCE != -Inf)
# model
test_model = lm(formula = log(SECONDS_PER_PICK) ~ FLOOR + SIDE + LR_ALLEY + 
                  TURNS + DISTANCE + RET_LOC + SUP_LOC + FLOOR:SIDE + FLOOR:LR_ALLEY + 
                  FLOOR:TURNS + FLOOR:DISTANCE + FLOOR:RET_LOC + FLOOR:SUP_LOC + 
                  SIDE:LR_ALLEY + SIDE:TURNS + SIDE:DISTANCE + SIDE:RET_LOC + 
                  LR_ALLEY:TURNS + LR_ALLEY:DISTANCE + LR_ALLEY:RET_LOC + LR_ALLEY:SUP_LOC + 
                  TURNS:DISTANCE + TURNS:RET_LOC + TURNS:SUP_LOC + DISTANCE:RET_LOC + 
                  DISTANCE:SUP_LOC + RET_LOC:SUP_LOC, data = clean_picks)
summary(test_model)


# search for better 
test_model %>% step()

proposed <- lm(formula = log(SECONDS_PER_PICK) ~ FLOOR + SIDE + LR_ALLEY + 
                 TURNS + DISTANCE + RET_LOC + SUP_LOC + FLOOR:SIDE + FLOOR:LR_ALLEY + 
                 FLOOR:DISTANCE + FLOOR:RET_LOC + FLOOR:SUP_LOC + SIDE:LR_ALLEY + 
                 SIDE:RET_LOC + LR_ALLEY:DISTANCE + LR_ALLEY:RET_LOC + LR_ALLEY:SUP_LOC + 
                 TURNS:DISTANCE + TURNS:RET_LOC + TURNS:SUP_LOC + DISTANCE:RET_LOC + 
                 DISTANCE:SUP_LOC + RET_LOC:SUP_LOC, data = clean_picks)

summary(proposed)


model <- lm(log(SECONDS_PER_PICK) ~ ., clean_picks)


# Function
coeffs = coef(proposed)
f <- function(x){
  coeffs[1] + coeffs[2]*x[1] + coeffs[3]*x[2] + coeffs[4]*x[3] +
    coeffs[5]*x[4] + coeffs[6]*x[5] + coeffs[7]*x[6] +
    coeffs[8]*x[7]
}


view(picks)
model
summary(model)

# f <- function(floor, side, alley, turns, distance, ret_loc, sup_loc){
#   coeffs[1] + coeffs[2]*floor + coeffs[3]*side + coeffs[4]*alley +
#     coeffs[5]*turns + coeffs[6]*distance + coeffs[7]*ret_loc +
#     coeffs[8]*sup_loc
# }


# floor (0,3)
# side (1,2)
# lr_alley (1,2) <- wywalic niejednolite
# turns (0,Inf)
# distance (0, Inf)
# suma pickow (0, Inf)
# sup_loc - liczba lokacji, sup_to_pick - liczba pobranych itemow 
# sex - 0 kobieta

res <- optim(rep(5,7),f, method="L-BFGS-B", lower=rep(0,7), upper=rep(10,7))


res <- optimize(f, lower = 0, upper = 5, maximum = FALSE)


hist(picks$SECONDS_PER_PICK)






car::vif(test_model)



clean_picks %>% gather() %>% head()
ggplot(gather(clean_picks), aes(log(value))) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')
