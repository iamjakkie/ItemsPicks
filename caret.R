library('caret')
library('tidyverse')
library("naniar")
library("visdat")
library("rpart")
library("recipes")
library("rpart.plot")
picks <- read.csv('Data/picks.csv')
glimpse(picks)
clean_picks <- select(picks, -RET_TO_PICK, -SUP_TO_PICK, -DURATION)

set.seed(1)
ind <- createDataPartition(y = clean_picks$SECONDS_PER_PICK, p=0.7, list=FALSE)
train <- slice(clean_picks, ind)
test <- slice(clean_picks, -ind)

skimr::skim(train)

hist(log(train$SECONDS_PER_PICK), br=30)


rec <- recipe(SECONDS_PER_PICK ~ ., train) %>%
  step_log(SECONDS_PER_PICK) %>%
  prep(train)

train <- bake(rec,train)
test <- bake(rec, test)


controls <- trainControl(method = "repeatedcv", number=10, repeats=5, verboseIter = TRUE)

m_lm <- train(SECONDS_PER_PICK ~ ., train, method = "lm", trControl = controls)
m_lm
summary(m_lm$finalModel)
varImp(m_lm)
car::vif(m_lm$finalModel)

m_rpart <- train(SECONDS_PER_PICK ~ ., train, method = "rpart", trControl=controls)
m_rpart$finalModel %>% rpart.plot(cex=0.6)


results <- tibble(
  lm = predict(m_lm, test),
  rpart = predict(m_rpart, test),
  SECONDS_PER_PICK = test$SECONDS_PER_PICK
)

results %>% select(-SECONDS_PER_PICK) %>% map(postResample, obs = results$SECONDS_PER_PICK)
results %>% 
  gather(key = "model", value = "pred", 1:3) %>%
  ggplot(aes(pred, fill = model)) + geom_density(alpha=0.5)


m_xgb <- train(SECONDS_PER_PICK ~ ., train, method = "xgbTree",
                trControl = controls)
postResample(predict(m_xgb, test), obs = results$SECONDS_PER_PICK)