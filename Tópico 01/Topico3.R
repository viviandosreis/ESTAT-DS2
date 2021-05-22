library(ggplot2)

loan3000 <- read.csv("E:\\Desktop\\Estatistica II\\loan3000.csv")
loan3000$outcome <- ordered(loan3000$outcome, levels=c('paid off', 'default'))

library(rpart)
loan_tree <- rpart(outcome ~ borrower_score + payment_inc_ratio,
                   data=loan3000, 
                   control = rpart.control(cp=.005))
plot(loan_tree, uniform=TRUE, margin=.05)
text(loan_tree, cex=.75)
# Ou então um gráfico mais bonito
library(rattle)
fancyRpartPlot(loan_tree, cex = 0.8)

# Particionamento recursivo

r_tree <- data.frame(x1 = c(0.575, 0.375, 0.375, 0.375, 0.475),
                     x2 = c(0.575, 0.375, 0.575, 0.575, 0.475),
                     y1 = c(0,         0, 10.42, 4.426, 4.426),
                     y2 = c(25,       25, 10.42, 4.426, 10.42),
                     rule_number = factor(c(1, 2, 3, 4, 5)))
r_tree <- as.data.frame(r_tree)

labs <- data.frame(x=c(.575 + (1-.575)/2, 
                       .375/2, 
                       (.375 + .575)/2,
                       (.375 + .575)/2, 
                       (.475 + .575)/2, 
                       (.375 + .475)/2
                      ),
                    y=c(12.5, 
                        12.5,
                        10.42 + (25-10.42)/2,
                        4.426/2, 
                        4.426 + (10.42-4.426)/2,
                        4.426 + (10.42-4.426)/2
                    ),
                    decision = factor(c('paid off', 'default',
                        'default', 'paid off', 'paid off',
                        'default')))



ggplot(data=loan3000, aes(x=borrower_score, y=payment_inc_ratio)) +
  geom_point( aes(color=outcome, shape=outcome), alpha=.5) +
  scale_color_manual(values=c('blue', 'red')) +
  scale_shape_manual(values = c(1, 46)) +
  # scale_shape_discrete(solid=FALSE) +
  geom_segment(data=r_tree, aes(x=x1, y=y1, xend=x2, yend=y2, linetype=rule_number), size=1.5, alpha=.7) +
  guides(colour = guide_legend(override.aes = list(size=1.5)),
         linetype = guide_legend(keywidth=3, override.aes = list(size=1))) +
  scale_x_continuous(expand=c(0,0)) + 
  scale_y_continuous(expand=c(0,0), limits=c(0, 25)) + 
  geom_label(data=labs, aes(x=x, y=y, label=decision)) +
  theme_bw()


# Floresta aleatória
loan3000 <- read.csv('loan3000.csv')
loan3000$outcome <- ordered(loan3000$outcome, levels=c('paid off', 'default'))
library(randomForest)
rf <- randomForest(outcome ~ borrower_score + payment_inc_ratio,
                   data=loan3000)

rf
error_df = data.frame(error_rate = rf$err.rate[,'OOB'],
                      num_trees = 1:rf$ntree)
ggplot(error_df, aes(x=num_trees, y=error_rate)) +
  geom_line()  +
  theme_bw()

# Variáveis de importância
library(dplyr)
loan_data <- read.csv("E:\\Desktop\\Estatistica II\\loan_data.csv")
loan_data <- select(loan_data, -X, -status)

rf_all <- randomForest(outcome ~ ., data=loan_data, importance=TRUE)
rf_all

varImpPlot(rf_all, type=1)
## ps.: demora...

# XGBoost
library(xgboost)
predictors <- data.matrix(loan3000[, c('borrower_score', 'payment_inc_ratio')])
label <- as.numeric(loan3000[,'outcome'])-1
xgb <- xgboost(data=predictors, label=label, objective = "binary:logistic", 
               params=list(subsample=.63, eta=0.1), nrounds=100)

# Erro train vs test
set.seed(42)
predictors <- data.matrix(loan_data[,-which(names(loan_data) %in% 'outcome')])
label <- as.numeric(loan_data$outcome)-1
test_idx <- sample(nrow(loan_data), 10000)

xgb_default <- xgboost(data=predictors[-test_idx,], label=label[-test_idx], 
                       objective = "binary:logistic", nrounds=250, verbose=0)
pred_default <- predict(xgb_default, predictors[test_idx,])
error_default <- abs(label[test_idx] - pred_default) > 0.5
xgb_default$evaluation_log[250,]
mean(error_default)

# Penalizando crescimento
xgb_penalty <- xgboost(data=predictors[-test_idx,], 
                       label=label[-test_idx], 
                       params=list(eta=.1, subsample=.63, lambda=1000),
                       objective = "binary:logistic", nrounds=250, verbose=0)
pred_penalty <- predict(xgb_penalty, predictors[test_idx,])
error_penalty <- abs(label[test_idx] - pred_penalty) > 0.5
xgb_penalty$evaluation_log[250,]
mean(error_penalty)

error_default <- rep(0, 250)
error_penalty <- rep(0, 250)
for(i in 1:250)
{
  pred_default <- predict(xgb_default, predictors[test_idx,], ntreelimit = i)
  error_default[i] <- mean(abs(label[test_idx] - pred_default) > 0.5)
  pred_penalty <- predict(xgb_penalty, predictors[test_idx,], ntreelimit = i)
  error_penalty[i] <- mean(abs(label[test_idx] - pred_penalty) > 0.5)
}

errors <- rbind(xgb_default$evaluation_log,
                xgb_penalty$evaluation_log,
                data.frame(iter=1:250, train_error=error_default),
                data.frame(iter=1:250, train_error=error_penalty))
errors$type <- rep(c('default train', 'penalty train', 
                     'default test', 'penalty test'), rep(250, 4))

ggplot(errors, aes(x=iter, y=train_error, group=type)) +
  geom_line(aes(linetype=type, color=type), size=1) +
  scale_linetype_manual(values=c('solid', 'dashed', 'dotted', 'longdash')) +
  theme_bw() +
  theme(legend.key.width = unit(1.5,"cm")) +
  labs(x="Iterations", y="Error") +
  guides(colour = guide_legend(override.aes = list(size=1)))

# Hiperparametros

N <- nrow(loan_data)
fold_number <- sample(1:5, N, replace = TRUE)
params <- data.frame(eta = rep(c(.1, .5, .9), 3),
                     max_depth = rep(c(3, 6, 12), rep(3,3)))
rf_list <- vector('list', 9)
error <- matrix(0, nrow=9, ncol=5)
for(i in 1:nrow(params)){
  for(k in 1:5){
    cat('Fold', k, 'for model', i, '\n')
    fold_idx <- (1:N)[fold_number == k]
    xgb <- xgboost(data=predictors[-fold_idx,], label=label[-fold_idx], 
                   params = list(eta = params[i, 'eta'], 
                                 max_depth = params[i, 'max_depth']),
                   objective = "binary:logistic", nrounds=100, verbose=0)
    pred <- predict(xgb, predictors[fold_idx,])
    error[i, k] <- mean(abs(label[fold_idx] - pred) >= 0.5)
  }
}

avg_error <- 100 * round(rowMeans(error), 4)
cbind(params, avg_error)