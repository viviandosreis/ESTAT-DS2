library(mgcv)
library(ggplot2)

loan_data <- read.csv("E:\\Desktop\\Estatistica II\\loan_data.csv")
loan_data$outcome <- ordered(loan_data$outcome, levels=c('paid off', 'default'))

# Modelo NaiveBayes
library(klaR)
naive_model <- NaiveBayes(outcome ~ purpose_ + home_ + emp_len_, 
                          data = na.omit(loan_data))
naive_model$table
# Vendo probabilidade em Naive Bayes
new_loan <- loan_data[147, c('purpose_', 'home_', 'emp_len_')]
row.names(new_loan) <- NULL
new_loan
predict(naive_model, new_loan)

# Modelo Logístico (family = binomial)
logistic_gam <- gam(outcome ~ s(payment_inc_ratio) + purpose_ + 
                      home_ + emp_len_ + s(borrower_score),
                    data=loan_data, family='binomial')
# Visualização
terms <- predict(logistic_gam, type='terms')
partial_resid <- resid(logistic_gam) + terms
df <- data.frame(payment_inc_ratio = loan_data[, 'payment_inc_ratio'],
                 terms = terms[, 's(payment_inc_ratio)'],
                 partial_resid = partial_resid[, 's(payment_inc_ratio)'])

ggplot(df, aes(x=payment_inc_ratio, y=partial_resid, solid = FALSE)) +
  geom_point(shape=46, alpha=.4) +
  geom_line(aes(x=payment_inc_ratio, y=terms), 
            color='red', alpha=.5, size=1.5) +
  labs(y='Partial Residual') +
  xlim(0, 25) +
  theme_bw()
# Valores em cima correspondem a 1 e o outro a 0