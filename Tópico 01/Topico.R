library(ggplot2)

LungDisease <- read.csv('~/ESTAT-DS2/Tópico 01/LungDisease.csv')

ggplot(LungDisease, aes(y=PEFR,x=Exposure)) + geom_point()
cor(LungDisease$Exposure,LungDisease$PEFR)

model <- lm(PEFR ~ Exposure, data= LungDisease)
ggplot(data=LungDisease, aes(y=PEFR, x= Exposure)) + 
  geom_point() + geom_abline(slope = model$coefficients[2],intercept=model$coefficients[1]) +
  geom_smooth()

summary(LungDisease$PEFR)

house <- read.csv('~/ESTAT-DS2/Tópico 01/house_sales.csv')

# Regressão Múltipla
head(house[, c("AdjSalePrice", "SqFtTotLiving", "SqFtLot", "Bathrooms", 
               "Bedrooms", "BldgGrade")])

house_lm <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + 
                 Bedrooms + BldgGrade,  
               data=house, na.action=na.omit)

house_lm

summary(house_lm)

summary(house$BldgGrade)

summary(house$AdjSalePrice)

indice_teste <- sample(nrow(LungDisease),nrow(LungDisease)*0.2)
teste <- LungDisease[indice_teste,]
treino <- LungDisease[-indice_teste,]

# Variáveis Dummy
head(house[,"PropertyType"])
levels(house[,"PropertyType"])
prop_type_dummies <- model.matrix(~PropertyType -1, data=house)
head(prop_type_dummies)

# Interações

library(dplyr)
table(house$ZipCode)
zip_groups <- house %>%
  mutate(resid = residuals(house_lm)) %>%
  group_by(ZipCode) %>%
  summarize(med_resid = median(resid),
            cnt = n()) %>%
  # sort the zip codes by the median residual
  arrange(med_resid) %>%
  mutate(cum_cnt = cumsum(cnt),
         ZipGroup = factor(ntile(cum_cnt, 5)))
house <- house %>%
  left_join(select(zip_groups, ZipCode, ZipGroup), by='ZipCode')
house_iter <- lm(AdjSalePrice ~ SqFtTotLiving*ZipGroup + SqFtLot +
                   Bathrooms + Bedrooms + BldgGrade + PropertyType, data=house, na.action=na.omit)
house_iter

# Regressão polinomial
house_98105 <- house[house$ZipCode == 98105,]
lm_poly <- lm(AdjSalePrice ~  poly(SqFtTotLiving, 2) + SqFtLot + 
                BldgGrade +  Bathrooms +  Bedrooms,
              data=house_98105)
terms <- predict(lm_poly, type='terms')
partial_resid <- resid(lm_poly) + terms

df <- data.frame(SqFtTotLiving = house_98105[, 'SqFtTotLiving'],
                 Terms = terms[, 1],
                 PartialResid = partial_resid[, 1])
ggplot(df, aes(SqFtTotLiving, PartialResid)) +
  geom_point(shape=1) + scale_shape(solid = FALSE) +
  geom_smooth(linetype=2) + 
  geom_line(aes(SqFtTotLiving, Terms))+
  theme_bw()

# GAM / Spline
library(mgcv)
lm_gam <- gam(AdjSalePrice ~ s(SqFtTotLiving) + SqFtLot +  Bathrooms +  Bedrooms + BldgGrade, 
                 data=house_98105)
terms <- predict.gam(lm_gam, type='terms')
partial_resid <- resid(lm_gam) + terms

df <- data.frame(SqFtTotLiving = house_98105[, 'SqFtTotLiving'],
                    Terms = terms[, 5], PartialResid = partial_resid[, 5])
ggplot(df, aes(SqFtTotLiving, PartialResid)) +
  geom_point(shape=1) + scale_shape(solid = FALSE) +
  geom_smooth(linetype=2) + 
  geom_line(aes(SqFtTotLiving, Terms))  +
  theme_bw()