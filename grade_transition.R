# setwd('/home/markp/projects/data_science/bc_education')

library(tidyverse)
library(scales)

# load data
df = read.csv('grade_transition.csv')
df2 = df[df$BASE_YEAR=='2015/2016',]
df3 = df2[(df2$DATA_LEVEL=='District Level' | 
            df2$PUBLIC_OR_INDEPENDENT=='BC Independent School' & df2$DATA_LEVEL=='Province Level')
          & (df2$SUB_POPULATION=='Indigenous' | df2$SUB_POPULATION=='Non Indigenous'),]

gtdf = df3[, c('BASE_YEAR', 'PUBLIC_OR_INDEPENDENT', 'DISTRICT_NUMBER',
                  'STUDENT_GRADE', 'SUB_POPULATION', 
                  'DENOMINATOR_STUDIED_COHORT', 'NUMERATOR_NUM_SUCCESSFUL')]

gtdf$BASE_YEAR = as.numeric(substr(gtdf$BASE_YEAR, 1, 4))

gtdf$PUBLIC_OR_INDEPENDENT = as.character(gtdf$PUBLIC_OR_INDEPENDENT)
gtdf[gtdf$PUBLIC_OR_INDEPENDENT=='BC Independent School',]$PUBLIC_OR_INDEPENDENT = 0
gtdf[gtdf$PUBLIC_OR_INDEPENDENT=='BC Public School',]$PUBLIC_OR_INDEPENDENT = 1
gtdf$PUBLIC_OR_INDEPENDENT = as.factor(gtdf$PUBLIC_OR_INDEPENDENT)
gtdf$PUBLIC_OR_INDEPENDENT
gtdf$SUB_POPULATION = as.character(gtdf$SUB_POPULATION)
gtdf[gtdf$SUB_POPULATION=='Indigenous',]$SUB_POPULATION=1
gtdf[gtdf$SUB_POPULATION=='Non Indigenous',]$SUB_POPULATION=0

gtdf[is.na(gtdf$DISTRICT_NUMBER),]$DISTRICT_NUMBER=0
gtdf$DISTRICT_NUMBER = as.factor(gtdf$DISTRICT_NUMBER)

gtdf$DENOMINATOR_STUDIED_COHORT = as.character(gtdf$DENOMINATOR_STUDIED_COHORT)
gtdf$NUMERATOR_NUM_SUCCESSFUL = as.character(gtdf$NUMERATOR_NUM_SUCCESSFUL)

gtdf = gtdf[gtdf$DENOMINATOR_STUDIED_COHORT!='Msk' & gtdf$NUMERATOR_NUM_SUCCESSFUL!='Msk',]

gtdf$DENOMINATOR_STUDIED_COHORT = as.numeric(gtdf$DENOMINATOR_STUDIED_COHORT)
gtdf$NUMERATOR_NUM_SUCCESSFUL = as.numeric(gtdf$NUMERATOR_NUM_SUCCESSFUL)

colnames(gtdf) = c('base_year', 'is_public', 'district_number', 'student_grade',
                   'is_indigenous', 'total', 'successful')
rownames(gtdf) = 1:length(gtdf$base_year)
head(gtdf)
gtdf$is_public
gtdf$is_public = as.numeric(gtdf$is_public)
gtdf$is_indigenous = as.numeric(gtdf$is_indigenous)
gtdf$success_rate = gtdf$successful / gtdf$total
write.csv(gtdf, 'grade_transition-R.csv', row.names=FALSE)

head(gtdf)

gtdf$is_public

# plot rate by indigeneity
agg_df = gtdf %>% group_by(is_indigenous) %>% 
  summarise(total=sum(total), successful=sum(successful))
agg_df$success_rate = agg_df$successful / agg_df$total
agg_df$is_indigenous = as.character(agg_df$is_indigenous)
agg_df[agg_df$is_indigenous==1,]$is_indigenous = 'Indigenous'
agg_df[agg_df$is_indigenous==0,]$is_indigenous = 'Non-Indigenous'

ggplot(aes(is_indigenous, success_rate), data=agg_df) + 
  geom_bar(stat='identity', fill='indianred3') + 
  scale_y_continuous(limits=c(0.95, 1), 
                     oob=rescale_none, 
                     labels=scales::percent) +
  xlab('') + ylab('Transition Rate') +
  ggtitle('Transition Rate by Indigeneity')

# plot rate by public/private
agg_df = gtdf %>% group_by(is_public) %>% 
  summarise(total=sum(total), successful=sum(successful))
agg_df$success_rate = agg_df$successful / agg_df$total
agg_df$is_public = as.character(agg_df$is_public)
agg_df[agg_df$is_public==1,]$is_public = 'Public'
agg_df[agg_df$is_public==0,]$is_public = 'Independent'

ggplot(aes(is_public, success_rate), data=agg_df) + 
  geom_bar(stat='identity', fill='indianred3') + 
  scale_y_continuous(limits=c(0.95, 1), 
                     oob=rescale_none, 
                     labels=scales::percent) +
  xlab('') + ylab('Transition Rate') +
  ggtitle('Transition Rate by School Type')

# plot rate by grade
agg_df = gtdf %>% group_by(is_indigenous, student_grade) %>% 
  summarise(total=sum(total), successful=sum(successful))
agg_df$success_rate = agg_df$successful / agg_df$total
agg_df$is_indigenous = as.character(agg_df$is_indigenous)
agg_df[agg_df$is_indigenous==1,]$is_indigenous = 'Indigenous'
agg_df[agg_df$is_indigenous==0,]$is_indigenous = 'Non-Indigenous'

agg_df = rename(agg_df, 'Sub-Population'=is_indigenous)

ggplot(aes(student_grade, success_rate), data=agg_df) + 
  geom_line(aes(color=`Sub-Population`)) + 
  geom_point(aes(color=`Sub-Population`)) +
  scale_y_continuous(labels=scales::percent) +
  xlab('Grade') + ylab('Transition Rate') +
  ggtitle('Transition Rate by Grade')
# fit base model
mod1 = glm(cbind(successful, total-successful) ~ is_public + student_grade + is_indigenous, 
           family='binomial',
           data=gtdf)

summary(mod1)

exp(mod1$coefficients)

preds = c()

for (is_public in 0:1) {
  for (is_indigenous in 0:1) {
    for (student_grade in 1:12) {
      coef1 = as.numeric(mod1$coefficients)
      logit_p = coef1[1] + coef1[2] * is_public + coef1[3] * student_grade + coef1[4] * is_indigenous
      p = exp(logit_p) / (1 + exp(logit_p))
      preds = rbind(preds, c(is_public, is_indigenous, student_grade, logit_p, p))
    }
  }
}

preds = as.data.frame(preds)
colnames(preds) = c('is_public', 'is_indigenous', 'student_grade', 'logit_p', 'p')

gtdf$fitted = mod1$fitted.values
gtdf$fit_success = round(gtdf$fitted * gtdf$total)
head(gtdf)

summary(mod1)
mod2 = glm(cbind(successful, total-successful) ~ student_grade + is_indigenous, 
           family='binomial',
           data=gtdf)
summary(mod2)

mod2$deviance - mod1$deviance
mod2$df.residual - mod1$df.residual

pchisq(mod2$deviance-mod1$deviance, mod2$df.residual-mod1$df.residual, lower.tail=FALSE)
anova(mod2, mod1, test='Chisq')

mod3 = glm(cbind(successful, total-successful) ~ student_grade + is_indigenous*is_public,
           family='binomial',
           data=gtdf)
summary(mod3)

pchisq(mod1$deviance - mod3$deviance,
       mod1$df.residual - mod3$df.residual,
       lower.tail=FALSE)

anova(mod1, mod3)

mod3$aic - mod1$aic

predict(mod3)

summary(mod1)

pchisq((0.063606 / 0.026806)**2, 1, lower.tail=FALSE)

gtdf$odds = gtdf$fitted / (1 - gtdf$fitted)
head(gtdf)
gtdf[gtdf$is_indigenous==1,]$odds / gtdf[gtdf$is_indigenous==0,]$odds

exp(mod1$coefficients)

gtdf
co = 0.5

min(gtdf$success_rate)


                                        