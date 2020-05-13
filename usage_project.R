library(tidyverse)
library(modelr)
library(scatterplot3d)
library(faraway)

# ----------------- Data Setup ---------------------------------------

abbreviations = read.csv('abbreviations.csv', header=FALSE, col.names=c('team', 'abbreviation'))

# Clean data from a given year 
# usg_mean -> mean usage rate for each team
# usg_mean_weighted -> weighted mean usage for minutes played
# usg_max -> max usage rate for each team
# usg_med -> median usage rate for each team

clean_data = function() {
  astats = read_csv(paste('astats', '.csv', sep=''))
  tstats = read_csv(paste('tstats', '.csv', sep=''))
  head(astats)
  head(tstats)
  astats = astats %>%
    select(Tm, 'USG%', MP, G, Season) %>%
    rename(abbreviation=Tm, usg='USG%', mp=MP, g=G, year=Season) %>%
    mutate(mpg = as.numeric(mp)/as.numeric(g), weighted_usg = as.numeric(usg)*as.numeric(mp)) %>%
    filter(mpg > 12) %>%
    group_by(abbreviation, year) %>%
    summarize(usg_mean=mean(as.numeric(usg)), usg_mean_weighted=sum(as.numeric(weighted_usg))/sum(as.numeric(mp)), usg_max=max(as.numeric(usg)), usg_med = median(as.numeric(usg)))
  astats = inner_join(astats, abbreviations)
  tstats = tstats %>%
    rename(team=Team, year=Season) %>%
    mutate(wins=as.numeric(substr(as.character(Overall), 1, 2))) %>%
    select(team, wins, year)
  inner_join(tstats, astats, by=c("team"="team", "year"="year")) %>%
    select(-c("abbreviation"))
}

data = data.frame(team=character(), wins=integer(), usg_mean=double(), usg_max=double(), year=integer())
data = rbind(data, clean_data())
head(data)

# -------------------- Diagnostic/Summary Methods ------------------------

str_format = function(string){
  words = strsplit(string, " ")[[1]]
  paste(toupper(substring(words, 1, 1)), substring(words, 2), sep='', collapse=" ")
}

# Plot predictor vs. response with best fit line
slr_summary = function(rvar, pvar, rstr, pstr){
  lmod = lm(rvar ~ pvar)
  print(summary(lmod))
  ggplot() +
    geom_point(mapping=aes(pvar, rvar)) +
    labs(title=paste(str_format(pstr), 'vs.', str_format(rstr))) +
    xlab(str_format(pstr)) +
    ylab(str_format(rstr)) + 
    geom_abline(slope=coef(lmod)[[2]], intercept=coef(lmod)[[1]])
}

# Plot fitted vs. residuals to check for non-constant variance and model structure
slr_residuals = function(rvec, pvec, rstr, pstr){
  lmod = lm(rvec ~ pvec)
  ggplot() +
    geom_point(mapping=aes(fitted(lmod), residuals(lmod))) +
    labs(title=paste("Fitted vs. Residuals (", str_format(rstr), " ~ ", str_format(pstr), ")", sep="")) +
    xlab("Fitted") +
    ylab("Residuals") +
    geom_hline(yintercept=0)
}

# QQ plot for normality
slr_qq = function(rvec, pvec) {
  lmod = lm(rvec ~ pvec) 
  qqnorm(residuals(lmod), ylab="Residuals")
  qqline(residuals(lmod))
}

# --------------------- Mean Usgage --------------------------------------

# SLR: wins ~ mean usage
slr_summary(data$wins, data$usg_mean, 'wins', 'mean usage')
# Mean usage seems insignificant
# Very little predictive power

# Residual vs. fitted (constant variance)
slr_residuals(data$wins, data$usg_mean, 'wins', 'mean usage')
# Indicates constant variance

# QQ-plot (normality)
slr_qq(data$wins, data$usg_mean)
# Relatively normal 

# --------------------- Weighted Mean Usgage --------------------------------------

# SLR: wins ~ weighted mean usage
slr_summary(data$wins, data$usg_mean_weighted, 'wins', 'weighted mean usage')
# Weighted mean usage seem significant, but it is a large sample
# adj r-squared is small

# Residual vs. fitted (constant variance)
slr_residuals(data$wins, data$usg_mean_weighted, 'wins', 'mean_usage')
# Indicates non-constant variance for fitted values above and below 45

# QQ-plot (normality)
slr_qq(data$wins, data$usg_mean_weighted)
# Relatively normal 

# ------------------ Max Usage ---------------------------------

# SLR: wins ~ max usage
slr_summary(data$wins, data$usg_max, 'wins', 'max usage')
# Max usage seems significant, but adj r-squared is small

# Residual vs. fitted (constant variance)
slr_residuals(data$wins, data$usg_max, 'wins', 'max usage')
# Similar issues to previous SLR model

# QQ-Plot (normality)
slr_qq(data$wins, data$usg_max)
# Relatively normal

# Use standard plots to check for influential observations
plot(lmod_max)
# No points to worry about

# ------------------------------- Median Usage -------------------

# SLR: wins ~ median usage
slr_summary(data$wins, data$usg_med, 'wins', 'median usage')
# Median usage seems significant, but adj r-squared is small

# Residuals vs. Fitted Values (constant variance)
slr_residuals(data$wins, data$usg_med, 'wins', 'median usage')
# Indicates constant variance

# QQ-Plot (normality)
slr_qq(data$wins, data$usg_med)
# Relatively normal

# ------------------------------ MLR -----------------------------------

# MLR: wins ~ mean usage + weighted mean usage + max usage + median usage
lmod_mlr = lm(wins ~ usg_mean + usg_mean_weighted + usg_max + usg_med, data)
summary(lmod_mlr)
# All predictors seem significant, but the adj r-squared is still small

# Collinearity 
ggplot(data) + 
  geom_point(mapping=aes(usg_mean, usg_mean_weighted, color=wins)) + 
  scale_color_gradient(low="blue", high="red") +
  labs(title="Mean Usage vs. Weighted Mean Usage") + 
  xlab("Mean Usage") +
  ylab("Weighted Mean Usage")
# Little collinearity 
ggplot(data) + 
  geom_point(mapping=aes(usg_mean, usg_max, color=wins)) + 
  scale_color_gradient(low="blue", high="red") +
  labs(title="Mean Usage vs. Max Usage") + 
  xlab("Mean Usage") +
  ylab("Max Usage")
# No collinearity
ggplot(data) + 
  geom_point(mapping=aes(usg_mean, usg_med, color=wins)) + 
  scale_color_gradient(low="blue", high="red") +
  labs(title="Mean Usage vs. Med Usage") + 
  xlab("Mean Usage") +
  ylab("Med Usage")
# Solid collinearity
ggplot(data) + 
  geom_point(mapping=aes(usg_mean_weighted, usg_max, color=wins)) + 
  scale_color_gradient(low="blue", high="red") +
  labs(title="Weighted Mean Usage vs. Max Usage") + 
  xlab("Weighted Mean Usage") +
  ylab("Max Usage")
# No collinearity
ggplot(data) + 
  geom_point(mapping=aes(usg_mean_weighted, usg_med, color=wins)) + 
  scale_color_gradient(low="blue", high="red") +
  labs(title="Weighted Mean Usage vs. Median Usage") + 
  xlab("Weighted Mean Usage") +
  ylab("Median Usage")
# Little collinearity
ggplot(data) + 
  geom_point(mapping=aes(usg_max, usg_med, color=wins)) + 
  scale_color_gradient(low="blue", high="red") +
  labs(title="Max Usage vs. Median Usage") + 
  xlab("Max Usage") +
  ylab("Median Usage")
# Some collinearity between max and median usage (negative relationship)

# Covariance
cov(data$usg_mean, data$usg_mean_weighted, use="pairwise.complete.obs")
# Small covariance
cov(data$usg_mean, data$usg_max, use="pairwise.complete.obs")
# Medium covariance
cov(data$usg_mean, data$usg_med, use="pairwise.complete.obs")
# Medium covariance
cov(data$usg_mean_weighted, data$usg_max, use="pairwise.complete.obs")
# Small covariance
cov(data$usg_mean_weighted, data$usg_med, use="pairwise.complete.obs")
# No covariance
cov(data$usg_max, data$usg_med, use="pairwise.complete.obs")
# High covariance

# VIF 
vif(model.matrix(lmod_mlr))
# small vif values indicate minimal collinearity

# Residuals vs. Fitted (constant variance)
data = data %>%
  add_predictions(lmod_mlr, var="pred_mlr") %>%
  add_residuals(lmod_mlr, var="resid_mlr")
ggplot(data) + 
  geom_point(mapping=aes(pred_mlr, resid_mlr, color=(pred_mlr<47.5))) + 
  geom_hline(yintercept=0) +
  labs(title="Residual Plot (Wins ~ Max Usage + Mean Usage)") +
  xlab("Fitted Values") +
  ylab("Residuals") + 
  geom_vline(xintercept=47.5, color='red') +
  theme(legend.position="none")
# Indicates groups similar to before

# F-test
var.test(residuals(lmod_mlr)[data$pred_mlr>47.5], residuals(lmod_max)[data$pred_mlr<47.5])
# indicates difference between groups

# QQ-plot (normality)
qqnorm(residuals(lmod_mlr), ylab="Residuals")
qqline(residuals(lmod_mlr))
# Relatively normal

# Back elimination
summary(lmod_mlr)
lmod_mlr = update(lmod_mlr, .~.-usg_mean)
summary(lmod_mlr)
lmod_mlr = update(lmod_mlr, .~.-usg_med)
summary(lmod_mlr)
# Got rid of the underlying collinearity, but decreased adj r-squared

# Prediction plane plot
colors <- c("#FF0000", "#008CFF")
colors = colors[as.numeric(data$wins > 50)+1]
p1 = scatterplot3d(x=data$usg_mean_weighted, y=data$usg_max, z=data$wins, pch=16, type='h', color=colors, xlab="Weighted Mean Usage", ylab="Max Usage", zlab="Wins")
p1$plane3d(lmod_mlr)
legend("right", legend=c("Below 50 Wins", "Above 50 Wins"), col=c("#FF0000", "#008CFF"), pch=16)
title("MLR (Wins ~ Weighted Mean Usage + Max Usage)")
# High win teams (arbitrary choice of 50 wins) are spread all over
# Indicates not much predictive power over wins

# ---------------- Principal Components -------------------------

# Principal components
prdata = prcomp(data[,3:6], scale=TRUE)
summary(prdata)
# All 4 principal components seem to contribute to the total variance

# Rotation matrix
prdata$rot
# First component measures similarity betwen median and mean, while accounting for low max?
# Second component measure weighted mean?
# Third component ? 
# Fourth component ? 

# PC: wins ~ principal components
lmod_pcr = lm(data$wins ~ prdata$x)
summary(lmod_pcr)
lmod_pcr = lm(data$wins ~prdata$x[,1:2])
summary(lmod_pcr)

# --------------------------- Interaction -----------------------

# MLR with interaction: all interaction terms for original MLR are included
lmod_interaction = lm(wins ~ usg_mean + usg_mean_weighted + usg_max + usg_med + I(usg_mean * usg_max) + I(usg_mean * usg_med) + I(usg_mean * usg_mean_weighted) + I(usg_mean_weighted * usg_max) + I(usg_mean_weighted * usg_med) + I(usg_max * usg_med), data)
summary(lmod_interaction)
# Indicates similar predictive power as previous mlr model, but less significant predictors

# Backwards elimination
lmod_interaction = update(lmod_interaction, .~.-I(usg_max * usg_med))
summary(lmod_interaction)
lmod_interaction = update(lmod_interaction, .~.-I(usg_mean_weighted * usg_med))
summary(lmod_interaction)
lmod_interaction = update(lmod_interaction, .~.-I(usg_mean * usg_med))
summary(lmod_interaction)
lmod_interaction = update(lmod_interaction, .~.-I(usg_mean_weighted * usg_max))
summary(lmod_interaction)
lmod_interaction = update(lmod_interaction, .~.-I(usg_mean * usg_mean_weighted))
summary(lmod_interaction)
# Final interaction term is (mean usage * max usage)

# Residauls vs. fitted values (constant variance)
data = data %>%
  add_predictions(lmod_interaction, var="pred_interaction") %>%
  add_residuals(lmod_interaction, var="resid_interaction")
ggplot(data) + 
  geom_point(mapping=aes(pred_med, resid_med)) + 
  geom_hline(yintercept=0) +
  labs(title="Residual Plot (Wins ~ + Interactive Term)") +
  xlab("Fitted Values") +
  ylab("Residuals")
# Constant variance

# QQ-plot (normality)
qqnorm(residuals(lmod_interaction), ylab="Residuals")
qqline(residuals(lmod_interaction))
# Relatively normal


