library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)
library(corrplot)
library(grid) 
library(matlib)

##intial plots to understand what this data looks like: correlations, distributions,
# any seasonal patterns 
#correlation structure
cor.mat <- cor(observations[2:6])
par(mfrow = c(1,2))
corrplot(cor.mat, method="pie",  col = COL2('PuOr', 10), 
         tl.col="black", number.cex=0.7, tl.cex=0.7)
corrplot(cor.mat, method="number", col = COL2('PuOr', 10), 
         tl.col="black", number.cex=0.7, tl.cex=0.7)
# density plots

density_plots <- function(var) {
  ggplot(observations[2:6], aes_string(x = var)) + 
    geom_histogram(fill = "pink", col = "grey20")
}
plot_list <- lapply(colnames(observations[2:6]), density_plots)
grid.arrange(grobs=plot_list, nrow=3, ncol = 2, as.table=FALSE)

#time series plots of all the data
p <- ggplot(observations, aes(x =YM, y =WSPD)) + geom_line() + 
  xlab("Time") + ylab("WSPD (m/s)")
p2 <- ggplot(observations, aes(x =YM, y =PRES)) + geom_line() + 
  xlab("Time") + ylab("PRES (hPa)")
p3 <- ggplot(observations, aes(x =YM, y =ATMP)) + geom_line() + 
  xlab("Time") + ylab("ATMP (deg. C)")
p4 <- ggplot(observations, aes(x =YM, y =WTMP)) + geom_line() + 
  xlab("Time") + ylab("WTMP (deg. C)")
p5 <- ggplot(observations, aes(x =YM, y =PRECIP)) + geom_line() + 
  xlab("Time") + ylab("monthly precip. avg (in.)")
p6 <- ggplot(discharge_amount, aes(x=YM, y=discharge_avg)) + geom_line() +
  xlab("Time") + ylab("Discharge amt. (cubic ft/s)")

ts.plot.list <- list(p, p2, p3, p4, p5, p6)
grid.arrange(grobs=ts.plot.list, nrow=5, ncol = 1, as.table=FALSE)

# STANDARD MULTIPLE LINEAR REGRESSION
covariates <- as.matrix(observations[2:6])
response <- as.vector(observations[7])
betas <- inv(t(as.matrix(covariates)) %*% as.matrix(covariates)) %*% 
  t(as.matrix(covariates)) %*% as.matrix(response)
epsilon <- matrix(rnorm(length(observations$YM), mean=0, sd=1), 
                  length(observations$YM), 1)

estimated_y <- covariates %*% betas + epsilon
estimated_y <- as.data.frame(estimated_y)
estimated_y <- cbind(observations$YM, estimated_y)
colnames(estimated_y)[1] <- 'date'
colnames(estimated_y)[2] <- 'estimates'
estimated_y <- cbind(estimated_y, observations$discharge_avg)
colnames(estimated_y)[3] <- 'observations'

p6 <- ggplot(estimated_y, aes(date)) + geom_line(aes(y = estimates, colour = "estimated"), 
                                                 color = "blue") + 
  geom_line(aes(y = observations, colour = "observations"), color = "magenta")
p6

ols.fit = lm(formula = discharge_avg ~ WSPD * PRES * ATMP * WTMP * PRECIP, data=observations)
summary(ols.fit)

# MULTIPLE REGRESSION DLM


# FUNCTION FOR FITTING THE STATE SPACE MODEL 
river_discharge <- function(par, mle_) { 
  model_t <- observations$YM 
  forecast_t <- c(observations$YM, future.discharge$YM)
  
  climate_x <- observations[2:6] 
  climate_f <- rbind(observations[2:6], observations_Y[2:6])
  discharge <- observations$discharge_avg
  discharge_f <- c(observations$discharge_avg, rep(NA, 16))
  discharge_t <- c(observations$discharge_avg, future.discharge$future.discharge)
  
  ssm <- function(parm, X) dlmModPoly(1, dV = parm[1]) + dlmModReg(X = X) 
  
  if (mle_opt){ 
    mle_ <- dlmMLE(y = discharge, parm = par, X=climate_x, build=ssm) 
    fit <- ssm(parm = mle_$par, X = climate_f)
    par <- mle_$par
  }
  else{
    fit <- fit <- ssm(parm = par, X = climate_f) 
  }
  
  filtered <- dlmFilter(discharge_f, fit) 
  
  df <- data.frame(dt = c(forecast_t, forecast_t), 
                   dis.forecast = c(discharge_t, filtered$f+discharge_t), 
                   series = c(rep("observed", length(forecast_t)), 
                              rep("prediction", length(forecast_t))))
  
  g <- ggplot(df, aes(x = dt, y = dis.forecast, col=series)) +
    geom_line() + 
    scale_color_manual(values = c("magenta", "blue")) + 
    geom_vline(xintercept = as.yearmon("2021-01"), col = "red") + 
    labs (x = "Time", y = "San Lorenzo River Discharge Amounts") + 
    scale_y_continuous(limits = c(0, max(df$dis.forecast))) 
  
  g2 <- ggplot(df, aes(x = dt, y = dis.forecast, col=series)) +
    geom_line() + 
    scale_color_manual(values = c("magenta", "blue")) + 
    geom_vline(xintercept = as.yearmon("2021-01"), col = "red") + 
    labs (x = "Time", y = "San Lorenzo River Discharge Amounts") + 
    scale_y_continuous(limits = c(0, max(df$dis.forecast))) 
  
  return(list(filtered = filtered, par = par, fit = fit, g = g, g2 = g2)) 
}

model_out <- river_discharge(par = rep(0,6), mle_opt = TRUE)
model_out$g