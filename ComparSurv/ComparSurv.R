ggplot(data.frame(summary(expo)), aes(x = time)) + 
  geom_line(aes(y = est, col = "Exponential")) +
  geom_line(data = data.frame(summary(weibull)), aes(y = est, col = "Weibull")) +
  geom_line(data = data.frame(summary(loglog)), aes(y = est, col = "Log-Logistic")) +
  geom_line(data = data.frame(summary(lnormal)), aes(y = est, col = "Log-Normal")) +
  geom_line(data = data.frame(summary(flex.spline)), aes(y = est, col = "Flexible Splines")) +
  geom_step(data = kap.dat, aes(x=time, y=surv, colour = "Kaplan-Meier"), size = 0.37)+
  geom_step(data = cox.dat, aes(x=time, y=surv, colour = "Cox PH"), size = 0.37)+
  labs(x = "Time (years)", y = "Survival Probability", col = "Models") + theme_classic()