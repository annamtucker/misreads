# observer- and flag-specific characteristics associated with misreads

obs = read.csv("output/resights_clean.csv")


# observer-specific ####

obs %>%
  group_by(ObsCode) %>%
  summarize(Tot = length(Flag.Code),
            NYears = length(unique(Year)),
            Withheld = sum(Withheld),
            NotDeployed = sum(NotDeployed, na.rm = T),
            Imposs = Withheld + NotDeployed,
            SingleObs = sum(SingleObs),
            error.min = Imposs/Tot,
            error.max = SingleObs/Tot) %>%
  ungroup() -> observer.errors

observer.errors %>%
  dplyr::select(error.min, error.max) %>%
  gather(type, rate, 1:2) %>%
  group_by(type) %>%
  summarize(med = median(rate),
            l_iqr = quantile(rate, 0.25),
            u_iqr = quantile(rate, 0.75),
            avg = mean(rate),
            sd = sd(rate))


# correlation between number of years and total number of resights

cor.test(observer.errors$NYears, log(observer.errors$Tot),
         method = "spearman")

summary(omod <- lm(log(Tot) ~ NYears, data = observer.errors))

ggplot(observer.errors, aes(x = NYears, y = Tot)) +
  geom_point(alpha = 0.5, position = position_jitter(width = 0.1),
             size = 3) +
  xlab("Number of years volunteering with project") +
  ylab("Total number of flag resightings recorded") +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  scale_y_continuous(trans = "log", breaks = c(5,50,500,5000)) 

write.csv(observer.errors, "output/observer_errors.csv", row.names = F)



# fig 2 ####


fig2A <- observer.errors %>%
  dplyr::select(ObsCode, Tot, NYears, error.min, error.max) %>%
  rename(Confirmed = error.min, Possible= error.max) %>%
  gather(type, prop, 4:5) %>%
  ggplot(aes(x = NYears, y = prop, col = type)) +
  geom_point(alpha = 0.7, position = position_dodge(width = 0.5),
             size = 3) +
  geom_hline(yintercept = 0, lty = 2, lwd = 1, col = "black") +
  geom_hline(yintercept = 0.08, lty = 2, lwd = 1, col = "gray40") +
  scale_color_manual(values = c("black",
                                "gray40"),
                     name = "") +
  ylab("Probable misread rate") +
  xlab("Number of years with project since 2008")+
  theme(legend.position = c(0.65, 0.9),
        legend.background = element_rect(fill = "transparent"),
        legend.text = element_text(size = 12))
fig2A

fig2B <- observer.errors %>%
  dplyr::select(ObsCode, Tot, NYears, error.min, error.max) %>%
  rename(Confirmed = error.min, Possible= error.max) %>%
  gather(type, prop, 4:5) %>%
  ggplot(aes(x =Tot, y = prop, col = type)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_hline(yintercept = 0, lty = 2, lwd = 1, col = "black") +
  geom_hline(yintercept = 0.08, lty = 2, lwd = 1, col = "gray40") +
  scale_color_manual(values = c("black",
                                "gray40"),
                     name = "") +
  scale_x_continuous(trans = "log", breaks = c(5,50,500,5000)) +
  ylab("Probable misread rate") +
  xlab("Total number of observations") +
  theme(legend.position = c(0.65, 0.9),
        legend.background = element_rect(fill = "transparent"),
        legend.text = element_text(size = 12))
fig2B

fig2 <- plot_grid(fig2A, fig2B, ncol = 1, labels = c("A", "B"), 
                  align = "v", scale = 0.9)
save_plot(fig2, base_height = 7,file = "figs/fig2_observer-errors.jpg",
          base_aspect_ratio = 0.9)

# beta-binomial model ####

# beta-binomial model with unequal variance
bbmod = function(int, b.yr, b.tot, bt, theta.int){
  
  x <- dat$y1
  size <- dat$n
  
  prob <- plogis(int + b.tot*dat$tot + b.yr*dat$yr)
  theta <- exp(theta.int + bt*dat$tot)
  
  -sum(dbetabinom(x, prob, size, theta, log = TRUE))
}

# impossible flags
observer.errors %>%
  mutate(y1 = Imposs,
         tot = log(Tot),
         yr = NYears,
         n = Tot) %>%
  dplyr::select(y1, tot, yr, n) -> dat

mod1 = mle2(bbmod, start = list(int = 0.2, b.yr = 0, 
                                b.tot = 0, bt = 0,
                                theta.int = log(5)),
            data = dat,
            method = "L-BFGS-B")

summary(mod1)

# single observation
observer.errors %>%
  mutate(y1 = SingleObs,
         tot = log(Tot),
         yr = NYears,
         n = Tot) %>%
  select(y1, tot, yr, n) -> dat

mod2 = mle2(bbmod, start = list(int = 0.2, b.yr = 0, 
                                b.tot = 0, bt = 0,
                                theta.int = log(5)),
            data = dat,
            method = "L-BFGS-B")

summary(mod2)


# test if beta-binomial is better fit than regular binomial GLM (equal variance)
glm1 = glm(data = observer.errors,
           cbind(Imposs, Tot-Imposs)~NYears+log(Tot),
           family = "binomial")

glm2 = glm(data = observer.errors,
           cbind(SingleObs, Tot-SingleObs)~NYears+log(Tot),
           family = "binomial")

AICtab(mod1, glm1, base = TRUE)
AICtab(mod2, glm2, base = TRUE)


# number of resights after which observers converge on median error rate
# confirmed
res.w = exp((qlogis(mean(observer.errors$error.min)) - 
               coef(mod1)[1])/coef(mod1)[3])

# possible
res.so = exp((qlogis(median(observer.errors$error.max)) - 
                coef(mod2)[1])/coef(mod2)[3])

print(paste("Impossible flags:", round(res.w,1)))
print(paste("single observation:", round(res.so,1)))

# difference in error rate above and below this threshold
observer.errors %>%
  mutate(experienced = ifelse(Tot < res.so, 0, 1)) %>%
  select(experienced, error.max, error.min) %>%
  gather(type, rate, 2:3) %>%
  group_by(experienced, type) %>%
  summarize(med = median(rate),
            l_iqr = quantile(rate, 0.25),
            u_iqr = quantile(rate, 0.75),
            avg = mean(rate))

