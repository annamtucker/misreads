# plot results of simulation

slopes = readRDS("output/slopes-1000-FINAL.rds")
phis = readRDS("output/phis-1000-FINAL.rds")
ps = readRDS("output/ps-1000-FINAL.rds")

# plot results ####
yrlabs = c("5" = "5-year study", "10" = "10-year study", "20" = "20-year study")

fig_slopes <- slopes %>% 
  mutate(years = as.character(years),
    Year = factor(yrlabs[years], levels = c("5-year study","10-year study",
                                                 "20-year study")),
         type = c("No data filtering", "Remove single-observations")[type]) %>% 
  ggplot(aes(x = Year, y = beta)) +
  geom_violin(aes(group = paste(years, error), fill = as.character(error))) +
  geom_hline(yintercept = 0, lty = 2) +
  coord_flip() +
  xlab("Number of capture occasions (years)") +
  ylab("Estimated trend in survival probability") +
  scale_y_continuous(breaks = c(-0.25, 0, 0.25), limits = c(-0.5, 0.5)) +
  scale_fill_viridis_d(name = "Error rate", direction = -1) +
  theme(legend.position = "top") +
  facet_grid(~type) +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(margin = margin(1,1,1,1)))
fig_slopes
  

fig_slopes20 <- slopes %>% 
  filter(years == 20) %>% 
  mutate(years = as.character(years),
         Year = factor(yrlabs[years], levels = c("5-year study","10-year study",
                                                 "20-year study")),
         type = c("No data filtering", "Remove single-observations")[type]) %>% 
  ggplot(aes(x = as.character(error), y = beta)) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_violin(aes(group = paste(years, error), 
                  fill = as.character(error)), alpha = 0.8) +
  coord_flip() +
  xlab("Error rate") +
  ylab("Estimated trend in survival probability") +
  scale_y_continuous(breaks = seq(-1, 1, 0.5), limits = c(-1, 1)) +
  # scale_fill_grey(name = "Error rate", start = 0.6, end = 0) +
  scale_fill_grey(name = "Error rate", start = 0.6, end = 0) +
  theme(legend.position = "none") +
  facet_grid(~type) +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(margin = margin(2,2,2,2)))
fig_slopes20

fig_slopes10 <- slopes %>% 
  filter(years == 10) %>% 
  mutate(years = as.character(years),
         Year = factor(yrlabs[years], levels = c("5-year study","10-year study",
                                                 "20-year study")),
         type = c("No data filtering", "Remove single-observations")[type]) %>% 
  ggplot(aes(x = as.character(error), y = beta)) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_violin(aes(group = paste(years, error), 
                  fill = as.character(error)), alpha = 0.8) +
  coord_flip() +
  xlab("Error rate") +
  ylab("Estimated trend in survival probability") +
  scale_y_continuous(breaks = seq(-1, 1, 0.5), limits = c(-1, 1)) +
  #scale_fill_grey(name = "Error rate", start = 0.6, end = 0) +
  scale_fill_viridis_d(name = "Error rate", direction = -1) +
  theme(legend.position = "none") +
  facet_grid(~type) +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(margin = margin(2,2,2,2)))
fig_slopes10

fig_slopes5 <- slopes %>% 
  filter(years == 5) %>% 
  mutate(years = as.character(years),
         Year = factor(yrlabs[years], levels = c("5-year study","10-year study",
                                                 "20-year study")),
         type = c("No data filtering", "Remove single-observations")[type]) %>% 
  ggplot(aes(x = as.character(error), y = beta)) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_violin(aes(group = paste(years, error), 
                  fill = as.character(error)), alpha = 0.8) +
  coord_flip() +
  xlab("Error rate") +
  ylab("Estimated trend in survival probability") +
  scale_y_continuous(breaks = seq(-1, 1, 0.5), limits = c(-1, 1)) +
  #scale_fill_grey(name = "Error rate", start = 0.6, end = 0) +
  scale_fill_viridis_d(name = "Error rate", direction = -1) +
  theme(legend.position = "none") +
  facet_grid(~type) +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(margin = margin(2,2,2,2)))
fig_slopes5
  
fig_phis <- phis %>% 
  mutate(years = as.character(years),
         yrlab = factor(yrlabs[years], levels = c("20-year study","10-year study",
                                                 "5-year study")),
         type = c("No data filtering", "Remove single-observations")[type]) %>% 
  group_by(yrlab, error, type, year) %>% 
  summarize(lower = quantile(estimate, 0.025),
            upper = quantile(estimate, 0.975),
            med = median(estimate)) %>% 
  ggplot(aes(x = year, group = error)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = as.character(error)), alpha = 0.3) +
  geom_line(aes(y = med, col = as.character(error)), lwd = 1) +
  geom_hline(yintercept= 0.8, col = "black", lty = 2) +
  facet_grid(yrlab ~ type, scales= "free") +
  ylim(0,1) +
  # scale_color_grey(name = "Error rate", start = 0.6, end = 0) +
  # scale_fill_grey(name = "Error rate", start = 0.6, end = 0) +
  scale_fill_viridis_d(name = "Error rate", direction = -1) +
  scale_color_viridis_d(name = "Error rate", direction = -1) +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(margin = margin(1,1,1,1)),
        legend.position = "none") +
  ylab("Estimated apparent annual\nsurvival probability") +
  annotate("segment", x= -Inf, xend = Inf, y = -Inf, yend = -Inf) +
  xlab("Sampling occasion (year)")
fig_phis


fig_phis20 <- phis %>% 
  filter(years == 20) %>% 
  mutate(years = as.character(years),
         yrlab = factor(yrlabs[years], levels = c("20-year study","10-year study",
                                                  "5-year study")),
         type = c("No data filtering", "Remove single-observations")[type]) %>% 
  group_by(yrlab, error, type, year) %>% 
  summarize(lower = quantile(estimate, 0.025),
            upper = quantile(estimate, 0.975),
            med = median(estimate)) %>% 
  ggplot(aes(x = year, group = error)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = as.character(error)), alpha = 0.3) +
  geom_line(aes(y = med, col = as.character(error)), lwd = 1) +
  geom_hline(yintercept= 0.8, col = "black", lty = 2) +
  facet_wrap(~ type, scales= "free") +
  ylim(0,1) +
  scale_x_continuous(breaks = seq(0, 20, 2))+
  # scale_color_grey(name = "Error rate", start = 0.6, end = 0) +
  # scale_fill_grey(name = "Error rate", start = 0.6, end = 0) +
  scale_fill_viridis_d(name = "Error rate", direction = -1) +
  scale_color_viridis_d(name = "Error rate", direction = -1) +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(margin = margin(2,2,2,2)),
        legend.position = "none") +
  ylab("Estimated apparent annual\nsurvival probability") +
  xlab("Sampling occasion (year)")
fig_phis20

fig_phis10 <- phis %>% 
  filter(years == 10) %>% 
  mutate(years = as.character(years),
         yrlab = factor(yrlabs[years], levels = c("20-year study","10-year study",
                                                  "5-year study")),
         type = c("No data filtering", "Remove single-observations")[type]) %>% 
  group_by(yrlab, error, type, year) %>% 
  summarize(lower = quantile(estimate, 0.025),
            upper = quantile(estimate, 0.975),
            med = median(estimate)) %>% 
  ggplot(aes(x = year, group = error)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = as.character(error)), alpha = 0.3) +
  geom_line(aes(y = med, col = as.character(error)), lwd = 1) +
  geom_hline(yintercept= 0.8, col = "black", lty = 2) +
  facet_wrap(~ type, scales= "free") +
  ylim(0,1) +
  scale_x_continuous(breaks = seq(0, 10, 2))+
  # scale_color_grey(name = "Error rate", start = 0.6, end = 0) +
  # scale_fill_grey(name = "Error rate", start = 0.6, end = 0) +
  scale_fill_viridis_d(name = "Error rate", direction = -1) +
  scale_color_viridis_d(name = "Error rate", direction = -1) +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(margin = margin(2,2,2,2)),
        legend.position = "none") +
  ylab("Estimated apparent annual\nsurvival probability") +
  xlab("Sampling occasion (year)")
fig_phis10


fig_phis5 <- phis %>% 
  filter(years == 5) %>% 
  mutate(years = as.character(years),
         yrlab = factor(yrlabs[years], levels = c("20-year study","10-year study",
                                                  "5-year study")),
         type = c("No data filtering", "Remove single-observations")[type]) %>% 
  group_by(yrlab, error, type, year) %>% 
  summarize(lower = quantile(estimate, 0.025),
            upper = quantile(estimate, 0.975),
            med = median(estimate)) %>% 
  ggplot(aes(x = year, group = error)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = as.character(error)), alpha = 0.3) +
  geom_line(aes(y = med, col = as.character(error)), lwd = 1.5) +
  geom_hline(yintercept= 0.8, col = "black", lty = 2) +
  facet_wrap(~ type, scales= "free") +
  ylim(0,1) +
  scale_x_continuous(breaks = seq(1, 5, 1))+
  # scale_color_grey(name = "Error rate", start = 0.6, end = 0) +
  # scale_fill_grey(name = "Error rate", start = 0.6, end = 0) +
  scale_fill_viridis_d(name = "Error rate", direction = -1) +
  scale_color_viridis_d(name = "Error rate", direction = -1) +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(margin = margin(2,2,2,2)),
        legend.position = "top") +
  ylab("Estimated apparent annual\nsurvival probability") +
  xlab("Sampling occasion (year)")
fig_phis5


fig3 <- plot_grid(fig_slopes5, fig_phis5, 
                  fig_slopes10, fig_phis10,
                  fig_slopes20, fig_phis20,
                  align = "hv", axis = "tblr",
                  nrow = 3, ncol = 2, scale = 0.98,
                  labels = c("A", "D",
                             "B", "E",
                             "C", "F"))
fig3

save_plot(fig3, filename = "figs/fig3_sim-results.jpg",
          base_width = 12.5, base_height = 12.5)


yrlabs = c("5" = "5 years", "10" = "10 years", "20" = "20 years")


ps %>% 
  mutate(years = as.character(years),
         yrlab = factor(yrlabs[years], levels = c("5 years","10 years",
                                         "20 years" )),
         type = c("No data filtering", "Remove single-observations")[type]) %>% 
  ggplot(aes(x = yrlab, y = estimate, fill = as.character(error))) +
  geom_hline(yintercept= 0.5, col = "black", lty = 2) +
  geom_violin(aes(group = paste(years, error), fill = as.character(error))) +
  ylim(0,1) +
  scale_fill_viridis_d(name = "Error rate", direction = -1) +
  theme(legend.position = "top") +
  facet_grid(~type) +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(margin = margin(1,1,1,1))) +
  ylab("Estimated detection probability") +
  xlab("Length of study")


# rmse and bias ####

truth = tibble(parm = c("phi", "p"),
               true = c(0.8, 0.5))

phis %>% 
  bind_rows(ps) %>% 
  full_join(truth, by = "parm") %>% 
  group_by(years, error, type, parm, true) %>%
  summarize(rmse = sqrt(sum((estimate - true)^2)/(length(estimate)-1)),
            bias = (sum((estimate-true)/true)/length(estimate))*100) %>% 
  ungroup() ->param.bias
  

param.bias %>% 
  dplyr::select(years, error, type, parm, rmse, bias) %>%
  mutate(id = rep(1:(nrow(param.bias)/2), each = 2)) %>% 
  gather(var, metric, 5:6) %>% 
  unite(temp, parm, var) %>% 
  spread(temp, metric) %>% 
  select(years, error, type, phi_rmse, phi_bias, p_rmse, p_bias) %>% 
  arrange(type, years, error) %>% 
  write.csv("output/rmse-bias.csv", row.names = F)




yrlabs = c("5" = "5-year study", "10" = "10-year study", "20" = "20-year study")


rmse <- param.bias %>%
  mutate(years = as.character(years),
         years = factor(yrlabs[as.character(years)],
                        levels = c("5-year study","10-year study",
                                   "20-year study" )),
         type = c("No data filtering", "Remove single-observations")[type]) %>% 
  ggplot(aes(x = as.character(error), y = rmse, col = parm)) +
  geom_hline(yintercept =0, lty = 2) +
  geom_point(size = 4, alpha = 0.8, position = position_dodge(width = 0.5)) +
  facet_grid(type ~ years, scales = "free") +
  ylim(0, 0.5)+
  scale_color_manual(name = "Parameter",
                     values = c("palegreen4","dodgerblue3"),
                     labels = c("detection", "survival")) +
  theme(strip.text = element_text(margin = margin(0.1, 0, 0.1, 0, "cm")), 
        strip.background = element_rect(fill = "white"),
        legend.position = "top") +
  ylab("RMSE") +
  xlab("Error rate") +
  annotate("segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf) +
  annotate("segment", x = -Inf, xend = -Inf, y= -Inf, yend = Inf)
rmse

bias <- param.bias %>%
  mutate(years = factor(yrlabs[as.character(years)],
                        levels = c("5-year study","10-year study",
                                   "20-year study" )),
         type = c("No data filtering", "Remove single-observations")[type]) %>%
  ggplot(aes(x = as.character(error), y = bias, col = parm)) +
  geom_point(size = 4, alpha = 0.8, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept =0, lty = 2) +
  facet_grid(type~years, scales = "free") +
  scale_color_manual(name = "Parameter",
                     values = c("palegreen4","dodgerblue3"),
                     labels = c("detection", "survival")) +
  theme(strip.text = element_text(margin = margin(0.1, 0, 0.1, 0, "cm")), 
        strip.background = element_rect(fill = "white"),
        legend.position = "top") +
  ylab("Relative bias (%)") +
  xlab("Error rate") +
  annotate("segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf) +
  annotate("segment", x = -Inf, xend = -Inf, y= -Inf, yend = Inf)
bias


rmse_bias <-param.bias %>% 
  filter(parm == "phi" ) %>% 
  gather(var, val, 6:7) %>% 
  mutate(years = factor(yrlabs[as.character(years)],
                        levels = c("5-year study","10-year study",
                                   "20-year study" )),
         type = c("No data filtering", "Remove single-observations")[type],
         var = c("bias" = "Relative bias (%)",
                 "rmse" = "RMSE")[var]) %>%
  ggplot(aes(x = as.character(error), y = val, fill = type)) +
  geom_hline(yintercept =0, lty = 2) +
  geom_line(lty = 3, aes(group = type)) +
  geom_point(size = 4, alpha = 0.8, pch = 21) +
  facet_grid(var~years, scales = "free", switch = "y") +
  scale_fill_grey(start = 0, end = 0.6, name = "") +
  theme(strip.text = element_text(margin = margin(0.1, 0, 0.1, 0, "cm"),
                                  size = rel(1),
                                  vjust = 25), 
        strip.background = element_rect(fill = "white"),
        axis.title.y = element_blank(),
        legend.position = "top",
        plot.margin = unit(c(5.5, 5.5, 5.5, 25), "points")) +
  xlab("Error rate") +
  annotate("segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf) +
  annotate("segment", x = -Inf, xend = -Inf, y= -Inf, yend = Inf)
rmse_bias


save_plot(rmse_bias, filename = "figs/fig4_rmse-bias.jpg",
          base_width = 10, base_height = 6)
