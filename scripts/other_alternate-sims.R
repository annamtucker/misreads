# plot results of simulation

slopes = readRDS("output/slopes-100.rds")
phis = readRDS("output/phis-100.rds")
ps = readRDS("output/ps-100.rds")

# plot results ####

yrlabs = c("5" = "5-year study", "10" = "10-year study", "20" = "20-year study")


slopefig <- slopes %>% 
  filter(type == 1) %>% 
  mutate(years = as.character(years),
         Year = factor(years, levels = c("5","10","20" )),
         type = c("No data cleaning", "Remove single-observations")[type],
         phi.lab = factor(phi, labels = c(expression(paste(phi, " = 0.4")),
                                           expression(paste(phi, " = 0.6")),
                                           expression(paste(phi, " = 0.8")))),
         p.lab = factor(p, labels = c(expression(paste("p", " = 0.2")), 
                                      expression(paste("p", " = 0.5")), 
                                      expression(paste("p", " = 0.8"))))) %>% 
  ggplot(aes(x = Year, y = beta)) +
  geom_violin(aes(group = paste(years, error), fill = as.character(error))) +
  geom_hline(yintercept = 0, lty = 2) +
  coord_flip() +
  xlab("Number of capture occasions (years)") +
  ylab("Estimated trend in survival probability over time") +
  scale_y_continuous(breaks = seq(-1.5, 1.5, 0.5), limits = c(-0.75, 0.75)) +
  scale_fill_viridis_d(name = "Error rate", direction = -1) +
  theme_bw()+
  facet_grid(phi.lab~p.lab, labeller = label_parsed, scales = "free") +
  theme(strip.text = element_text(size = 14, 
                                  margin = margin(0.1, 0, 0.1, 0, "cm")), 
        strip.background = element_rect(fill = "white"),
        legend.position = "top",
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.text = element_text(size= 14)) 

save_plot(slopefig, file = "slopes_100reps.jpg", base_width = 12, base_height = 12)

phis <- phis %>% 
  filter(type == 1 & years == 20) %>% 
  mutate(phi.lab = factor(phi, labels = c(expression(paste(phi, " = 0.4")),
                                          expression(paste(phi, " = 0.6")),
                                          expression(paste(phi, " = 0.8")))),
         p.lab = factor(p, labels = c(expression(paste("p", " = 0.2")), 
                                      expression(paste("p", " = 0.5")), 
                                      expression(paste("p", " = 0.8"))))) %>% 
  group_by(years, error, type, phi.lab, phi, p.lab, year) %>% 
  summarize(lower = quantile(estimate, 0.025),
            upper = quantile(estimate, 0.975),
            med = median(estimate)) %>% 
  ggplot(aes(x = year, group = error)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, 
                  fill = as.character(error)), alpha = 0.3) +
  geom_line(aes(y = med, col = as.character(error)), lwd = 1) +
  geom_hline(aes(yintercept= phi), col = "black", lty = 2) +
  facet_grid(phi.lab~p.lab, labeller = label_parsed, scales = "free") +
  ylim(0,1)+
  scale_color_viridis_d(name = "Error rate", direction = -1) +
  scale_fill_viridis_d(name = "Error rate", direction = -1) +
  theme_bw() +
  theme(strip.text = element_text(size = 14, 
                                  margin = margin(0.1, 0, 0.1, 0, "cm")), 
        strip.background = element_rect(fill = "white"),
        legend.position = "top",
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.text = element_text(size= 14)) +
  xlab("Sampling occasion (years)") +
  ylab("Estimated annual survival probability")
  
save_plot(phis, file = "phis-100reps.jpg", base_width = 12, base_height = 10)

phis %>% 
  filter(phi == 0.4 & p == 0.8) %>% 
  mutate(type = c("No data cleaning", "Remove single-observations")[type]) %>% 
  ggplot(aes(x = year)) +
  geom_hline(yintercept= 0.4, col = "black", lty = 2) +
  geom_linerange(aes(ymin = lcl, ymax= ucl), lwd = 1, alpha = 0.1,
                 position = position_dodge(width = 1)) +
  geom_point(aes(y = estimate, col = as.character(error)), 
             size = 2, alpha = 0.5,  
             position = position_dodge(width = 1)) +
  facet_grid(years~type, scales= "free") +
  scale_x_continuous(breaks = seq(2, 20, 2), limits = c(0, 20)) +
  ylim(0,1) +
  scale_color_grey(name = "Error rate", end = 0.7) +
  theme(legend.position = "top") +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(margin = margin(1,1,1,1))) +
  ylab("Estimated apparent annual\nsurvival probability") +
  annotate("segment", x= -Inf, xend = Inf, y = -Inf, yend = -Inf)


ps %>% 
  mutate(Year = factor(years, levels = c(5, 10, 20)),
         type = c("No data cleaning", "Remove single-observations")[type]) %>% 
  ggplot(aes(x = Year, fill = as.character(error))) +
  geom_hline(yintercept= 0.5, col = "black", lty = 2) +
  geom_linerange(aes(ymin = lcl, ymax= ucl), lwd = 1, alpha = 0.5,
                 position = position_dodge(width = 0.5)) +
  geom_point(aes(y = estimate), size = 2, alpha = 0.8, pch = 21,
             position = position_dodge(width = 0.5)) +
  ylim(0,1) +
  scale_fill_grey(name = "Error rate") +
  theme(legend.position = "top") +
  facet_wrap(~type) +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(margin = margin(1,1,1,1))) +
  ylab("Estimated detection probability")



# rmse and bias ####

phis %>% 
  group_by(years, error, type, scenario, phi, p) %>%
  summarize(rmse = sqrt(sum((estimate - phi)^2)/(length(estimate)-1)),
            bias = (sum((estimate-phi)/phi)/length(estimate))*100) %>%
  ungroup() -> phi.bias


phibias <- phi.bias %>% 
  mutate(phi.lab = factor(phi, labels = c(expression(paste(phi, " = 0.4")),
                                          expression(paste(phi, " = 0.6")),
                                          expression(paste(phi, " = 0.8"))))) %>% 
  filter(type == 1) %>% 
  ggplot(aes(x = p, y = bias, fill = as.character(error))) +
  geom_line(aes(group = error), lwd = 1, lty = 3) +
  geom_point(size = 5, alpha = 0.8, pch = 21) +
  facet_grid(years~phi.lab, labeller = label_parsed, scales = "free") +
  ylim(0, 70) +
  scale_fill_viridis_d(end = 0.95, name = "Error rate") +
  ylab("Relative bias (%)\nof survival estimates") +
  xlab("Detection probability (p)") +
  theme_bw() +
  theme(strip.text = element_text(size = 14, 
                                  margin = margin(0.1, 0, 0.1, 0, "cm")), 
        strip.background = element_rect(fill = "white"),
        legend.position = "top",
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.text = element_text(size= 14)) 
phibias  

save_plot(phibias, file = "phi_bias-100.jpg", base_width =12, base_height = 10)

phi.bias %>% 
  filter(type == 2 & years == 20) %>% 
  ggplot(aes(x = p, y = phi, fill = rmse)) +
  geom_tile() +
  facet_wrap(~error) +
  scale_fill_viridis_c()


phi.bias %>% 
  filter(type == 1 & years == 20) %>% 
  ggplot(aes(x = p, y = bias, fill = as.character(error))) +
  geom_line(aes(group = error)) +
  geom_point(size = 4.5, alpha = 0.8, pch = 21) +
  facet_wrap(~phi) +
  scale_fill_viridis_d(end = 0.9, name = "Error rate") +
  ggtitle("Without data cleaning")

phi.bias %>% 
  filter(type == 2 & years == 20) %>% 
  ggplot(aes(x = p, y = bias, fill = as.character(error))) +
  geom_line(aes(group = error)) +
  geom_point(size = 4.5, alpha = 0.8, pch = 21) +
  facet_wrap(~phi) +
  scale_fill_viridis_d(end = 0.9, name = "Error rate") +
  ggtitle("Remove all single-observations")

phi.bias %>% 
  filter(type == 1) %>% 
  ggplot(aes(x = p, y = phi, fill = rmse)) +
  geom_tile() +
  facet_grid(years~error) +
  scale_fill_viridis_c(name = "RMSE") +
  ylab("Annual survival probability") +
  xlab("Detection probability") +
  theme(strip.text = element_text(margin = margin(0.1, 0, 0.1, 0, "cm")), 
        strip.background = element_rect(fill = "white"),
        legend.position = "top") 

phi.bias %>% 
  filter(type == 1) %>% 
  ggplot(aes(x = p, y = phi, fill = bias)) +
  geom_tile() +
  facet_grid(years~error) +
  scale_fill_viridis_c(name = "Relative bias (%)") +
  ylab("Annual survival probability") +
  xlab("Detection probability") +
  theme(strip.text = element_text(margin = margin(0.1, 0, 0.1, 0, "cm")), 
        strip.background = element_rect(fill = "white"),
        legend.position = "top")


param.bias = readRDS("output/bias.rds")

yrlabs = c("5" = "5-year study", "10" = "10-year study", "20" = "20-year study")

param.bias %>%
  mutate(years = factor(yrlabs[as.character(years)],
                        levels = c("5-year study","10-year study",
                                   "20-year study" )),
         type = c("No data cleaning", "Remove single-observations")[type]) %>% 
  ggplot(aes(x = as.character(error), y = rmse, col = parm)) +
  geom_hline(yintercept =0, lty = 2) +
  geom_point(size = 4, alpha = 0.8, position = position_dodge(width = 0.5)) +
  facet_grid(type ~ years, scales = "free") +
  ylim(0, 0.2) +
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

param.bias %>%
  mutate(years = factor(yrlabs[as.character(years)],
                        levels = c("5-year study","10-year study",
                                   "20-year study" )),
         type = c("No data cleaning", "Remove single-observations")[type]) %>%
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
  ylim(-30, 20) +
  ylab("Relative bias (%)") +
  xlab("Error rate") +
  annotate("segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf) +
  annotate("segment", x = -Inf, xend = -Inf, y= -Inf, yend = Inf)




slopes %>% 
  group_by(years, error, phi, p, type) %>% 
  summarize(mean = mean(beta),
            se = sd(beta)/sqrt(length(beta))) %>%
  mutate(rmse = sqrt((mean - 0)^2 + se^2),
         bias = (mean-0)*100) %>%
  ungroup() %>% 
  mutate(phi.lab = factor(phi, labels = c(expression(paste(phi, " = 0.4")),
                                          expression(paste(phi, " = 0.6")),
                                          expression(paste(phi, " = 0.8")))),
         g = paste(type, error)) %>% 
  ggplot(aes(x = p, y = rmse)) +
  geom_line(aes(lty = as.character(type)), lwd = 1) +
  geom_point(size = 5, alpha = 0.8,
             aes(fill = as.character(error),
                 shape = as.character(type))) +
  facet_wrap(~phi.lab, labeller = label_parsed, scales = "free") +
  ylim(0, 0.3) +
  scale_fill_viridis_d(end = 0.9, name = "Error rate") +
  scale_shape_manual(name = "", 
                     labels = c("No data cleaning",
                                "Remove single-observations"),
                     values = c(21, 22)) +
  ylab("Root mean squared error (RMSE)\nof survival estimates") +
  xlab("Detection probability (p)") +
  theme(strip.text = element_text(size = 14, 
                                  margin = margin(0.1, 0, 0.1, 0, "cm")), 
        strip.background = element_rect(fill = "white"),
        legend.position = "top",
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.text = element_text(size= 14)) 
 