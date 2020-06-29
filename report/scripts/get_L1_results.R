read_csv("../stanout/posterior_accuracymodelL1eng.csv") -> L1
L1 %>% as_tibble() %>%
  transmute(L1eng = ifelse(L1eng ==1, "English", "Others"),
            M = estimate__,
            lo = lower__,
            up = upper__) %>%
  mutate(value = 1) -> l1sum

L1_acc <- read_csv("../stanout/posterior_accL1en.csv")

plot_acc <- ggplot(L1_acc, aes(x = value, fill=L1eng, colour = L1eng)) + 
  geom_density(alpha=.65, size = .0, adjust =1) +
  geom_vline(aes(xintercept=.10), linetype="dashed", size=.5) +
  #  geom_errorbarh(data = l1sum, aes(y = 1, xmax = up, xmin = lo, height = 1)) +
  theme_few() +
  labs(y = expression(paste("Posterior probability" )), x = "Response accuracy") +
  ggtitle("a.") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_colorblind("First language: ") +
  scale_color_colorblind("First language: ") +
  theme(legend.position = "none",
        legend.justification = "right",
        axis.title.x = element_text(hjust = 0),
        axis.title.y = element_text(hjust = 0),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA)
  )

L1_conf <- read_csv("../stanout/posterior_confidencemodelL1eng.csv")
plot_conf <- L1_conf %>%
  as_tibble() %>%
  dplyr::select(acc, L1eng, estimate__, lower__, upper__) %>%
  rename(M = estimate__,
         lo = lower__,
         up = upper__ ) %>%
  mutate(L1eng = ifelse(L1eng == 1, "English", "Other")) %>%
  ggplot(aes(y = M, x= acc, colour = L1eng, fill = L1eng)) +
  geom_ribbon(aes(ymin = lo, ymax=up, colour = NA), alpha = .65, show.legend = F) +
  geom_smooth(show.legend = T, size = .5) +
  ggtitle("b.") +
  scale_color_colorblind("First language: " ) +
  scale_fill_colorblind("First language: ") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(limits = c(6,9), breaks = seq(0, 10, 1)) +
  theme_few() +
  labs(y = "Posterior confidence rating\nwith 95% PIs", x = "Response accuracy") +
  theme(legend.position = "top",
        axis.title.x = element_text(hjust = 0),
        axis.title.y = element_text(hjust = 0),
        legend.justification = "right")

L1_plot <- gridExtra::grid.arrange(plot_acc, plot_conf, nrow = 1)

path <- "../plots/L1engplot.png"
ggsave(path, plot = L1_plot, width = 6, height = 3)
path <- "../plots/L1engplot.pdf"
ggsave(path, plot = L1_plot, width = 6, height = 3)
