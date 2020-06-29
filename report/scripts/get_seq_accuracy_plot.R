p.dist <- samps_seq %>% 
  ggplot(aes(x = value, fill=Target, colour = Target)) + 
  geom_density(alpha=.35, size = .0, adjust =1) +
  facet_grid(FA~.) +
  geom_vline(aes(xintercept=.10), linetype="dashed", size=.5) +
  geom_errorbarh(data = chanceexp, aes(y = 1, xmax = upper, xmin = lower, height = 1), color = "black") +
  theme_few() +
  labs(y = expression(paste("Posterior probability" )), x = "Response accuracy") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_colorblind("Target: ") +
  scale_color_colorblind("Target: ") +
  ggtitle("Lineup: sequential parade") +
  theme(
    legend.position = "bottom",
    legend.justification = "right",
    axis.text.y = element_blank(),
    strip.text.y = element_text(size = 10, angle = 360),
    axis.ticks = element_blank(),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA)
  )

path <- "../plots/sequential_online_probdist_byFA.png"
ggsave(path, width = 6, height = 4)
path <- "../plots/sequential_online_probdist_byFA.pdf"
ggsave(path, width = 6, height = 4)

