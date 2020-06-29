p.dist <- samps %>%
  mutate(`Parade type` = Parade,
         `Parade instruction` = recode(FA, `Strong FA warning` = "strong warning",
                                       `Standard FA warning` =  "standard warning")) %>%
  
  ggplot(aes(x = value, fill=Target, colour = Target)) + 
  geom_vline(aes(xintercept=.10), linetype="dashed", size=.5) +
  geom_density(alpha=.75, size = .0) +
  facet_wrap(`Parade type`~`Parade instruction`, labeller = label_context) +
  labs(y = expression(paste("Posterior probability density" )), x = "Response accuracy") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_colorblind("Target presence: ") +
  scale_color_colorblind("Target presence: ") +
  theme(
    legend.position = "top",
    legend.justification = "right",
    strip.text = element_text(hjust = 0),
    strip.background = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_text(hjust = 0),
    axis.title.y = element_text(hjust = 0),
    panel.grid.major = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA)
  )#;p.dist


path <- "../plots/online_probdist_byFA_L1.png"
ggsave(path, width = 6, height = 5)
path <- "../plots/online_probdist_byFA_L1.pdf"
ggsave(path, width = 6, height = 5)

