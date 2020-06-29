p_chance2 <- p_chance %>% mutate(`Parade type` = Parade,
                      FA = recode(FA, `Strong FA warning` = "strong warning",
                                  `Standard FA warning` =  "standard warning"))

chance_plot <- p_chance2 %>%
  ggplot(aes(x = FA, y = P/100, group = Target)) +
  geom_col(aes(colour = Target, fill = Target),
           position = position_dodge(.75), 
           width = 1, alpha = .75) +
  facet_wrap(`Parade type`~., strip.position = "top", nrow = 2, labeller = label_both) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0,1)) +
  scale_x_discrete(limits = rev(levels(factor(p_chance2$FA)))) +
  scale_fill_colorblind("Target presence: ") +
  scale_color_colorblind("Target presence: ") +
  labs(y = bquote("Posterior probability of below chance-level\nresponse accuracy (10%)"), x ="Parade instruction") +
  geom_label(aes(y = P/100, label = paste0(round(P,0), "%")), size = 2,
             position = position_dodge(width = .75)) +
  theme(
    legend.position = "top",
    legend.justification = "right",
    strip.text.x = element_text(hjust = 0, angle = 360),
    strip.background = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_text(hjust = 0),
    axis.title.y = element_text(hjust = 1, angle = 360),
    panel.grid.major = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA)
  ) +
  coord_flip()#;chance_plot

path <- "../plots/prob_below_chance.png"
ggsave(path, width = 6, height = 4)
path <- "../plots/prob_below_chance.pdf"
ggsave(path, width = 6, height = 4)