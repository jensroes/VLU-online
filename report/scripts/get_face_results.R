face_acc <- read_csv("../stanout/posterior_accuracyfaceskills_mes.csv") %>%
  as_tibble() %>%
  transmute(FaceAbility = FaceAbility,
            M = estimate__,
            lo = lower__,
            up = upper__) %>%
  ggplot(aes(y = M,
             x = FaceAbility,
             ymin = lo,
             ymax = up)) +
  geom_ribbon(alpha = .2) +
  ggtitle("a.") +
  geom_line() +
  #  geom_hline(yintercept = .1, linetype = "dashed", colour = "grey") +
  scale_y_continuous(limits = c(0,.6), labels = scales::percent_format(accuracy = 1)) +
  labs(y = "Response accuracy", x = "Face recognition ability (scaled)") +
  theme_few() 

face_conf <- read_csv("../stanout/posterior_confidencemodelfaceability.csv") %>%
  dplyr::select(acc, FaceAbility, conf, estimate__, lower__, upper__) %>%
  mutate(FaceAbility = factor(FaceAbility, labels = c("low", "medium", "high"))) %>%
  rename(M = estimate__,
         lo = lower__,
         up = upper__) %>%
  ggplot(aes(y = M, ymin=lo, ymax = up, x = acc, fill = FaceAbility, linetype = FaceAbility, colour = FaceAbility)) +
  geom_ribbon(alpha = .2, colour = NA, show.legend = T) +
  geom_line(show.legend = T) +
  geom_smooth(size = .35, se = F, show.legend = F) +
  ggtitle("b.") +
  scale_x_continuous(limits = c(0,1), labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(breaks = seq(1,10,1), limits = c(6,9)) +
  labs(x = "Response accuracy", y = "Confidence ratings\n  ") +
  theme_few() +
  scale_fill_colorblind("Face recognition ability:")+ 
  scale_colour_colorblind("Face recognition ability:") +
  scale_linetype("Face recognition ability:") +
  theme(legend.position = "bottom",
        legend.justification = "right")

face_plot <- gridExtra::grid.arrange(face_acc, face_conf, nrow = 2, heights = c(1,1.25))

path <- "../plots/faceplot.png"
ggsave(path, plot = face_plot, width = 6, height = 10)
path <- "../plots/faceplot.pdf"
ggsave(path, plot = face_plot, width = 6, height = 10)