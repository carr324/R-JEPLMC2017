# Vogel, Carr, Davis, & Winkielman (2017)
# Code for figures in final paper
# Evan W. Carr, 05.20.2016

require(tidyverse)
require(extrafont)

exp1_data_liking <- read_csv("exp1_data_liking.csv")
exp1_data_classifications <- read_csv("exp1_data_classifications.csv")
exp1_data_RTs <- read_csv("exp1_data_RTs.csv")

exp2_data_liking <- read_csv("exp2_data_liking.csv")
exp2_data_classifications <- read_csv("exp2_data_classifications.csv")
exp2_data_RTs <- read_csv("exp2_data_RTs.csv")

exp3_data_liking <- read_csv("exp3_data_liking.csv")

gcm_outputs <- read_csv("gcm_modelOutputs.csv")

exp1_fig_liking <- ggplot(data = exp1_data_liking, 
                          aes(x = Level, y = liking_mean, color = stim_type, fill = stim_type)) +
                      geom_point(aes(shape = stim_type), size = 4) +
                      geom_line(size = 1) +
                      geom_ribbon(aes(ymin = liking_mean - liking_sem, ymax = liking_mean + liking_sem), alpha = 0.15, color = NA) +
                      theme_classic() +
                      labs(y = "Liking (0-100 +/- 1 SEM)", x = "Category Distinctiveness") +
                      theme(text = element_text(family="Gill Sans MT"),
                            axis.text.x = element_text(size = 12),
                            axis.title.x = element_text(size = 14, face = "bold"),
                            axis.text.y = element_text(size = 12),
                            axis.title.y = element_text(size = 14, face = "bold"),
                            axis.line.x = element_line(colour = "black"),
                            axis.line.y = element_line(colour = "black"),
                            legend.title = element_blank())
exp1_fig_classifications <- ggplot(data = exp1_data_classifications, 
                                   aes(x = Level, y = classification_mean, color = stim_type, fill = stim_type)) +
                              geom_point(aes(shape = stim_type), size = 4) +
                              geom_line(size = 1) +
                              geom_ribbon(aes(ymin = classification_mean - classification_sem, ymax = classification_mean + classification_sem), alpha = 0.15, color = NA) +
                              theme_classic() +
                              labs(y = "Proportion of Correct\nClassifications (Acks vs. Blubs)", x = "Category Distinctiveness") +
                              theme(text = element_text("Gill Sans MT"),
                                    axis.text.x = element_text(size = 12),
                                    axis.title.x = element_text(size = 14, face = "bold"),
                                    axis.text.y = element_text(size = 12),
                                    axis.title.y = element_text(size = 14, face = "bold"),
                                    axis.line.x = element_line(colour = "black"),
                                    axis.line.y = element_line(colour = "black"),
                                    legend.title = element_blank())
exp1_fig_RTs <- ggplot(data = exp1_data_RTs, 
                       aes(x = Level, y = RT_mean, color = stim_type, fill = stim_type)) +
                  geom_point(aes(shape = stim_type), size = 4) +
                  geom_line(size = 1) +
                  geom_ribbon(aes(ymin = RT_mean - RT_sem, ymax = RT_mean + RT_sem), alpha = 0.15, color = NA) +
                  theme_classic() +
                  labs(y = "Classification RTs in ms (+/- 1 SEM)", x = "Category Distinctiveness") +
                  theme(text = element_text("Gill Sans MT"),
                        axis.text.x = element_text(size = 12),
                        axis.title.x = element_text(size = 14, face = "bold"),
                        axis.text.y = element_text(size = 12),
                        axis.title.y = element_text(size = 14, face = "bold"),
                        axis.line.x = element_line(colour = "black"),
                        axis.line.y = element_line(colour = "black"),
                        legend.title = element_blank())

exp2_fig_liking <- ggplot(data = exp2_data_liking, 
                          aes(x = Level, y = liking_mean, color = stim_type, fill = stim_type)) +
                    geom_point(aes(shape = stim_type), size = 4) +
                    geom_line(size = 1) +
                    geom_ribbon(aes(ymin = liking_mean - liking_sem, ymax = liking_mean + liking_sem), alpha = 0.15, color = NA) +
                    theme_classic() +
                    labs(y = "Liking (0-100 +/- 1 SEM)", x = "Category Distinctiveness") +
                    theme(text = element_text(family="Gill Sans MT"),
                          axis.text.x = element_text(size = 12),
                          axis.title.x = element_text(size = 14, face = "bold"),
                          axis.text.y = element_text(size = 12),
                          axis.title.y = element_text(size = 14, face = "bold"),
                          axis.line.x = element_line(colour = "black"),
                          axis.line.y = element_line(colour = "black"),
                          legend.title = element_blank())
exp2_fig_classifications <- ggplot(data = exp2_data_classifications, 
                                   aes(x = Level, y = classification_mean, color = stim_type, fill = stim_type)) +
                              geom_point(aes(shape = stim_type), size = 4) +
                              geom_line(size = 1) +
                              geom_ribbon(aes(ymin = classification_mean - classification_sem, ymax = classification_mean + classification_sem), alpha = 0.15, color = NA) +
                              theme_classic() +
                              labs(y = "Proportion of Typical vs.\nAtypical Member Classifications", x = "Category Distinctiveness") +
                              theme(text = element_text("Gill Sans MT"),
                                    axis.text.x = element_text(size = 12),
                                    axis.title.x = element_text(size = 14, face = "bold"),
                                    axis.text.y = element_text(size = 12),
                                    axis.title.y = element_text(size = 14, face = "bold"),
                                    axis.line.x = element_line(colour = "black"),
                                    axis.line.y = element_line(colour = "black"),
                                    legend.title = element_blank())
exp2_fig_RTs <- ggplot(data = exp2_data_RTs, 
                       aes(x = Level, y = RT_mean, color = stim_type, fill = stim_type)) +
                  geom_point(aes(shape = stim_type), size = 4) +
                  geom_line(size = 1) +
                  geom_ribbon(aes(ymin = RT_mean - RT_sem, ymax = RT_mean + RT_sem), alpha = 0.15, color = NA) +
                  theme_classic() +
                  labs(y = "Classification RTs in ms (+/- 1 SEM)", x = "Category Distinctiveness") +
                  theme(text = element_text("Gill Sans MT"),
                        axis.text.x = element_text(size = 12),
                        axis.title.x = element_text(size = 14, face = "bold"),
                        axis.text.y = element_text(size = 12),
                        axis.title.y = element_text(size = 14, face = "bold"),
                        axis.line.x = element_line(colour = "black"),
                        axis.line.y = element_line(colour = "black"),
                        legend.title = element_blank())

exp3_fig_liking <- ggplot(data = exp3_data_liking, 
                          aes(x = Level, y = liking_mean, color = stim_type, fill = stim_type)) +
  geom_point(aes(shape = stim_type), size = 4) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = liking_mean - liking_sem, ymax = liking_mean + liking_sem), alpha = 0.15, color = NA) +
  theme_classic() +
  labs(y = "Liking (0-100 +/- 1 SEM)", x = "Category Distinctiveness") +
  theme(text = element_text(family="Gill Sans MT"),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        legend.title = element_blank())

gcm_fig_prChoice <- ggplot(data = gcm_outputs, 
                           aes(x = Level, y = gcm_prChoice, color = stim_type)) +
  geom_point(aes(shape = stim_type), size = 4) +
  geom_line(size = 1) +
  theme_classic() +
  labs(y = "GCM Average A/B Choice Probabilities", x = "Category Distinctiveness") +
  theme(text = element_text("Gill Sans MT"),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        legend.title = element_blank())

gcm_fig_prTypical <- ggplot(data = gcm_outputs, 
                            aes(x = Level, y = gcm_prTypical, color = stim_type)) +
  geom_point(aes(shape = stim_type), size = 4) +
  geom_line(size = 1) +
  theme_classic() +
  labs(y = "GCM Average Typical/Atypical Choices", x = "Category Distinctiveness") +
  theme(text = element_text("Gill Sans MT"),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        legend.title = element_blank())




            