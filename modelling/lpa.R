#------------------------------------------
# This script sets out to produce run a
# Latent Profile Analysis of players in the
# 2020 season
#
# NOTE: This script requires setup.R to
# have been run first
#------------------------------------------

#----------------------- Pre processing ----------------------------

finals <- c("EF", "SF", "QF", "PF", "GF")
'%ni%' <- Negate('%in%')

# Pull data for the 2020 season and aggregate to player averages

season_2020 <- get_afltables_stats(start_date = "2020-01-01", end_date = "2020-12-01") %>%
  clean_names() %>%
  filter(round %ni% finals) %>%
  mutate(player_name = paste0(first_name, " ", surname)) %>%
  group_by(player_name, playing_for) %>%
  summarise(kicks = mean(kicks),
            marks = mean(marks),
            goals = mean(goals),
            behinds = mean(behinds),
            contested_possessions = mean(contested_possessions),
            contested_marks = mean(contested_marks),
            inside_50s = mean(inside_50s),
            clearances = mean(clearances),
            handballs = mean(handballs)) %>%
  ungroup()

season_2020 <- mutate(season_2020, id = rownames(season_2020))

#----------------------- Run the LPA -------------------------------

m1 <- season_2020 %>%
  dplyr::select(-c(id, player_name, playing_for)) %>%
  single_imputation() %>%
  estimate_profiles(1:6,
                    variances = c("equal", "varying"),
                    covariances = c("zero", "varying"))

#----------------------- Assess the fit indices --------------------

m1 %>%
  compare_solutions(statistics = c("AIC", "BIC", "AWE", "CLC", "KIC"))

#----------------------- Plot the model ----------------------------

m1 %>%
  plot_profiles()

#----------------------- Pull outputs into a dataframe -------------

# Get outputs in a dataframe

lpa_outputs <- get_data(m1)

# Filter to just Model 6/Classes 4 as this model had the best BIC value

lpa_outputs_filt <- lpa_outputs %>%
  filter(model_number == "6" & classes_number == 4)

# Join back in to main dataset

final_profiles <- season_2020 %>%
  mutate(id = as.integer(id)) %>%
  inner_join(lpa_outputs_filt, by = c("id" = "id", "kicks" = "kicks", "marks" = "marks",
                                      "goals" = "goals", "behinds" = "behinds",
                                      "contested_possessions" = "contested_possessions", 
                                      "contested_marks" = "contested_marks",
                                      "inside_50s" = "inside_50s", 
                                      "clearances" = "clearances",
                                      "handballs" = "handballs")) %>%
  group_by(id) %>%
  slice(which.max(Probability)) %>%
  ungroup() %>%
  mutate(Class = as.factor(Class))

#----------------------- Look at composition by class --------------

class_freqs <- final_profiles %>%
  mutate(Class = paste0("Class ",Class)) %>%
  group_by(Class, playing_for) %>%
  summarise(counter = n()) %>%
  group_by(Class) %>%
  mutate(props = round((counter / sum(counter))*100, digits = 2)) %>%
  ungroup()

# Make a function as faceting ruins bar ordering

#'
#' @param data dataset to draw from
#' @param class_of_choice the class to be plotted
#'

plot_the_classes <- function(data, class_of_choice){
  my_plot <- data %>%
    filter(Class == class_of_choice) %>%
    ggplot(aes(x = reorder(playing_for, props), y = props)) +
    geom_bar(stat = "identity", fill = "slategray1", colour = "white") +
    labs(title = class_of_choice,
         x = NULL,
         y = "Proportion of players in the profile") +
    scale_y_continuous(labels = function(x) paste0(x,"%")) +
    coord_flip() +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) 
  return(my_plot)
}

# Run function

p <- plot_the_classes(class_freqs, "Class 1")
p1 <- plot_the_classes(class_freqs, "Class 2")
p2 <- plot_the_classes(class_freqs, "Class 3")
p3 <- plot_the_classes(class_freqs, "Class 4")

# Pull into one plot and export

CairoPNG("output/lpa.png", 1000, 600)
ggarrange(p,p1,p2,p3,
          ncol = 2, nrow = 2)
dev.off()
