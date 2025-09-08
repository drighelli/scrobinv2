df <- data.frame(
    Percentage = c("1%", "1%", "1%", "1%",
                   "5%", "5%", "5%", "5%",
                   "10%", "10%", "10%", "10%",
                   "15%", "15%", "15%", "15%",
                   "20%", "20%", "20%", "20%",
                   "25%", "25%", "25%", "25%",
                   "30%", "30%", "30%", "30%",
                   "35%", "35%", "35%", "35%",
                   "40%", "40%", "40%", "40%",
                   "45%", "45%", "45%", "45%",
                   "50%", "50%", "50%", "50%"),
    Ncells = c(580, 580, 580, 580,
               2900, 2900, 2900, 2900,
               5800, 5800, 5800, 5800,
               8700, 8700, 8700, 8700,
               11600, 11600, 11600, 11600,
               14500, 14500, 14500, 14500,
               17400, 17400, 17400, 17400,
               20300, 20300, 20300, 20300,
               23200, 23200, 23200, 23200,
               26100, 26100, 26100, 26100,
               29000, 29000, 29000, 29000),
    Edges = c(17429, 17429, 17429, 17429,
              86796, 86796, 86796, 86796,
              189771, 189771, 189771, 189771,
              307048, 307048, 307048, 307048,
              419723, 419723, 419723, 419723,
              541615, 541615, 541615, 541615,
              656453, 656453, 656453, 656453,
              770781, 770781, 770781, 770781,
              893303, 893303, 893303, 893303,
              1013844, 1013844, 1013844, 1013844,
              1129064, 1129064, 1129064, 1129064),
    Algorithm = c("robinCompare", "robinCompare", "robinRobust", "robinRobust",
                  "robinCompare", "robinCompare", "robinRobust", "robinRobust",
                  "robinCompare", "robinCompare", "robinRobust", "robinRobust",
                  "robinCompare", "robinCompare", "robinRobust", "robinRobust",
                  "robinCompare", "robinCompare", "robinRobust", "robinRobust",
                  "robinCompare", "robinCompare", "robinRobust", "robinRobust",
                  "robinCompare", "robinCompare", "robinRobust", "robinRobust",
                  "robinCompare", "robinCompare", "robinRobust", "robinRobust",
                  "robinCompare", "robinCompare", "robinRobust", "robinRobust",
                  "robinCompare", "robinCompare", "robinRobust", "robinRobust",
                  "robinCompare", "robinCompare", "robinRobust", "robinRobust"),
    Cores = c(12, 1, 12, 1,
              12, 1, 12, 1,
              12, 1, 12, 1,
              12, 1, 12, 1,
              12, 1, 12, 1,
              12, 1, 12, 1,
              12, 1, 12, 1,
              12, 1, 12, 1,
              12, 1, 12, 1,
              12, 1, 12, 1,
              12, 1, 12, 1),
    Hours = c(0, 0, 0, 0,
              0, 0, 0, 0,
              0, 0, 0, 0,
              0, 0, 0, 0,
              0, 0, 0, 0,
              0, 1, 0, 0,
              0, 1, 0, 0,
              0, 1, 0, 0,
              0, 2, 0, 0,
              0, 2, 0, 0,
              0, 3, 0, 0),
    Minutes = c(0, 0, 0, 0,
                0, 5, 0, 0,
                1, 14, 0, 2,
                3, 32, 0, 3,
                4, 45, 0, 5,
                7, 7, 1, 8,
                9, 26, 1, 10,
                12, 53, 1, 14,
                15, 18, 2, 16,
                19, 54, 2, 20,
                24, 32, 2, 24),
    Seconds = c(2.43, 14.64, 1.73, 6.74,
                42.94, 8.94, 6.33, 46.46,
                49.22, 48.85, 17.23, 6.63,
                24.09, 41.86, 31.03, 54.26,
                58.84, 41.24, 44.92, 51.25,
                2.14, 41.65, 8.97, 34.31,
                24.08, 36.35, 21.76, 55.40,
                24.79, 45.06, 51.31, 37.30,
                9.90, 18.95, 13.25, 59.66,
                33.79, 13.74, 28.70, 45.26,
                6.72, 8.02, 58.78, 52.98)
)
library(ggplot2)
library(dplyr)

# Aggiungo colonna totale in secondi
df <- df %>%
    mutate(TotalSeconds = Hours * 3600 + Minutes * 60 + Seconds)

# Filtro solo robinCompare
df_compare <- df %>% filter(Algorithm == "robinCompare")

# Plot
ggplot(df_compare, aes(x = as.numeric(sub("%", "", Percentage)),
                       y = TotalSeconds,
                       color = factor(Cores),
                       group = Cores)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    scale_color_manual(values = c("12" = "orange", "1" = "red"),
                       labels = c("12 cores", "1 core"),
                       name = NULL) +
    scale_y_continuous(breaks = seq(0, 14000, by = 2000)) +   # tick ogni 2000
    labs(title = "Performance of robinCompare Algorithm",
         x = "Graph Sampling Percentage",
         y = "Time (seconds)") +
    theme_minimal(base_size = 14) +
    theme(legend.position = c(0.05, 0.95),
          legend.justification = c("left", "top"),
          legend.background = element_rect(fill = alpha("white", 0.6),
                                           colour = "gray80"))


# Filtro solo robinCompare
df_compare <- df %>% filter(Algorithm == "robinRobust")

# Plot
ggplot(df_compare, aes(x = as.numeric(sub("%", "", Percentage)),
                       y = TotalSeconds,
                       color = factor(Cores),
                       group = Cores)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    scale_color_manual(values = c("12" = "orange", "1" = "red"),
                       labels = c("12 cores", "1 core"),
                       name = NULL) +
    scale_y_continuous(breaks = seq(0, 2000, by = 200)) +   # tick ogni 2000
    labs(title = "Performance of robinRobust Algorithm",
         x = "Graph Sampling Percentage",
         y = "Time (seconds)") +
    theme_minimal(base_size = 14) +
    theme(legend.position = c(0.05, 0.95),
          legend.justification = c("left", "top"),
          legend.background = element_rect(fill = alpha("white", 0.6),
                                           colour = "gray80"))

