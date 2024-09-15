library(tidyverse)
library(spcstyle)

d <- tribble(~concept, ~better_than,
  "Safely handle complex, multiple datasets", "Stata,SPSS,Excel",
  "Intuitive approach to rectangular data", "Python",
  "Easy to extend via functions and packages", "SPSS, SQL, Power BI,Excel",
  "All the statistical functionality", "Excel,Power BI,SPSS,SQL",
  "Data visualisation", "SAS,Stata,SPSS,Excel,SQL",
  "Geocomputation", "Stata,SPSS,Excel,Power BI",
  "All in code and integrated with version control", "Excel,Power BI,SPSS,Stata",
  "Integrate analysis, output and formatted text", "SPSS,Stata,SAS,SQL"
) |>
  separate(better_than, into = letters, sep = ",", fill = "right") |>
  gather(sequence, software, -concept) |>
  drop_na() |>
  select(-sequence) |>
  mutate(software = str_squish(software)) |>
  mutate(count = 0)  |>
  complete(software, concept, fill = list(count = 1)) |>
  mutate(concept = fct_reorder(concept, count, .fun = sum),
         software = fct_reorder(software, count, .fun = sum))

d <- rbind(d,
           tibble(software = "R", concept = unique(d$concept), count = 1))


p <- d |>
  mutate(strength = ifelse(count == 1, "Yes", "No")) |>
  ggplot(aes(x = software, y = concept)) +
  geom_tile(aes(fill = strength), colour = "white") +
  theme_minimal() +
  labs(title = "Comparing R to other software",
       x = "",
       y = "",
       fill = "Strength?:") +
  scale_fill_manual(values = spc_cols(1:2)) +
  theme(legend.position = "bottom")

png("output/outperform.png", w = 5000, h = 3500, res = 600, type = "cairo",
    bg = "transparent")
print(p)
dev.off()
