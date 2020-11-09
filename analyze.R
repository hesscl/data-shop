library(tidyverse)

#set wd to project base folder
setwd("R:/Project/natrent-covid/data-shop")

#read combined source summaries
sum_tbl <- read_csv("./output/multisource_summary_table.csv")


#### A. compare frequency ------------------------------------------------------

overall_n_plot <- sum_tbl %>%
  mutate(metro_name = str_split_fixed(metro_name, ", ", n = 2)[,1]) %>%
  pivot_longer(cols = c(-cbsafp, -metro_name, -source, -cat_beds)) %>%
  filter(name == "n") %>%
  group_by(cbsafp, metro_name, source) %>%
  summarize(count = sum(value)) %>%
  mutate(prop = count/sum(count),
         source = fct_reorder(source, count))

ggplot(overall_n_plot, aes(x = source, y = count)) +
  facet_wrap(~ metro_name, ncol = 1) +
  coord_flip() +
  geom_bar(stat = "identity") +
  theme_bw() +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Source\n") +
  ggsave(filename = "./output/n_by_metro_and_source.pdf",
         width = 10, height = 8, dpi = 300)


#### B. compare empirical rent distributions -----------------------------------


rent_plot <- sum_tbl %>%
  mutate(metro_name = str_split_fixed(metro_name, ", ", n = 2)[,1]) %>%
  pivot_longer(cols = c(-cbsafp, -metro_name, -source, -cat_beds)) %>%
  filter(!str_detect(name, "n|iqr|sd")) %>%
  mutate(p_rank = parse_number(name))

#quantile plot
ggplot(rent_plot %>% filter(cat_beds == "2"), 
       aes(x = p_rank, y = value, color = fct_reorder(source, value))) +
  facet_wrap(~ metro_name) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = unique(rent_plot$p_rank)) + 
  scale_y_continuous(labels = scales::dollar) +
  theme_bw() +
  guides(color = guide_legend(reverse = TRUE)) +
  labs(x = "\nPercentile Rank", y = "Rent Asked\n", color = "Source") +
  ggsave(filename = "./output/rent_dist_by_metro_and_source.pdf",
         width = 10, height = 8, dpi = 300)

#violin plot

