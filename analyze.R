library(tidyverse)

#set wd to project base folder
setwd("R:/Project/natrent-data-shop")

#read combined source summaries
sum_tbl <- read_csv("./output/multisource_summary_table.csv") %>%
  mutate(metro_name = str_split_fixed(metro_name, ", ", n = 2)[,1])


#### A. compare frequency ------------------------------------------------------

overall_n_plot <- sum_tbl %>%
  pivot_longer(cols = c(-cbsafp, -metro_name, -source, -cat_beds)) %>%
  filter(name == "n", source != "2018 ACS") %>%
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
  scale_color_manual(values = c("Black", RColorBrewer::brewer.pal(5, "Set1"))) +
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(reverse = TRUE, nrow = 1)) +
  labs(x = "\nPercentile Rank", y = "Rent Asked\n", color = "Source") +
  ggsave(filename = "./output/rent_dist_by_metro_and_source.pdf",
         width = 12, height = 6, dpi = 300)

#ratio of scraped source median to ACS median
rent_ratio_plot <- rent_plot %>%
  filter(p_rank == 50, cat_beds == "2") %>%
  group_by(cbsafp, metro_name, cat_beds) %>%
  mutate(ratio = value / value[source == "2018 ACS"]) %>%
  filter(source != "2018 ACS") %>%
  mutate(source = fct_reorder(source, ratio))

ggplot(rent_ratio_plot, aes(x = metro_name, y = ratio, fill = source, group = source)) +
  coord_flip(ylim = c(1, 2.25)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(breaks = c(1, 1.25, 1.5, 1.75, 2, 2.25)) +
  scale_fill_viridis_d(option = "D", direction = -1) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.major.y = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "", y = "\nSource Median ÷ 2018 ACS Median", fill = "Source") +
  ggsave(filename = "./output/rent_ratio_by_metro_and_source.pdf",
         width = 8, height = 6, dpi = 300)
  

#### C. Compare distribution by bedroom size -----------------------------------  

#bedroom size composition
beds_plot <- sum_tbl %>%
  filter(source != "2018 ACS") %>%
  select(cbsafp, metro_name, source, cat_beds, n) %>%
  group_by(cbsafp, metro_name, source) %>%
  mutate(prop = n/sum(n)) %>%
  arrange(cat_beds)

#get the sources (somewhat) diagnolized
beds_plot$source <- factor(beds_plot$source)
beds_plot$source <- factor(beds_plot$source, levels = levels(beds_plot$source)[c(2, 5, 3, 4, 1)])

#get legend ordering right
beds_plot$cat_beds <- fct_rev(beds_plot$cat_beds)

#make plot
ggplot(beds_plot, aes(x = source, y = prop, fill = cat_beds)) +
  facet_wrap(~ metro_name) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d(option = "D", direction = 1) +
  theme_bw() +
  theme(legend.position = "bottom", panel.spacing = unit(.2, "in"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "", y = "Share of Listings\n", fill = "Bedroom Size") +
  ggsave(filename = "./output/bed_comp_by_metro_and_source.pdf",
         width = 8, height = 6, dpi = 300)
