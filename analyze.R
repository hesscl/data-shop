library(tidyverse)

#set wd to project base folder
setwd("R:/Project/natrent-data-shop")

#read combined source summaries
sum_tbl <- read_csv("./output/multisource_summary_table.csv") %>%
  mutate(metro_name = str_split_fixed(metro_name, ", ", n = 2)[,1],
         source = fct_reorder(source, q50))


#### A. compare frequency ------------------------------------------------------

#data for plotting frequency by source and metro
overall_n_plot <- sum_tbl %>%
  pivot_longer(cols = c(-cbsafp, -metro_name, -source, -cat_beds)) %>%
  filter(name == "n", source != "2018 ACS") %>%
  group_by(cbsafp, metro_name, source) %>%
  summarize(count = sum(value)) %>% 
  mutate(prop = count/sum(count),
         source = fct_reorder(source, count))

#plot of frequency by source and metro
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

#data for rent quantile plot
rent_plot <- sum_tbl %>%
  pivot_longer(cols = c(-cbsafp, -metro_name, -source, -cat_beds)) %>%
  filter(!str_detect(name, "n|iqr")) %>%
  mutate(p_rank = parse_number(name))

#rent quantile plot
ggplot(rent_plot %>% filter(cat_beds == "2"), 
       aes(x = p_rank, y = value, color = fct_reorder(source, value))) +
  facet_wrap(~ metro_name) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = unique(rent_plot$p_rank)) + 
  scale_y_continuous(labels = scales::dollar) +
  scale_color_discrete(direction = -1) +
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(reverse = TRUE, nrow = 1)) +
  labs(x = "\nPercentile Rank", y = "Rent Asked\n", color = "Source") +
  ggsave(filename = "./output/rent_dist_by_metro_and_source.pdf",
         width = 12, height = 6, dpi = 300)

#data for iqr plot
iqr_plot <- sum_tbl %>% 
  filter(cat_beds == "2") %>%
  ungroup()

#IQR for 2 bedroom units
ggplot(iqr_plot, 
       aes(x = source, y = iqr, color = source)) +
  coord_flip(ylim = c(0, 925)) +
  facet_wrap(~ metro_name) +
  geom_segment(aes(x = source, xend = source, y = 0, yend = iqr)) +
  geom_point(size = 8) +
  geom_text(aes(label = round(iqr)), color = "white", size = 3) +
  scale_y_continuous(labels = scales::dollar) +
  scale_color_discrete(direction = -1) +
  theme_bw() +
  theme(legend.position = "bottom", panel.spacing = unit(.2, "in")) +
  guides(color = FALSE) +
  labs(x = "Source\n", y = "\nRent Asked IQR", color = "") +
  ggsave(filename = "./output/rent_iqr_by_metro_and_source.pdf",
         width = 8, height = 6, dpi = 300)

#data for plotting ratio of scraped source median to ACS median
rent_ratio_plot <- rent_plot %>%
  filter(p_rank == 50, cat_beds == "2") %>%
  group_by(cbsafp, metro_name, cat_beds) %>%
  mutate(ratio = value / value[source == "2018 ACS"]) %>%
  filter(source != "2018 ACS") 

#rent ratio plot
ggplot(rent_ratio_plot, 
       aes(x = source, y = ratio, color = source)) +
  coord_flip(ylim = c(1, 2.3)) +
  facet_wrap(~ metro_name) +
  geom_segment(aes(x = source, xend = source, y = 0, yend = ratio)) +
  geom_point(size = 8) +
  geom_text(aes(label = round(ratio, 1)), color = "white", size = 3) +
  scale_color_manual(values = rev(scales::hue_pal()(8)[1:7])) +
  theme_bw() +
  theme(legend.position = "bottom", panel.spacing = unit(.2, "in")) +
  guides(color = FALSE) +
  labs(x = "Source\n", y = "\nSource Median Rent ÷ 2018 ACS Median Rent", color = "") +
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
beds_plot$source <- factor(beds_plot$source, levels = levels(beds_plot$source)[c(7, 4, 2, 3, 1, 6, 5)])

#get legend ordering right
beds_plot$cat_beds <- fct_rev(beds_plot$cat_beds)

#make plot
ggplot(beds_plot, aes(x = source, y = prop, fill = cat_beds)) +
  coord_flip() +
  facet_wrap(~ metro_name) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d(option = "D", direction = 1) +
  theme_bw() +
  theme(legend.position = "bottom", panel.spacing = unit(.2, "in"),
        axis.text.x = element_text()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "", y = "\nShare of Listings", fill = "Bedroom Size") +
  ggsave(filename = "./output/bed_comp_by_metro_and_source.pdf",
         width = 8, height = 6, dpi = 300)
