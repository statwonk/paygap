# devtools::install_github("hadley/readr", ref = "fc901adb32cc731cf427cc57a8cb3db778b39794")
# devtools::install_github("hadley/dplyr", ref = "v0.4.3")
# devtools::install_github("hadley/tidyr", ref = "v0.3.1")
# devtools::install_github("hadley/ggplot2", ref = "v1.0.1")
# devtools::install_github("hadley/scales", ref = "v0.3.0")
# devtools::install_github("wilkelab/cowplot", ref = "0.5.0")
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(cowplot)

series <- tbl_df(
  read_tsv(
    url("http://download.bls.gov/pub/time.series/le/le.series")
  )) %>%
  # first select the series:
  # http://download.bls.gov/pub/time.series/le/
  filter(sexs_code == 1 | sexs_code == 2,
         lfst_code == 25, # full employment - http://download.bls.gov/pub/time.series/le/le.lfst
         pcts_code == 0, # http://download.bls.gov/pub/time.series/le/le.pcts
         earn_code == 1, # median nominal earnings - http://download.bls.gov/pub/time.series/le/le.earn
         class_code == 16, # http://download.bls.gov/pub/time.series/le/le.class
         unin_code == 0, # http://download.bls.gov/pub/time.series/le/le.unin
         indy_code == 0, # http://download.bls.gov/pub/time.series/le/le.indy
         education_code == 0, # all levels - http://download.bls.gov/pub/time.series/le/le.education
         ages_code == 0, # 16 or older  http://download.bls.gov/pub/time.series/le/le.ages
         race_code == 0, # all races - http://download.bls.gov/pub/time.series/le/le.race
         orig_code == 0, # http://download.bls.gov/pub/time.series/le/le.orig
         born_code == 0, # http://download.bls.gov/pub/time.series/le/le.born
         seasonal == "U") # http://download.bls.gov/pub/time.series/le/le.seasonal

median_weekly_earnings <- inner_join(
  inner_join(
    # earnings
    tbl_df(tbl_df(read_tsv(
      url("http://download.bls.gov/pub/time.series/le/le.data.0.Current")
    ))),
    series),
  # occupations
  tbl_df(
    read_tsv(
      url("http://download.bls.gov/pub/time.series/le/le.occupation")
    )
  ) %>% select(occupation_code, occupation_text)
) %>%
  filter(year == 2014, period == "A01") %>%
  arrange(occupation_text, sexs_code) %>%
  select(occupation_text, sexs_code, value) %>%
  mutate(sexs_code = ifelse(sexs_code == 2, "Woman", "Man")) %>%
  spread(sexs_code, value) %>%
  mutate(woman_to_man_pay = Woman / Man) %>%
  arrange(desc(woman_to_man_pay))

p <- ggplot(median_weekly_earnings %>%
         filter(!is.na(woman_to_man_pay)) %>%
         mutate(occupation_text = factor(occupation_text,
                                         levels = rev(occupation_text))),
       aes(y = occupation_text,
           x = woman_to_man_pay)) +
  geom_vline(xintercept = 1, colour = 'red') +
  geom_point() +
  scale_x_continuous(labels = percent,
                     breaks = seq(0, 1, 0.05)) +
  ylab("") +
  xlab("Ratio of women's pay to men's pay") +
  ggtitle("Pay gap by occupation, U.S. full time workers (http://www.bls.gov/cps/cpsaat39.htm)\n") +
  theme_bw(base_size = 20) +
  theme(axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 16))

ggdraw(switch_axis_position(p, 'xy'))
ggsave(filename = "paygap.svg", height = 20, width = 15)
