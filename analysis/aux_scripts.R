# # Dodged bar plot of cases and deaths per country
# data %>% 
#   filter(country == "Belgium", date >= dmy("01-03-2020")) %>% 
#   select(date, cases_new_smooth, deaths_new_smooth) %>% 
#   pivot_longer(cols = -date, names_to = "var", values_to = "val") %>% 
#   ggplot(aes(x = date, y = val, fill = var)) + 
#   geom_col(position = position_dodge(0.8)) + 
#   scale_fill_manual(values = c("darkturquoise", "orangered"))
# 
# data %>% 
#   filter(country == "Belgium", date >= dmy("01-03-2020")) %>% 
#   select(date, cases_new, cases_new_smooth, deaths_new, deaths_new_smooth) %>% 
#   ggplot(aes(x = date)) + 
#   geom_col(aes(y = cases_new), fill = "darkturquoise") +
#   geom_col(aes(y = deaths_new), fill = "orangered") +
#   geom_line(aes(y = dplyr::lead(cases_new_smooth, 3)), color = "blue", size = 2) +
#   geom_line(aes(y = dplyr::lead(deaths_new_smooth, 3)), color = "red", size = 2)
# 
# data %>% 
#   group_by(country) %>% 
#   filter(max(cases_cum) >= 1000) %>% 
#   mutate(cases_new = scales::squish(cases_new, 
#                             quantile(cases_new, c(.01, .99), na.rm = T)),
#          deaths_new = scales::squish(deaths_new, 
#                              quantile(deaths_new, c(.01, .99), na.rm = T))) %>% 
#   ungroup() %>% 
#   filter(date >= dmy("01-03-2020")) %>% 
#   select(country, date, cases_new, cases_new_smooth, cases_cum,
#          deaths_new, deaths_new_smooth, deaths_cum) %>% 
#   ggplot(aes(x = date)) + 
#   geom_col(aes(y = cases_new), fill = "darkturquoise") +
#   geom_line(aes(y = dplyr::lead(cases_new_smooth, 3)), color = "blue", size = 1) +
#   trelliscopejs::facet_trelliscope(
#     ~country, nrow = 2, ncol = 5, scales = c("same", "free"), as_plotly = F,
#     name = "Statistics by country",  
#     desc = "Daily counts with 7-day rolling average",
#     state = list(sort = list(sort_spec("cases_cum_max", dir = "desc")))
#   )
# 
# data %>% 
#   group_by(country) %>% 
#   filter(max(cases_cum) >= 1000) %>% 
#   mutate(cases_new = scales::squish(cases_new, 
#                                     quantile(cases_new, c(.01, .99), na.rm = T)),
#          deaths_new = scales::squish(deaths_new, 
#                                      quantile(deaths_new, c(.01, .99), na.rm = T))) %>% 
#   ungroup() %>% 
#   filter(date >= dmy("01-03-2020")) %>% 
#   select(country, date, cases_new, cases_new_smooth, cases_cum,
#          deaths_new, deaths_new_smooth, deaths_cum) %>% 
#   ggplot(aes(x = date)) + 
#   geom_col(aes(y = deaths_new), fill = "orange") +
#   geom_line(aes(y = dplyr::lead(deaths_new_smooth, 3)), color = "red", size = 1) +
#   trelliscopejs::facet_trelliscope(
#     ~country, nrow = 2, ncol = 5, scales = c("same", "free"), as_plotly = F,
#     name = "Statistics by country",  
#     desc = "Daily counts with 7-day rolling average",
#     state = list(sort = list(sort_spec("deaths_cum_max", dir = "desc")))
#   )

library(trelliscopejs)

data %>% 
  group_by(country) %>% 
  filter(max(cases_cum) >= 1000) %>% 
  mutate(cases_new_per_mil = 
           scales::squish(cases_new_per_mil, 
                          quantile(cases_new_per_mil, c(.01, .99), na.rm = T)),
         deaths_new_per_mil = 
           scales::squish(deaths_new_per_mil, 
                          quantile(deaths_new_per_mil, c(.01, .99), na.rm = T))
  ) %>% 
  ungroup() %>% 
  mutate_if(is.numeric, function(x) (abs(x)+x)/2) %>% 
  filter(date >= dmy("01-03-2020")) %>% 
  select(country, date, 
         cases_new_per_mil, cases_new_smooth_per_mil, cases_cum_per_mil,
         deaths_new_per_mil, deaths_new_smooth_per_mil, deaths_cum_per_mil) %>% 
  ggplot(aes(x = date)) + 
  geom_col(aes(y = cases_new_per_mil), fill = "darkturquoise") +
  geom_line(aes(y = dplyr::lead(cases_new_smooth_per_mil, 3)), 
            color = "blue", size = 1) +
  trelliscopejs::facet_trelliscope(
    ~country, nrow = 2, ncol = 5, scales = c("same", "free"), as_plotly = F,
    name = "Statistics by country",  
    desc = "Daily counts with 7-day rolling average",
    state = list(sort = list(sort_spec("cases_cum_per_mil_max", dir = "desc")))
  )

data %>% 
  group_by(country) %>% 
  filter(max(cases_cum) >= 1000) %>% 
  mutate(cases_new_per_mil = 
           scales::squish(cases_new_per_mil, 
                          quantile(cases_new_per_mil, c(.01, .99), na.rm = T)),
         deaths_new_per_mil = 
           scales::squish(deaths_new_per_mil, 
                          quantile(deaths_new_per_mil, c(.01, .99), na.rm = T))
  ) %>% 
  ungroup() %>% 
  mutate_if(is.numeric, function(x) (abs(x)+x)/2) %>% 
  filter(date >= dmy("01-03-2020")) %>% 
  select(country, date, 
         cases_new_per_mil, cases_new_smooth_per_mil, cases_cum_per_mil,
         deaths_new_per_mil, deaths_new_smooth_per_mil, deaths_cum_per_mil) %>% 
  ggplot(aes(x = date)) + 
  geom_col(aes(y = deaths_new_per_mil), fill = "orange") +
  geom_line(aes(y = dplyr::lead(deaths_new_smooth_per_mil, 3)), 
            color = "red", size = 1) +
  trelliscopejs::facet_trelliscope(
    ~country, nrow = 2, ncol = 5, scales = c("same", "free"), as_plotly = F,
    name = "Statistics by country",  
    desc = "Daily counts with 7-day rolling average",
    state = list(sort = list(sort_spec("deaths_cum_per_mil_max", dir = "desc")))
  )
