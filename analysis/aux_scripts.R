# Dodged bar plot of cases and deaths per country
data %>% 
  filter(country == "Belgium", date >= dmy("01-03-2020")) %>% 
  select(date, cases_new_smooth, deaths_new_smooth) %>% 
  pivot_longer(cols = -date, names_to = "var", values_to = "val") %>% 
  ggplot(aes(x = date, y = val, fill = var)) + 
  geom_col(position = position_dodge(0.8)) + 
  scale_fill_manual(values = c("darkturquoise", "orangered"))

data %>% 
  filter(country == "Belgium", date >= dmy("01-03-2020")) %>% 
  select(date, cases_new, cases_new_smooth, deaths_new, deaths_new_smooth) %>% 
  ggplot(aes(x = date)) + 
  geom_col(aes(y = cases_new), fill = "darkturquoise") +
  geom_col(aes(y = deaths_new), fill = "orangered") +
  geom_line(aes(y = dplyr::lead(cases_new_smooth, 3)), color = "blue", size = 2) +
  geom_line(aes(y = dplyr::lead(deaths_new_smooth, 3)), color = "red", size = 2)
