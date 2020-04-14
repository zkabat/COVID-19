# Dodged bar plot of cases and deaths per country
data %>% 
  filter(country == "Belgium", date >= dmy("01-03-2020")) %>% 
  select(date, cases_new_smooth, deaths_new_smooth, recovered_new_smooth) %>% 
  pivot_longer(cols = -date, names_to = "var", values_to = "val") %>% 
  ggplot(aes(x = date, y = val, fill = var)) + 
  geom_col(position = position_dodge(0.8)) + 
  scale_fill_manual(values = c("darkturquoise", "orangered", "gold"))
