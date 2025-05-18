# Objective 2

# Arrange and clean global confirmed and death totals data frames
global_conf_clean <- global_conf_totals %>% 
  rename(Country = `Country/Region`, Count = total_conf) %>% 
  select(Country, Count) %>% 
  arrange(-Count) %>% 
  mutate(Rank = c(1:nrow(global_conf_totals))) %>% 
  relocate(Rank, .before = Country)
global_death_clean <- global_death_totals %>% 
  rename(Country = `Country/Region`, Count = total_deaths) %>% 
  select(Country, Count) %>% 
  arrange(-Count)

# Format data frames to have commas for clarity
global_conf_clean$Count <- format(global_conf_clean$Count, big.mark = ',',
                                  scientific = FALSE)
global_death_clean$Count <- format(global_death_clean$Count, big.mark = ',',
                                   scientific = FALSE)

# Build scroll kable table with formatting
kbl(cbind(global_conf_clean, global_death_clean)) %>% 
  add_header_above(c(' ' = 1,'Confirmations' = 2, 'Deaths' = 2)) %>% 
  kable_paper(bootstrap_options = 'striped') %>% 
  scroll_box(width = '100%', height = '400px')