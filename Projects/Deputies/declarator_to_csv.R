require(stringr)
require(dplyr)

x <- data.table::fread("Test_declarations.csv", nrows = 1)

nam <- x %>% 
  select(main.person.name, main.year, main.office.name, main.party.name,
         matches("incomes\\..+\\.size"), matches("incomes\\..+\\.comment")) %>% 
  names()

df <- data.table::fread("Test_declarations.csv", select = nam)

year_aggregate <- df %>% 
  mutate(total_income = rowSums(select(., matches("incomes\\..+\\.size")), na.rm = T)) %>% 
  group_by(main.year) %>% 
  summarise(declarations = n(),
            total_income = sum(total_income),
            income_per_declaration = total_income / declarations) %>% 
  janitor::adorn_totals()

data.table::fwrite(year_aggregate, "total_income_by_year.csv")

party_aggregate <- df %>% 
  mutate(total_income = rowSums(select(., matches("incomes\\..+\\.size")), na.rm = T)) %>% 
  group_by(main.year, main.party.name) %>% 
  filter(main.party.name != '') %>% 
  summarise(total_income = sum(total_income), 
            declarations = n(),
            income_per_declaration = total_income / declarations,
            max_income = max(total_income), 
            min_income = min(total_income),
            mean_income = mean(total_income),
            median_income = median(total_income)
  ) %>% 
  janitor::adorn_totals()

data.table::fwrite(party_aggregate, "total_income_by_partyes.csv")

office_aggregate <- df %>% 
  mutate(total_income = rowSums(select(., matches("incomes\\..+\\.size")), na.rm = T)) %>% 
  group_by(main.year, main.office.name) %>% 
  filter(main.office.name != '') %>% 
  summarise(total_income = sum(total_income), 
            declarations = n(),
            income_per_declaration = total_income / declarations,
            max_income = max(total_income), 
            min_income = min(total_income),
            mean_income = mean(total_income),
            median_income = median(total_income)
  ) %>% 
  janitor::adorn_totals()

data.table::fwrite(office_aggregate, "total_income_by_office.csv")

incomes <- df %>% 
  transmute(name = main.person.name, year = main.year, office = main.office.name, party = main.party.name,
            total_income = rowSums(select(., matches("incomes\\..+\\.size")), na.rm = T)) %>% 
  arrange(name)

data.table::fwrite(incomes, "incomes_deputies.csv")

