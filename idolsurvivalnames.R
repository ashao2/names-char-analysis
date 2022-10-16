library(tidyverse)

names <- read_csv("/Users/allison/Desktop/c_idol_names.csv")

long_names <- names %>% filter(nchar(name)>3)

# Create surnames var
names$surname <- ifelse(nchar(names$name) == 4,
                        substring(names$name, 1, 2),
                        substring(names$name, 1, 1))
# Create given names var
names$givenname <- ifelse(nchar(names$name) == 4,
                          substring(names$name, 3, nchar(names$name)),
                          substring(names$name, 2, nchar(names$name)))

# Create given name 1st char var
names$given1 <- substring(names$givenname, 1, 1)
# Create given name 2nd char var
names$given2 <- ifelse(nchar(names$givenname) == 2,
                       substring(names$givenname, 2, 2),
                       NA)

write_csv(names, "/Users/allison/Desktop/add_pinyin.csv")

# See most common surnames
top_surname <- names %>%
  group_by(surname) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# 1st character
first_char <- names %>%
  select(gender, given1) %>%
  rename(characters = given1)
# 2nd character
second_char <- names %>%
  select(gender, given2) %>%
  filter(!is.na(given2)) %>%
  rename(characters = given2)
# Concat
given_name_chars <- rbind(first_char, second_char)

#given_name_chars <- given_name_chars[!is.na(given_name_chars$characters),]

# See most common chars
top_chars <- given_name_chars %>%
  group_by(characters) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# See most common chars for girls
f_top_chars <- given_name_chars %>%
  filter(gender == "Female") %>%
  group_by(characters) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# See most common chars for guys
m_top_chars <- given_name_chars %>%
  filter(gender == "Male") %>%
  group_by(characters) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Separate single-char given names
single_char <- names %>%
  filter(is.na(given2)) %>%
  select(gender, given1) %>%
  rename(characters = given1)

# Top single-char given names
top_single_char <- single_char %>%
  group_by(characters) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Top female single-char names
f_single_char <- single_char %>%
  filter(gender == "Female") %>%
  group_by(characters) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Top male single-char names
m_single_char <- single_char %>%
  filter(gender == "Male") %>%
  group_by(characters) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Isolate 1st char (shuangming)
two_char_1 <- names %>%
  filter(!is.na(given2)) %>%
  select(gender, given1) %>%
  rename(characters = given1)

top_1st_char <- two_char_1 %>%
  group_by(characters) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# See most common FIRST char female
f_char_1 <- two_char_1 %>%
  filter(gender == "Female") %>%
  group_by(characters) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# See most common FIRST char male
m_char_1 <- two_char_1 %>%
  filter(gender == "Male") %>%
  group_by(characters) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Isolate 2nd char (shuangming)
two_char_2 <- names %>%
  filter(!is.na(given2)) %>%
  select(gender, given2) %>%
  rename(characters = given2)

top_2nd_char <- two_char_2 %>%
  group_by(characters) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# See most common SECOND char female
f_char_2 <- two_char_2 %>%
  filter(gender == "Female") %>%
  group_by(characters) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# See most common SECOND char male
m_char_2 <- two_char_2 %>%
  filter(gender == "Male") %>%
  group_by(characters) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# df %>% add_column(z = -1:1, w = 0)
f_top_chars <- f_top_chars %>% add_column(rankf = c(1:681))
m_top_chars <- m_top_chars %>% add_column(rankm = c(1:758))

# Create a joint table to compare rankings by gender
compare_gender <- full_join(f_top_chars, m_top_chars, by = "characters")
compare_gender$rankt <- compare_gender$rankf + compare_gender$rankm
compare_gender <- compare_gender %>% arrange(rankt)

# Characters only in female names
female_coded <- compare_gender %>%
  filter(is.na(rankm)) %>%
  arrange(desc(count.x))

# Characters only in male names
male_coded <- compare_gender %>%
  filter(is.na(rankf)) %>%
  arrange(desc(count.y))

# Compare counts instead
compare_gender[is.na(compare_gender)] = 0
compare_gender$countt <- abs(compare_gender$count.x - compare_gender$count.y)

# Discrepancy in character position
top_1st_char <- top_1st_char %>% add_column(rank = c(1:709))
top_2nd_char <- top_2nd_char %>% add_column(rank = c(1:673))
discrep <- full_join(top_1st_char, top_2nd_char, by = "characters")
discrep$count.y[is.na(discrep$count.y)] = 0
discrep$count.y[is.na(discrep$count.y)] = 0
discrep$delta <- (discrep$count.x - discrep$count.y)
discrep <- discrep %>% arrange(count.y, desc(count.x))
discrep <- discrep %>% arrange(count.x, desc(count.y))

discrep$rankdelta <- abs(discrep$rank.x - discrep$rank.y)

female_discrep <-full_join(f_char_1, f_char_2, by = "characters")
female_discrep[is.na(female_discrep)] = 0
female_discrep$delta <- (female_discrep$count.x - female_discrep$count.y)
female_discrep <- female_discrep %>% arrange(desc(abs(delta)))
female_discrep <- female_discrep %>% arrange(count.y, desc(count.x))
female_discrep <- female_discrep %>% arrange(count.x, desc(count.y))

male_discrep <- full_join(m_char_1, m_char_2, by = "characters")
male_discrep[is.na(male_discrep)] = 0
male_discrep$delta <- (male_discrep$count.x - male_discrep$count.y)
male_discrep <- male_discrep %>% arrange(count.y, desc(count.x))
male_discrep <- male_discrep %>% arrange(count.x, desc(count.y))

overall_top <- names %>%
  group_by(givenname) %>%
  summarise(count=n()) %>%
  arrange(desc(count))
overall_top_double <- overall_top %>% filter(nchar(givenname) > 1)


names %>%
  filter(given1 == given2) %>%
  group_by(givenname) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  print(n=25)
