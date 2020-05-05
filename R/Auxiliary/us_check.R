#If we check the plot of individual items it appears everything is OK

data_ya %>%
  select(contains("scsf"), wave) %>%
  pivot_longer(contains("scsf"), "item") %>%
  group_by(item, wave) %>%
  summarise(value = mean(value, na.rm = T)) %>%
  ggplot(aes(x = wave, y = value, color = item, group = item))+
  geom_line()

#However, if we check the aggregate, something is odd

data_ya %>%
  mutate(scsf = select(., contains("scsf")) %>%
           rowMeans(na.rm = T)) %>%
  group_by(wave) %>%
  summarise(scsf = mean(scsf, na.rm = T)) %>%
  ggplot(aes(x = wave, y = scsf, group = 1))+
  geom_line()

#Let's look at them all together

data_ya %>%
  mutate(scsf = select(., contains("scsf")) %>%
           rowMeans(na.rm = T)) %>%
  select(contains("scsf"), wave) %>%
  pivot_longer(contains("scsf"), "item") %>%
  group_by(item, wave) %>%
  summarise(value = mean(value, na.rm = T)) %>%
  ggplot(aes(x = wave, y = value, color = item, group = item))+
  geom_line()

#There are no changes in minima and maxima of items

data_ya %>%
  select(contains("scsf"), wave) %>%
  pivot_longer(contains("scsf"), "item") %>%
  filter(wave != 1) %>%
  group_by(wave, item) %>%
  summarise(min = min(value, na.rm = T), max = max(value, na.rm = T)) %>%
  pivot_longer(c(min, max), "indicator") %>%
  ggplot(aes(x = wave, y = value, group = 1))+
  geom_line()+
  facet_grid(cols = vars(indicator), rows = vars(item))

#Pretty messy code from this point onwards, trying to extract the items from individual files with minimal reshaping to see if the pattern is due to some coding error

files_ya_detailed <- map(files_pya, read_spss)
files_ya_detailed <- map(files_ya_detailed, select, contains("scsf"))

files_ya_detailed[[1]] <- NULL

for(i in 1:8){
  for(j in 1:length(colnames(files_ya_detailed[[i]]))){
    colnames(files_ya_detailed[[i]])[[j]] <- substring(colnames(files_ya_detailed[[i]])[[j]], 3)
  }
}

for(i in 1:8){
  files_ya_detailed[[i]]$wave <- i
}

files_ya_detailed[[1]] %>%
  full_join(files_ya_detailed[[2]]) %>%
  full_join(files_ya_detailed[[3]]) %>%
  full_join(files_ya_detailed[[4]]) %>%
  full_join(files_ya_detailed[[5]]) %>%
  full_join(files_ya_detailed[[6]]) %>%
  full_join(files_ya_detailed[[7]]) %>%
  full_join(files_ya_detailed[[8]]) -> files_ya_2

files_ya_2[files_ya_2 < 0] <- NA

files_ya_2 %>%
  remove_labels() %>%
  select(contains("scsf"), wave) %>%
  pivot_longer(contains("scsf"), "item") %>%
  group_by(item, wave) %>%
  summarise(value = mean(value, na.rm = T)) %>%
  ggplot(aes(x = wave, y = value, color = item, group = item))+
  geom_line()

files_ya_2 %>%
  remove_labels() %>%
  mutate(scsf = select(., contains("scsf")) %>%
           rowMeans(na.rm = T)) %>%
  group_by(wave) %>%
  summarise(scsf = mean(scsf, na.rm = T)) %>%
  ggplot(aes(x = wave, y = scsf, group = 1))+
  geom_line()

files_ya_2 %>%
  remove_labels() %>%
  mutate(scsf = select(., contains("scsf")) %>%
           rowMeans(na.rm = T)) %>%
  select(contains("scsf"), wave) %>%
  pivot_longer(contains("scsf"), "item") %>%
  group_by(item, wave) %>%
  summarise(value = mean(value, na.rm = T)) %>%
  ggplot(aes(x = wave, y = value, color = item, group = item))+
  geom_line()
