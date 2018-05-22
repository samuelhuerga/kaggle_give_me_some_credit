#_________________________________
# 01 - Target variable -----------
#_________________________________

train_df %>% 
  count(seriousdlqin2yrs) %>% 
  mutate(per = n/sum(n))

#_________________________________
# 02 - Target variable vs rest of variables -----------
#_________________________________

names(train_df)

# revolvingutilizationofunsecuredlines 
train_df %>% 
  mutate(seriousdlqin2yrs = as.character(seriousdlqin2yrs)) %>% 
  ggplot(aes(x = revolvingutilizationofunsecuredlines, color = seriousdlqin2yrs)) +
  geom_density() +
  xlim(c(0,2.5))
  
# age 
train_df %>% 
  mutate(seriousdlqin2yrs = as.character(seriousdlqin2yrs)) %>% 
  ggplot(aes(x = age, color = seriousdlqin2yrs)) +
  geom_density()
  

# numberoftime30_59dayspastduenotworse 
train_df %>% 
  mutate(seriousdlqin2yrs = as.character(seriousdlqin2yrs)) %>% 
  ggplot(aes(x = numberoftime30_59dayspastduenotworse, fill = seriousdlqin2yrs)) +
  geom_bar() +
  facet_grid(seriousdlqin2yrs~.)+
  xlim(c(0,15))
  
# debtratio 
train_df %>% 
  mutate(seriousdlqin2yrs = as.character(seriousdlqin2yrs)) %>% 
  ggplot(aes(x = debtratio, color = seriousdlqin2yrs)) +
  geom_density() +
  xlim(0,5)

train_df %>% 
  mutate(seriousdlqin2yrs = as.character(seriousdlqin2yrs),
         debtratio = cut(debtratio,c(0,1,2,Inf))) %>% 
  ggplot(aes(x = debtratio,fill = seriousdlqin2yrs)) +
  geom_bar()
  
train_df %>% 
  mutate(seriousdlqin2yrs = as.character(seriousdlqin2yrs),
         debtratio = cut(debtratio,c(0,1,2,Inf))) %>% 
  group_by(debtratio,seriousdlqin2yrs) %>% 
  summarise(n = n()) %>% 
  mutate(per = n / sum(n)) %>% 
  ggplot(aes(x = debtratio,y = per,fill = seriousdlqin2yrs)) +
  geom_bar(stat = "identity")


# monthlyincome 
train_df %>% 
  mutate(seriousdlqin2yrs = as.character(seriousdlqin2yrs)) %>% 
  ggplot(aes(x = monthlyincome, color = seriousdlqin2yrs)) +
  geom_density() +
  xlim(0,50000)

train_df %>% 
  mutate(seriousdlqin2yrs = as.character(seriousdlqin2yrs)) %>% 
  ggplot(aes(x = log(monthlyincome), color = seriousdlqin2yrs)) +
  geom_density() 

