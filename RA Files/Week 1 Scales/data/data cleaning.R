load("~/Box/networks/IPIP100items04apr2006thru7feb2017.rdata")
load("~/Box/networks/data sources/SAPAdata18aug2010thru7feb2017.rdata")

# get item #'s for miniipip20 items
ipip20_items <- melt(ItemLists) %>%
  filter(grepl("miniIPIP20", L1) == T & L1 != "miniIPIP20") %>%
  mutate(value = as.character(value)) %>%
  arrange(L1) %>%
  group_by(L1) %>%
  mutate(name = seq(1,n(),1),
         name = paste(L1, name, sep = "_")) %>%
  ungroup()

# get item #'s for ipip50 items
ipip50_items <- melt(ItemLists) %>%
  filter(grepl("IPIP50", L1) == T & L1 != "IPIP50") %>%
  mutate(value = as.character(value)) %>%
  arrange(L1)
ipip50_items <- ipip50_items %>%
  group_by(L1) %>%
  mutate(name = seq(1,n(),1),
         name = paste(L1, name, sep = "_")) %>%
  ungroup()

# get item #'s for the ipip100 items
ipip100_items <- melt(ItemLists) %>%
  filter(grepl("IPIP100", L1) == T & L1 != "IPIP100") %>%
  mutate(value = as.character(value)) %>%
  rbind(c("q_55", "IPIP100extraversion20")) %>%
  arrange(L1) %>%
  group_by(L1) %>%
  mutate(name = seq(1,n(),1),
         name = paste(L1, name, sep = "_")) %>%
  ungroup()

ipip20 <- IPIP100items04apr2006thru7feb2017 %>% tbl_df %>%
  select(RID, gender, age, education, one_of(ipip20_items$value)) %>%
  filter(row_number() %in% 1:500) %>%
  group_by(RID) %>%
  mutate(resp = seq(1, n(), 1)) %>%
  filter(resp == 1) %>%
  select(-resp) %>%
  ungroup() %>%
  setNames(c("RID", "gender", "age", "education", ipip20_items$name)) 

ipip50 <- IPIP100items04apr2006thru7feb2017 %>% tbl_df %>%
  select(RID, gender, age, education, one_of(ipip50_items$value)) %>%
  filter(row_number() %in% 1:500) %>%
  group_by(RID) %>%
  mutate(resp = seq(1, n(), 1)) %>%
  filter(resp == 1) %>%
  select(-resp) %>%
  ungroup() %>%
  setNames(c("RID", "gender", "age", "education", ipip50_items$name)) 

ipip100 <- IPIP100items04apr2006thru7feb2017 %>% tbl_df %>%
  select(RID, gender, age, education, one_of(ipip100_items$value)) %>%
  filter(row_number() %in% 300000:301000) %>%
  group_by(RID) %>%
  mutate(resp = seq(1, n(), 1)) %>%
  filter(resp == 1) %>%
  select(-resp) %>%
  ungroup() %>%
  setNames(c("RID", "gender", "age", "education", ipip100_items$name)) 

library(Amelia)
ipip20 <- (amelia(ipip20, idvars = c("RID", "gender", "age", "education"), m = 1))$imputations$imp1
ipip50 <- (amelia(ipip50, idvars = c("RID", "gender", "age", "education"), m = 1))$imputations$imp1
ipip100 <- (amelia(ipip100, idvars = c("RID", "gender", "age", "education"), m = 1))$imputations$imp1

write.csv(ipip20, file = "~/Dropbox (Brown)/Summer 2018/RA Files/Week 1 Scales/data/ipip20.csv", row.names = F)
write.csv(ipip50, file = "~/Dropbox (Brown)/Summer 2018/RA Files/Week 1 Scales/data/ipip50.csv", row.names = F)
write.csv(ipip100, file = "~/Dropbox (Brown)/Summer 2018/RA Files/Week 1 Scales/data/ipip100.csv", row.names = F)

