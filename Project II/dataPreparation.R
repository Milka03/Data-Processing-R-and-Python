
library("dplyr")

DF2004 <- read.csv("./data/2004.csv.bz2")
DF2005 <- read.csv("./data/2005.csv.bz2")
DF2006 <- read.csv("./data/2006.csv.bz2")
DF2007 <- read.csv("./data/2007.csv.bz2")
DF2008 <- read.csv("./data/2008.csv.bz2")

DF2004 %>% filter(Cancelled == 1) %>% select(Year, UniqueCarrier, Origin, Dest, Distance, Cancelled, CancellationCode) -> Cancel2004
DF2005 %>% filter(Cancelled == 1) %>% select(Year, UniqueCarrier, Origin, Dest, Distance, Cancelled, CancellationCode) -> Cancel2005
DF2006 %>% filter(Cancelled == 1) %>% select(Year, UniqueCarrier, Origin, Dest, Distance, Cancelled, CancellationCode) -> Cancel2006
DF2007 %>% filter(Cancelled == 1) %>% select(Year, UniqueCarrier, Origin, Dest, Distance, Cancelled, CancellationCode) -> Cancel2007
DF2008 %>% filter(Cancelled == 1) %>% select(Year, UniqueCarrier, Origin, Dest, Distance, Cancelled, CancellationCode) -> Cancel2008

Cancellations0408 <- bind_rows(Cancel2004, Cancel2005, Cancel2006, Cancel2007, Cancel2008)

write.csv(Cancellations0408, "./data/cancellations.csv", row.names = FALSE)