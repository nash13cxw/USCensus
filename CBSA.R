###################
####US POPULATION##
##Xiaowei Cheng####
###################


library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(treemapify)
library(grid)
library(ggpubr)
library(RColorBrewer)
library(wesanderson)
library(ggthemes)
library(bubbles)


#########Data Sets########
#read data
cbsa <- fread('/Users/xiaoweicheng/Downloads/cbsa-est2017-alldata.csv')
#data cleaning
#select columns
cbsa <- cbsa %>% select(-c(MDIV,STCOU))
#rename
colnames(cbsa) <- c("cbsa","county","state","lsad","census10","est10","pop10","pop11","pop12","pop13","pop14","pop15","pop16","pop17","nch10","nch11","nch12","nch13","nch14","nch15","nch16","nch17","b10","b11","b12","b13","b14","b15","b16","b17","d10","d11","d12","d13","d14","d15","d16","d17","n10","n11","n12","n13","n14","n15","n16","n17","ii10","ii11","ii12","ii13","ii14","ii15","ii16","ii17","di10","di11","di12","di13","di14","di15","di16","di17","ni10","ni11","ni12","ni13","ni14","ni15","ni16","ni17","r10","r11","r12","r13","r14","r15","r16","r17")
############################

#######Check on population by different State/County/Area(Statistical)##########
#data set
statepop <- cbsa %>% select(c(state,pop10,pop11,pop12,pop13,pop14,pop15,pop16,pop17))
#only state
statepop1 <- statepop %>% filter(state %in% c("AK","AL","AR","AZ","CA","CO","CT","DC","DE","FL","GA","HI","IA","ID","IL","IN","KS","KY","LA","MA","MD","ME","MI","MN","MO","ND","NE","NH","NJ","NM","NV","NY","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VA","VT","WA","WI","WV","WY","NC","MS","MT"))
#multiple state area (statistical area)
statepop2 <- statepop %>% filter(!state %in% c("AK","AL","AR","AZ","CA","CO","CT","DC","DE","FL","GA","HI","IA","ID","IL","IN","KS","KY","LA","MA","MD","ME","MI","MN","MO","ND","NE","NH","NJ","NM","NV","NY","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VA","VT","WA","WI","WV","WY","NC","MS","MT"))
#summarize total 
statepop1 <- group_by(statepop1, state) %>%
  summarise(total10=sum(pop10),total11=sum(pop11),total12=sum(pop12),total13=sum(pop13),total14=sum(pop14),total15=sum(pop15),total16=sum(pop16),total17=sum(pop17))
statepop2 <- group_by(statepop2, state) %>%
  summarise(total10=sum(pop10),total11=sum(pop11),total12=sum(pop12),total13=sum(pop13),total14=sum(pop14),total15=sum(pop15),total16=sum(pop16),total17=sum(pop17))

#region
#region population
northeast <- statepop1 %>% filter(state %in% c("PA","CT","ME","MA","NH","RI","VT","NJ","NY")) %>% mutate(region = "east")
midwest <- statepop1 %>% filter(state %in% c("IL","IN","MI","OH","WI","IA","KS","MN","MS","NE","ND","SD")) %>% mutate(region = "midwest")
south <- statepop1 %>% filter(state %in% c("DE","FL","GA","MD","NC","SC","VA","DC","WV","AL","KY","MS","TN","AR","LA","OK","TX")) %>% mutate(region = "south")                            
west <- statepop1 %>% filter(state %in% c("AZ","CO","ID","MT","NV","NM","UT","WY","AK","CA","HI","OR","WA")) %>% mutate(region = "west")                              
region <- rbind(northeast,midwest,south,west)
regionzone <- region %>% group_by(region) %>% summarise(total10=sum(total10),total11=sum(total11),total12=sum(total12),total13=sum(total13),total14=sum(total14),total15=sum(total15),total16=sum(total16),total17=sum(total17))
#treemap
ggplot(region, aes(area = total10/1e7,fill=total10/1e7,label=state,subgroup = region)) + 
  geom_treemap() +
  geom_treemap_subgroup_border() +
  geom_treemap_subgroup_text(place = "centre", grow = T, alpha=.5, colour="black",fontface="italic",min.size = 0) +
  geom_treemap_text(fontface = "italic", colour = "white", place = "topleft",reflow=TRUE,alpha=.6) +
  scale_fill_distiller(palette="Spectral")
#regional comparison
rbar10 <- ggplot(regionzone,aes(x=region,y=total10/1e8,fill=region)) + geom_bar(aes(reorder(region,-total10)),stat = "identity",position = "dodge") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
rbar15 <- ggplot(regionzone,aes(x=region,y=total15/1e8,fill=region)) + geom_bar(aes(reorder(region,-total15)),stat = "identity",position = "dodge") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
rbar17 <- ggplot(regionzone,aes(x=region,y=total17/1e8,fill=region)) + geom_bar(aes(reorder(region,-total17)),stat = "identity",position = "dodge") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
multiplot(rbar10,rbar15,rbar17)
dot10 <- ggdotchart(region, x="state", y="total10", color = "region", 
           palette = c("Zissou"), 
           sorting = "descending", add = "segments",
           rotate = TRUE, dot.size = 3, y.text.col=TRUE,
           ggtheme = theme_pubr()) 
dot15 <- ggdotchart(region, x="state", y="total15", color = "region", 
                    palette = c("Zissou"), 
                    sorting = "descending", add = "segments",
                    rotate = TRUE, dot.size = 3, y.text.col=TRUE,
                    ggtheme = theme_pubr()) 
dot17 <- ggdotchart(region, x="state", y="total17", color = "region", 
                    palette = c("Zissou"), 
                    sorting = "descending", add = "segments",
                    rotate = TRUE, dot.size = 3, y.text.col=TRUE,
                    ggtheme = theme_pubr()) 
multiplot(dot10,dot15,dot17)
#timetrend
ne <- regionzone %>% filter(region == "north") %>% select(-c(region))
ne <- as.data.frame((t(ne)))
colnames(ne)[colnames(ne)=="V1"] <- "NE" 
subdatne <- regionzone[regionzone$region == "east", c("total10","total11","total12","total13","total14","total15","total16","total17")]
subdat_lne <- data.frame(Value=unlist(subdatne),year = factor(10:17))
nebar <- ggplot(subdat_lne,aes(x=year,y=Value/1e7,fill="steelbule")) + 
         geom_bar(stat = 'identity',width=0.65) + 
         labs(x="Year",y="Population") +
         ggtitle("2010-2017 North-East POPULATION TREND") +
         theme_economist(base_size = 10) +
         scale_fill_economist() + 
         theme(legend.position = "none",axis.ticks.length=unit(0.5,'cm'))
mw <- regionzone %>% filter(region == "midwest") %>% select(-c(region))
mw <- as.data.frame((t(mw)))
colnames(mw)[colnames(mw)=="V1"] <- "MW" 
subdatmw <- regionzone[regionzone$region == "midwest", c("total10","total11","total12","total13","total14","total15","total16","total17")]
subdat_lmw <- data.frame(Value=unlist(subdatmw),year = factor(10:17))
mwbar <- ggplot(subdat_lne,aes(x=year,y=Value/1e7,fill="steelbule")) + 
  geom_bar(stat = 'identity',width=0.65) + 
  labs(x="Year",y="Population") +
  ggtitle("2010-2017 Mid-West POPULATION TREND") +
  theme_economist(base_size = 10) +
  scale_fill_economist() + 
  theme(legend.position = "none",axis.ticks.length=unit(0.5,'cm'))
s <- regionzone %>% filter(region == "south") %>% select(-c(region))
s <- as.data.frame((t(s)))
colnames(s)[colnames(s)=="V1"] <- "S" 
subdats <- regionzone[regionzone$region == "south", c("total10","total11","total12","total13","total14","total15","total16","total17")]
subdat_ls <- data.frame(Value=unlist(subdats),year = factor(10:17))
sbar <- ggplot(subdat_ls,aes(x=year,y=Value/1e7,fill="steelbule")) + 
  geom_bar(stat = 'identity',width=0.65) + 
  labs(x="Year",y="Population") +
  ggtitle("2010-2017 South POPULATION TREND") +
  theme_economist(base_size = 10) +
  scale_fill_economist() + 
  theme(legend.position = "none",axis.ticks.length=unit(0.5,'cm'))
w <- regionzone %>% filter(region == "west") %>% select(-c(region))
w <- as.data.frame((t(w)))
colnames(w)[colnames(w)=="V1"] <- "W" 
subdatw <- regionzone[regionzone$region == "west", c("total10","total11","total12","total13","total14","total15","total16","total17")]
subdat_lw <- data.frame(Value=unlist(subdatw),year = factor(10:17))
wbar <- ggplot(subdat_lw,aes(x=year,y=Value/1e7,fill="steelbule")) + 
  geom_bar(stat = 'identity',width=0.65) + 
  labs(x="Year",y="Population") +
  ggtitle("2010-2017 West POPULATION TREND") +
  theme_economist(base_size = 10) +
  scale_fill_economist() + 
  theme(legend.position = "none",axis.ticks.length=unit(0.5,'cm'))
multiplot(wbar,sbar,nebar,mwbar,cols = 2)

#2010 State population status
#bar graph
bar10 <- ggplot(statepop1,aes(x=state,y=total10/1e7,fill=state)) + geom_bar(aes(reorder(state,-total10)),stat = "identity",position = "dodge") + theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "none") 
#tree map
tree10<- ggplot(statepop1, aes(area = total10/1e7,fill=total10/1e7,label=state)) + 
         geom_treemap() +
         geom_treemap_text(fontface = "italic", colour = "black", place = "centre",grow=TRUE,alpha=.6) +
         scale_fill_distiller(palette="Spectral")
#2015 State population status
#bar graph
bar15 <- ggplot(statepop1,aes(x=state,y=total15/1e7,fill=state)) + geom_bar(aes(reorder(state,-total15)),stat = "identity",position = "dodge") + theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "none") 
#tree map
tree15<- ggplot(statepop1, aes(area = total15/1e7,fill=total11/1e7,label=state)) + 
  geom_treemap() +
  geom_treemap_text(fontface = "italic", colour = "black", place = "centre",grow=TRUE,alpha=.6) +
  scale_fill_distiller(palette="Spectral")
#2017 State population status
#bar graph
bar17 <- ggplot(statepop1,aes(x=state,y=total17/1e7,fill=state)) + geom_bar(aes(reorder(state,-total17)),stat = "identity",position = "dodge") + theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "none") 
#tree map
tree17<- ggplot(statepop1, aes(area = total17/1e7,fill=total17/1e7,label=state)) + 
  geom_treemap() +
  geom_treemap_text(fontface = "italic", colour = "black", place = "centre",grow=TRUE,alpha=.6) +
  scale_fill_distiller(palette="Spectral")

#Camparison
#2010 vs.2015 Population comparison
multiplot(bar10,bar15,cols=2)
multiplot(tree10,tree15,cols=2)
#2010 Vs. 2017
multiplot(bar10,bar17,cols=2)
multiplot(tree10,tree17,cols=2)
#2015 vs. 2017
multiplot(bar15,bar17,cols=2)
multiplot(tree15,tree17,cols=2)
#Overrall(2010-2015-2017)
multiplot(bar10,bar15,bar17)
multiplot(tree10,tree15,tree17,cols=3)

#Particular state detail
#DC
dc <- region %>% filter(state == "DC") %>% select(-c(region))
dc <- as.data.frame((t(dc)))
colnames(dc)[colnames(dc)=="V1"] <- "DC" 
subdat <- region[region$state == "DC", c("total10","total11","total12","total13","total14","total15","total16","total17")]
subdat_l <- data.frame(Value=unlist(subdat),year = factor(10:17))
dcbar <- ggplot(subdat_l,aes(x=year,y=Value/1e7,fill=Value)) + geom_bar(stat = 'identity') + ggtitle("2010-2011 Distric of Columbia POPULATION TREND")
#CA
ca <- region %>% filter(state == "CA") %>% select(-c(region))
ca <- as.data.frame((t(ca)))
colnames(ca)[colnames(ca)=="V1"] <- "CA" 
subdatca <- region[region$state == "CA", c("total10","total11","total12","total13","total14","total15","total16","total17")]
subdat_lca <- data.frame(Value=unlist(subdatca),year = factor(10:17))
cabar <- ggplot(subdat_lca,aes(x=year,y=Value/1e7,fill=Value)) + geom_bar(stat = 'identity') + ggtitle("2010-2011 California POPULATION TREND")
#TX
tx <- region %>% filter(state == "TX") %>% select(-c(region))
tx <- as.data.frame((t(tx)))
colnames(tx)[colnames(tx)=="V1"] <- "TX" 
subdattx <- region[region$state == "TX", c("total10","total11","total12","total13","total14","total15","total16","total17")]
subdat_ltx <- data.frame(Value=unlist(subdattx),year = factor(10:17))
txbar <- ggplot(subdat_ltx,aes(x=year,y=Value/1e7,fill=Value)) + geom_bar(stat = 'identity') + ggtitle("2010-2011 Texas POPULATION TREND")
#FL
fl <- region %>% filter(state == "FL") %>% select(-c(region))
fl <- as.data.frame((t(fl)))
colnames(fl)[colnames(fl)=="V1"] <- "FL" 
subdatfl <- region[region$state == "FL", c("total10","total11","total12","total13","total14","total15","total16","total17")]
subdat_lfl <- data.frame(Value=unlist(subdatfl),year = factor(10:17))
flbar <- ggplot(subdat_lfl,aes(x=year,y=Value/1e7,fill=Value)) + geom_bar(stat = 'identity') + ggtitle("2010-2011 Florida POPULATION TREND")
#NY
ny <- region %>% filter(state == "NY") %>% select(-c(region))
ny <- as.data.frame((t(ny)))
colnames(ny)[colnames(ny)=="V1"] <- "NY" 
subdatny <- region[region$state == "NY", c("total10","total11","total12","total13","total14","total15","total16","total17")]
subdat_lny <- data.frame(Value=unlist(subdatny),year = factor(10:17))
nybar <- ggplot(subdat_lny,aes(x=year,y=Value/1e7,fill=Value)) + geom_bar(stat = 'identity') + ggtitle("2010-2011 New York POPULATION TREND")
#PA
pa <- region %>% filter(state == "PA") %>% select(-c(region))
pa <- as.data.frame((t(pa)))
colnames(pa)[colnames(pa)=="V1"] <- "PA" 
subdatpa <- region[region$state == "PA", c("total10","total11","total12","total13","total14","total15","total16","total17")]
subdat_lpa <- data.frame(Value=unlist(subdatpa),year = factor(10:17))
pabar <- ggplot(subdat_lpa,aes(x=year,y=Value/1e7,fill=Value)) + geom_bar(stat = 'identity') + ggtitle("2010-2011 Pennsylvania POPULATION TREND")
#ND
nd <- region %>% filter(state == "ND") %>% select(-c(region))
nd <- as.data.frame((t(nd)))
colnames(nd)[colnames(nd)=="V1"] <- "ND" 
subdatnd <- region[region$state == "ND", c("total10","total11","total12","total13","total14","total15","total16","total17")]
subdat_lnd <- data.frame(Value=unlist(subdatnd),year = factor(10:17))
ndbar <- ggplot(subdat_lnd,aes(x=year,y=Value/1e7,fill=Value)) + geom_bar(stat = 'identity') + ggtitle("2010-2011 North Dakota POPULATION TREND")
#WY
wy <- region %>% filter(state == "WY") %>% select(-c(region))
wy <- as.data.frame((t(WY)))
colnames(wy)[colnames(wy)=="V1"] <- "WY" 
subdatwy <- region[region$state == "WY", c("total10","total11","total12","total13","total14","total15","total16","total17")]
subdat_lwy <- data.frame(Value=unlist(subdatwy),year = factor(10:17))
wybar <- ggplot(subdat_lwy,aes(x=year,y=Value/1e7,fill=Value)) + geom_bar(stat = 'identity') + ggtitle("2010-2011 Wyoming POPULATION TREND")
#Comparison of Last 3
multiplot(dcbar, wybar, ndbar,cols = 3)
#Comparison of Top5
multiplot(cabar,nybar,txbar,pabar,flbar,cols = 3)
#Most vs. Least
multiplot(cabar,dcbar,cols=2)

#Stattistical Area/Metropolitan Area
#2010 Statistical Area population status
metro10 <- ggplot(statepop2,aes(x=state,y=total10/1e7,fill=state)) + geom_bar(aes(reorder(state,-total10)),stat = "identity",position = "dodge") + theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "none")
#2015 Statistical Area population status
metro15 <- ggplot(statepop2,aes(x=state,y=total15/1e7,fill=state)) + geom_bar(aes(reorder(state,-total15)),stat = "identity",position = "dodge") + theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "none")
#2017 Statistical Area population status
metro17 <- ggplot(statepop2,aes(x=state,y=total17/1e7,fill=state)) + geom_bar(aes(reorder(state,-total17)),stat = "identity",position = "dodge") + theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "none")

#comparison
multiplot(metro10,metro15,metro17)
#difference between state population and metropolitan area population
#DC-VA-MD-WV

#######END OF POPULATION PART############

############OTHER EFFECTS################
#Numeric Change 
statench <- cbsa %>% select(c(state,nch10,nch11,nch12,nch13,nch14,nch15,nch16,nch17))
#only state
statench1 <- statench %>% filter(state %in% c("AK","AL","AR","AZ","CA","CO","CT","DC","DE","FL","GA","HI","IA","ID","IL","IN","KS","KY","LA","MA","MD","ME","MI","MN","MO","ND","NE","NH","NJ","NM","NV","NY","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VA","VT","WA","WI","WV","WY","NC","MS","MT"))
#multiple state area (statistical area)
statench2 <- statench %>% filter(!state %in% c("AK","AL","AR","AZ","CA","CO","CT","DC","DE","FL","GA","HI","IA","ID","IL","IN","KS","KY","LA","MA","MD","ME","MI","MN","MO","ND","NE","NH","NJ","NM","NV","NY","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VA","VT","WA","WI","WV","WY","NC","MS","MT"))
#summarize total 
statench1 <- group_by(statench1, state) %>%
  summarise(total10=sum(nch10),total11=sum(nch11),total12=sum(nch12),total13=sum(nch13),total14=sum(nch14),total15=sum(nch15),total16=sum(nch16),total17=sum(nch17))
statench2 <- group_by(statench2, state) %>%
  summarise(total10=sum(nch10),total11=sum(nch11),total12=sum(nch12),total13=sum(nch13),total14=sum(nch14),total15=sum(nch15),total16=sum(nch16),total17=sum(nch17))
#2010 State population status
#bar graph
bnch10 <- ggplot(statench1,aes(x=state,y=total10,fill=state)) + geom_bar(aes(reorder(state,-total10)),stat = "identity",position = "dodge") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
#tree map
tnch10<- ggplot(statench1, aes(area = total10,fill=total10,label=state)) + 
  geom_treemap() +
  geom_treemap_text(fontface = "italic", colour = "black", place = "centre",grow=TRUE,alpha=.6) +
  scale_fill_distiller(palette="Spectral")
#Metropolitan Area
mnch10t <- ggplot(statench2, aes(area = total10,fill=total10,label=state)) + 
  geom_treemap() +
  geom_treemap_text(fontface = "italic", colour = "black", place = "centre",grow=TRUE,alpha=.6) +
  scale_fill_distiller(palette="Spectral")
#2015 State population status
#bar graph
bnch15 <- ggplot(statench1,aes(x=state,y=total15,fill=state)) + geom_bar(aes(reorder(state,-total15)),stat = "identity",position = "dodge") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
#tree map
tnch15<- ggplot(statench1, aes(area = total15,fill=total15,label=state)) + 
  geom_treemap() +
  geom_treemap_text(fontface = "italic", colour = "black", place = "centre",grow=TRUE,alpha=.6) +
  scale_fill_distiller(palette="Spectral")
#Metropolitan Area
mnch15t <- ggplot(statench2, aes(area = total15,fill=total15,label=state)) + 
  geom_treemap() +
  geom_treemap_text(fontface = "italic", colour = "black", place = "centre",grow=TRUE,alpha=.6) +
  scale_fill_distiller(palette="Spectral")
#2017 State population status
#bar graph
bnch17 <- ggplot(statench1,aes(x=state,y=total17,fill=state)) + geom_bar(aes(reorder(state,-total17)),stat = "identity",position = "dodge") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
#tree map
tnch17<- ggplot(statench1, aes(area = total17,fill=total17,label=state)) + 
  geom_treemap() +
  geom_treemap_text(fontface = "italic", colour = "black", place = "centre",grow=TRUE,alpha=.6) +
  scale_fill_distiller(palette="Spectral")
#Metropolitan Area
mnch17t <- ggplot(statench2, aes(area = total17,fill=total17,label=state)) + 
  geom_treemap() +
  geom_treemap_text(fontface = "italic", colour = "black", place = "centre",grow=TRUE,alpha=.6) +
  scale_fill_distiller(palette="Spectral")
#comparison
multiplot(tnch10,tnch15,tnch17)
multiplot(mnch10t,mnch15t,mnch17t)

#Births&Deaths Situation
#Births/Deaths Change 
statebd <- cbsa %>% select(c(state,b11,b12,b13,b14,b15,b16,b17,d11,d12,d13,d14,d15,d16,d17))
#only state
statebd1 <- statebd %>% filter(state %in% c("AK","AL","AR","AZ","CA","CO","CT","DC","DE","FL","GA","HI","IA","ID","IL","IN","KS","KY","LA","MA","MD","ME","MI","MN","MO","ND","NE","NH","NJ","NM","NV","NY","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VA","VT","WA","WI","WV","WY","NC","MS","MT"))
#multiple state area (statistical area)
statebd2 <- statebd %>% filter(!state %in% c("AK","AL","AR","AZ","CA","CO","CT","DC","DE","FL","GA","HI","IA","ID","IL","IN","KS","KY","LA","MA","MD","ME","MI","MN","MO","ND","NE","NH","NJ","NM","NV","NY","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VA","VT","WA","WI","WV","WY","NC","MS","MT"))
#summarize total 
statebd1 <- group_by(statebd1, state) %>%
  summarise(b11=sum(b11),b12=sum(b12),b13=sum(b13),b14=sum(b14),b15=sum(b15),b16=sum(b16),b17=sum(b17),d11=sum(d11),d12=sum(d12),d13=sum(d13),d14=sum(d14),d15=sum(d15),d16=sum(d16),d17=sum(d17))
statebd2 <- group_by(statebd2, state) %>%
  summarise(b11=sum(b11),b12=sum(b12),b13=sum(b13),b14=sum(b14),b15=sum(b15),b16=sum(b16),b17=sum(b17),d11=sum(d11),d12=sum(d12),d13=sum(d13),d14=sum(d14),d15=sum(d15),d16=sum(d16),d17=sum(d17))
#Birth/Death State Comparison 2017
b17bar <- ggplot(statebd1,aes(x=state,y=b17/1e5,fill=state)) + 
  geom_bar(stat = "identity",position = "dodge") + 
  coord_flip() +
  scale_y_reverse() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "none") +
  ggtitle("2017 Births Information")
d17bar <- ggplot(statebd1,aes(x=state,y=d17/1e5,fill=state)) + 
  geom_bar(stat = "identity",position = "dodge") + 
  coord_flip() +
  theme(axis.title.y = element_blank()) +
  ggtitle("2017 Deaths Information")
multiplot(b17bar,d17bar,cols = 2)
#Birth/Death Metropolitan Comparison 2017
bmetro17bar <- ggplot(statebd2,aes(x=state,y=b17/1e5,fill=state)) + 
  geom_bar(stat = "identity",position = "dodge") + 
  coord_flip() +
  scale_y_reverse() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "none") +
  ggtitle("2017 Births Information")
dmetro17bar <- ggplot(statebd2,aes(x=state,y=d17/1e5,fill=state)) + 
  geom_bar(stat = "identity",position = "dodge") + 
  coord_flip() +
  theme(axis.title.y = element_blank()) +
  ggtitle("2017 Deaths Information")
multiplot(bmetro17bar,dmetro17bar,cols = 2)

#Immigration(Domestic/International)
stateim <- cbsa %>% select(c(state,ii11,ii12,ii13,ii14,ii15,ii16,ii17,di11,di12,di13,di14,di15,di16,di17))
#only state
stateim1 <- stateim %>% filter(state %in% c("AK","AL","AR","AZ","CA","CO","CT","DC","DE","FL","GA","HI","IA","ID","IL","IN","KS","KY","LA","MA","MD","ME","MI","MN","MO","ND","NE","NH","NJ","NM","NV","NY","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VA","VT","WA","WI","WV","WY","NC","MS","MT"))
#multiple state area (statistical area)
stateim2 <- stateim %>% filter(!state %in% c("AK","AL","AR","AZ","CA","CO","CT","DC","DE","FL","GA","HI","IA","ID","IL","IN","KS","KY","LA","MA","MD","ME","MI","MN","MO","ND","NE","NH","NJ","NM","NV","NY","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VA","VT","WA","WI","WV","WY","NC","MS","MT"))
#summarize total 
stateim1 <- group_by(stateim1, state) %>%
  summarise(ii11=sum(ii11),ii12=sum(ii12),ii13=sum(ii13),ii14=sum(ii14),ii15=sum(ii15),ii16=sum(ii16),ii17=sum(ii17),di11=sum(di11),di12=sum(di12),di13=sum(di13),di14=sum(di14),di15=sum(di15),di16=sum(di16),di17=sum(di17))
stateim2 <- group_by(stateim2, state) %>%
  summarise(ii11=sum(ii11),ii12=sum(ii12),ii13=sum(ii13),ii14=sum(ii14),ii15=sum(ii15),ii16=sum(ii16),ii17=sum(ii17),di11=sum(di11),di12=sum(di12),di13=sum(di13),di14=sum(di14),di15=sum(di15),di16=sum(di16),di17=sum(di17))
#Comparison
#International Immigration
iibar17 <- ggplot(stateim1,aes(x=state,y=ii17/1e5,fill=state)) + 
  geom_bar(aes(reorder(state,-ii17)),stat = "identity",position = "dodge") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
#Domestic Migration
dibar17 <- ggplot(stateim1,aes(x=state,y=di17/1e5,fill=state)) + 
  geom_bar(aes(reorder(state,-di17)),stat = "identity",position = "dodge") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#########################
#Bubble Graph
color1<-brewer.pal(9,"YlOrBr")
color2<-brewer.pal(9,"BuGn")
color<-c(color1,color2)
colorpan<-sample(color,51,replace = TRUE)
colorpan1<-sample(color,56,replace = TRUE)
totalpopulation <- bubbles(value =statepop1$total17,label=statepop1$state,color=colorpan)
totalcountypopulation <- bubbles(value =statepop2$total17,label=statepop2$state,color=colorpan1)
internationalimmigration <- bubbles(value =stateim1$ii17,label=stateim1$state,color=colorpan)


#######TO BE CONTINUE##############


#multiplot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#ggbarplot(statepop1, x="state", y="total10", fill = "state",color = "white", 
            #palette = "jco", 
            #sort.val = "desc", 
            #sort.by.groups=FALSE, 
            #x.text.angle=60)
#mytheme<-function (base_size = 12, base_family = ""){
  #theme_bw(base_size = base_size, base_family = base_family) %+replace% 
    #theme(axis.line = element_blank(), axis.text.x=element_blank(), 
          #axis.ticks=element_blank(), axis.title = element_blank(), 
          #panel.background=element_blank(),panel.border =element_blank(), 
          #panel.grid = element_blank(), plot.background = element_blank(), 
          #strip.background=element_blank(),legend.position = c(0.9,0.15))}

