library(rvest)
library(plyr)
library(tidyverse)
library(plotly)
library(ballr)
library(data.table)
library(mice)
library(stats)
library(factoextra)
library(caret)
options(scipen = 999)
####

# GETTING ALL LINKS #
letter_link = "https://www.basketball-reference.com/players/"
letters = c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","y","z")
letter_links = paste0(letter_link, letters)
`%notin%` = Negate(`%in%`)

#### SAMPLE ####

# ## Getting Player Table ##
# a = letter_links[1]
# 
# players_a = read_html(a) %>%
#   html_table(fill = TRUE) %>%
#   `[[`(1)
# 
# urls = read_html(a) %>% 
#   html_nodes("a") %>%
#   html_attr("href")
# 
# relevant_urls = urls[grepl(urls,pattern="/players/",fixed=T)]
# relevant_urls = relevant_urls[3:(length(relevant_urls) - 27)]
# 
# players_a = players_a %>%
#   mutate(link = relevant_urls,
#          player_code = substr(link, 12, nchar(link)-5))
# 
# players_a = players_a %>%
#   filter((From >= 2010)|(To >= 2011)) %>%
#   select(Player, From, To, Pos, Ht, link, player_code) %>%
#   separate(Ht, c("Ft","In"), sep = "-") %>%
#   mutate(Ft = as.integer(Ft),
#          In = as.integer(In),
#          Ht = (Ft * 12) + In) %>%
#   mutate(Pos = substr(Pos,1,1)) %>%
#   mutate(link = paste0("https://www.basketball-reference.com",link)) %>%
#   select(Player, From, To, Pos, Ht, link, player_code)
# 
# 
# ### PER 100 POSSESSIONS ###
# per100 = players_a$link[1] %>%
#   read_html %>%
#   html_nodes(xpath = '//comment()') %>%
#   html_text() %>%
#   paste(collapse='') %>%
#   read_html() %>% 
#   html_node("#per_poss") %>% 
#   html_table()
# per100 = per100 %>%
#   select_if(not_all_na) %>%
#   mutate(player_code = players_a$player_code[1]) %>%
#   select(player_code, everything()) %>%
#   filter(Lg == "NBA")
# per100 = per100[-nrow(per100),]
# 
# ### ADVANCED STATS ###
# advanced =  players_a$link[1] %>%
#   read_html %>%
#   html_nodes(xpath = '//comment()') %>%
#   html_text() %>%
#   paste(collapse='') %>%
#   read_html() %>% 
#   html_node("#advanced") %>% 
#   html_table()
# advanced <- Filter(function(x)!all(is.na(x)), advanced)
# advanced = advanced %>%
#   filter(Lg == "NBA") %>%
#   select(-Age, -Tm, -Lg, -Pos, -G, -MP)
# advanced = advanced[-nrow(advanced),]
# 
# ### SHOOTING STATS ###
# shooting =  players_a$link[1] %>%
#   read_html %>%
#   html_nodes(xpath = '//comment()') %>%
#   html_text() %>%
#   paste(collapse='') %>%
#   read_html() %>% 
#   html_node("#shooting") %>% 
#   html_table()
# colnames(shooting) = paste(colnames(shooting), shooting[1,], shooting[2,], sep = "")
# shooting = shooting[3:(nrow(shooting) - 1),]
# shooting = shooting %>%
#   select(-Age, -Tm, -Lg, -Pos, -G, -MP)
# 
# ### MERGING TOGETHER ###
# stats = per100 %>%
#   inner_join(advanced, by = "Season") %>%
#   inner_join(shooting, by = "Season")
# 
# # MERGING PLAYER AND STATS #
# players_a2 = players_a %>%
#   inner_join(stats, by = "player_code")

#####
#####
#####
#####
#####
#####

# FUNCTIONS #

## Getting Player Table ##
get_player_list = function(x) {
  players = read_html(x) %>%
    html_table(fill = TRUE) %>%
    `[[`(1)
  
  urls = read_html(x) %>%
    html_nodes("a") %>%
    html_attr("href")
  
  rel_urls = urls[grepl(urls, pattern = "/players/",fixed = T)]
  rel_urls = rel_urls[3:(length(rel_urls) - 27)]
  
  players = players %>%
    mutate(link = rel_urls,
           player_code = substr(link, 12, nchar(link)-5))
  
  players = players %>%
    filter((From >= 2010)|(To >= 2011)) %>%
    select(Player, From, To, Pos, Ht, link, player_code) %>%
    separate(Ht, c("Ft","In"), sep = "-") %>%
    mutate(Ft = as.integer(Ft),
           In = as.integer(In),
           Ht = (Ft * 12) + In) %>%
    mutate(Pos = substr(Pos,1,1)) %>%
    mutate(link = paste0("https://www.basketball-reference.com",link)) %>%
    select(Player, From, To, Pos, Ht, link, player_code)
  
  full_stats = rbind.fill(map(players$link,get_player_stats))
  full_player_stats = players %>%
    inner_join(full_stats, by = "player_code")
  
  return(full_player_stats)
}

get_player_stats = function(url) {
  per100 = url %>%
    read_html %>%
    html_nodes(xpath = '//comment()') %>%
    html_text() %>%
    paste(collapse='') %>%
    read_html() %>% 
    html_node("#per_poss") %>% 
    html_table()
  per100 <- Filter(function(x)!all(is.na(x)), per100)
  per100 = per100 %>%
    filter(!is.na(Age)) %>%
    mutate(player_code = substr(url, 48, nchar(url)-5)) %>%
    mutate(unique_code = paste0(player_code, Season, Tm)) %>%
    select(unique_code, player_code, everything())
  
  advanced =  url %>%
    read_html %>%
    html_nodes(xpath = '//comment()') %>%
    html_text() %>%
    paste(collapse='') %>%
    read_html() %>% 
    html_node("#advanced") %>% 
    html_table()
  advanced <- Filter(function(x)!all(is.na(x)), advanced)
  advanced = advanced %>%
    filter(!is.na(Age)) %>%
    mutate(player_code = substr(url, 48, nchar(url)-5)) %>%
    mutate(unique_code = paste0(player_code, Season, Tm)) %>%
    select(unique_code, player_code, everything()) %>%
    select(-Age, -Tm, -Lg, -Pos, -G, -MP)
  
  shooting =  url %>%
    read_html %>%
    html_nodes(xpath = '//comment()') %>%
    html_text() %>%
    paste(collapse='') %>%
    read_html() %>% 
    html_node("#shooting") %>% 
    html_table()
  colnames(shooting) = paste(colnames(shooting), shooting[1,], shooting[2,], sep = "")
  shooting = shooting[3:(nrow(shooting)),]
  row.names(shooting) = NULL
  shooting = shooting %>%
    filter(Age != "") %>%
    mutate(player_code = substr(url, 48, nchar(url)-5)) %>%
    mutate(unique_code = paste0(player_code, Season, Tm)) %>%
    select(unique_code, player_code, everything()) %>%
    select(-Age, -Tm, -Lg, -Pos, -G, -MP)
  
  stats = per100 %>%
    inner_join(advanced, by = "unique_code") %>%
    inner_join(shooting, by = "unique_code") %>%
    mutate(unique_code = paste0(player_code, Season)) %>%
    filter(Lg == "NBA") %>%
    filter(Tm != "TOT") %>%
    filter((unique_code %in% unique_code[duplicated(unique_code)])|(G >= 30))
  
  return(stats)
}

## Getting Player Data Tables ##
#player_list = lapply(letter_links, get_player_list) %>% bind_rows
a = get_player_list(letter_links[1])
b = get_player_list(letter_links[2])
c = get_player_list(letter_links[3])
d = get_player_list(letter_links[4])
e = get_player_list(letter_links[5])
f = get_player_list(letter_links[6])
g = get_player_list(letter_links[7])
h = get_player_list(letter_links[8])
i = get_player_list(letter_links[9])
j = get_player_list(letter_links[10])
k = get_player_list(letter_links[11])
l = get_player_list(letter_links[12])
m = get_player_list(letter_links[13])
n = get_player_list(letter_links[14])
o = get_player_list(letter_links[15])
p = get_player_list(letter_links[16])
q = get_player_list(letter_links[17])
r = get_player_list(letter_links[18])
s = get_player_list(letter_links[19])
t = get_player_list(letter_links[20])
u = get_player_list(letter_links[21])
v = get_player_list(letter_links[22])
w = get_player_list(letter_links[23])
y = get_player_list(letter_links[24])
z = get_player_list(letter_links[25])

b = b[,1:85]
b = b[,c(1:28,85,29:84)]

c = c[,1:85]
c = c[,c(1:28,85,29:84)]

k = k[,1:85]
k = k[,c(1:28,85,29:84)]

l = l[,1:85]
l = l[,c(1:28,85,29:84)]

m = m[,1:85]
m = m[,c(1:28,85,29:84)]

s = s[,1:85]
s = s[,c(1:28,85,29:84)]

w = w[,1:85]
w = w[,c(1:28,85,29:84)]

playerlist = rbind(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,y,z)

playerlist2 = playerlist %>%
  select(Player, From, To, Pos.x, Pos.y, Ht, link, player_code, Season.x, Age, Tm, G, GS,MP,FG, FGA, `FG%.x`, `3P`, `3PA`, `3P%`, `2P`, `2PA`, `2P%`, FT, FTA, 
         `FT%`, ORB, DRB, TRB, AST, STL, BLK, TOV, PF, PTS, ORtg, DRtg, PER, `TS%`, `3PAr` ,FTr, `ORB%`, `DRB%`, `TRB%`, `AST%`, `STL%`, `BLK%`, `TOV%`, 
         `USG%`, OWS, DWS, WS, `WS/48`, OBPM, DBPM, BPM, VORP, `Dist.`, `% of FGA by Distance0-3`,`% of FGA by Distance3-10`,`% of FGA by Distance10-16`,
         `% of FGA by Distance16-3pt`,`% of FGA by Distance3P`,`FG% by Distance2P`,`FG% by Distance0-3`,`FG% by Distance3-10`,`FG% by Distance10-16`,`FG% by Distance16-3pt`,
         `FG% by Distance3P`,`2-Pt Field Goals%Ast'd`,`2-Pt Field GoalsDunks%FGA`, `2-Pt Field GoalsDunksMd.`, `3-Pt Field Goals%Ast'd`,`3-Pt Field GoalsCorner%3PA`,
         `3-Pt Field GoalsCorner3P%`, `3-Pt Field GoalsHeavesAtt.`,`3-Pt Field GoalsHeavesMd.`)

playerlist3 = playerlist2 %>%
  rename("PosGroup" = "Pos.x",
         "Pos" = "Pos.y",
         "Height" = "Ht",
         "BRefLink" = "link",
         "Season" = "Season.x",
         "FG_perc" = "FG%.x",
         "FT_perc" = "FT%",
         "TrueShooting" = "TS%",
         "ORBrate" = "ORB%",
         "DRBrate" = "DRB%",
         "TRBrate" = "TRB%",
         "ASTrate" = "AST%",
         "STLrate" = "STL%",
         "BLKrate" = "BLK%",
         "TOVrate" = "TOV%",
         "USGrate" = "USG%",
         "WSper48" = "WS/48",
         "Ave_Dist" = "Dist.",
         "FGAperc0to3" = "% of FGA by Distance0-3",
         "FGAperc3to10" = "% of FGA by Distance3-10",
         "FGAperc10to16" = "% of FGA by Distance10-16",
         "FGAperc16to3P" = "% of FGA by Distance16-3pt",
         "FGAperc3P" = "% of FGA by Distance3P",
         "FGperc2P" = "FG% by Distance2P",
         "FGperc0to3" = "FG% by Distance0-3",
         "FGperc3to10" = "FG% by Distance3-10",
         "FGperc10to16" = "FG% by Distance10-16",
         "FGperc16to3P" = "FG% by Distance16-3pt",
         "FGperc3P" = "FG% by Distance3P",
         "2PTperc_Assisted" = "2-Pt Field Goals%Ast'd",
         "DunkspercFGA" = "2-Pt Field GoalsDunks%FGA",
         "DunksMade" = "2-Pt Field GoalsDunksMd.", 
         "3PTperc_Assisted" = "3-Pt Field Goals%Ast'd",
         "CornerPerc3PA" = "3-Pt Field GoalsCorner%3PA",
         "Corner3PTperc" = "3-Pt Field GoalsCorner3P%", 
         "HeavesAttempted" = "3-Pt Field GoalsHeavesAtt.",
         "HeavesMade" = "3-Pt Field GoalsHeavesMd.")

write_csv(playerlist3, "/Volumes/GoogleDrive/My Drive/Laptop Files/1920B2/MW 2 Sports Analytics/05 Project/Sports Analytics Project/playerdata.csv",na = "", col_names = TRUE)

# LDA/PCA
playerlist3 = read_csv("/Volumes/GoogleDrive/My Drive/Laptop Files/1920B2/MW 2 Sports Analytics/05 Project/Sports Analytics Project/playerdata.csv")
playerlistfinal = playerlist3

str(playerlistfinal)
playerlistfinal2 = playerlistfinal %>%
  mutate(Ave_Dist = as.numeric(Ave_Dist),
         FGAperc0to3 = as.numeric(FGAperc0to3),
         FGAperc3to10 = as.numeric(FGAperc3to10),
         FGAperc10to16 = as.numeric(FGAperc10to16),
         FGAperc16to3P = as.numeric(FGAperc16to3P),
         FGAperc3P = as.numeric(FGAperc3P),
         FGperc2P = as.numeric(FGperc2P),
         FGperc0to3 = as.numeric(FGperc0to3),
         FGperc3to10 = as.numeric(FGperc3to10),
         FGperc10to16 = as.numeric(FGperc10to16),
         FGperc16to3P = as.numeric(FGperc16to3P),
         FGperc3P = as.numeric(FGperc3P),
         `2PTperc_Assisted` = as.numeric(`2PTperc_Assisted`),
         DunkspercFGA = as.numeric(DunkspercFGA),
         DunksMade = as.numeric(DunksMade),
         `3PTperc_Assisted` = as.numeric(`3PTperc_Assisted`),
         CornerPerc3PA = as.numeric(CornerPerc3PA),
         Corner3PTperc = as.numeric(Corner3PTperc),
         HeavesAttempted = as.numeric(HeavesAttempted),
         HeavesMade = as.numeric(HeavesMade),
         From = as.numeric(From),
         To = as.numeric(To),
         Height = as.numeric(Height),
         Age = as.numeric(Age),
         G = as.numeric(G),
         GS = as.numeric(GS),
         MP = as.numeric(MP))

playerlistfinal = playerlistfinal2

playerlistfinalvars = playerlistfinal %>%
  dplyr::select(Player,
                From,
                To,
                Pos,
                Height,
                player_code,
                Season,
                G,
                GS,
                MP,
                PER,
                ORBrate,
                DRBrate,
                ASTrate,
                STLrate,
                BLKrate,
                TOVrate,
                PTS,
                USGrate,
                FTr,
                FT_perc,
                FGA,
                FGperc2P,
                FGperc3P,
                `2PTperc_Assisted`,
                FGAperc3P,
                CornerPerc3PA,
                `3PTperc_Assisted`,
                DunkspercFGA,
                FGAperc0to3,
                FGAperc3to10,
                FGAperc10to16,
                FGAperc16to3P)
  
playerlistfinalvars[is.na(playerlistfinalvars)] <- 0

playerlistfinal_num = playerlistfinalvars %>%
  keep(is.numeric) %>%
  mutate(Pos = playerlistfinal$Pos) %>%
  dplyr::select(-c(From, To, G, GS, MP)) %>%
  dplyr::select(Pos, everything())

playerlistfinal_num$Pos[playerlistfinal_num$Pos == "PG"] <- 1
playerlistfinal_num$Pos[playerlistfinal_num$Pos == "SG"] <- 2
playerlistfinal_num$Pos[playerlistfinal_num$Pos == "SF"] <- 3
playerlistfinal_num$Pos[playerlistfinal_num$Pos == "PF"] <- 4
playerlistfinal_num$Pos[playerlistfinal_num$Pos == "C"] <- 5

playerlistfinal_num = playerlistfinal_num %>%
  dplyr::select(everything(), Pos) %>%
  mutate(Pos = as.numeric(Pos))

playerlistfinal_num[is.na(playerlistfinal_num)] <- 0

#Split into Training and Test Set
library(caTools)
set.seed(123)
# split = sample.split(playerlistfinal_num, SplitRatio = 0.8)
# training_set = subset(playerlistfinal_num, split == TRUE)
# test_set = subset(playerlistfinal_num, split == FALSE)
# 
# #Scale Datasets
# training_set[-1] = scale(training_set[-1])
# test_set[-1] = scale(test_set[-1])
# 
# #LDA
library(MASS)
# lda = lda(formula = Pos ~ ., data = training_set)
# training_set = as.data.frame(predict(lda, training_set)) # LDA needs a datafram, in PCA we got a dataframe in our preprocessing
# 
# training_set = training_set[c(7, 8, 9, 10, 1)] # we need to get the columns in the right order
# # same for the test set
# test_set = as.data.frame(predict(lda, test_set)) # LDA needs a datafram, in PCA we got a dataframe in our preprocessing
# test_set = test_set[c(7, 8, 9, 10, 1)]
# 
# #Fitting SVM with training set
library(e1071)
# classifier = svm(formula = class ~ .,  # note the customer segment is now called class
#                  data = training_set,
#                  type = 'C-classification',
#                  kernel = 'linear')
# 
# #Predicting Test Set results
# y_pred = predict(classifier, newdata = test_set[-5])
# 
# #Confusion Matrix
# cm = table(test_set[, 5], y_pred)
# cm
# 
# accuracy = (264+264+299+249+260)/(264+264+299+249+260+1+1+1+1+4+1)
# accuracy
# 
# ### Reduced Dataset
scaledset = playerlistfinal_num
scaledset[-1] = scale(playerlistfinal_num[-1])
lda_full = lda(formula = Pos ~ ., data = scaledset)

scaledset2 = as.data.frame(predict(lda_full, scaledset))

ct = table(playerlistfinal_num$Pos, scaledset2$class)
diag(prop.table(ct,1))
sum(diag(prop.table(ct)))

scaledset3 = scaledset2[,7:10]
# 
# wcss = vector()
# n = 20
# set.seed(1234)
# for(k in 1:n) {
#   wcss[k] <- sum(kmeans(scaledset3, k)$withinss)
# }
# 
# #visualizing elbow method
# tibble(value = wcss) %>%
#   ggplot(mapping=aes(x=seq(1,length(wcss)), y=value)) +
#   geom_point()+
#   geom_line() +
#   labs(title = "The Elbow Method", y = "WCSS", x = "Number of Clusters (k)" ) +
#   theme_minimal()
# 
# #7-9 would be a reasonable k (8)
# 
# set.seed(1234)
# k8 = kmeans(scaledset3, centers = 8, nstart = 25)
# 
# #checking clusters
# k8$size
# k8$centers
# 
# 
# #2. visualizing cluster
# fviz_cluster(k8, data = scaledset3)
# 
# #Part IV
# #applying cluster IDs to original dataframe
# playerclust = playerlistfinalvars
# playerclust$cluster = k8$cluster
# playerclust %>%
#   dplyr::select(Player, Pos, Season, cluster)
# 
# playerclust %>%
#   group_by(cluster) %>%
#   dplyr::summarize(count = n()) %>%
#   ggplot(aes(x = cluster, y = count)) +
#   geom_bar(stat = "identity") +
#   scale_x_continuous(labels = playerclust$cluster, breaks = playerclust$cluster)
# 
# playerdetails = playerclust[,c(1:5,7:8)]
# 
# playerclust[is.na(playerclust)] <- 0
# 
# playerclust = playerclust %>%
#   dplyr::select(Player, Pos, player_code, Season, G, GS, MP, everything())
# 
# clust1 = playerclust %>%
#   filter(cluster == "1")
# 
# clust1scaled = clust1
# clust1scaled[-(1:9)] = scale(clustscaled[-(1:9)])
# 
# clust1scaled = clust1scaled %>%
#   pivot_longer(c(10:34), names_to = "Stat", values_to = "value")
# 
# clust1scaled %>%
#   ggplot(aes(x = Stat, y = value)) +
#   geom_boxplot()

## Model Based clustering
library(mclust)
scaledset3 = scaledset3
class = playerlistfinalvars$Pos
clPairs(scaledset3, class)

BIC = mclustBIC(scaledset3)
# plot(BIC)

summary(BIC)

mod1 = Mclust(scaledset3, x = BIC)
summary(mod1, parameters = TRUE)

# plot(mod1, what = "classification")

# table(class, mod1$classification)
#1 - mostly PG/SG, some SF
#2 - Mostly C/PF
#3 - Balanced
#4 - Mostly SG/SF
#5 - Mostly SF/PF, some SG
#6 - Mostly C/PF
#7 - Mostly PG, some SG

# plot(mod1, what = "uncertainty")
# 
# ICL = mclustICL(scaledset3)
# summary(ICL)
# plot(ICL)
# 
# LRT = mclustBootstrapLRT(scaledset3, modelName = "VVV")
# LRT

#Initialization
hc1 = hc(scaledset3, modelName = "VVV", use = "SVD")
hc1

BIC1 = mclustBIC(scaledset3, initialization = list(hcPairs = hc1))
summary(BIC1)

hc2 = hc(scaledset3, modelName = "VVV", use = "VARS")
hc2

BIC2 = mclustBIC(scaledset3, initialization = list(hcPairs = hc2))
summary(BIC2)

# hc3 = hc(scaledset3, modelName = "EEE", use = "SVD")
# hc3
# 
# BIC3 = mclustBIC(scaledset3, initialization = list(hcPairs = hc3))
# summary(BIC3)

BIC = mclustBICupdate(BIC1,BIC2)
summary(BIC)
plot(BIC)

mod2 = Mclust(scaledset3, x = BIC)
summary(mod2, parameters = TRUE)
plot(mod2, what = "classification")
table(class, mod2$classification)
#1 - mostly PG/SG, some SF --> 7
#2 - Mostly C/PF --> 3
#3 - Balanced --> 6
#4 - Mostly SG/SF --> 1
#5 - Mostly SF/PF, some SG --> 2
#6 - Mostly C/PF --> 5
#7 - Mostly PG, some SG --> 4


mod1class = mod1$classification
mod2class = mod2$classification
head(mod1class)
head(mod2class)

### Putting Clusters in Original Dataset
clusteredplayers = playerlistfinalvars %>%
  mutate(cluster = mod2class)

clusteredplayers = clusteredplayers %>%
  dplyr::select(cluster, everything())

#Traditional Position vs Cluster
clusteredplayers %>%
  group_by(cluster, Pos) %>%
  dplyr::summarize(count = n()) %>%
  ggplot(aes(x = reorder(cluster,count), y = count, fill = Pos)) +
  geom_bar(stat = "identity") +
  labs(title = "Clusters vs Traditional Positions",
       x = "Count",
       y = "") +
  theme(text = element_text(family = "Avenir Next",
                            color = 'black'),
        plot.title = element_text(color = "black",
                                  face = "bold",
                                  hjust = 0.5,
                                  size = 20),
        plot.subtitle = element_text(color = "#252525",
                                     face = "bold",
                                     hjust = 0.5),
        #vjust = -1,
        #size = 20),
        axis.text = element_text(color = 'black'),
        #size = 13),
        plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'),
        axis.title = element_text(face = "bold"),
        #size = 13),
        axis.text.y = element_text(face = "bold",
                                   color = "#252525"),
        #size = 10),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank())

clusteredplayers = clusteredplayers %>%
  dplyr::select(-From, -To) %>%
  dplyr::select(cluster, Player, Pos, player_code, Season, G, GS, MP, everything())

### Clusters
cluster1 = clusteredplayers %>%
  dplyr::filter(cluster == "1")

cluster2 = clusteredplayers %>%
  dplyr::filter(cluster == "2")

cluster3 = clusteredplayers %>%
  dplyr::filter(cluster == "3")

cluster4 = clusteredplayers %>%
  dplyr::filter(cluster == "4")

cluster5 = clusteredplayers %>%
  dplyr::filter(cluster == "5")

cluster6 = clusteredplayers %>%
  dplyr::filter(cluster == "6")

cluster7 = clusteredplayers %>%
  dplyr::filter(cluster == "7")

### Explore Stats
clustersscaled = clusteredplayers
clustersscaled = clustersscaled[-c(1:8)]
clustersscaled = as.data.frame(scale(clustersscaled))
clustersscaled = clustersscaled %>%
  mutate(cluster = clusteredplayers$cluster) %>%
  dplyr::select(cluster,everything())
clustersscaled = clustersscaled %>%
  pivot_longer(c(2:25), names_to = "Stat", values_to = "value")
clusterstats = clustersscaled %>%
  dplyr::group_by(cluster, Stat) %>%
  dplyr::summarize(mean = mean(value),
                   median = median(value)) %>%
  dplyr::arrange(cluster, desc(median))

top4 = clusterstats %>%
  dplyr::group_by(cluster, Stat) %>%
  dplyr::summarize(median = median,
                   mean = mean) %>%
  top_n(4, median)

bottom4 = clusterstats %>%
  dplyr::group_by(cluster, Stat) %>%
  dplyr::summarize(median = median,
                   mean = mean) %>%
  top_n(-4, median)
  
clusterstats4 = top4 %>%
  bind_rows(bottom4) %>%
  arrange(cluster, desc(median))

clustersscaled = clustersscaled %>%
  group_by(cluster, Stat) %>%
  mutate(statmedian = median(value))

clustersscaled %>%
  filter(cluster == "1") %>%
  filter(Stat %in% clusterstats4$Stat[clusterstats4$cluster == "1"]) %>%
  ggplot(aes(x = reorder(Stat,desc(statmedian)), y = value, fill = ifelse(statmedian > 0, "positive", "negative"))) +
  geom_hline(yintercept = 0, alpha = 0.4, color = "red") +
  geom_boxplot(outlier.shape = NA, show.legend = F) +
  scale_fill_manual(values = c("#b11111", "#2eab4f")) + 
  ylim(-2,2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Top and Bottom Cluster Stats",
       subtitle = "Cluster 1: 3PT Shooting Swingmen",
       x = "Stats",
       y = "Scaled Value") +
  theme(text = element_text(family = "Avenir Next",
                            color = 'black'),
        plot.title = element_text(color = "black",
                                  face = "bold",
                                  hjust = 0.5,
                                  size = 20),
        plot.subtitle = element_text(color = "#252525",
                                     face = "bold",
                                     hjust = 0.5),
                                     #vjust = -1,
                                     #size = 20),
        axis.text = element_text(color = 'black'),
                                 #size = 13),
        plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'),
        axis.title = element_text(face = "bold"),
                                  #size = 13),
        axis.text.y = element_text(face = "bold",
                                   color = "#252525"),
                                   #size = 10),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank())

clustersscaled %>%
  filter(cluster == "2") %>%
  filter(Stat %in% clusterstats4$Stat[clusterstats4$cluster == "2"]) %>%
  ggplot(aes(x = reorder(Stat,desc(statmedian)), y = value, fill = ifelse(statmedian > 0, "positive", "negative"))) +
  geom_hline(yintercept = 0, alpha = 0.4, color = "red") +
  geom_boxplot(outlier.shape = NA, show.legend = F) +
  scale_fill_manual(values = c("#b11111", "#2eab4f")) + 
  ylim(-2,2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Top and Bottom Cluster Stats",
       subtitle = "Cluster 2: Stretch Forwards",
       x = "Stats",
       y = "Scaled Value") +
  theme(text = element_text(family = "Avenir Next",
                            color = 'black'),
        plot.title = element_text(color = "black",
                                  face = "bold",
                                  hjust = 0.5,
                                  size = 20),
        plot.subtitle = element_text(color = "#252525",
                                     face = "bold",
                                     hjust = 0.5),
        #vjust = -1,
        #size = 20),
        axis.text = element_text(color = 'black'),
        #size = 13),
        plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'),
        axis.title = element_text(face = "bold"),
        #size = 13),
        axis.text.y = element_text(face = "bold",
                                   color = "#252525"),
        #size = 10),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank())

clustersscaled %>%
  filter(cluster == "3") %>%
  filter(Stat %in% clusterstats4$Stat[clusterstats4$cluster == "3"]) %>%
  ggplot(aes(x = reorder(Stat,desc(statmedian)), y = value, fill = ifelse(statmedian > 0, "positive", "negative"))) +
  geom_hline(yintercept = 0, alpha = 0.4, color = "red") +
  geom_boxplot(outlier.shape = NA, show.legend = F) +
  scale_fill_manual(values = c("#b11111", "#2eab4f")) + 
  ylim(-2,2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Top and Bottom Cluster Stats",
       subtitle = "Cluster 3: Rim-Running Athletic Bigs",
       x = "Stats",
       y = "Scaled Value") +
  theme(text = element_text(family = "Avenir Next",
                            color = 'black'),
        plot.title = element_text(color = "black",
                                  face = "bold",
                                  hjust = 0.5,
                                  size = 20),
        plot.subtitle = element_text(color = "#252525",
                                     face = "bold",
                                     hjust = 0.5),
        #vjust = -1,
        #size = 20),
        axis.text = element_text(color = 'black'),
        #size = 13),
        plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'),
        axis.title = element_text(face = "bold"),
        #size = 13),
        axis.text.y = element_text(face = "bold",
                                   color = "#252525"),
        #size = 10),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank())

clustersscaled %>%
  filter(cluster == "4") %>%
  filter(Stat %in% clusterstats4$Stat[clusterstats4$cluster == "4"]) %>%
  ggplot(aes(x = reorder(Stat,desc(statmedian)), y = value, fill = ifelse(statmedian > 0, "positive", "negative"))) +
  geom_hline(yintercept = 0, alpha = 0.4, color = "red") +
  geom_boxplot(outlier.shape = NA, show.legend = F) +
  scale_fill_manual(values = c("#b11111", "#2eab4f")) + 
  ylim(-2,2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Top and Bottom Cluster Stats",
       subtitle = "Cluster 4: High Usage Guards",
       x = "Stats",
       y = "Scaled Value") +
  theme(text = element_text(family = "Avenir Next",
                            color = 'black'),
        plot.title = element_text(color = "black",
                                  face = "bold",
                                  hjust = 0.5,
                                  size = 20),
        plot.subtitle = element_text(color = "#252525",
                                     face = "bold",
                                     hjust = 0.5),
        #vjust = -1,
        #size = 20),
        axis.text = element_text(color = 'black'),
        #size = 13),
        plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'),
        axis.title = element_text(face = "bold"),
        #size = 13),
        axis.text.y = element_text(face = "bold",
                                   color = "#252525"),
        #size = 10),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank())

clustersscaled %>%
  filter(cluster == "5") %>%
  filter(Stat %in% clusterstats4$Stat[clusterstats4$cluster == "5"]) %>%
  ggplot(aes(x = reorder(Stat,desc(statmedian)), y = value, fill = ifelse(statmedian > 0, "positive", "negative"))) +
  geom_hline(yintercept = 0, alpha = 0.4, color = "red") +
  geom_boxplot(outlier.shape = NA, show.legend = F) +
  scale_fill_manual(values = c("#b11111", "#2eab4f")) + 
  ylim(-2,2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Top and Bottom Cluster Stats",
       subtitle = "Cluster 5: Traditional Bigs",
       x = "Stats",
       y = "Scaled Value") +
  theme(text = element_text(family = "Avenir Next",
                            color = 'black'),
        plot.title = element_text(color = "black",
                                  face = "bold",
                                  hjust = 0.5,
                                  size = 20),
        plot.subtitle = element_text(color = "#252525",
                                     face = "bold",
                                     hjust = 0.5),
        #vjust = -1,
        #size = 20),
        axis.text = element_text(color = 'black'),
        #size = 13),
        plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'),
        axis.title = element_text(face = "bold"),
        #size = 13),
        axis.text.y = element_text(face = "bold",
                                   color = "#252525"),
        #size = 10),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank())

clustersscaled %>%
  filter(cluster == "6") %>%
  filter(Stat %in% clusterstats4$Stat[clusterstats4$cluster == "6"]) %>%
  ggplot(aes(x = reorder(Stat,desc(statmedian)), y = value, fill = ifelse(statmedian > 0, "positive", "negative"))) +
  geom_hline(yintercept = 0, alpha = 0.4, color = "red") +
  geom_boxplot(outlier.shape = NA, show.legend = F) +
  scale_fill_manual(values = c("#b11111", "#2eab4f")) + 
  ylim(-2,2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Top and Bottom Cluster Stats",
       subtitle = "Cluster 6: Versatile Role Players",
       x = "Stats",
       y = "Scaled Value") +
  theme(text = element_text(family = "Avenir Next",
                            color = 'black'),
        plot.title = element_text(color = "black",
                                  face = "bold",
                                  hjust = 0.5,
                                  size = 20),
        plot.subtitle = element_text(color = "#252525",
                                     face = "bold",
                                     hjust = 0.5),
        #vjust = -1,
        #size = 20),
        axis.text = element_text(color = 'black'),
        #size = 13),
        plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'),
        axis.title = element_text(face = "bold"),
        #size = 13),
        axis.text.y = element_text(face = "bold",
                                   color = "#252525"),
        #size = 10),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank())

clustersscaled %>%
  filter(cluster == "7") %>%
  filter(Stat %in% clusterstats4$Stat[clusterstats4$cluster == "7"]) %>%
  ggplot(aes(x = reorder(Stat,desc(statmedian)), y = value, fill = ifelse(statmedian > 0, "positive", "negative"))) +
  geom_hline(yintercept = 0, alpha = 0.4, color = "red") +
  geom_boxplot(outlier.shape = NA, show.legend = F) +
  scale_fill_manual(values = c("#b11111", "#2eab4f")) + 
  ylim(-2,2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Top and Bottom Cluster Stats",
       subtitle = "Cluster 7: Ball Dominant Playmakers",
       x = "Stats",
       y = "Scaled Value") +
  theme(text = element_text(family = "Avenir Next",
                            color = 'black'),
        plot.title = element_text(color = "black",
                                  face = "bold",
                                  hjust = 0.5,
                                  size = 20),
        plot.subtitle = element_text(color = "#252525",
                                     face = "bold",
                                     hjust = 0.5),
        #vjust = -1,
        #size = 20),
        axis.text = element_text(color = 'black'),
        #size = 13),
        plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'),
        axis.title = element_text(face = "bold"),
        #size = 13),
        axis.text.y = element_text(face = "bold",
                                   color = "#252525"),
        #size = 10),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank())

#1: 3PT Shooting Swingmen
#2: Stretch Forwards
#3: Rim-Running Athletic Bigs
#4: High Usage Guard
#5: Traditional Bigs
#6: Versatile Role Players
#7: Ball Dominant Playmakers (Scorers/Distributors)

clusteredplayers %>%
  filter(cluster == "4") %>%
  group_by(cluster, Pos) %>%
  summarize(count = n())


### Assigning name to cluster
cluster1name = "3PT Shooting Swingmen"
cluster2name = "Stretch Forwards"
cluster3name = "Rim-Running Athletic Bigs"
cluster4name = "High Usage Guards"
cluster5name = "Traditional Bigs"
cluster6name = "Versatile Role Players"
cluster7name = "Ball Dominant Playmakers"

clusteredplayers$cluster[clusteredplayers$cluster == "1"] <- cluster1name
clusteredplayers$cluster[clusteredplayers$cluster == "2"] <- cluster2name
clusteredplayers$cluster[clusteredplayers$cluster == "3"] <- cluster3name
clusteredplayers$cluster[clusteredplayers$cluster == "4"] <- cluster4name
clusteredplayers$cluster[clusteredplayers$cluster == "5"] <- cluster5name
clusteredplayers$cluster[clusteredplayers$cluster == "6"] <- cluster6name
clusteredplayers$cluster[clusteredplayers$cluster == "7"] <- cluster7name

### Cluster that's highest per stat

highestperstat_median = clustersscaled %>%
  group_by(Stat, cluster) %>%
  summarize(med = median(value),
            mean = mean(value)) %>%
  top_n(1, med)

highestperstat_mean = clustersscaled %>%
  group_by(Stat, cluster) %>%
  summarize(med = median(value),
            mean = mean(value)) %>%
  top_n(1, mean)

highestperstat_median$cluster[highestperstat_median$cluster == "1"] <- cluster1name
highestperstat_median$cluster[highestperstat_median$cluster == "2"] <- cluster2name
highestperstat_median$cluster[highestperstat_median$cluster == "3"] <- cluster3name
highestperstat_median$cluster[highestperstat_median$cluster == "4"] <- cluster4name
highestperstat_median$cluster[highestperstat_median$cluster == "5"] <- cluster5name
highestperstat_median$cluster[highestperstat_median$cluster == "6"] <- cluster6name
highestperstat_median$cluster[highestperstat_median$cluster == "7"] <- cluster7name

highestperstat_mean$cluster[highestperstat_mean$cluster == "1"] <- cluster1name
highestperstat_mean$cluster[highestperstat_mean$cluster == "2"] <- cluster2name
highestperstat_mean$cluster[highestperstat_mean$cluster == "3"] <- cluster3name
highestperstat_mean$cluster[highestperstat_mean$cluster == "4"] <- cluster4name
highestperstat_mean$cluster[highestperstat_mean$cluster == "5"] <- cluster5name
highestperstat_mean$cluster[highestperstat_mean$cluster == "6"] <- cluster6name
highestperstat_mean$cluster[highestperstat_mean$cluster == "7"] <- cluster7name

highestperstat_mean %>%
  ggplot(aes(x = mean, y = Stat)) +
  geom_bar(stat = "identity") +
  geom_label(
    aes(label=paste0(cluster, " ",mean)), 
    nudge_x = 0.1
  ) + 
  labs(y = "Statistic",
       x = "Mean") +
  theme(text = element_text(family = "Avenir Next",
                            color = 'black'),
        axis.text = element_text(color = 'black'),
        plot.background = element_rect(fill = 'white'),
        #panel.background = element_rect(fill = 'white'),
        axis.title = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold",
                                   color = "#252525"),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        label)

clusteredplayers$Player = stringr::str_replace(clusteredplayers$Player, '\\*', '')


write_csv(clusteredplayers, path = "/Volumes/GoogleDrive/My Drive/Laptop Files/1920B2/MW 2 Sports Analytics/05 Project/Sports Analytics Project/clusteredplayers.csv", na = "NA", col_names = TRUE)

## RAPTOR
clusteredplayers = read_csv("/Volumes/GoogleDrive/My Drive/Laptop Files/1920B2/MW 2 Sports Analytics/05 Project/Sports Analytics Project/clusteredplayers.csv")
raptor = read_csv("/Volumes/GoogleDrive/My Drive/Laptop Files/1920B2/MW 2 Sports Analytics/05 Project/Sports Analytics Project/historical_RAPTOR_by_player.csv")
raptor_latest = read_csv("/Volumes/GoogleDrive/My Drive/Laptop Files/1920B2/MW 2 Sports Analytics/05 Project/Sports Analytics Project/latest_RAPTOR_by_player.csv")

raptor_latest = raptor_latest %>%
  dplyr::select(player_name, player_id, season, poss, mp, raptor_offense, raptor_defense, raptor_total, war_total, war_reg_season, war_playoffs, predator_offense, predator_defense, predator_total, pace_impact)

raptor = rbind(raptor,raptor_latest)
raptor = raptor %>%
  dplyr::select(player_name, player_id, season, poss, mp, raptor_total, war_reg_season, predator_total, pace_impact)

### Changing season values in clusteredplayers data
clusteredplayers$Season[clusteredplayers$Season == "2000-01"] <- 2001
clusteredplayers$Season[clusteredplayers$Season == "2001-02"] <- 2002
clusteredplayers$Season[clusteredplayers$Season == "2002-03"] <- 2003
clusteredplayers$Season[clusteredplayers$Season == "2003-04"] <- 2004
clusteredplayers$Season[clusteredplayers$Season == "2004-05"] <- 2005
clusteredplayers$Season[clusteredplayers$Season == "2005-06"] <- 2006
clusteredplayers$Season[clusteredplayers$Season == "2006-07"] <- 2007
clusteredplayers$Season[clusteredplayers$Season == "2007-08"] <- 2008
clusteredplayers$Season[clusteredplayers$Season == "2008-09"] <- 2009
clusteredplayers$Season[clusteredplayers$Season == "2009-10"] <- 2010
clusteredplayers$Season[clusteredplayers$Season == "2010-11"] <- 2011
clusteredplayers$Season[clusteredplayers$Season == "2011-12"] <- 2012
clusteredplayers$Season[clusteredplayers$Season == "2012-13"] <- 2013
clusteredplayers$Season[clusteredplayers$Season == "2013-14"] <- 2014
clusteredplayers$Season[clusteredplayers$Season == "2014-15"] <- 2015
clusteredplayers$Season[clusteredplayers$Season == "2015-16"] <- 2016
clusteredplayers$Season[clusteredplayers$Season == "2016-17"] <- 2017
clusteredplayers$Season[clusteredplayers$Season == "2017-18"] <- 2018
clusteredplayers$Season[clusteredplayers$Season == "2018-19"] <- 2019
clusteredplayers$Season[clusteredplayers$Season == "2019-20"] <- 2020

clusteredplayers = clusteredplayers %>%
  mutate(idseas = paste0(player_code,Season))

### joining raptor
raptorjoin = raptor %>%
  dplyr::select(-player_name) %>%
  mutate(idseas = paste0(player_id, season))

clusteredplayers = clusteredplayers %>%
  inner_join(raptorjoin, by = "idseas") 

clusteredplayers = clusteredplayers %>%
  dplyr::select(-idseas, -player_id, -season)

write_csv(clusteredplayers, path = "/Volumes/GoogleDrive/My Drive/Laptop Files/1920B2/MW 2 Sports Analytics/05 Project/Sports Analytics Project/clusteredplayers.csv", 
          na = "NA", 
          col_names = TRUE)

### EDA
clusteredplayers = read_csv("/Volumes/GoogleDrive/My Drive/Laptop Files/1920B2/MW 2 Sports Analytics/05 Project/Sports Analytics Project/clusteredplayers.csv")
clusterraptor = clusteredplayers %>%
  group_by(cluster) %>%
  summarize(raptor = mean(raptor_total),
            war = mean(war_reg_season),
            predator = mean(predator_total),
            paceimp = mean(pace_impact))


### Z-score from model
zscore = as.data.frame(mod2$z)
colnames(zscore) = c(cluster1name,cluster2name,cluster3name,cluster4name,cluster5name,cluster6name,cluster7name)
library(corrplot)
zscore %>%
  cor() %>%
  corrplot()
#Highest Correlation with each cluster:
#3PT Shooting Swingmen - Stretch Forwards, Ball Dominant Playmakers
#Stretch Forwards - 3PT Shooting Swingmen, Rim-Running Athletic Bigs
#Rim-Running Athletic Bigs - Traditional Bigs, Versatile Role Players
#High-Usage Guards - Ball Dominant Playmakers, Versatile Role Players
#Traditional Bigs - Rim-Running Athletic Bigs, Versatile Role Players
#Versatile Role Players - Rim-Running Athletic Bigs, Ball Dominant Playmakers
#Ball Dominant Playmakers - High Usage Guards, Versatile Role Players

zscore = round(zscore,4)

clusteredplayersprobs = clusteredplayers %>%
  cbind(zscore)

playerzscore = zscore %>%
  mutate(player = clusteredplayers$Player,
         season = clusteredplayers$Season) %>%
  dplyr::select(player,season,everything())

## Notable Players with high likelihood of being from multiple clusters
#1. Mike Bibby (2001) - 50.1% Ball Dominant Playmaker, 49.7% High Usage Guard (slightly more well-rounded)
#2. Andre Drummond (2019) - 50.5% Rim-Running Athletic Big, 45.4% Traditional Big (more offense)
#3. Jae Crowder (2014) - 50.9% 3PT Shooting Swingmen, 48.5% Stretch Forward (Shorter, better defensive stats - opposite: Mike Miller 2014)

write_csv(clusteredplayersprobs, path = "/Volumes/GoogleDrive/My Drive/Laptop Files/1920B2/MW 2 Sports Analytics/05 Project/Sports Analytics Project/clusteredplayersfinal.csv", 
          na = "NA", 
          col_names = TRUE)

### Moving to LineUp Analysis
#clusterfinal = clusteredplayersprobs
clusterfinal = read_csv("/Volumes/GoogleDrive/My Drive/Laptop Files/1920B2/MW 2 Sports Analytics/05 Project/Sports Analytics Project/clusteredplayersfinal.csv")
clustercurrent = clusterfinal %>%
  dplyr::filter(Season == "2020")

contracts = read_csv("/Volumes/GoogleDrive/My Drive/Laptop Files/1920B2/MW 2 Sports Analytics/05 Project/Sports Analytics Project/contracts.csv")
contractsjoin = contracts %>%
  dplyr::select(player_code, CurrentSalary)
clustercurrent = clustercurrent %>%
  inner_join(contractsjoin, by = "player_code") %>%
  dplyr::select(cluster, Player, Pos, player_code, Season, CurrentSalary, everything())

### Parsing Lineup Data
lineupnotparsed = read_csv("/Volumes/GoogleDrive/My Drive/Laptop Files/1920B2/MW 2 Sports Analytics/05 Project/Sports Analytics Project/LineupsNotParsed.csv")

lineupnotparsed = lineupnotparsed %>%
  separate(everything, c("player1", "player2", "player3", "player4", "rest"), sep = ",")

lineupnotparsed2 = lineupnotparsed %>%
  separate(rest,
           into = c("player5","rest"),
           sep = "\\-*\\d+\\.*\\d*")

lineupnotparsed2 = lineupnotparsed2 %>%
  mutate(count = nchar(player5))

stats = lineupnotparsed %>%
  mutate(count = lineupnotparsed2$count)
stats = stats %>%
  mutate(stats = str_sub(rest, start = count, end = nchar(rest)))
stats = stats %>%
  dplyr::select(-rest,-count) %>%
  separate(stats,
           into = c("stat1","stat2","stat3","stat4","stat5","stat6","stat7","stat8","stat9","stat10","stat11","stat12","stat13","stat14","stat15","stat16","stat17"),
           sep = " ")
stats = stats %>%
  mutate(stat1 = lineupnotparsed2$player5) %>%
  mutate(Team = str_sub(stat1,start = (nchar(stat1)-3), end = nchar(stat1))) %>%
  mutate(stat1 = str_sub(stat1, start = 1, end = (nchar(stat1)-5)))
stats = stats[,-1]

columnnames = c("player1","player2","player3", "player4", "player5", "GP", "Min",'OFFRTG', 'DEFRTG', 'NETRTG', 'AST_perc', 'AST_TO', 'ASTRatio', 'OREB_perc', 'DREB_perc','REB_perc', 'TOratio', 'EFG_perc', 'TS_perc', 'Pace', "Pie","Tm")
colnames(stats) = columnnames

stats = stats %>%
  mutate_at(c("GP", "Min",'OFFRTG', 'DEFRTG', 'NETRTG', 'AST_perc', 'AST_TO', 'ASTRatio', 'OREB_perc', 'DREB_perc','REB_perc', 'TOratio', 'EFG_perc', 'TS_perc', 'Pace', "Pie"), as.numeric)
stats = stats %>%
  mutate(Poss = round((Pace/48*Min),0))

relevantstats = stats %>%
  dplyr::select("player1","player2","player3", "player4", "player5","Tm", "GP", "Min",'NETRTG',"Poss")

relevantstats = relevantstats %>%
  mutate(player1 = str_remove_all(player1," Jr.")) %>%
  mutate(player1 = str_remove_all(player1," Sr.")) %>%
  mutate(player1 = str_remove_all(player1," II")) %>%
  mutate(player1 = str_remove_all(player1," III")) %>%
  mutate(player1 = str_remove_all(player1," IV")) %>%
  mutate(player2 = str_remove_all(player2," Jr.")) %>%
  mutate(player2 = str_remove_all(player2," Sr.")) %>%
  mutate(player2 = str_remove_all(player2," II")) %>%
  mutate(player2 = str_remove_all(player2," III")) %>%
  mutate(player2 = str_remove_all(player2," IV")) %>%
  mutate(player3 = str_remove_all(player3," Jr.")) %>%
  mutate(player3 = str_remove_all(player3," Sr.")) %>%
  mutate(player3 = str_remove_all(player3," II")) %>%
  mutate(player3 = str_remove_all(player3," III")) %>%
  mutate(player3 = str_remove_all(player3," IV")) %>%
  mutate(player4 = str_remove_all(player4," Jr.")) %>%
  mutate(player4 = str_remove_all(player4," Sr.")) %>%
  mutate(player4 = str_remove_all(player4," II")) %>%
  mutate(player4 = str_remove_all(player4," III")) %>%
  mutate(player4 = str_remove_all(player4," IV")) %>%
  mutate(player5 = str_remove_all(player5," Jr.")) %>%
  mutate(player5 = str_remove_all(player5," Sr.")) %>%
  mutate(player5 = str_remove_all(player5," II")) %>%
  mutate(player5 = str_remove_all(player5," III")) %>%
  mutate(player5 = str_remove_all(player5," IV"))

relevantstats = relevantstats %>%
  mutate(player1 = str_remove_all(player1,"\\.")) %>%
  mutate(player1 = str_remove_all(player1," ")) %>%
  mutate(player2 = str_remove_all(player2,"\\.")) %>%
  mutate(player2 = str_remove_all(player2," ")) %>%
  mutate(player3 = str_remove_all(player3,"\\.")) %>%
  mutate(player3 = str_remove_all(player3," ")) %>%
  mutate(player4 = str_remove_all(player4,"\\.")) %>%
  mutate(player4 = str_remove_all(player4," ")) %>%
  mutate(player5 = str_remove_all(player5,"\\.")) %>%
  mutate(player5 = str_remove_all(player5," "))
  
relevantstats = relevantstats %>%
  mutate(player1 = paste0(player1 ,Tm)) %>%
  mutate(player2 = paste0(player2 ,Tm)) %>%
  mutate(player3 = paste0(player3 ,Tm)) %>%
  mutate(player4 = paste0(player4 ,Tm)) %>%
  mutate(player5 = paste0(player5 ,Tm)) 

playerlistfinal2 = playerlistfinal %>%
  filter(Season == "2019-20") %>%
  dplyr::select(Player,Tm, player_code)

playerlistfinal2[44,1] = "Bogdan Bogdanovic"
playerlistfinal2[45,1] = "Bojan Bogdanovic"
playerlistfinal2[117,1] = "Luka Doncic"
playerlistfinal2[120,1] = "Goran Dragic"
playerlistfinal2[185,1] = "Juan Hernangomez"
playerlistfinal2[186,1] = "Juan Hernangomez"
playerlistfinal2[187,1] = "Willy Hernangomez"
playerlistfinal2[221,1] = "Nikola Jokic"
playerlistfinal2[241,1] = "Skal Labissiere"
playerlistfinal2[260,1] = "Boban Marjanovic"
playerlistfinal2[329,1] = "Kristaps Porzingis"
playerlistfinal2[352,1] = "Dario Saric"
playerlistfinal2[353,1] = "Tomas Satoransky"
playerlistfinal2[354,1] = "Dennis Schroder"
playerlistfinal2[386,1] = "Jonas Valanciunas"
playerlistfinal2[393,1] = "Nikola Vucevic"

playerlistfinal2 = playerlistfinal2 %>%
  separate(Player, into = c("First","Last"),
           sep = " ")
playerlistfinal2 = playerlistfinal2 %>%
  mutate(code = paste0(str_sub(First, 1, 1),Last,Tm))
playerlistfinal2 = playerlistfinal2 %>%
  dplyr::select(player_code, code)

clustercurrent2 = clustercurrent %>%
  inner_join(playerlistfinal2,by = "player_code")

index = which(duplicated(clustercurrent2$code) | duplicated(clustercurrent2[nrow(clustercurrent2):1, ])[nrow(clustercurrent2):1]) - 1

clustercurrent3 = clustercurrent2[-index,]
row.names(clustercurrent3) = NULL

write_csv(clustercurrent3, "/Volumes/GoogleDrive/My Drive/Laptop Files/1920B2/MW 2 Sports Analytics/05 Project/Sports Analytics Project/currentcluster3.csv", 
          na = "NA", 
          col_names = TRUE)
write_csv(relevantstats, "/Volumes/GoogleDrive/My Drive/Laptop Files/1920B2/MW 2 Sports Analytics/05 Project/Sports Analytics Project/lineup.csv", 
          na = "NA", 
          col_names = TRUE)

# ### Sample Replacement - Gary Harris
# # clustercurrent3 = read_csv("/Volumes/GoogleDrive/My Drive/Laptop Files/1920B2/MW 2 Sports Analytics/05 Project/Sports Analytics Project/currentcluster3.csv")
# # relevantstats = read_csv("/Volumes/GoogleDrive/My Drive/Laptop Files/1920B2/MW 2 Sports Analytics/05 Project/Sports Analytics Project/lineup.csv")
# trial_replacements_GaryHarris = clustercurrent3 %>%
#   filter((raptor_total > -0.74) & (CurrentSalary <= 17839286) & (cluster == "3PT Shooting Swingmen")) %>%
#   mutate(comp_index = ((raptor_total - (-0.74)))/(-0.74)+((CurrentSalary - 17839286)/17839286),
#          raptordiff = abs(((raptor_total - (-0.74)))/(-0.74)),
#          salarydiff = abs(((CurrentSalary - 17839286)/17839286)))
# 
# trial_replacements_GaryHarris[,49:50] = as.data.frame(scale(trial_replacements_GaryHarris[,49:50]))
# trial_replacements_GaryHarris = trial_replacements_GaryHarris  %>%
#   mutate(comp_index_add = raptordiff + salarydiff)

# Lineup Analysis
relevantstats = read_csv("/Volumes/GoogleDrive/My Drive/Laptop Files/1920B2/MW 2 Sports Analytics/05 Project/Sports Analytics Project/lineup.csv")
clustercurrent3 = read_csv("/Volumes/GoogleDrive/My Drive/Laptop Files/1920B2/MW 2 Sports Analytics/05 Project/Sports Analytics Project/currentcluster3.csv")
lineup_final = relevantstats
team = c("MIL","LAL","TOR","LAC","BOS","DEN","MIA","UTA","DAL","HOU","OKC","IND","PHI","MEM","BKN","ORL","POR","NOP","SAC","SAS","PHX","WAS","CHA","CHI","NYK","ATL","DET","CLE","MIN","GSW")
teamNR = c(10.7,7.1,6.4,6.4,6.1,3.1,3.0,3.3,5.8,3.4,2.5,2.0,2.1,-1.0,-0.6,-1.2,-1.5,-1.0,-1.7,-1.5,-1.0,-3.9,-7.0,-3.1,-6.5,-7.4,-3.5,-7.9,-4.0,-8.6)
team_nr = as.data.frame(team) %>%
  mutate(teamNR = teamNR)

lineup_final$player1 = gsub('\\s+','', lineup_final$player1)
lineup_final$player2 = gsub('\\s+','', lineup_final$player2)
lineup_final$player3 = gsub('\\s+','', lineup_final$player3)
lineup_final$player4 = gsub('\\s+','', lineup_final$player4)
lineup_final$player5 = gsub('\\s+','', lineup_final$player5)
lineup_final$Tm = gsub('\\s+','', lineup_final$Tm)
lineup_final = lineup_final %>%
  inner_join(team_nr, by = c("Tm" = "team"))
lineup_final = lineup_final %>%
  mutate(AdjNR = ifelse(Poss >= 550,NETRTG, round(((Poss/550)*NETRTG)+((1-(Poss/550))*teamNR),4)))
lineup_trim = lineup_final %>%
  mutate(lineup = c(1:nrow(lineup_final))) %>%
  dplyr::select(lineup,player1,player2,player3,player4,player5,AdjNR)

clustercurrent_trim = clustercurrent3 %>%
  dplyr::select(code, `3PT Shooting Swingmen`, `Stretch Forwards`, `Rim-Running Athletic Bigs`, `High Usage Guards`, `Traditional Bigs`, `Versatile Role Players`,`Ball Dominant Playmakers`)

lineup_trim_longer = lineup_trim %>%
  pivot_longer(cols = c(player1,player2,player3,player4, player5),
               names_to = "player",
               values_to = "code") %>%
  dplyr::select(lineup, player, code, AdjNR)

lineup_trim_probs = lineup_trim_longer %>%
  inner_join(clustercurrent_trim, by = "code") %>%
  dplyr::select(-AdjNR, AdjNR)

colnames(lineup_trim_probs) = c("lineup","player","code","TPSS","SFor","RRAB","HUG","TDB","VRP","BDP","AdjNR")

lineup_trim_probs2 = lineup_trim_probs %>%
  group_by(lineup) %>%
  summarize(TPSS = sum(TPSS),
            SFor = sum(SFor),
            RRAB = sum(RRAB),
            HUG = sum(HUG),
            TDB = sum(TDB),
            VRP = sum(VRP),
            BDP = sum(BDP),
            AdjNR = mean(AdjNR)
  )

lineupmod = lm(AdjNR ~ TPSS + SFor + RRAB + HUG + TDB + VRP + BDP, data = lineup_trim_probs2)
summary(lineupmod)

coef = as.data.frame(lineupmod$coefficients)
coef[,1] = rownames(coef)
rownames(coef) = NULL
colnames(coef) = c("Cluster","Coefficient")
coef = coef %>%
  filter(Cluster != "(Intercept)")

ggplot(coef, aes(x = Coefficient, y = reorder(Cluster,Coefficient))) +
  geom_point(color = ifelse(round(coef$Coefficient,2) >= 0, "#329C23","#B43D23")) +
  geom_vline(xintercept = 0,
             alpha = 0.5) +
  geom_label(aes(label = round(Coefficient,2)),nudge_x = 0.3,
             color = ifelse(round(coef$Coefficient,2) >= 0, "#329C23","#B43D23"),
             family = "Avenir Next",
             fontface = "bold") +
  labs(title = "Regression Coefficients of Each Cluster",
       x = "Coefficient",
       y = "") +
  theme(text = element_text(family = "Avenir Next",
                            color = 'black'),
        plot.title = element_text(color = "black",
                                  face = "bold",
                                  hjust = 0.5,
                                  size = 20),
        plot.subtitle = element_text(color = "#252525",
                                     face = "bold",
                                     hjust = 0.5),
        #vjust = -1,
        #size = 20),
        axis.text = element_text(color = 'black'),
        #size = 13),
        plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'),
        axis.title = element_text(face = "bold"),
        #size = 13),
        axis.text.y = element_text(face = "bold",
                                   color = "#252525"),
        #size = 10),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())


### Random Forest
modelLookup("rf")

grid <- expand.grid(.mtry = 7)

grid

ctrl <-
  trainControl(method = "boot",
               number = 100,
               selectionFunction = "best")

# CAUTION: This takes a while to run!
set.seed(1234)
rf.mod <-
  train(
    AdjNR ~ TPSS + SFor + RRAB + HUG + TDB + VRP + BDP,
    data = lineup_trim_probs2,
    method = "rf",
    metric = "RMSE",
    trControl = ctrl,
    tuneGrid = grid
  )

rf.mod

rf.mod$trainingData$BDP


ggplot(rf.mod$trainingData, aes(x = rf.mod$trainingData$HUG, y = rf.mod$trainingData$SFor, col = rf.mod$finalModel$predicted)) +
  geom_point() +
  scale_color_gradient(low = "black",
                       high = "red")

library(randomForest)
set.seed(1234)
rf.mod2 = randomForest(AdjNR ~ TPSS + SFor + RRAB + HUG + TDB + VRP + BDP,
                       data = lineup_trim_probs2,
                       mtry = 7)
pred2 = predict(rf.mod2, newdata = matrix_sub)

### Player Frequency per cluster

cluster1 %>%
  group_by(Player) %>%
  summarize(count = n()) %>%
  arrange(desc(count))


cluster2 %>%
  group_by(Player) %>%
  summarize(count = n()) %>%
  arrange(desc(count))


cluster3 %>%
  group_by(Player) %>%
  summarize(count = n()) %>%
  arrange(desc(count))


cluster4 %>%
  group_by(Player) %>%
  summarize(count = n()) %>%
  arrange(desc(count))


cluster6 %>%
  group_by(Player) %>%
  summarize(count = n()) %>%
  arrange(desc(count))


cluster7 %>%
  group_by(Player) %>%
  summarize(count = n()) %>%
  arrange(desc(count))


#MATRIX

N = 5
# matrix = expand.grid(c(0,0.25,0.5,0.75,1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5,3.75,4,4.25,4.5,4.75,5),
#                      c(0,0.25,0.5,0.75,1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5,3.75,4,4.25,4.5,4.75,5),
#                      c(0,0.25,0.5,0.75,1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5,3.75,4,4.25,4.5,4.75,5),
#                      c(0,0.25,0.5,0.75,1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5,3.75,4,4.25,4.5,4.75,5),
#                      c(0,0.25,0.5,0.75,1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5,3.75,4,4.25,4.5,4.75,5),
#                      c(0,0.25,0.5,0.75,1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5,3.75,4,4.25,4.5,4.75,5),
#                      c(0,0.25,0.5,0.75,1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5,3.75,4,4.25,4.5,4.75,5))


matrix = expand.grid(c(0,0.33,0.67,1,1.33,1.67,2,2.33,2.67,3,3.33,3.67,4,4.33,4.67,5),
                     c(0,0.33,0.67,1,1.33,1.67,2,2.33,2.67,3,3.33,3.67,4,4.33,4.67,5),
                     c(0,0.33,0.67,1,1.33,1.67,2,2.33,2.67,3,3.33,3.67,4,4.33,4.67,5),
                     c(0,0.33,0.67,1,1.33,1.67,2,2.33,2.67,3,3.33,3.67,4,4.33,4.67,5),
                     c(0,0.33,0.67,1,1.33,1.67,2,2.33,2.67,3,3.33,3.67,4,4.33,4.67,5),
                     c(0,0.33,0.67,1,1.33,1.67,2,2.33,2.67,3,3.33,3.67,4,4.33,4.67,5),
                     c(0,0.33,0.67,1,1.33,1.67,2,2.33,2.67,3,3.33,3.67,4,4.33,4.67,5))


matrix_sub = matrix[rowSums(matrix) == N,]
rownames(matrix_sub) = NULL

matrix_sub$AdjNR = ""
lineup_trim_probs3 = lineup_trim_probs2 %>%
  dplyr::select(-lineup)
colnames(matrix_sub) = colnames(lineup_trim_probs3)

write_csv(matrix_sub, path = "/Volumes/GoogleDrive/My Drive/Laptop Files/1920B2/MW 2 Sports Analytics/05 Project/Sports Analytics Project/matrix.csv", 
          na = "NA", 
          col_names = TRUE)

matrix_sub = read_csv("/Volumes/GoogleDrive/My Drive/Laptop Files/1920B2/MW 2 Sports Analytics/05 Project/Sports Analytics Project/matrix.csv")
rf.pred <- predict(rf.mod, matrix_sub, type = "raw")

matrix_sub$AdjNR = rf.pred

ggplot(matrix_sub, aes(x = TDB, y = BDP, col = AdjNR)) +
  geom_jitter() +
  scale_color_gradient(low = "white",
                       high = "red") +
  ylim(0,2) +
  xlim(0,2)

#For every high usage player (High Usage Guard, Ball Dominant Playmaker - have just 1), you need 1-2 Three Point Shooting Swingmen (more valuable than Stretch Forwards, but you could also use them) and 1-2 Rim-Running Athletic Big/Traditional Big.

###

most_used = lineup_final %>%
  group_by(Tm) %>%
  top_n(1, Poss)
