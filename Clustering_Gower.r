
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(gridExtra))
suppressMessages(library(vcd))
suppressMessages(library(cluster))
suppressMessages(library(Rtsne))

hr <- read.csv("../input/HR_comma_sep.csv", stringsAsFactors = F)
colnames(hr) <- c("satisfaction", "evaluation", "projects", "hours", "tenure", "accident", "left", "promotion", "department", "salary")
hr$accident <- factor(hr$accident)
hr$left <- factor(hr$left)
hr$promotion <- factor(hr$promotion)
hr$salary <- ordered(hr$salary, c("low", "medium", "high"))
hr$department <- factor(hr$department)

set.seed(150555)

train <- sample_frac(hr, 0.7)

gowerd <- daisy(train[,-9], metric = "gower")

clusterfit <- pam(gowerd, diss = T, k = 5)

clusters <- train %>% 
            mutate(cluster = clusterfit$clustering) %>% 
            group_by(cluster) %>% 
            do(allclusters = summary(.))

clusters$allclusters

tsne <- Rtsne(gowerd, is_distance = T)

tsned <- tsne$Y %>%
        data.frame() %>%
        setNames(c("X", "Y")) %>%
        mutate(cluster = factor(clusterfit$clustering), id = row.names(train))

ggplot(aes(x = X, y = Y), data = tsned) + 
        geom_point(aes(color = cluster))

train$id <- row.names(train)

subcluster5_1 = tsned %>%
                filter(X > -65 & X < -45, Y > -5 & Y < 13) %>%
                left_join(train, by = "id")

g1 <- ggplot(subcluster5_1, 
             aes(x = evaluation, y = satisfaction, color = salary)) + 
             geom_jitter() + 
             ggtitle("Medium salary/Low satisfaction")

subcluster5_2 = tsned %>%
                filter(Y < -13, Y > -40, X > 30 ) %>%
                left_join(train, by = "id") 

g2 <- ggplot(subcluster5_2, 
             aes(x = evaluation, y = satisfaction, color = salary)) + 
             geom_jitter() + 
             ggtitle("Low salary/Low satisfaction")

subcluster5_3 = tsned %>%
                filter(Y > -13, Y < 10, X > 25 & X < 35 ) %>%
                left_join(train, by = "id") 

g3 <- ggplot(subcluster5_3, 
             aes(x = evaluation, y = satisfaction, color = salary)) + 
             geom_jitter() + 
             ggtitle("Low salary/High satisfaction")

grid.arrange(g1, g2, g3)

train$EffortRewardImbalance <- "No"
train$EffortRewardImbalance[train$evaluation >= .84 & train$projects >= 5 & train$hours >= 243 & train$tenure >= 4 & train$promotion == 0] <- "Yes"
train$WorkUnderload <- "No"
train$WorkUnderload[train$satisfaction <= .44 & train$evaluation <= .55 & train$projects == 2 & train$hours <= 154 & train$tenure <= 3 & train$promotion == 0] <- "Yes"
train$EffortRewardImbalance <- factor(train$EffortRewardImbalance)
train$WorkUnderload <- factor(train$WorkUnderload)

sieve(Freq ~ 
      WorkUnderload + EffortRewardImbalance | left, 
      data = train, 
      labeling = labeling_values, 
      shade = T, 
      scale = 1)
