

# Load packages and data

if(!require(pacman)) install.packages("pacman")

pacman::p_load(dplyr, lubridate, ggplot2, egg)

options(scipen = 999)

test.level.data <- read.csv("EMCAT test level data clean.csv")
item.level.data <- read.csv("EMCAT item level data clean.csv")
  
cn <- read.csv("completed notifications.csv") # bar chart plotting data - CAT notifications sent per person and CAT assessments completed

# Count the number of responses per participant, during the test period

plyr::count(dplyr::filter(test.level.data, test_id == 'CAT'), "study_id") %>% 
  arrange("ID")

# Calculate time between notification and response

test.level.data$created_time <- dmy_hms(test.level.data$created_time)
test.level.data$updated_time <- dmy_hms(test.level.data$updated_time)

test.level.data$notification.to.response.seconds <- test.level.data$updated_time - test.level.data$created_time

# In seconds
dseconds(as.numeric(test.level.data$notification.to.response.seconds))

# In hours, for plotting
df <- as.numeric(test.level.data$notification.to.response.seconds)/60/60 
df <- as.data.frame(df)

plot24 <- ggplot(df, aes(x = df)) +
  geom_density(fill = "navy", alpha = 0.3) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0,24,1), limits = c(0,24), expand = c(0, 0.5)) +
  scale_y_continuous(expand = c(0, 0)) +
  ylab("Denisty") +
  xlab("Hours between notification and response")


plot208 <- ggplot(df, aes(x = df)) +
  geom_density(fill = "navy", alpha = 0.3) +
  theme_minimal() +
  ylab("Denisty") +
  xlab("Hours between notification and response") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.background = element_rect(color = "black", size = 1)) +
  ylim(0,0.4) +
  #geom_segment(x = -4, xend = -4, y = -0.01, yend = 0.4, color = "thistle3") +
  #geom_segment(x = 24, xend = 24, y = -0.01, yend = 0.4, color = "thistle3") +
  #geom_segment(x = -4, xend = 24, y = 0.4, yend = 0.4, color = "thistle3") +
  #geom_segment(x = -4, xend = 24, y = -0.01, yend = -0.01, color = "thistle3") +
  theme(axis.title = element_text(size = 9))


plot24 +
  annotation_custom(
    ggplotGrob(plot208),
    xmin = 10, xmax = 24, ymin = 0.2, ymax = 0.42
  )


# Calculate median response time
quantile(as.numeric(test.level.data$notification.to.response.seconds)) %>% seconds_to_period()

# Bar plot
cn$CAT.notifications.sent <- NULL

long <- tidyr::pivot_longer(cn, 
                            cols = c( "CAT.assessments.completed", "CAT.assessments.not.completed"),
                            names_to = "type")


long$Study.ID <- factor(long$Study.ID,
                        levels = c("spare3", # Empty rows to separate bars
                                   "spare4",
                                   "X1",
                                   "X4",
                                   "X7",
                                   "X11",
                                   "X14",
                                   "X17",
                                   "Y1",
                                   "Y4",
                                   "Y7",
                                   "Y11",
                                   "Y14",
                                   "Y17",
                                   "spare",
                                   "spare5",
                                   "spare6",
                                   "X2",
                                   "X5",
                                   "X8",
                                   "X10",
                                   "X12",
                                   "X15",
                                   "X18",
                                   "X20",
                                   "Y2",
                                   "Y5",
                                   "Y8",
                                   "Y10",
                                   "Y15",
                                   "Y18",
                                   "Y20",
                                   "spare2",
                                   "spare7",
                                   "spare8",
                                   "X3",
                                   "X6",
                                   "X9",
                                   "X13",
                                   "X16",
                                   "X19",
                                   "Y3",
                                   "Y6",
                                   "Y9",
                                   "Y13",
                                   "Y16",
                                   "Y19"))


long$type <- factor(long$type,
                    levels = c("CAT.assessments.completed", "CAT.assessments.not.completed"))


ggplot(long, aes(x = forcats::fct_rev(Study.ID), y = value,  fill = forcats::fct_rev(type))) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.y = element_blank()) +
  xlab("Each participant") +
  ylab("Number of assessments") +
  scale_fill_manual(values = c("#882255", "#69b3a2"),
                    labels = c("Missed assessments", "Completed assessments"),
                    guide = guide_legend(reverse = TRUE),
                    name = "") +
  theme(legend.position = c(0.858, 0.15)) +
  annotate("text", label = "Thrice weekly", x = 14, y = 15) +
  annotate("text", label = "Daily", x = 32, y = 6) +
  annotate("text", label = "Thrice daily", x = 47, y = 12.5) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )

# Two example time series plots - participants B19 and C5

X19 <- test.level.data %>% 
  dplyr::filter(test_id == "CAT" & study_id == "X19")

Y5 <- test.level.data %>% 
  dplyr::filter(test_id == "CAT" & study_id == "Y5")

# No CIs

simpletimeseriesX19 <- ggplot(X19, aes(x = updated_time, y = theta)) +
  geom_line(colour = "darkmagenta", size = 1) +
  geom_point(colour = "darkmagenta", size = 1) +
  ylab(expression(theta)) +
  xlab("Date") +
  theme_minimal() +
  theme(text = element_text(size = 14)) +
  scale_y_continuous(limits = c(-3, 2))


simpletimeseriesY5 <- ggplot(Y5, aes(x = updated_time, y = theta)) +
  geom_line(colour = "firebrick", size = 1) +
  geom_point(colour = "firebrick", size = 1) +
  ylab(expression(theta)) +
  xlab("Date") +
  theme_minimal() +
  theme(text = element_text(size = 14)) +
  scale_y_continuous(limits = c(-3, 2))

simpletimeseriesX19
simpletimeseriesY5

ggarrange(simpletimeseriesX19, simpletimeseriesY5, nrow = 1)


# Shaded CIs

ggplot(X19, aes(x = updated_time, y = theta)) +
  geom_line(colour = "darkmagenta", size = 1) +
  geom_point(colour = "darkmagenta", size = 1) +
  ylab(expression(theta)) +
  xlab("Date") +
  theme_minimal() +
  theme(text = element_text(size = 14)) +
  geom_ribbon(aes(ymin=theta_lower_confidence_interval, ymax=theta_upper_confidence_interval), 
              fill = "darkmagenta",
              alpha=0.2)


# Variable shaded CIs

X19$plus95 <- X19$theta + 1.959964*X19$sem # These valuses correspond to the z values for each confidence level
X19$plus90 <- X19$theta + 1.644854*X19$sem
X19$plus85 <- X19$theta + 1.439531*X19$sem
X19$plus80 <- X19$theta + 1.281551*X19$sem
X19$plus75 <- X19$theta + 1.150350*X19$sem
X19$plus70 <- X19$theta + 1.036433*X19$sem
X19$plus65 <- X19$theta + 0.934589*X19$sem
X19$plus60 <- X19$theta + 0.841621*X19$sem
X19$plus55 <- X19$theta + 0.755415*X19$sem
X19$plus50 <- X19$theta + 0.674490*X19$sem
X19$plus45 <- X19$theta + 0.597760*X19$sem
X19$plus40 <- X19$theta + 0.524401*X19$sem
X19$plus35 <- X19$theta + 0.453762*X19$sem
X19$plus30 <- X19$theta + 0.385321*X19$sem
X19$plus25 <- X19$theta + 0.318640*X19$sem
X19$plus20 <- X19$theta + 0.253346*X19$sem
X19$plus15 <- X19$theta + 0.189118*X19$sem
X19$plus10 <- X19$theta + 0.125661*X19$sem

X19$minus95 <- X19$theta - 1.959964*X19$sem
X19$minus90 <- X19$theta - 1.644854*X19$sem
X19$minus85 <- X19$theta - 1.439531*X19$sem
X19$minus80 <- X19$theta - 1.281551*X19$sem
X19$minus75 <- X19$theta - 1.150350*X19$sem
X19$minus70 <- X19$theta - 1.036433*X19$sem
X19$minus65 <- X19$theta - 0.934589*X19$sem
X19$minus60 <- X19$theta - 0.841621*X19$sem
X19$minus55 <- X19$theta - 0.755415*X19$sem
X19$minus50 <- X19$theta - 0.674490*X19$sem
X19$minus45 <- X19$theta - 0.597760*X19$sem
X19$minus40 <- X19$theta - 0.524401*X19$sem
X19$minus35 <- X19$theta - 0.453762*X19$sem
X19$minus30 <- X19$theta - 0.385321*X19$sem
X19$minus25 <- X19$theta - 0.318640*X19$sem
X19$minus20 <- X19$theta - 0.253346*X19$sem
X19$minus15 <- X19$theta - 0.189118*X19$sem
X19$minus10 <- X19$theta - 0.125661*X19$sem

ggplot(X19, aes(x = updated_time, y = theta)) +
  ylab(expression(theta)) +
  xlab("Date") +
  theme_minimal() +
  theme(text = element_text(size = 14)) +
  geom_ribbon(aes(ymin=minus10, ymax=plus10), fill = "darkmagenta", alpha=0.1) +
  geom_ribbon(aes(ymin=minus15, ymax=plus15), fill = "darkmagenta", alpha=0.1) +
  geom_ribbon(aes(ymin=minus20, ymax=plus20), fill = "darkmagenta", alpha=0.1) +
  geom_ribbon(aes(ymin=minus25, ymax=plus25), fill = "darkmagenta", alpha=0.1) +
  geom_ribbon(aes(ymin=minus30, ymax=plus30), fill = "darkmagenta", alpha=0.1) +
  geom_ribbon(aes(ymin=minus35, ymax=plus35), fill = "darkmagenta", alpha=0.1) +
  geom_ribbon(aes(ymin=minus40, ymax=plus40), fill = "darkmagenta", alpha=0.1) +
  geom_ribbon(aes(ymin=minus45, ymax=plus45), fill = "darkmagenta", alpha=0.1) +
  geom_ribbon(aes(ymin=minus50, ymax=plus50), fill = "darkmagenta", alpha=0.1) +
  geom_ribbon(aes(ymin=minus55, ymax=plus55), fill = "darkmagenta", alpha=0.1) +
  geom_ribbon(aes(ymin=minus60, ymax=plus60), fill = "darkmagenta", alpha=0.1) +
  geom_ribbon(aes(ymin=minus65, ymax=plus65), fill = "darkmagenta", alpha=0.1) +
  geom_ribbon(aes(ymin=minus70, ymax=plus70), fill = "darkmagenta", alpha=0.1) +
  geom_ribbon(aes(ymin=minus75, ymax=plus75), fill = "darkmagenta", alpha=0.1) +
  geom_ribbon(aes(ymin=minus80, ymax=plus80), fill = "darkmagenta", alpha=0.1) +
  geom_ribbon(aes(ymin=minus85, ymax=plus85), fill = "darkmagenta", alpha=0.1) +
  geom_ribbon(aes(ymin=minus90, ymax=plus90), fill = "darkmagenta", alpha=0.1) +
  geom_ribbon(aes(ymin=minus95, ymax=plus95), fill = "darkmagenta", alpha=0.1)


# Time series smoothing

ggplot(X19, aes(x = updated_time, y = theta)) +
  geom_point(colour = "darkmagenta", size = 1.5) +
  ylab(expression(theta)) +
  xlab("Date") +
  theme_minimal() +
  theme(text = element_text(size = 14)) +
  scale_y_continuous(limits = c(-2.5, 1.5), breaks = seq(-2.5,1.5,1)) +
  stat_smooth(colour = "darkmagenta", fill = "darkmagenta")

# C5 against other patients with TBOA

# First, place all patients with TBOA onto a common time scale - days in study

OA <- c("Y1", "Y2", "Y3", "Y4", "Y5","Y6", "Y7", "Y8", "Y9", "Y10",
        "X1", "X2", "X3", "X4", "X5","X6", "X7", "X8", "X9", "X10")

all.OA <- test.level.data %>%
  dplyr::filter(condition == "TBOA" & test_id == "CAT")

all.OA$time.entered.study <- NA

min.times <- rep(NA, 20)

for (i in c(1:20)){
  
  id <- OA[i]
  df <- all.OA %>% dplyr::filter(study_id == id)
  min <- min(df$created_time)
  
  min.times[i] <- as.character(min)
  
}


for(i in 1:20){
  
  id <- OA[i]
  all.OA[all.OA$study_id == id, "time.entered.study"] <- min.times[i]
  
}


for(i in 1:length(all.OA$updated_time)){
  
  all.OA[i,"time.in.study"] <- ymd_hms(all.OA[i, "updated_time"]) - ymd_hms(all.OA[i, "time.entered.study"])
  
}

# Then plot

ggplot(all.OA, aes(x = time.in.study/24, y = theta)) +
  geom_line(size = 1, aes(colour = study_id)) +
  geom_point(size = 1, aes(colour = study_id)) +
  ylab(expression(theta)) +
  xlab("Days into study") +
  theme_minimal() +
  theme(text = element_text(size = 14)) +
  xlim(0,80)

# Now change colours to highlight C5

cols <- c("grey88", "grey88", "grey88", "grey88", "grey88", "grey88", "grey88", "grey88",
           "grey88", "grey88", "grey88", "grey88", "grey88", "grey88", "grey88", "grey88", 
           "grey88", "grey88", "grey88", "red")

all.OA %>% 
  mutate(study_id = factor(study_id)) %>% 
  mutate(study_id = relevel(study_id, ref = "Y5")) %>% 
  mutate(study_id = factor(study_id, levels = rev(levels(study_id)))) %>%
  ggplot(aes(x = time.in.study/24, y = theta)) + 
  geom_line(aes(color=study_id), size = 1, show.legend = FALSE) +
  scale_color_manual(values = cols) +
  ylab(expression(theta)) +
  xlab("Days into study") +
  theme_minimal() +
  theme(text = element_text(size = 14)) +
  xlim(0,80)


# Compare full-length PEM scores to CAT scores on the same day

pemandcat <- test.level.data %>% 
  dplyr::filter(test_id == "PEM" | test_id == "CAT")

pemandcat$updated_time <- as.Date(pemandcat$updated_time)

ids <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10",
         "X11", "X12", "X13", "X14", "X15", "X16", "X17", "X18", "X19", "X20",
         "Y1", "Y2", "Y3", "Y4", "Y5", "Y6", "Y7", "Y8", "Y9", "Y10",
         "Y11", "Y12", "Y13", "Y14", "Y15", "Y16", "Y17", "Y18", "Y19", "Y20")

list <- rep(list(NA), 40)

for(i in 1:40){
  
  PEMscores <- pemandcat %>% # Select the theta and date of each PEM assessment
    dplyr::filter(study_id == ids[i]) %>% 
    dplyr::filter(test_id == "PEM") %>%
    dplyr::select("study_id", "updated_time", "theta")
  
  dates <- pemandcat %>% # Pull the dates that PEM was completed
    dplyr::filter(study_id == ids[i]) %>% 
    dplyr::filter(test_id == "PEM") %>% 
    dplyr::pull(updated_time) 
  
  CATscores <- pemandcat %>% # For the dates where PEM was completed, pull the CAT scores
    dplyr::filter(study_id == ids[i]) %>%
    dplyr::filter(test_id == "CAT") %>%
    dplyr::filter(updated_time %in% dates) %>%
    dplyr::select("study_id", "updated_time", "theta")
  
  # Now need to merge these by updatedTime
  merged <- merge(PEMscores, CATscores, by = c("study_id", "updated_time"), all = T)
  colnames(merged) <- c("ID", "Date", "PEM", "CAT")
  
  list[[i]] <- merged
  
}

compare.PEM.CAT.df <- bind_rows(list, .id = "ID")
compare <- compare.PEM.CAT.df[- which(is.na(compare.PEM.CAT.df$CAT)), ] %>% na.omit()

dim(compare) # 34 paired CAT and PEM measurements 

devtools::install_github("MrConradHarrison/cleftqCATsim")
library(cleftqCATsim)

BAtable <- BAtable(compare$PEM, compare$CAT)
BAplot(BAtable)

mean(BAtable$Difference) # Mean error of 0.006 logits


# Plot full length PEM scores and CAT scores from B19

X19incPEM <- test.level.data %>%
  dplyr::filter(test_id == "CAT" | test_id == "PEM") %>%
  dplyr::filter(study_id == "X19")


ggplot(X19incPEM, aes(x = updated_time)) +
  geom_point(aes(y = theta, group = test_id, colour = test_id), size = 2) +
  scale_colour_manual(values = c("grey", "red"), 
                      labels = c("EMCAT", "Full-length questionnaire"),
                      name = "") +
  ylab(expression(theta)) +
  xlab("Date") +
  theme_minimal() +
  theme(text = element_text(size = 14)) +
  geom_errorbar(aes(ymin = theta - 1.96*sem, ymax = theta + 1.96*sem, color = test_id), size = 1) +
  theme(legend.position = c(0.75, 0.87))


# Time taken to complete CAT assessments

item.level.data

CAT.times <- item.level.data %>%
  dplyr::filter(test_id == "CAT") %>%
  group_by(nom.id.string) %>%
  summarise(sum = sum(time_taken))

quantile(CAT.times$sum)/1000


# Number of items used per CAT assessment

number.items <- item.level.data %>%
  dplyr::filter(test_id == "CAT") %>%
  dplyr::group_by(nom.id.string) %>%
  plyr::count("nom.id.string")

quantile(number.items$freq)


# Which items were used in the CAT assessments


item.level.data %>% dplyr::filter(test_id == "CAT") %>% pull(item_id) %>% unique()

items.used <- item.level.data %>%
  dplyr::filter(test_id == "CAT") %>%
  plyr::count("item_id")


items <- seq(1,11,1)
freq <- c(0, 0, items.used[3:9,2], items.used[1:2,2])

items.used.df <- cbind(items, freq) %>% as.data.frame

ggplot(items.used.df, aes(x = factor(items), y = freq)) +
  geom_bar(stat = "identity", fill = "thistle3", colour = "black") +
  theme_minimal() +
  ylab("Frequency") +
  scale_x_discrete(name = "Item number",
                   labels = as.character(seq(1,11,1))) +
  theme(text = element_text(size = 14))  


# Time taken to complete PEM

PEM.times <- item.level.data %>%
  dplyr::filter(test_id == "PEM") %>%
  group_by(nom.id.string) %>%
  summarise(sum = sum(time_taken))

quantile(PEM.times$sum)/1000


# UES scores, factor scores, and specific item responses for each person


ues1 <- item.level.data %>%
  dplyr::filter(test_id == "UES") %>%
  dplyr::select("nom.id.string", "item_id", "score") %>%
  tidyr::pivot_wider(names_from = item_id, values_from = score) 

colnames.ues <- rep(NA, 32)  

for(i in 1:31){
  
  colnames.ues[i+1] <- paste0("Item_", i)
  
}

colnames.ues[1] <- "Nom"
colnames(ues1) <- colnames.ues

# Subscale scores

ues.FA <- ues1 %>%
  dplyr::select("Nom", "Item_1":"Item_7")

ues.PU <- ues1 %>%
  dplyr::select("Nom", "Item_8":"Item_15")

ues.AE <- ues1 %>%
  dplyr::select("Nom", "Item_16":"Item_20")

ues.RW <- ues1 %>%
  dplyr::select("Nom", "Item_21":"Item_30")

# Inspect missing data

rowSums(is.na(ues.FA))
rowSums(is.na(ues.PU))
rowSums(is.na(ues.AE))
rowSums(is.na(ues.RW))

FA.scores <- rowMeans(ues.FA[,2:8])

PU.scores <- rowMeans(ues.PU[,2:9])

AE.scores <- rowMeans(ues.AE[,2:6], na.rm = T)

ues.RW <- ues.RW[-c(6,9),] # Excluded these two non-responders

RW.scores <- rowMeans(ues.RW[,2:11])

quantile(FA.scores)
quantile(PU.scores)
quantile(AE.scores)
quantile(RW.scores, na.rm = T)


# Does PU score or the "was taxing" item score vary with response frequency?

ues2 <- item.level.data %>%
  dplyr::filter(test_id == "UES") %>%
  dplyr::select("study_id", "nom.id.string", "item_id", "score") %>%
  tidyr::pivot_wider(names_from = item_id, values_from = score) 


thrice.daily <- c("X1", "X4", "X7", "X11", "X14", "X17", "Y1", "Y4", "Y7", "Y11", "Y14", "Y17")
daily <-c("X2","X5", "X8","X10","X12", "X15","X18","X20","Y2","Y5","Y8","Y10","Y15","Y18","Y20")
thrice.weekly <- c("X3","X6","X9","X13","X16","X19","Y3","Y6","Y9","Y13","Y16","Y19")


ues2$freq <- NA

for (i in 1:nrow(ues2)){
  
  if (ues2[i,"study_id"] %in% thrice.daily){
    ues2[i, "freq"] <- "thrice.daily"
  }
  
}


for (i in 1:nrow(ues2)){
  
  if (ues2[i,"study_id"] %in% daily){
    ues2[i, "freq"] <- "daily"
  }
  
}

for (i in 1:nrow(ues2)){
  
  if (ues2[i,"study_id"] %in% thrice.weekly){
    ues2[i, "freq"] <- "thrice.weekly"
  }
  
}


colnames.ues2 <- rep(NA, 34)  

for(i in 1:31){
  
  colnames.ues2[i+1] <- paste0("Item_", i)
  
}

colnames.ues2[1] <- "ID"
colnames.ues2[2] <- "nom"
colnames.ues2[34] <- "freq"

colnames(ues2) <- colnames.ues2

ues.PU2 <- ues2 %>%
  dplyr::select("Item_8":"Item_15", "freq")

PU.scores2 <- rowMeans(ues.PU2[,1:8])

table <- cbind(PU.scores2, ues.PU2$freq) %>% as.data.frame()
table$PU.scores2 <- as.numeric(table$PU.scores2)

table %>% dplyr:: filter(V2 == "thrice.daily") %>% pull(PU.scores2) %>% quantile()
table %>% dplyr:: filter(V2 == "daily") %>% pull(PU.scores2) %>% quantile()
table %>% dplyr:: filter(V2 == "thrice.weekly") %>% pull(PU.scores2) %>% quantile()

ues.item12 <- ues.PU2 %>% dplyr::select("Item_12", "freq")

ues.item12



