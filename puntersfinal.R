#will clear all objects includes hidden objects.
rm(list = ls(all.names = TRUE)) 
#free up memrory and report the memory usage.
gc() 

library(tidyverse) #data cleaning, includes dplyr and ggplot
library(nflfastR) #nfl team colors and logos
library(ggrepel) # better labels
library(ggimage) # plot images
library(gt) # beautiful tables
library(plotly) # plot 3D
library(lubridate) # time
library(knitr) #tables
library(scales) #visualization
library(pracma) #linspace
library(stargazer) #Regression Table
library(GGally) #multicollinearity test
library(glmnet) #Ridge Regression
library(plotmo) # for plot_glmnet
library(caret) #confusion matrix
library(paletteer) #Colors
library(randomForest) #Random Forest Algo
library(cvms) #plot confusion Matrix

#gg_field from Marschall Furman (2020)
source("C:/Users/owner/Documents/NFL Evaluation/code/gg_field.R")

#Plays
plays <- read.csv("C:/Users/owner/Documents/NFL Evaluation/databowl/plays.csv")
plays <- plays %>% 
  mutate(scoredif = abs(preSnapHomeScore - preSnapVisitorScore)) %>%
  select(-c(returnerId, kickBlockerId, penaltyJerseyNumbers, passResult)) %>%
  rename(nflId = kickerId, PlayType = specialTeamsPlayType, Result = specialTeamsResult)
#PFF Scouting Data
scout <- read.csv("C:/Users/owner/Documents/NFL Evaluation/databowl/PFFScoutingData.csv")
scout <- scout %>% 
  select(-c(missedTackler:vises))  
#Games
games <- read.csv("C:/Users/owner/Documents/NFL Evaluation/databowl/games.csv")
games <- games %>% 
  rename(HomeTm = homeTeamAbbr, AwayTm = visitorTeamAbbr)
#Players
players <- read.csv("C:/Users/owner/Documents/NFL Evaluation/databowl/players.csv")
players <- players %>% 
  select(-c(birthDate, collegeName))
#Tracking Data
#tracking2018 <- read.csv("C:/Users/owner/Documents/NFL Evaluation/databowl/tracking2018.csv")
#tracking2019 <- read.csv("C:/Users/owner/Documents/NFL Evaluation/databowl/tracking2019.csv")
#tracking2020 <- read.csv("C:/Users/owner/Documents/NFL Evaluation/databowl/tracking2020.csv")
#track2018 <- tracking2018[tracking2018$team=="football",]
#track2019 <- tracking2019[tracking2019$team=="football",]
#track2020 <- tracking2020[tracking2020$team=="football",]
#track0 <- rbind(track2018, track2019)
#track <- rbind(track0, track2020)
#write.csv(track, file = "C:\\Users\\owner\\Documents\\NFL Evaluation\\databowl\\track.csv", row.names=FALSE)
track <- read.csv("C:/Users/owner/Documents/NFL Evaluation/databowl/track.csv")
track <- track %>% 
  select(-c(o, dir, nflId, jerseyNumber, displayName, team, position) )

#Merge Kaggle Data
data <- plays %>%
  left_join(scout, by = c("gameId", "playId")) %>%
  left_join(games, by = c("gameId"))  %>%
  left_join(players, by = c("nflId")) %>%
  filter(PlayType=="Punt")  %>%
  mutate(#A numeric value that helps with data manipulation later
    frames = round(10*hangTime),
    #Yds from EZ (ball snap)
    ydsEZ = ifelse(possessionTeam==yardlineSide & yardlineNumber<=49, 100 - yardlineNumber, 
                   ifelse(possessionTeam!=yardlineSide & yardlineNumber>=35, yardlineNumber, 
                          ifelse(yardlineNumber==50, yardlineNumber, 0))),
    #Adjusted Punt Distance (for Touchbacks)
    apLength = ifelse(playResult=="Touchback", kickLength-20, kickLength),
    #Punt Long (< 65 yards from the EZ)
    pL = ifelse(possessionTeam==yardlineSide & yardlineNumber<35, 1, 0),
    #Punt Medium (50-65 yards from the EZ)
    pM = ifelse(possessionTeam==yardlineSide & yardlineNumber>=35 | yardlineNumber==50, 1, 0),
    #Punt Short (35-50 yards from the EZ)
    pS = ifelse(possessionTeam!=yardlineSide & yardlineNumber>=35 | yardlineNumber==50, 1, 0),
    #Aussie Style Punt
    kickaussie = ifelse(PlayType=="Punt" & kickType=="A", 1, 0),
    #Normal Style Punt
    kicknormal = ifelse(PlayType=="Punt" & kickType=="N", 1, 0))

#Remove all but the merge and track files
rm(plays, scout, games, players)
#height

#nflfastr
seasons <- 2018:2020
nfl <- nflfastR::load_pbp(seasons)
nfl <- nfl %>% 
  rename(gameId = old_game_id, playId = play_id) %>%
  mutate(gameId = as.integer(gameId))

#Merge w/ Kaggle data
data <- nfl %>%
  filter(special==1) %>%
  select(gameId, playId, punter_player_id, ep, epa) %>%
  right_join(data, by = c("gameId", "playId")) %>%
  select(-c(playDescription, gameClock))
#rm(nfl)

track2 <- data %>%
  select(gameId, playId, PlayType, nflId, possessionTeam, displayName, kickType, HomeTm, pL, pM, pS, hangTime, frames) %>%
  right_join(track, by = c("gameId", "playId")) %>%
  mutate(id = row_number(),
         datetime = as.POSIXct(time, format="%Y-%m-%dT%H:%M:%OS")) %>%
  left_join(teams_colors_logos, by = c('HomeTm' = 'team_abbr'))

tracksnap <- track2 %>%
  group_by(gameId, playId) %>%
  filter(event=="ball_snap") %>%
  mutate(xsnap = x) %>%
  select(gameId, playId, xsnap)

trackpunt <- track2 %>%
  group_by(gameId, playId) %>%
  mutate(startid = row_number()) %>%
  filter(event=="punt") %>%
  mutate(endid = frames + startid,
         xpunt = x) %>%
  select(gameId, playId, startid, endid, xpunt) %>%
  right_join(tracksnap, by = c('gameId', 'playId')) %>%
  right_join(track2, by = c('gameId', 'playId')) %>%
  mutate(id = row_number()) %>%
  filter(id>=startid, id<=endid) %>%
  mutate(xland = last(x),
         z0 = 0, 
         sumd = sum(dis),
         t = round(as.numeric(difftime(datetime, first(datetime), units="secs")), 1),
         vy = 0.5 * hangTime * 32.17405,
         vx = (3 * sumd) / hangTime,
         iv = sqrt(vy**2 + vx**2),
         rad = atan(vy / vx),
         xi = vx * t,
         z = ((2 + tan(rad)*xi - ((32.17405/2)*xi**2)/(iv**2 * (cos(rad))**2)))/3)

rm(tracksnap)

plotplayer <- trackpunt %>%
  filter(displayName=="Cameron Johnston", possessionTeam==HomeTm, xsnap>45, xsnap<=60, pM==1)

pp0 <- plotplayer %>%
  ggplot(aes(x=x, y=y, z=z)) +
  gg_field(endzone_color = plotplayer$team_color, sideline_color = plotplayer$team_color2) +
  geom_image(aes(x=60, y=53.33/2, image = team_logo_espn),size=0.13) +
  geom_point(col="#654321", cex=1.5)

pp0

plotplayer <- trackpunt %>%
  filter(displayName=="Cameron Johnston", possessionTeam==HomeTm, xsnap>45, xsnap<=60, pM==1)

#Endzone
x0 = linspace(0.25, 9.75, n = 9)
x1 = linspace(110.25, 119.75, n = 9)
y0 = linspace(0, 53.33, n = 53.33)
z_EZ = matrix(0, ncol = 9, nrow = 53.33)

#Sidelines
x_side = linspace(0.25, 119.75, n = 120)
y_side0 = linspace(-5.5, -0.5, n = 5)
y_side1 = linspace(53.83, 58.83, n = 5)
z_side = matrix(0, ncol = 120, nrow = 5)

#Field
z_field = matrix(0, ncol = 5, nrow = 53.33)
x_field0 = linspace(10.25, 14.90, n = 5)
x_field1 = linspace(15.10, 19.90, n = 5)
x_field2 = linspace(20.10, 24.90, n = 5)
x_field3 = linspace(25.10, 29.90, n = 5)
x_field4 = linspace(30.10, 34.90, n = 5)
x_field5 = linspace(35.10, 39.90, n = 5)
x_field6 = linspace(40.10, 44.90, n = 5)
x_field7 = linspace(45.10, 49.90, n = 5)
x_field8 = linspace(50.10, 54.90, n = 5)
x_field9 = linspace(55.10, 59.75, n = 5)
x_field10 = linspace(60.25, 64.90, n = 5)
x_field11 = linspace(65.10, 69.90, n = 5)
x_field12 = linspace(70.10, 74.90, n = 5)
x_field13 = linspace(75.10, 79.90, n = 5)
x_field14 = linspace(80.10, 84.90, n = 5)
x_field15 = linspace(85.10, 89.90, n = 5)
x_field16 = linspace(90.10, 94.90, n = 5)
x_field17 = linspace(95.10, 99.90, n = 5)
x_field18 = linspace(100.10, 104.90, n = 5)
x_field19 = linspace(105.10, 109.75, n = 5)

pp1 <- plot_ly (data = plotplayer) %>% 
  add_trace(x = ~x, y = ~y, z = ~z, type = 'scatter3d', mode = 'lines', opacity = 1, color="#654321", colors="#654321")  %>%
  #Endzones
  add_surface(x = ~x0, y = ~y0, z = z_EZ, opacity = 1, colorscale = "Greens") %>%
  add_surface(x = ~x1, y = ~y0, z = z_EZ, opacity = 1, colorscale = "Greens") %>%
  #Sideline
  add_surface(x = ~x_side, y = ~y_side0, z = z_side, opacity = 1, colorscale = "Greens") %>%
  add_surface(x = ~x_side, y = ~y_side1, z = z_side, opacity = 1, colorscale = "Greens") %>%
  #Field
  add_surface(x = ~x_field0, y = ~y0, z = z_field, opacity = 1, colorscale = "Greens") %>%
  add_surface(x = ~x_field1, y = ~y0, z = z_field, opacity = 1, colorscale = "Greens") %>%
  add_surface(x = ~x_field2, y = ~y0, z = z_field, opacity = 1, colorscale = "Greens") %>%
  add_surface(x = ~x_field3, y = ~y0, z = z_field, opacity = 1, colorscale = "Greens") %>%
  add_surface(x = ~x_field4, y = ~y0, z = z_field, opacity = 1, colorscale = "Greens") %>%
  add_surface(x = ~x_field5, y = ~y0, z = z_field, opacity = 1, colorscale = "Greens") %>%
  add_surface(x = ~x_field6, y = ~y0, z = z_field, opacity = 1, colorscale = "Greens") %>%
  add_surface(x = ~x_field7, y = ~y0, z = z_field, opacity = 1, colorscale = "Greens") %>%
  add_surface(x = ~x_field8, y = ~y0, z = z_field, opacity = 1, colorscale = "Greens") %>%
  add_surface(x = ~x_field9, y = ~y0, z = z_field, opacity = 1, colorscale = "Greens") %>%
  add_surface(x = ~x_field10, y = ~y0, z = z_field, opacity = 1, colorscale = "Greens") %>%
  add_surface(x = ~x_field11, y = ~y0, z = z_field, opacity = 1, colorscale = "Greens") %>%
  add_surface(x = ~x_field12, y = ~y0, z = z_field, opacity = 1, colorscale = "Greens") %>%
  add_surface(x = ~x_field13, y = ~y0, z = z_field, opacity = 1, colorscale = "Greens") %>%
  add_surface(x = ~x_field14, y = ~y0, z = z_field, opacity = 1, colorscale = "Greens") %>%
  add_surface(x = ~x_field15, y = ~y0, z = z_field, opacity = 1, colorscale = "Greens") %>%
  add_surface(x = ~x_field16, y = ~y0, z = z_field, opacity = 1, colorscale = "Greens") %>%
  add_surface(x = ~x_field17, y = ~y0, z = z_field, opacity = 1, colorscale = "Greens") %>%
  add_surface(x = ~x_field18, y = ~y0, z = z_field, opacity = 1, colorscale = "Greens") %>%
  add_surface(x = ~x_field19, y = ~y0, z = z_field, opacity = 1, colorscale = "Greens") %>%
  layout(title = "Punts: 50-65 Yards From Endzone",
         scene = list(xaxis = list(title = 'Field Length (Yds)'),
                      yaxis = list(title = 'Field Width (Yds)'),
                      zaxis = list(title = 'Height (Yds)')))

pp2 <- hide_guides(pp1)

pp2

trackfinal <- trackpunt %>% 
  filter(event=="punt") %>%
  select(gameId, playId, xpunt, xsnap, x, y, iv, rad, sumd, xland, datetime)

data <- data %>%
  right_join(trackfinal, by = c("gameId", "playId"))

punt <- data %>% 
  filter(playResult!="Blocked Punt", !is.na(gameId), ydsEZ!=0) %>%
  mutate(ydsEZ = as.numeric(ydsEZ),
         #Launch Angle
         launch = (rad)*(180/pi)) %>%
         drop_na(apLength)

fig <- plot_ly(alpha = 0.6) %>%
  add_histogram(x = ~punt$apLength, name="Actual Yards Per Punt") %>%
  add_histogram(x = ~punt$playResult, name="Net Yards Per Punt") %>%
  layout(barmode = "stacked",
         title = "Punts (2018-2020)",
         xaxis = list(title = "Distance of Punt"),
         yaxis = list(title = "Number of Punts"))

fig

#The Model
Punt_model <- lm(apLength ~ ydsEZ + iv + launch, data = punt)
stargazer(Punt_model, type="text", intercept.bottom = FALSE,
          dep.var.labels=c("Actual Punt Distance"), 
          title="OLS Regression Output",
          covariate.labels=c("Constant", "Yards from EZ", "Initial Velocity", "Launch Angle"))
cat("\n")

#Multicollinearity
corr <- punt %>%
  select(ydsEZ, iv, launch)
ggpairs(corr)

#Ridge Regression
set.seed(123)
model <- punt %>%
  select(apLength, ydsEZ, iv, launch)

#define predictor and response variables
y <- model$apLength
x <- model %>% select(ydsEZ, iv, launch) %>% data.matrix()
lambdas <- 10^seq(3, -2, by = -.1)

#fit model
rmodel <- glmnet(x, y, alpha = 0)
#find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 0)
plot(cv_model)
#find optimal valuethat minimizes MSE
best_lambda <- cv_model$lambda.min
#produce Ridge trace plot
plot_glmnet(rmodel, xvar= "lambda")
#find coefficients of best model
best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
#use best model to make predictions
y_predicted <- predict(rmodel, s = best_lambda, newx = x)

#find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
#find R-Squared
rsq <- round((1 - sse / sst), digits = 3)

#Summary
coef(best_model)
cat("\n")
cat('R Squared: ', rsq)

ppy <- data.frame(punt, y_predicted)

ML <- ppy %>%
  select(c("Result", "ydsEZ", "iv", "launch")) %>%
  mutate(Result = factor(Result)) %>%
  filter(Result != "Muffed", Result != "Out of Bounds") %>% 
  droplevels()

set.seed(222)
#random split
ran <- sample(1:nrow(ML),0.80 * nrow(ML))
##training dataset extracted
rf_train <- ML[ran,]
##test dataset extracted
rf_test <- ML[-ran,]

rf <- randomForest(Result~., data=rf_train, ntree=500, mtry=2, proximity=TRUE) 
print(rf)

prf <- predict(rf, rf_test[,c(2:4)])
confusionMatrix(prf, rf_test$Result)

cM <- confusion_matrix(targets = rf_test$Result,
                       predictions = prf)

plot_confusion_matrix(cM, palette = "Greens")

salary <- read.csv("C:/Users/owner/Documents/NFL Evaluation/databowl/salaries.csv")
salary <- salary %>% 
  rename(Name = ï..Name) %>% 
  select(c(Name, Cap))

pyoe <- ppy %>%
  group_by(punter_player_id, displayName, possessionTeam) %>%
  rename(Name = displayName, Team = possessionTeam) %>%
  mutate(PY = apLength,
         ePY = s1,
         short = pS * (apLength - s1),
         med = pM * (apLength - s1),
         long = pL * (apLength - s1)) %>%
  summarize(
    Punts = n(),
    PY = sum(apLength, na.rm=T)/Punts,
    ePY = sum(s1, na.rm=T)/Punts, 
    PYOE = PY - ePY,
    Long = sum(long)/sum(pL),
    Med = sum(med)/sum(pM),
    Short = sum(short)/sum(pS)
  ) %>%
  filter(Punts >= 100) %>%
  left_join(salary, by = c('Name')) %>%
  arrange(-PYOE) %>%
  ungroup() %>%
  select(-c(punter_player_id)) %>%
  mutate(CapRank = rank(-as.numeric(Cap)),
         Rank = as.numeric(paste0(row_number()))) %>%
  select(-c(Cap)) %>%
  slice(1:10)

gt_pyoe <- gt(pyoe) %>%
  tab_header(title = md("**Punt Yards Over Expected: 2018 - 2020**")) %>%
  cols_move_to_start(columns = c(Rank)) %>%
  cols_label(
    Name = "Punter",
    Team = "Team",
    Punts = "Punts",
    PY = "PY",
    ePY = "ePY",
    PYOE = "PYOE",
    Long = "Long",
    Med = "Med",
    Short = "Short",
    CapRank = "ACNR"
  ) %>%
  fmt_number(columns = c(PY, ePY, PYOE, Long, Med, Short), decimals = 2) %>%
  cols_align(align = "center", columns = c(Rank, Name, Team, PY, ePY, PYOE, Long, Med, Short, CapRank)) %>%
  tab_style(style = cell_text(size = "large"), locations = cells_title(groups = "title")) %>%
  tab_style(style = cell_text(align = "center", size = "medium"), locations = cells_body()) %>%
  tab_source_note(source_note = "") %>%
  text_transform(
    locations = cells_body(c(Team)),
    fn = function(x) web_image(url = paste0("https://a.espncdn.com/i/teamlogos/nfl/500/", x, ".png"))
  ) %>%
  cols_width(c(Team) ~ px(45)) %>%
  tab_style(style = list(cell_borders(sides = "left", color = "black", weight = px(3))),
            locations = list(cells_body(columns = c('Long')))) %>%
  tab_style(style = list(cell_borders(sides = "bottom", color = "black", weight = px(3))),
            locations = list(cells_column_labels(columns = everything()))) %>%
  data_color(columns = c(Short, Med, Long, CapRank),
             colors = col_numeric(palette = c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
                                  domain = NULL)) 

gt_pyoe

#P
punters <- ppy %>%
  group_by(possessionTeam, punter_player_id) %>%
  summarize(
    punts = n(),  
    PY = mean(apLength, na.rm = T),
    ePY = sum(s1, na.rm = T) / punts, 
    PYOE = PY - ePY,
    epa = mean(epa, na.rm = T),
    NY = sum(playResult, na.rm = T) / punts,
    NPY = NY - PY,
    team = last(possessionTeam),
    name = last(displayName)
  ) %>%
  ungroup() %>%
  filter(punts >= 100) %>%
  left_join(teams_colors_logos, by = c('team' = 'team_abbr'))

punters %>%
  ggplot(aes(x = NY, y = epa)) +
  #horizontal line
  geom_hline(yintercept = mean(punters$epa), color = "red", linetype = "dashed", alpha=0.5) +
  #vertical line
  geom_vline(xintercept =  mean(punters$NY), color = "red", linetype = "dashed", alpha=0.5) +
  #add image
  geom_image(aes(image = team_logo_espn), size = punters$punts / 6500, asp = 16 / 9) +
  #add names
  geom_text_repel(aes(label=name)) +
  #add line
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm')+
  #titles and caption
  labs(x = "Net Yards per Punt (NY/P)",
       y = "Expected Points Added per Punt (EPA/P)",
       title = "NFL Punters: 2018 - 2020",
       subtitle = "(min. 100 punts)",
       caption = "Data: @Kaggle") +
  theme_bw() +
  #center title
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  #better ticks
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = pretty_breaks(n = 10))

punters %>%
  ggplot(aes(x = PYOE, y = epa)) +
  #horizontal line
  geom_hline(yintercept = mean(punters$epa), color = "red", linetype = "dashed", alpha=0.5) +
  #vertical line
  geom_vline(xintercept =  mean(punters$PYOE), color = "red", linetype = "dashed", alpha=0.5) +
  #add image
  geom_image(aes(image = team_logo_espn), size = punters$punts / 6500, asp = 16 / 9) +
  #add names
  geom_text_repel(aes(label=name)) +
  #add line
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm')+
  #titles and caption
  labs(x = "Punt Yards Over Expected (PYOE)",
       y = "Expected Points Added per Punt (EPA/P)",
       title = "NFL Punters: 2018 - 2020",
       subtitle = "(min. 100 punts)",
       caption = "Data: @Kaggle") +
  theme_bw() +
  #center title
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  #better ticks
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = pretty_breaks(n = 10))
