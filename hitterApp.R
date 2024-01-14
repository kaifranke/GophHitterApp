install.packages(c("dplyr", "class", "caret", "ggforce", "shiny"))

library(dplyr)
library(class)
library(caret)
library(ggforce)

setwd("C:/Users/kaifr/OneDrive/Miscellaneous/Desktop/Productivity/Baseball Research/Gopher Baseball/Apps/Hitter App")

data = read.csv("Data10122023.csv")

data = data %>%
  mutate(ID = row_number(),
         swing = ifelse(PitchCall %in% c("FoulBall", "InPlay", "StrikeSwinging"), 1, 0))

inPlay = filter(data, !is.na(ExitSpeed) & PlayResult != "Undefined") %>%
  select(ID, ExitSpeed, Angle, Direction, PlayResult) %>%
  mutate(wOBA = ifelse(PlayResult == "Single", 0.94, NA),
         wOBA = ifelse(PlayResult == "Double", 1.34, wOBA),
         wOBA = ifelse(PlayResult == "Triple", 1.67, wOBA),
         wOBA = ifelse(PlayResult == "HomeRun", 2.08, wOBA),
         wOBA = ifelse(is.na(wOBA), 0, wOBA),
         hit = ifelse(PlayResult %in% c("Single", "Double", "Triple", "HomeRun"), 1, 0))

plays = data %>%
  filter(PlayResult != "Undefined" | KorBB != "Undefined",
         PlayResult != "CaughtStealing", PlayResult != "StolenBase") %>%
  mutate(playEnd = ifelse(KorBB == "Undefined", PlayResult, KorBB)) %>%
  select(ID, Batter, BatterTeam, Date, Time, BatterSide, PitcherThrows, Outs, Balls, Strikes, 
         AutoPitchType, TaggedPitchType, PitchCall, PlateLocHeight, PlateLocSide, 
         ExitSpeed, Angle, Direction, Distance, TaggedHitType, AutoHitType, 
         ContactPositionX, ContactPositionY, ContactPositionZ, HitSpinRate, HitSpinAxis,
         playEnd) %>%
  mutate(wOBA = ifelse(playEnd == "Single", 0.94, NA),
         wOBA = ifelse(playEnd == "Double", 1.34, wOBA),
         wOBA = ifelse(playEnd == "Triple", 1.67, wOBA),
         wOBA = ifelse(playEnd == "HomeRun", 2.08, wOBA),
         wOBA = ifelse(playEnd == "Walk", 0.76, wOBA),
         wOBA = ifelse(is.na(wOBA), 0, wOBA),
         hit = ifelse(playEnd %in% c("Single", "Double", "Triple", "HomeRun"), 1, 0),
         hc_x = Distance * sin(Direction * (pi/180)),
         hc_y = Distance * cos(Direction * (pi/180))) %>%
  rename(actExitVelo = ExitSpeed,
         actLaunchAng = Angle)

scaled = cbind(ID = plays$ID, data.frame(scale(plays[16:17]))) %>%
  rename(Angle = actLaunchAng, 
         ExitSpeed = actExitVelo) %>%
  na.omit()

plays2 = left_join(plays, scaled)


xwOBAmod = readRDS(file = "xwOBAmodel")
xBAmod = readRDS(file = "xBAmodel")
strMod = readRDS(file = "StrikeMod")


labs = unique(xwOBAmod$learn$y)

predict(xwOBAmod, newdata = scaled[2:3])

predsxwOBA = cbind(scaled, xwOBA = predict(xwOBAmod, scaled[2:3]))

xwOBAfun = function(sing, dou, tri, hr) {
  xwOBA = sing * 0.94 + dou * 1.34 + tri * 1.67 + hr * 2.08
  return(xwOBA)
}


preds = predsxwOBA %>%
  mutate(xwOBA = xwOBAfun(xwOBA.0.94, xwOBA.1.34, xwOBA.1.67, xwOBA.2.08)) %>%
  select(ID, ExitSpeed, Angle, xwOBA)

predsxBA = cbind(scaled, xBA = predict(xBAmod, scaled[2:3])) %>%
  rename(xBA = xBA.1) %>%
  select(xBA)

preds = cbind(preds, predsxBA)

finPlays = left_join(plays, preds) %>%
  mutate(xwOBA = ifelse(playEnd == "Strikeout", 0, xwOBA),
         xwOBA = ifelse(playEnd == "Walk", .76, xwOBA),
         xBA = ifelse(playEnd == "Strikeout", 0, xBA))

data = left_join(data, preds, by = "ID")


data = data %>%
  mutate(wOBA = ifelse(PlayResult == "Single", 0.94, NA),
         wOBA = ifelse(PlayResult == "Double", 1.34, wOBA),
         wOBA = ifelse(PlayResult == "Triple", 1.67, wOBA),
         wOBA = ifelse(PlayResult == "HomeRun", 2.08, wOBA),
         wOBA = ifelse(PlayResult == "Walk", 0.76, wOBA),
         wOBA = ifelse(PlayResult %in% c("Sacrifice", "Out", "FieldersChoice", "Error") | KorBB %in% c("Strikeout", "Walk"), 0, wOBA),
         hit = ifelse(wOBA > 0.76, 1, 0))

data = data %>%
  mutate(count = paste0(Balls, "-", Strikes))

modData = data %>% 
  mutate(PitcherThrows_Right = ifelse(PitcherThrows == "Right", 1, 0),
         count_X0.1 = ifelse(count == "0-1", 1, 0),
         count_X0.2 = ifelse(count == "0-2", 1, 0),
         count_X1.0 = ifelse(count == "1-0", 1, 0),
         count_X1.1 = ifelse(count == "1-1", 1, 0),
         count_X1.2 = ifelse(count == "1-2", 1, 0),
         count_X2.0 = ifelse(count == "2-0", 1, 0),
         count_X2.1 = ifelse(count == "2-1", 1, 0),
         count_X2.2 = ifelse(count == "2-2", 1, 0),
         count_X3.0 = ifelse(count == "3-0", 1, 0),
         count_X3.1 = ifelse(count == "3-1", 1, 0),
         count_X3.2 = ifelse(count == "3-2", 1, 0)) %>%
  select(PitchofPA, RelSpeed, VertRelAngle, HorzRelAngle, VertApprAngle, HorzApprAngle, pfxx,
         pfxz, RelHeight, RelSide, Extension, PlateLocHeight, PlateLocSide, PitcherThrows_Right, 
         starts_with("count_X"))

strProb = predict(strMod, newdata = data.matrix(modData), type = "prob")

data = bind_cols(data, strProb = strProb) %>%
  mutate(strProb = ifelse(strProb < 0.5, 0.5 + abs(0.5 - strProb), 0.5 - abs(0.5 - strProb)))

gophHitters = finPlays %>%
  filter(BatterTeam == "MIN_GOL") %>%
  select(Batter) %>%
  distinct() %>%
  arrange(Batter)


siebert =   ggplot() + geom_segment(aes(x = 0, y = 0, xend = -233.345, yend = 233.345)) +
  geom_segment(aes(x = 0, y = 0, xend = 233.345, yend = 233.345)) +
  geom_segment(aes(x = -233.345, y = 233.345, xend = -139.679, yend = 337.217), color = "black") +
  geom_segment(aes(x = -139.679, y = 337.217, xend = -30, yend = 390), color = "black") +
  geom_segment(aes(x = -30, y = 390, xend = 30, yend = 390), color = "black") +
  geom_segment(aes(x = 30, y = 390, xend = 75, yend = 355), color = "black") +
  geom_segment(aes(x = 75, y = 355, xend = 149.679, yend = 337.217), color = "black") +
  geom_segment(aes(x = 149.679, y = 337.217, xend = 225.345, yend = 260.345), color = "black") +
  geom_segment(aes(x = 225.345, y = 260.345, xend = 233.345, yend = 233.345), color = "black") +
  geom_segment(aes(x = 63.65, y = 63.65, xend = 0, yend = 127.3)) +
  geom_segment(aes(x = -63.65, y = 63.65, xend = 0, yend = 127.3))

#setwd("C:/Users/kaifr/OneDrive/Productivity/Baseball Research/Gopher Baseball/Apps")

pitchers = data %>%
  filter(PitcherTeam != "MIN_GOL") %>%
  select(Pitcher) %>%
  distinct() %>%
  arrange(Pitcher)

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("Minnesota Gophers Hitting App"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput('tab_selector',
                  'Select Mode:',
                  choices = c("Development", "Scouting")),
      conditionalPanel(
        condition = "input.tab_selector == 'Development'",
        selectInput('name',
                    'Select Hitter:',
                    choices = gophHitters)
      ),
      conditionalPanel(
        condition = "input.tab_selector == 'Scouting'",
        selectInput('pitcher_name',
                    'Select Pitcher:',
                    choices = pitchers)
      )
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("Development",
                 tabsetPanel(
                   tabPanel("Spray Chart",
                            plotOutput("sprayPlot"),
                            tableOutput("avgTable")),
                   tabPanel("Pitch Location",
                            plotOutput("zonePlot"),
                            plotOutput("depthPlot")),
                   tabPanel("Swing Decisions",
                            plotOutput("swingDec"),
                            tableOutput("swingDecTable")),
                   tabPanel("Pitch Performance",
                            plotOutput("pitchPlot"),
                            tableOutput("pitchTable"))
                 )
        ),
        tabPanel("Scouting",
                 tabsetPanel(
                   tabPanel("Movement Plot",
                            plotOutput("movementPlot"),
                            tableOutput("perfTable")),
                   tabPanel("Pitch Usage",
                            plotOutput("usagePlot")),
                   tabPanel("Pitch Location",
                            plotOutput("locPlot"))
                 ))
      )
    )
  )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ###
  
  
  
  
  
  ###
  
  
  
  batterData = reactive({
    finPlays %>%
      filter(Batter == input$name)
    
  })
  
  dataDec = reactive({
    data %>%
      filter(Batter == input$name)
  })
  
  
  
  output$sprayPlot = renderPlot({
    
    data = batterData()  # Use batterData() as a function to access the reactive data
    
    siebert + 
      geom_point(aes(x = data$hc_x, y = data$hc_y, color = data$xwOBA), size = 5) +
      scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.4) +
      coord_fixed() +
      xlim(-240, 240) +
      ylim(-10, 400) +
      theme_bw() +
      labs(x = "Left/Right Hit Location", y = "Distance Away from Home", 
           title = paste0("Spray Chart with xwOBA for ", input$name), color = "xwOBA", subtitle = "Distance in Feet") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, face = "bold"))
    
    
  })
  
  output$zonePlot = renderPlot({
    data = batterData()
    ggplot(data, aes(PlateLocSide, PlateLocHeight, z = xwOBA)) +
      stat_summary2d(binwidth = 0.7) +
      scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.4) +
      annotate("rect", xmin = -0.7, xmax = 0.7, ymin = 1.6, ymax = 3.5, color = "black", alpha = 0.3) +
      coord_fixed() +
      theme_bw() +
      labs(x = "Horizontal Pitch Location", y = "Pitch Height", title = paste0("xwOBA by Pitch Location for ", input$name),
           fill = "xwOBA") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, face = "bold")) +
      xlim(-2,2) +
      ylim(0,5)
  })
  
  output$avgTable = renderTable({
    
    finPlays %>%
      group_by(Batter) %>%
      summarise(`PA` = n(),
                AB = PA - sum(playEnd == "Walk"),
                H = as.integer(sum(hit)),
                BB = sum(playEnd == "Walk"),
                K = sum(playEnd == "Strikeout"),
                `Avg. Exit Velocity` = mean(actExitVelo, na.rm = T),
                `Avg. Launch Angle` = mean(actLaunchAng, na.rm = T),
                wOBA = mean(wOBA, na.rm = T),
                xwOBA = round(mean(xwOBA, na.rm = T), 4),
                BA = sum(hit) / (AB),
                xBA = sum(xBA, na.rm = T) / (AB),
                `Hard Hit Rate` = 100 * mean(actExitVelo >= 95, na.rm = T),
                `K%` = 100 * sum(playEnd == "Strikeout") / `PA`,
                `BB%` = 100 * sum(playEnd == "Walk") / `PA`) %>%
      filter(Batter == input$name) %>%
      select(-Batter)
  })
  
  
  output$swingDec = renderPlot({
    
    data = dataDec()
    
    data %>%
      mutate(swing = ifelse(swing == 1, "Swung", "Took")) %>%
      ggplot(aes(PlateLocSide, PlateLocHeight, color = strProb)) +
      geom_point(size = 3) +
      scale_color_gradient2(low = "red", mid = "white", high = "green", midpoint = 0.5) +
      annotate("rect", xmin = -0.7, xmax = 0.7, ymin = 1.6, ymax = 3.5, color = "black", alpha = 0.3) +
      xlim(-4, 4) +
      ylim(-1, 5) +
      coord_fixed() +
      labs(x = "Horizontal Pitch Location", y = "Pitch Height", title = paste0("Swing Decisions for ", input$name),
           color = "Strike Prob") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, face = "bold")) +
      facet_wrap(~swing)
    
  })
  
  output$swingDecTable = renderTable({
    
    data %>%
      mutate(inZone = ifelse(PlateLocSide >= -.7 & PlateLocSide <= .7 & PlateLocHeight <= 3.5 & PlateLocHeight >= 1.6, 1, 0),
             inPlay = ifelse(PitchCall == "InPlay", 1, 0),
             whiff = ifelse(PitchCall == "StrikeSwinging", 1, 0)) %>%
      group_by(Batter) %>%
      summarise(Pitches = n(),
                Swings = as.integer(sum(swing)),
                `Swing%` = 100 * Swings / Pitches,
                Whiffs = as.integer(sum(whiff)),
                `Whiff%` = 100 * Whiffs / Swings,
                `In-Zone Swing%` = 100 * sum(ifelse(inZone == 1, swing, NA), na.rm = T) / sum(inZone, na.rm = T),
                `Out-Zone Swing%` = 100 * sum(ifelse(inZone == 1, NA, swing), na.rm = T) / sum(inZone == 0, na.rm = T)) %>%
      filter(Batter == input$name) %>%
      ungroup() %>%
      select(-Batter)
    
    
  })
  
  
  output$depthPlot = renderPlot({
    
    dataSw = batterData()
    
    ggplot() +
      geom_point(aes(dataSw$ContactPositionZ, dataSw$ContactPositionX, color = dataSw$xwOBA), size = 4) +
      scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.4) +
      theme_bw() +
      coord_fixed() +
      geom_segment(aes(x = -.7, y = 0, xend = .7, yend = 0)) +
      geom_segment(aes(x = .7, y = 0, xend = .7, yend = -.7)) +
      geom_segment(aes(x = .7, y = -.7, xend = 0, yend = -1.42)) +
      geom_segment(aes(x = 0, y = -1.42, xend = -.7, yend = -.7)) +
      geom_segment(aes(x = -.7, y = -.7, xend = -.7, yend = 0)) +
      xlim(-2,2) +
      labs(x = "Horizontal Contact Point", y = "Distance Caught in Front", title = paste0("Contact Location for ", input$name), 
           color = "xwOBA", subtitle = "Distance in Feet") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, face = "bold"))
    
  })
  
  
  output$pitchTable = renderTable({
    
    data %>%
      mutate(whiff = ifelse(PitchCall == "StrikeSwinging", 1, 0)) %>%
      group_by(Batter, AutoPitchType) %>%
      summarise(PA = sum(PlayResult != "Undefined", na.rm = T),
                Pitches = n(),
                Swings = as.integer(sum(swing)),
                Whiffs = as.integer(sum(whiff)),
                `Whiff%` = 100 * Whiffs / Swings,
                `Hard Hit Rate` = 100 * mean(ifelse(ExitSpeed.x >= 95, 1, 0), na.rm = T),
                `Sweet Spot Rate` = 100 * mean(ifelse(Angle.x >= 8 & Angle.x <= 32, 1, 0), na.rm = T),
                BA = mean(hit, na.rm = T),
                xBA = mean(xBA, na.rm = T),
                wOBA = mean(wOBA, na.rm = T),
                xwOBA = mean(xwOBA, na.rm = T)) %>%
      filter(Batter == input$name,
             AutoPitchType != "") %>%
      arrange(AutoPitchType) %>%
      rename(`Pitch Type` = AutoPitchType) %>%
      ungroup() %>%
      select(-Batter, -PA)
    
  }) 
  
  
  output$pitchPlot = renderPlot({
    
    
    ggplot(filter(data, !is.na(xwOBA) & AutoPitchType != "" & Batter == input$name), aes(PlateLocSide, PlateLocHeight, z = xwOBA)) +
      stat_summary_2d(binwidth = 0.7) +
      facet_wrap(~AutoPitchType) +
      scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.4) +
      theme_bw() +
      labs(x = "Horizontal Pitch Location", y = "Pitch Height", fill = "xwOBA") +
      annotate("rect", xmin = -0.7, xmax = 0.7, ymin = 1.6, ymax = 3.5, color = "black", alpha = 0.3) +
      coord_fixed()
    
    
  })
  
  
  output$movementPlot = renderPlot({
    
    
    ggplot(filter(data, Pitcher == input$pitcher_name, AutoPitchType != c("", "Other")), aes(pfxx, pfxz, color = AutoPitchType)) +
      geom_point(size = 3, alpha = 0.7) +
      ggforce::geom_mark_ellipse(aes(fill = AutoPitchType), expand = unit(0.5,"mm"), show.legend = F, alpha = 0.3) +
      geom_vline(xintercept = 0) +
      geom_hline(yintercept = 0) +
      theme_bw() +
      xlim(-25, 25) +
      ylim(-25, 25) +
      coord_fixed() +
      labs(title = paste0("Pitch Movement Plot for ", input$pitcher_name),
           subtitle = "Hitter's Point of View", x = "Horizontal Pitch Movement",
           y = "Vertical Pitch Movement", color = "Pitch Type") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, face = "bold"))
    
    
  })
  
  output$usagePlot = renderPlot({
    
    
    ggplot(filter(data, Pitcher == input$pitcher_name, AutoPitchType != c("", "Other")), aes(x = AutoPitchType, 
                                                            y = (..count..)/sum(..count..), 
                                                            fill = AutoPitchType)) +
      geom_bar(show.legend = F) +
      theme_bw() +
      labs(x = "Pitch Type", y = "Times Thrown", title = paste0("Pitch Usage for ", input$pitcher_name)) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, face = "bold")) +
      facet_wrap(~count)
    
  })
  
  
  output$perfTable = renderTable({
    
    data %>%
      mutate(whiff = ifelse(PitchCall == "StrikeSwinging", 1, 0),
             xwOBA = ifelse(KorBB == "Strikeout", 0, xwOBA),
             xwOBA = ifelse(KorBB == "Walk", .76, xwOBA),
             wOBA = ifelse(KorBB == "Strikeout", 0, wOBA),
             wOBA = ifelse(KorBB == "Walk", .76, wOBA),
             xBA = ifelse(KorBB == "Strikeout", 0, xBA)) %>%
      filter(AutoPitchType != c("", "Other")) %>%
      group_by(Pitcher, AutoPitchType) %>%
      summarise(`Times Thrown` = n(),
                `Avg. Velo` = mean(RelSpeed, na.rm = T),
                `Max Velo` = max(RelSpeed, na.rm = T),
                `Avg. Spin Rate` = mean(SpinRate, na.rm = T),
                nSwings = sum(swing),
                nWhiff = sum(whiff),
                `Whiff%` = 100 * nWhiff / nSwings,
                xwOBA = mean(xwOBA, na.rm = T),
                xBA = mean(xBA, na.rm = T)) %>%
      filter(Pitcher == input$pitcher_name) %>%
      arrange(desc(`Times Thrown`))
    
  })
  
  
  output$locPlot = renderPlot({
    
    
    ggplot(filter(data, Pitcher == input$pitcher_name, AutoPitchType != c("", "Other")), aes(PlateLocSide, PlateLocHeight, color = AutoPitchType)) +
      geom_point(size = 3) +
      annotate("rect", xmin = -0.7, xmax = 0.7, ymin = 1.6, ymax = 3.5, color = "black", alpha = 0.3) +
      xlim(-4, 4) +
      ylim(-1, 5) +
      theme_bw() +
      labs(x = "Horizontal Pitch Location", y = "Pitch Height", title = paste0("Pitch Locations for ", input$pitcher_name),
           color = "Pitch Type") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, face = "bold")) +
      facet_wrap(~count) +
      coord_fixed()
    
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
