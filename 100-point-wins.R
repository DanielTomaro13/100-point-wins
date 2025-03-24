library(dplyr)
library(fitzRoy)
library(ggplot2)
library(shiny)

results <- fetch_results_afltables(1897:2024)
colnames(results)

# Add winner and winning score columns
results_with_winner <- results %>%
  mutate(
    Winning.Team = case_when(
      Home.Points > Away.Points ~ Home.Team,
      Away.Points > Home.Points ~ Away.Team,
      TRUE ~ "Draw"
    ),
    Winning.Points = pmax(Home.Points, Away.Points) # Amount of points winning team had
  ) %>%
  filter(Winning.Team != "Draw")  # Remove drawn games 

# Games where the winning team scored 100 or more
wins_100 <- results_with_winner %>%
  filter(Winning.Points >= 100) %>%
  select(Date, Season, Venue, Round = Round.Number, 
         Home.Team, Home.Points, Away.Team, Away.Points, 
         Margin, Winning.Team, Winning.Points)
nrow(wins_100)

# Games where the winning team scored less than 100
wins_less_100 <- results_with_winner %>%
  filter(Winning.Points < 100) %>%
  select(Date, Season, Venue, Round = Round.Number, 
         Home.Team, Home.Points, Away.Team, Away.Points, 
         Margin, Winning.Team, Winning.Points)
nrow(wins_less_100)

# Create team-level data for home teams
home_data <- results %>%
  transmute(
    Date, Season, Venue,
    Team = Home.Team,
    Points = Home.Points,
    Opponent = Away.Team,
    OpponentPoints = Away.Points,
    Win = case_when(Home.Points > Away.Points ~ 1,
                    Home.Points < Away.Points ~ 0,
                    TRUE ~ NA_real_)
  )

# Create team-level data for away teams
away_data <- results %>%
  transmute(
    Date, Season, Venue,
    Team = Away.Team,
    Points = Away.Points,
    Opponent = Home.Team,
    OpponentPoints = Home.Points,
    Win = case_when(Away.Points > Home.Points ~ 1,
                    Away.Points < Home.Points ~ 0,
                    TRUE ~ NA_real_)
  )

# Combine both
team_results <- bind_rows(home_data, away_data) %>%
  filter(!is.na(Win))  # Exclude draws

team_results <- team_results %>%
  mutate(Scored100 = ifelse(Points >= 100, 1, 0))

team_results %>%
  group_by(Scored100) %>%
  summarise(
    Games = n(),
    Wins = sum(Win),
    WinRate = mean(Win),
    .groups = "drop"
  )

model <- glm(Win ~ Points, data = team_results, family = "binomial")
summary(model)

# Create new data for prediction
pred_data <- data.frame(Points = seq(40, 160, by = 1))
# Predict probabilities
pred_data$Win_Prob <- predict(model, newdata = pred_data, type = "response")

ggplot(pred_data, aes(x = Points, y = Win_Prob)) +
  geom_line(color = "blue", size = 1) +
  geom_vline(xintercept = 100, linetype = "dashed", color = "red", size = 0.8) +
  labs(
    title = "Probability of Winning vs Points Scored",
    subtitle = "Dashed red line indicates 100 points",
    x = "Points Scored",
    y = "Predicted Win Probability"
  ) +
  theme_minimal(base_size = 14)

# Actual win rates by score
actual_win_rates <- team_results %>%
  group_by(Points) %>%
  summarise(WinRate = mean(Win), Games = n(), .groups = "drop") %>%
  filter(Games >= 10)  # Only include scores seen in at least 10 games

# Plot with actuals + model
ggplot() +
  geom_line(data = pred_data, aes(x = Points, y = Win_Prob), color = "blue", size = 1) +
  geom_point(data = actual_win_rates, aes(x = Points, y = WinRate), color = "darkorange", alpha = 0.6) +
  geom_vline(xintercept = 100, linetype = "dashed", color = "red", size = 0.8) +
  labs(
    title = "Modelled vs Actual Win Probability by Points Scored",
    subtitle = "Orange points are actual win rates (min 10 games per score)",
    x = "Points Scored",
    y = "Win Probability"
  ) +
  theme_minimal(base_size = 14)

team_results <- team_results %>%
  mutate(
    ScoreBracket = case_when(
      Points < 50 ~ "<50",
      Points >= 50 & Points < 75 ~ "50–74",
      Points >= 75 & Points < 85 ~ "75–84",
      Points >= 85 & Points < 100 ~ "85–99",
      Points >= 100 ~ "100+"
    )
  )
score_bracket_summary <- team_results %>%
  group_by(ScoreBracket) %>%
  summarise(
    Games = n(),
    Wins = sum(Win),
    WinRate = mean(Win),
    .groups = "drop"
  ) %>%
  arrange(factor(ScoreBracket, levels = c("<50", "50–74", "75–84", "85–99", "100+")))
score_bracket_summary

ggplot(score_bracket_summary, aes(x = ScoreBracket, y = WinRate)) +
  geom_col(fill = "skyblue") +
  geom_text(aes(label = paste0(round(WinRate * 100, 1), "%")), 
            vjust = -0.5, size = 5) +
  labs(
    title = "Win Rate by Points Scored Bracket",
    x = "Score Bracket",
    y = "Win Rate"
  ) +
  ylim(0, 1) +
  theme_minimal(base_size = 14)

# Lets look at the margin of games where one team scored 100
wins_100_plus <- team_results %>%
  filter(Win == 1 & Points >= 100)
wins_100_plus <- wins_100_plus %>%
  mutate(
    Margin = Points - OpponentPoints,
    OpponentScoreBracket = case_when(
      OpponentPoints < 50 ~ "<50",
      OpponentPoints < 75 ~ "50–74",
      OpponentPoints < 90 ~ "75–89",
      OpponentPoints < 100 ~ "90–99",
      TRUE ~ "100+"
    )
  )
ggplot(wins_100_plus, aes(x = Margin)) +
  geom_histogram(binwidth = 10, fill = "darkgreen", color = "white") +
  labs(
    title = "Margin in Games Where Team Scored 100+ and Won",
    x = "Winning Margin",
    y = "Number of Games"
  ) +
  theme_minimal(base_size = 14)
wins_100_plus %>%
  group_by(OpponentScoreBracket) %>%
  summarise(
    Games = n(),
    AvgMargin = mean(Margin),
    .groups = "drop"
  ) %>%
  arrange(desc(OpponentScoreBracket))

team_results <- team_results %>%
  mutate(
    ScoreBracket = case_when(
      Points < 50 ~ "<50",
      Points >= 50 & Points < 75 ~ "50–74",
      Points >= 75 & Points < 85 ~ "75–84",
      Points >= 85 & Points < 100 ~ "85–99",
      Points >= 100 ~ "100+"
    ),
    Decade = paste0(floor(Season / 10) * 10, "s")
  )

# Define UI
ui <- fluidPage(
  titlePanel("AFL Win Rate by Points Scored"),
  sidebarLayout(
    sidebarPanel(
      selectInput("decade", "Select Decade:", 
                  choices = c("All", sort(unique(team_results$Decade))),
                  selected = "All")
    ),
    mainPanel(
      plotOutput("winRatePlot"),
      tableOutput("summaryTable")
    )
  )
)

# Define Server
server <- function(input, output) {
  
  filtered_data <- reactive({
    if (input$decade == "All") {
      team_results
    } else {
      team_results %>% filter(Decade == input$decade)
    }
  })
  
  summary_data <- reactive({
    filtered_data() %>%
      group_by(ScoreBracket) %>%
      summarise(
        Games = n(),
        Wins = sum(Win),
        WinRate = mean(Win),
        .groups = "drop"
      ) %>%
      arrange(factor(ScoreBracket, levels = c("<50", "50–74", "75–84", "85–99", "100+")))
  })
  
  output$winRatePlot <- renderPlot({
    ggplot(summary_data(), aes(x = ScoreBracket, y = WinRate)) +
      geom_col(fill = "steelblue") +
      geom_text(aes(label = paste0(round(WinRate * 100, 1), "%")),
                vjust = -0.5, size = 5) +
      labs(
        title = paste("Win Rate by Score Bracket", ifelse(input$decade == "All", "", paste("(", input$decade, ")"))),
        x = "Score Bracket",
        y = "Win Rate"
      ) +
      ylim(0, 1) +
      theme_minimal(base_size = 14)
  })
  
  output$summaryTable <- renderTable({
    summary_data()
  })
}

shinyApp(ui = ui, server = server)

