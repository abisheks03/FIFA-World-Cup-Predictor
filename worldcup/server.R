#FIRST TAB

data_pos <- read.csv('data_pos.csv')
server <- function(input, output) {
  # Create variable to store winner
  winner <- ""
  
  observeEvent(input$submit, {
    
    # Filter dataset by input year
    year_data <- data %>%
      filter(Year == input$year & Round=="Final")
    
    
    # Output winner to user
    output$winner <- renderText({
        paste("The winner of the", input$year, "World Cup is", year_data$Winner)
    })
    
    output$winning_percentages <- renderPlot({
      # Filter the data for the input year
      year_data <- data[data$Year == input$year & data$Winner != "Tie", ]
      if(nrow(year_data) == 0) {
        showModal(modalDialog(
          title = "Invalid Year",
          "No World Cup held in that year.",
          easyClose = TRUE,
          footer = NULL
        ))
        return()
      }
      
      # Calculate the number of wins for each team
      wins <- as.data.frame(table(year_data$Winner))
      names(wins) <- c("Team", "Wins")
      
      # Count the total number of matches played in the tournament
      total_matches <- nrow(year_data)
      
      # Merge the wins dataframe with total matches
      wins$Total_matches <- total_matches
      
      # Calculate the winning percentage for each team
      wins$Winning_Percentage <- round(wins$Wins / wins$Total_matches * 100, 2)
      
      # Plot the winning percentages in a bar graph
      ggplot(wins, aes(x = Team, y = Winning_Percentage, fill = Team)) + 
        geom_bar(stat = "identity") +
        ggtitle(paste("Winning Percentages of FIFA World Cup Teams in", input$year)) + 
        xlab("Team") + ylab("Winning Percentage (%)")
    })
    
      
      output$player_statistics <- renderPlot({
        # Get the winning team name
        winning_team <- as.character(year_data[1, "Winner"])
        
        # Load the player rating dataset
        player_data <- read.csv("fifa_cleaned1.csv")
        
        # Filter the player data for the winning team and the input year
        winning_team_data <- player_data[player_data$national_team == winning_team & year_data$Year == input$year, ]
        
        # Plot the player statistics in a horizontal bar graph
        ggplot(winning_team_data, aes(x = overall_rating, y = name)) + 
          geom_bar(stat = "identity", width = 0.5, fill = "red", color="black") +
          ggtitle(paste("Player Ratings of the Winning Team (", winning_team, ") in", input$year)) + 
          xlab("Player Rating") + ylab("Player Name") + theme(axis.text.x = element_text(angle = 75, vjust = 1, hjust=1)) +
          coord_flip()
      })
})
  
  
#SECOND TAB
  observeEvent(input$predict,{
    # Filter the data for the selected teams
    team1_data <- data_pos[data_pos$team == input$team1, ]
    team2_data <- data_pos[data_pos$team == input$team2, ]
    
    # Calculate the average possession for each team
    team1_possession <- mean(team1_data$possession)
    team2_possession <- mean(team2_data$possession)
    
    # Predict the winner based on possession
    if(team1_possession > team2_possession) {
      winner <- input$team1
    } else {
      winner <- input$team2
    }
    
    # Print the result
    output$result <- renderText(paste("Predicted Winner:", winner))
  })
  
  output$team1_plot <- renderPlot({
    team1_data <- data_pos[data_pos$team == input$team1, c("possession","gk_save_pct",
                                                           "shots_on_target_pct","passes_pct","passes_pct_short",
                                                           "passes_pct_medium","passes_pct_long","assisted_shots",
                                                           "dribbles_completed_pct",
                                                           "dribble_tackles_pct","aerials_won_pct",
                                                           "blocked_passes","tackles_won")]
    
    # Plot the data
    ggplot(team1_data, aes(x = 1, y = possession, fill = "possession")) +
      geom_bar(stat = "identity", width = 0.2, color="black") +
      geom_bar(aes(x = 1.2, y = gk_save_pct, fill = "gk_save_pct"), 
               stat = "identity", width = 0.2, color="black") +
      geom_bar(aes(x = 1.4, y = shots_on_target_pct, fill = "shots_on_target_pct"), 
               stat = "identity", width = 0.2, color="black") +
      geom_bar(aes(x = 1.6, y = passes_pct, fill = "passes_pct"), 
               stat = "identity", width = 0.2, color="black") +
      geom_bar(aes(x = 1.8, y = passes_pct_short, fill = "passes_pct_short"), 
               stat = "identity", width = 0.2, color="black") +
      geom_bar(aes(x = 2, y = passes_pct_medium, fill = "passes_pct_medium"), 
               stat = "identity", width = 0.2, color="black") +
      geom_bar(aes(x = 2.2, y = passes_pct_long, fill = "passes_pct_long"), 
               stat = "identity", width = 0.2, color="black") +
      geom_bar(aes(x = 2.4, y = assisted_shots, fill = "assisted_shots"), 
               stat = "identity", width = 0.2, color="black") +
      geom_bar(aes(x = 2.6, y = dribbles_completed_pct, fill = "dribbles_completed_pct"), 
               stat = "identity", width = 0.2, color="black") +
      geom_bar(aes(x = 2.8, y = dribble_tackles_pct, fill = "dribble_tackles_pct"), 
               stat = "identity", width = 0.2, color="black") +
      geom_bar(aes(x = 3, y = aerials_won_pct, fill = "aerials_won_pct"), 
               stat = "identity", width = 0.2, color="black") +
      geom_bar(aes(x = 3.2, y = blocked_passes, fill = "blocked_passes"), 
               stat = "identity", width = 0.2, color="black") +
      geom_bar(aes(x = 3.4, y = tackles_won, fill = "tackles_won"), 
               stat = "identity", width = 0.2, color="black") +
      scale_x_continuous(breaks = c(1, 1.2, 1.4, 1.6, 1.8, 2, 2.2, 2.4, 2.6, 2.8, 3, 3.2, 3.4), 
                         labels = c("possession", "gk_save_pct", "shots_on_target_pct",
                                    "passes_pct","passes_pct_short", "passes_pct_medium", "passes_pct_long",
                                  "assisted_shots", "dribble_completed_pct",  "dribble_tackles_pct", "aerials_won_pct",
                                    "blocked_passes", "tackles_won")) +
      ggtitle(paste("Team Performance of ", input$team1)) +
      xlab("Stats") + ylab("Percentage (%)") +
      coord_flip()
    
  })
  output$team2_plot <- renderPlot({
    # Filter the data for the selected team
    team2_data <- data_pos[data_pos$team == input$team2, c("possession","gk_save_pct",
                                                 "shots_on_target_pct","passes_pct","passes_pct_short",
                                                 "passes_pct_medium","passes_pct_long","assisted_shots",
                                                 "dribbles_completed_pct",
                                                 "dribble_tackles_pct","aerials_won_pct",
                                                 "blocked_passes","tackles_won")]
    # Plot the data
    ggplot(team2_data, aes(x = 1, y = possession, fill = "possession")) +
      geom_bar(stat = "identity", width = 0.2, color="black") +
      geom_bar(aes(x = 1.2, y = gk_save_pct, fill = "gk_save_pct"), 
               stat = "identity", width = 0.2, color="black") +
      geom_bar(aes(x = 1.4, y = shots_on_target_pct, fill = "shots_on_target_pct"), 
               stat = "identity", width = 0.2, color="black") +
      geom_bar(aes(x = 1.6, y = passes_pct, fill = "passes_pct"), 
               stat = "identity", width = 0.2, color="black") +
      geom_bar(aes(x = 1.8, y = passes_pct_short, fill = "passes_pct_short"), 
               stat = "identity", width = 0.2, color="black") +
      geom_bar(aes(x = 2, y = passes_pct_medium, fill = "passes_pct_medium"), 
               stat = "identity", width = 0.2, color="black") +
      geom_bar(aes(x = 2.2, y = passes_pct_long, fill = "passes_pct_long"), 
               stat = "identity", width = 0.2, color="black") +
      geom_bar(aes(x = 2.4, y = assisted_shots, fill = "assisted_shots"), 
               stat = "identity", width = 0.2, color="black") +
      geom_bar(aes(x = 2.6, y = dribbles_completed_pct, fill = "dribbles_completed_pct"), 
               stat = "identity", width = 0.2, color="black") +
      geom_bar(aes(x = 2.8, y = dribble_tackles_pct, fill = "dribble_tackles_pct"), 
               stat = "identity", width = 0.2, color="black") +
      geom_bar(aes(x = 3, y = aerials_won_pct, fill = "aerials_won_pct"), 
               stat = "identity", width = 0.2, color="black") +
      geom_bar(aes(x = 3.2, y = blocked_passes, fill = "blocked_passes"), 
               stat = "identity", width = 0.2, color="black") +
      geom_bar(aes(x = 3.4, y = tackles_won, fill = "tackles_won"), 
               stat = "identity", width = 0.2, color="black") +
      scale_x_continuous(breaks = c(1, 1.2, 1.4, 1.6, 1.8, 2, 2.2, 2.4, 2.6, 2.8, 3, 3.2, 3.4), 
                         labels = c("possession", "gk_save_pct", "shots_on_target_pct",
                                    "passes_pct","passes_pct_short", "passes_pct_medium", "passes_pct_long",
                                    "assisted_shots", "dribble_completed_pct",  "dribble_tackles_pct", "aerials_won_pct",
                                    "blocked_passes", "tackles_won")) +
      ggtitle(paste("Team Performance of ", input$team2)) +
      xlab("Stats") + ylab("Percentage (%)") +
      coord_flip()
    
})


#THIRD TAB
  output$data <- renderDataTable(data)
  output$player_stats <- renderDataTable(player_stats)
  output$data_pos <- renderDataTable(data_pos)
}
  

    

