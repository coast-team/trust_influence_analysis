if (!require(zTree)) {
  install.packages("zTree")
}
library (zTree)

pop.var <- function(x) var(x, na.rm = TRUE) * (length(x)-1) / length(x)

pop.sd <- function(x) {
  if (length(x) == 0) {
    0
  }
  else {
    if (is.na(sqrt(pop.var(x)))) {
      0
    } else {
      sqrt(pop.var(x))
    }
  }
}

readMultiXLS <- function (directory) {
  # source("http://www.kirchkamp.de/lab/zTree.R")
  sessions <- sessions<-list.files(directory,"[0-9]{6}_[0-9]{4}.xls",recursive=TRUE, full.names = TRUE)
  zTT <- zTree::zTreeTables(sessions, zTree.silent=TRUE)
  zTT
}

# process multiple files
processMultiXLS <- function (zTT, num_users = 6, average_rounds = 5, num_games = 4) {
  num_rounds_per_game = (num_users - 1) * average_rounds 
  num_rounds_per_exp = num_rounds_per_game * num_games
  num_exp = nrow (zTT[1]$globals) / num_rounds_per_exp
  
  # read through games
  # collect corresponding game and add to the list
  
  # first, create empty data frames to hold all the particular games
  simple_games <- zTT[2]$subjects[0,]
  id_games <- zTT[2]$subjects[0,]
  score_games <- zTT[2]$subjects[0,]
  combine_games <- zTT[2]$subjects[0,]
  
  for (exp_id in 1:num_exp) {
    first_round_of_exp_globals = (exp_id - 1) * num_rounds_per_exp + 1
    last_round_of_exp_globals = exp_id * num_rounds_per_exp
    globals_of_exp = zTT[1]$globals[first_round_of_exp_globals:last_round_of_exp_globals,]
    
    first_round_of_exp_subjects = (exp_id - 1) * num_rounds_per_exp * num_users + 1
    last_round_of_exp_subjects = exp_id * num_rounds_per_exp * num_users
    subjects_of_exp = zTT[2]$subjects[first_round_of_exp_subjects:last_round_of_exp_subjects,]
    
    SIMPLE_GAME_ORDER = globals_of_exp[1,]$SIMPLE_GAME
    ID_GAME_ORDER = globals_of_exp[1,]$ID_GAME
    SCORE_GAME_ORDER = globals_of_exp[1,]$SCORE_GAME
    COMBINE_GAME_ORDER = globals_of_exp[1,]$COMBINE_GAME
    
    simple_games <- rbind(simple_games, subjects_of_exp[((SIMPLE_GAME_ORDER - 1) * num_rounds_per_game * num_users + 1): (SIMPLE_GAME_ORDER * num_rounds_per_game * num_users),])
    id_games <- rbind(id_games, subjects_of_exp[((ID_GAME_ORDER - 1) * num_rounds_per_game * num_users + 1): (ID_GAME_ORDER * num_rounds_per_game * num_users),])
    score_games <- rbind(score_games, subjects_of_exp[((SCORE_GAME_ORDER - 1) * num_rounds_per_game * num_users + 1): (SCORE_GAME_ORDER * num_rounds_per_game * num_users),])
    combine_games <- rbind(combine_games, subjects_of_exp[((COMBINE_GAME_ORDER - 1) * num_rounds_per_game * num_users + 1): (COMBINE_GAME_ORDER * num_rounds_per_game * num_users),])
  }
  
  # write all the data for each games in CSV file
  # using write.csv
  Type_names = c("Sender", "Receiver")
  SIMPLE_GAME_ORDERS =      c(3,2,4,1,2)
  ID_GAME_ORDERS =          c(1,4,1,2,3)
  SCORE_GAME_ORDERS =       c(2,1,3,4,4)
  COMBINE_GAME_ORDERS =     c(4,3,2,3,1)
  
  require(car)
  for (type in 0:1) {
    # AbsSend: absolute sending
    # RelSend: send proportion of maximum
    # Profit: profit get at this round
    # game_pos:  order of game to play in this experiment
    df <- data.frame("id" = as.numeric(), 
                     "GroupID"= as.numeric(),
                     "SHOW_TRUST"=as.numeric(),
                     "SHOW_ID"=as.numeric(),
                     "AbsSend" = as.numeric(),  
                     "RelSend" = as.numeric(),  
                     "Profit" = as.numeric(),
                     "game_pos" = as.numeric(),
                     "response_time" = as.numeric())
    for (j in 1:num_users) {
      for (exp_id in 1:num_exp) {
        for (k in 1:num_games) {
          first_round_of_exp_subjects = (exp_id - 1) * num_rounds_per_game * num_users + 1
          last_round_of_exp_subjects = exp_id * num_rounds_per_game * num_users
          simple_game = simple_games[first_round_of_exp_subjects:last_round_of_exp_subjects,]
          id_game = id_games [first_round_of_exp_subjects:last_round_of_exp_subjects,]
          score_game = score_games [first_round_of_exp_subjects:last_round_of_exp_subjects,]
          combine_game = combine_games[first_round_of_exp_subjects:last_round_of_exp_subjects,]
          
          user_id = j + num_users * (exp_id - 1)
          
          if (k == 1) {
            new_row = c(as.numeric (user_id),
                        as.numeric (exp_id),
                        0,
                        0,
                        sum (simple_game[simple_game$Subject == j & simple_game$Type == type,]$Contribution),
                        mean (simple_game[simple_game$Subject == j & simple_game$Type == type & simple_game$send_proportion >= 0,]$send_proportion),
                        sum (simple_game[simple_game$Subject == j & simple_game$Type == type,]$CurrGameProfit),
                        SIMPLE_GAME_ORDERS[exp_id],
                        mean (simple_game[simple_game$Subject == j & simple_game$Type == type,]$response_time)
            )
            df[nrow(df)+1,] <- new_row
          }
          if (k == 2) {
            new_row = c(user_id,
                        exp_id,
                        0,
                        1,
                        sum (id_game[id_game$Subject == j & id_game$Type == type,]$Contribution),
                        mean (id_game[id_game$Subject == j & id_game$Type == type & id_game$send_proportion >= 0,]$send_proportion),
                        sum (id_game[id_game$Subject == j & id_game$Type == type,]$CurrGameProfit),
                        ID_GAME_ORDERS[exp_id],
                        mean (id_game[id_game$Subject == j & id_game$Type == type,]$response_time)
            )
            df[nrow(df)+1,] <- new_row
          }
          if (k == 3) {
            new_row = c(user_id,
                        exp_id,
                        1,
                        0,
                        sum (score_game[score_game$Subject == j & score_game$Type == type,]$Contribution),
                        mean (score_game[score_game$Subject == j & score_game$Type == type & score_game$send_proportion >= 0,]$send_proportion),
                        sum (score_game[score_game$Subject == j & score_game$Type == type,]$CurrGameProfit),
                        SCORE_GAME_ORDERS[exp_id],
                        mean (score_game[score_game$Subject == j & score_game$Type == type,]$response_time)
            )
            df[nrow(df)+1,] <- new_row
          }
          if (k == 4) {
            new_row = c(user_id,
                        exp_id,
                        1,
                        1,
                        sum (combine_game[combine_game$Subject == j & combine_game$Type == type,]$Contribution),
                        mean (combine_game[combine_game$Subject == j & combine_game$Type == type & combine_game$send_proportion >= 0,]$send_proportion),
                        sum (combine_game[combine_game$Subject == j & combine_game$Type == type,]$CurrGameProfit),
                        COMBINE_GAME_ORDERS[exp_id],
                        mean (combine_game[combine_game$Subject == j & combine_game$Type == type,]$response_time)
            )
            df[nrow(df)+1,] <- new_row
          }
        }
      }
    }
    
    df$id <- as.factor(df$id)
    df$GroupID <- as.factor(df$GroupID)
    df$SHOW_TRUST <- as.factor (df$SHOW_TRUST)
    df$SHOW_ID <- as.factor(df$SHOW_ID)
    df$game_pos <- as.factor(df$game_pos)
  }
  
  # calculate regression of showing_trust_score and send_proportion
  
  for (type in 0:1) {
    df_simple <- data.frame ("id" = as.numeric(),
                             "trust_value" = as.numeric(),
                             "RelSend" = as.numeric(),
                             "AbsSend" = as.numeric(),
                             "response_time" = as.numeric(),
                             "game_position" = as.numeric(),
                             "peak_end_trust" = as.numeric(),
                             "AbsPartnerSend" = as.numeric())
    df_id <- data.frame ("id" = as.numeric(),
                         "trust_value" = as.numeric(),
                         "RelSend" = as.numeric(),
                         "AbsSend" = as.numeric(),
                         "response_time" = as.numeric(),
                         "game_position" = as.numeric(),
                         "peak_end_trust" = as.numeric(),
                         "AbsPartnerSend" = as.numeric())
    df_score <- data.frame ("id" = as.numeric(),
                            "trust_value" = as.numeric(),
                            "RelSend" = as.numeric(),
                            "AbsSend" = as.numeric(),
                            "response_time" = as.numeric(),
                            "game_position" = as.numeric(),
                            "peak_end_trust" = as.numeric(),
                            "AbsPartnerSend" = as.numeric())
    df_combine <- data.frame ("id" = as.numeric(),
                              "trust_value" = as.numeric(),
                              "RelSend" = as.numeric(),
                              "AbsSend" = as.numeric(),
                              "response_time" = as.numeric(),
                              "game_position" = as.numeric(),
                              "peak_end_trust" = as.numeric(),
                              "AbsPartnerSend" = as.numeric())
    for (exp_id in 1:num_exp) {
      first_round_of_exp_subjects = (exp_id - 1) * num_rounds_per_game * num_users + 1
      last_round_of_exp_subjects = exp_id * num_rounds_per_game * num_users
      simple_game = simple_games[first_round_of_exp_subjects:last_round_of_exp_subjects,]
      id_game = id_games [first_round_of_exp_subjects:last_round_of_exp_subjects,]
      score_game = score_games [first_round_of_exp_subjects:last_round_of_exp_subjects,]
      combine_game = combine_games[first_round_of_exp_subjects:last_round_of_exp_subjects,]
      for (user_id in 1:num_users) {
        new_row = c((exp_id - 1) * num_users + user_id,
                    mean(simple_game[simple_game$Type == type & simple_game$Subject == user_id,]$showing_trust_score),
                    mean(simple_game[simple_game$Type == type & simple_game$Subject == user_id,]$send_proportion),
                    mean(simple_game[simple_game$Type == type & simple_game$Subject == user_id,]$Contribution),
                    mean(simple_game[simple_game$Type == type & simple_game$Subject == user_id,]$response_time),
                    SIMPLE_GAME_ORDERS[exp_id],
                    mean (calc_peak_end_trust(simple_game[simple_game$Type == type & simple_game$Subject == user_id,]$showing_trust_score)),
                    mean(simple_game[simple_game$Type == type & simple_game$Subject == user_id,]$PartnerDecision)
                    )
        df_simple[nrow(df_simple) + 1,] <- new_row
        new_row = c((exp_id - 1) * num_users + user_id,
                    mean(id_game[id_game$Type == type & id_game$Subject == user_id,]$showing_trust_score),
                    mean(id_game[id_game$Type == type & id_game$Subject == user_id,]$send_proportion),
                    mean(id_game[id_game$Type == type & id_game$Subject == user_id,]$Contribution),
                    mean(id_game[id_game$Type == type & id_game$Subject == user_id,]$response_time),
                    ID_GAME_ORDERS[exp_id],
                    mean(calc_peak_end_trust(id_game[id_game$Type == type & id_game$Subject == user_id,]$showing_trust_score)),
                    mean(id_game[id_game$Type == type & id_game$Subject == user_id,]$PartnerDecision)     
        )
        df_id[nrow(df_id) + 1,] <- new_row
        new_row = c((exp_id - 1) * num_users + user_id,
                    mean(score_game[score_game$Type == type & score_game$Subject == user_id,]$showing_trust_score),
                    mean(score_game[score_game$Type == type & score_game$Subject == user_id,]$send_proportion),
                    mean(score_game[score_game$Type == type & score_game$Subject == user_id,]$Contribution),
                    mean(score_game[score_game$Type == type & score_game$Subject == user_id,]$response_time),
                    SCORE_GAME_ORDERS[exp_id],
                    mean (calc_peak_end_trust(score_game[score_game$Type == type & score_game$Subject == user_id,]$showing_trust_score)),
                    mean(score_game[score_game$Type == type & score_game$Subject == user_id,]$PartnerDecision)      
        )
        df_score[nrow(df_score) + 1,] <- new_row
        new_row = c((exp_id - 1) * num_users + user_id,
                    mean(combine_game[combine_game$Type == type & combine_game$Subject == user_id,]$showing_trust_score),
                    mean(combine_game[combine_game$Type == type & combine_game$Subject == user_id,]$send_proportion),
                    mean(combine_game[combine_game$Type == type & combine_game$Subject == user_id,]$Contribution),
                    mean(combine_game[combine_game$Type == type & combine_game$Subject == user_id,]$response_time),
                    COMBINE_GAME_ORDERS[exp_id],
                    mean (calc_peak_end_trust(combine_game[combine_game$Type == type & combine_game$Subject == user_id,]$showing_trust_score)),
                    mean(combine_game[combine_game$Type == type & combine_game$Subject == user_id,]$PartnerDecision)
        )
        df_combine[nrow(df_combine) + 1,] <- new_row
      }
    }
  }
  
  print ("Group interaction on regression power of sending behavior on two trust scores")
  for (type in 0:1) {
    df_simple <- data.frame ("id" = as.numeric(),
                             "trust_value" = as.numeric(),
                             "RelSend" = as.numeric(),
                             "AbsSend" = as.numeric(),
                             "response_time" = as.numeric(),
                             "game_pos" = as.numeric(),
                             "peak_end_trust" = as.numeric(),
                             "AbsPartnerSend" = as.numeric(),
                             "my_trust_value" = as.numeric(),
                             "r_value" = as.numeric())
    df_id <- data.frame ("id" = as.numeric(),
                         "trust_value" = as.numeric(),
                         "RelSend" = as.numeric(),
                         "AbsSend" = as.numeric(),
                         "response_time" = as.numeric(),
                         "game_pos" = as.numeric(),
                         "peak_end_trust" = as.numeric(),
                         "AbsPartnerSend" = as.numeric(),
                         "my_trust_value" = as.numeric(),
                         "r_value" = as.numeric())
    df_score <- data.frame ("id" = as.numeric(),
                            "trust_value" = as.numeric(),
                            "RelSend" = as.numeric(),
                            "AbsSend" = as.numeric(),
                            "response_time" = as.numeric(),
                            "game_pos" = as.numeric(),
                            "peak_end_trust" = as.numeric(),
                            "AbsPartnerSend" = as.numeric(),
                            "my_trust_value" = as.numeric(),
                            "r_value" = as.numeric())
    df_combine <- data.frame ("id" = as.numeric(),
                              "trust_value" = as.numeric(),
                              "RelSend" = as.numeric(),
                              "AbsSend" = as.numeric(),
                              "response_time" = as.numeric(),
                              "game_pos" = as.numeric(),
                              "peak_end_trust" = as.numeric(),
                              "AbsPartnerSend" = as.numeric(),
                              "my_trust_value" = as.numeric(),
                              "r_value" = as.numeric())
    df_total <- data.frame("id" = as.numeric(),
                           "GroupID" = as.numeric(),
                           "SHOW_TRUST" = as.numeric(),
                           "SHOW_ID" = as.numeric(),
                           "score" = as.numeric())
    for (exp_id in 1:num_exp) {
      first_round_of_exp_subjects = (exp_id - 1) * num_rounds_per_game * num_users + 1
      last_round_of_exp_subjects = exp_id * num_rounds_per_game * num_users
      simple_game = simple_games[first_round_of_exp_subjects:last_round_of_exp_subjects,]
      id_game = id_games [first_round_of_exp_subjects:last_round_of_exp_subjects,]
      score_game = score_games [first_round_of_exp_subjects:last_round_of_exp_subjects,]
      combine_game = combine_games[first_round_of_exp_subjects:last_round_of_exp_subjects,]
      for (user_id in 1:num_users) {
        r1 = 0
        r2 = 0
        r3 = 0
        r4 = 0
        for (partner_id in 1:num_users) {
          if (user_id == partner_id) {next()}
          cur_game <- simple_game
          if (length(cur_game[cur_game$Type == type & cur_game$Subject == user_id & cur_game$Partner == partner_id,]$Contribution) > 0) {
            temp_r <- summary (lm (cur_game[cur_game$Type == type & cur_game$Subject == user_id & cur_game$Partner == partner_id,]$Contribution ~ 
                                     cur_game[cur_game$Type == type & cur_game$Subject == user_id & cur_game$Partner == partner_id,]$showing_trust_score
                                + cur_game[cur_game$Type == (1 - type) & cur_game$Partner == user_id & cur_game$Subject == partner_id,]$showing_trust_score))$r.squared
            temp_r <- sqrt (temp_r)
            if (!is.na(temp_r)) {
              r1 <- temp_r
            }
          }
          
          cur_game <- id_game
          if (length(cur_game[cur_game$Type == type & cur_game$Subject == user_id & cur_game$Partner == partner_id,]$Contribution) > 0) {
            temp_r <- summary (lm (cur_game[cur_game$Type == type & cur_game$Subject == user_id & cur_game$Partner == partner_id,]$Contribution ~ 
                                     cur_game[cur_game$Type == type & cur_game$Subject == user_id & cur_game$Partner == partner_id,]$showing_trust_score
                                   + cur_game[cur_game$Type == (1 - type) & cur_game$Partner == user_id & cur_game$Subject == partner_id,]$showing_trust_score))$r.squared
            temp_r <- sqrt (temp_r)
            if (!is.na(temp_r)) {
              r2 <- temp_r
            }
          }
          
          cur_game <- score_game
          if (length(cur_game[cur_game$Type == type & cur_game$Subject == user_id & cur_game$Partner == partner_id,]$Contribution) > 0) {
            temp_r <- summary (lm (cur_game[cur_game$Type == type & cur_game$Subject == user_id & cur_game$Partner == partner_id,]$Contribution ~ 
                                     cur_game[cur_game$Type == type & cur_game$Subject == user_id & cur_game$Partner == partner_id,]$showing_trust_score
                                   + cur_game[cur_game$Type == (1 - type) & cur_game$Partner == user_id & cur_game$Subject == partner_id,]$showing_trust_score))$r.squared
            temp_r <- sqrt (temp_r)
            if (!is.na(temp_r)) {
              r3 <- temp_r
            }
          }
          
          cur_game <- combine_game
          if (length(cur_game[cur_game$Type == type & cur_game$Subject == user_id & cur_game$Partner == partner_id,]$Contribution) > 0) {
            temp_r <- summary (lm (cur_game[cur_game$Type == type & cur_game$Subject == user_id & cur_game$Partner == partner_id,]$Contribution ~ 
                                     cur_game[cur_game$Type == type & cur_game$Subject == user_id & cur_game$Partner == partner_id,]$showing_trust_score
                                   + cur_game[cur_game$Type == (1 - type) & cur_game$Partner == user_id & cur_game$Subject == partner_id,]$showing_trust_score))$r.squared
            temp_r <- sqrt (temp_r)
            if (!is.na(temp_r)) {
              r4 <- temp_r
            }
          }
        }
        new_row = c((exp_id - 1) * num_users + user_id,
                    mean(simple_game[simple_game$Type == type & simple_game$Subject == user_id,]$showing_trust_score),
                    mean(simple_game[simple_game$Type == type & simple_game$Subject == user_id,]$send_proportion),
                    mean(simple_game[simple_game$Type == type & simple_game$Subject == user_id,]$Contribution),
                    mean(simple_game[simple_game$Type == type & simple_game$Subject == user_id,]$response_time),
                    SIMPLE_GAME_ORDERS[exp_id],
                    mean (calc_peak_end_trust(simple_game[simple_game$Type == type & simple_game$Subject == user_id,]$showing_trust_score)),
                    mean(simple_game[simple_game$Type == type & simple_game$Subject == user_id,]$PartnerDecision),
                    mean(simple_game[simple_game$Type == (1  - type) & simple_game$Partner == user_id,]$showing_trust_score),
                    r1
        )
        df_simple[nrow(df_simple) + 1,] <- new_row
        df_total[nrow(df_total) + 1,] <- c(user_id * num_users * (exp_id - 1),
                                           exp_id,
                                           0,
                                           0,
                                           r1)
        
        new_row = c((exp_id - 1) * num_users + user_id,
                    mean(id_game[id_game$Type == type & id_game$Subject == user_id,]$showing_trust_score),
                    mean(id_game[id_game$Type == type & id_game$Subject == user_id,]$send_proportion),
                    mean(id_game[id_game$Type == type & id_game$Subject == user_id,]$Contribution),
                    mean(id_game[id_game$Type == type & id_game$Subject == user_id,]$response_time),
                    ID_GAME_ORDERS[exp_id],
                    mean(calc_peak_end_trust(id_game[id_game$Type == type & id_game$Subject == user_id,]$showing_trust_score)),
                    mean(id_game[id_game$Type == type & id_game$Subject == user_id,]$PartnerDecision),
                    mean(id_game[id_game$Type == (1  - type) & id_game$Partner == user_id,]$showing_trust_score),
                    r2
        )
        df_id[nrow(df_id) + 1,] <- new_row
        df_total[nrow(df_total) + 1,] <- c(user_id * num_users * (exp_id - 1),
                                           exp_id,
                                           0,
                                           1,
                                           r2)
        
        new_row = c((exp_id - 1) * num_users + user_id,
                    mean(score_game[score_game$Type == type & score_game$Subject == user_id,]$showing_trust_score),
                    mean(score_game[score_game$Type == type & score_game$Subject == user_id,]$send_proportion),
                    mean(score_game[score_game$Type == type & score_game$Subject == user_id,]$Contribution),
                    mean(score_game[score_game$Type == type & score_game$Subject == user_id,]$response_time),
                    SCORE_GAME_ORDERS[exp_id],
                    mean (calc_peak_end_trust(score_game[score_game$Type == type & score_game$Subject == user_id,]$showing_trust_score)),
                    mean(score_game[score_game$Type == type & score_game$Subject == user_id,]$PartnerDecision),
                    mean(score_game[score_game$Type == (1  - type) & score_game$Partner == user_id,]$showing_trust_score),
                    r3
        )
        df_score[nrow(df_score) + 1,] <- new_row
        df_total[nrow(df_total) + 1,] <- c(user_id * num_users * (exp_id - 1),
                                           exp_id,
                                           1,
                                           0,
                                           r3)
        
        new_row = c((exp_id - 1) * num_users + user_id,
                    mean(combine_game[combine_game$Type == type & combine_game$Subject == user_id,]$showing_trust_score),
                    mean(combine_game[combine_game$Type == type & combine_game$Subject == user_id,]$send_proportion),
                    mean(combine_game[combine_game$Type == type & combine_game$Subject == user_id,]$Contribution),
                    mean(combine_game[combine_game$Type == type & combine_game$Subject == user_id,]$response_time),
                    COMBINE_GAME_ORDERS[exp_id],
                    mean (calc_peak_end_trust(combine_game[combine_game$Type == type & combine_game$Subject == user_id,]$showing_trust_score)),
                    mean(combine_game[combine_game$Type == type & combine_game$Subject == user_id,]$PartnerDecision),
                    mean(combine_game[combine_game$Type == (1  - type) & combine_game$Partner == user_id,]$showing_trust_score),
                    r4
        )
        df_combine[nrow(df_combine) + 1,] <- new_row
        df_total[nrow(df_total) + 1,] <- c(user_id * num_users * (exp_id - 1),
                                           exp_id,
                                           1,
                                           1,
                                           r4)
      }
    }
    df_total$id <- as.factor (df_total$id)
    df_total$GroupID <- as.factor (df_total$GroupID)
    df_total$SHOW_TRUST <- as.factor (df_total$SHOW_TRUST)
    df_total$SHOW_ID <- as.factor (df_total$SHOW_ID)
    
    print ("ANOVA analysis of regression power on sending behavior on 2 trust scores on Group:SHOW_TRUST:SHOW_ID")
    anova_analysis(df_total, type = type, num_way = 3)
  }
  
  for (type in 0:1) {
    df_simple = data.frame("id" = as.numeric(),
                           "GroupID" = as.numeric(),
                           "SHOW_TRUST" = as.numeric(),
                           "SHOW_ID" = as.numeric(),
                           "R_value" = as.numeric())
    df_id = data.frame("id" = as.numeric(),
                       "GroupID" = as.numeric(),
                       "SHOW_TRUST" = as.numeric(),
                       "SHOW_ID" = as.numeric(),
                       "R_value" = as.numeric())
    df_score = data.frame("id" = as.numeric(),
                          "GroupID" = as.numeric(),
                          "SHOW_TRUST" = as.numeric(),
                          "SHOW_ID" = as.numeric(),
                          "R_value" = as.numeric())
    df_combine = data.frame("id" = as.numeric(),
                            "GroupID" = as.numeric(),
                            "SHOW_TRUST" = as.numeric(),
                            "SHOW_ID" = as.numeric(),
                            "R_value" = as.numeric())
    for (exp_id in 1:num_exp) {
#       if (exp_id == 3) {
#         next()
#       }
      first_round_of_exp_subjects = (exp_id - 1) * num_rounds_per_game * num_users + 1
      last_round_of_exp_subjects = exp_id * num_rounds_per_game * num_users
      
      simple_game = simple_games[first_round_of_exp_subjects:last_round_of_exp_subjects,]
      id_game = id_games [first_round_of_exp_subjects:last_round_of_exp_subjects,]
      score_game = score_games [first_round_of_exp_subjects:last_round_of_exp_subjects,]
      combine_game = combine_games[first_round_of_exp_subjects:last_round_of_exp_subjects,]
      
      # add up all partner decision to 1 unit to remove the NaN problem of R squared
      simple_game$PartnerDecision = simple_game$PartnerDecision + 0
      id_game$PartnerDecision = id_game$PartnerDecision + 0
      score_game$PartnerDecision = score_game$PartnerDecision + 0
      combine_game$PartnerDecision = combine_game$PartnerDecision + 0
      
      for (user_id in 1:num_users) {
        # cur_data <- simple_game[simple_game$Type ==type & simple_game$Partner == user_id & simple_game$send_proportion >= 0,]
        cur_data <- simple_game[simple_game$Type ==type & simple_game$Subject == user_id & simple_game$send_proportion >= 0,]
        new_row <- c((exp_id - 1) * num_users + user_id,
                     exp_id,
                     0,
                     0,
                     #summary(lm(PartnerDecision ~ showing_trust_score, data = simple_game[simple_game$Type ==type & simple_game$Partner == user_id,]))$r.squared
                     # cor(cur_data$PartnerDecision, cur_data$showing_trust_score)
                     cor(cur_data$Contribution, cur_data$showing_trust_score)
        )
        df_simple [nrow(df_simple) + 1,] <- new_row
#         if (type == 1 & user_id == 2 & exp_id == 2) {
#           print (cur_data)
#         }
        
        # cur_data <- id_game[id_game$Type ==type & id_game$Partner == user_id & id_game$send_proportion >= 0,]
        cur_data <- id_game[id_game$Type ==type & id_game$Subject == user_id & id_game$send_proportion >= 0,]
        new_row <- c((exp_id - 1) * num_users + user_id,
                     exp_id,
                     0,
                     1,
                     # summary(lm(PartnerDecision ~ showing_trust_score, data = id_game[id_game$Type ==type & id_game$Partner == user_id,]))$r.squared
                     # cor(cur_data$PartnerDecision, cur_data$showing_trust_score)
                     cor(cur_data$Contribution, cur_data$showing_trust_score)
        )
        df_id [nrow(df_id) + 1,] <- new_row
        
        # cur_data <- score_game[score_game$Type ==type & score_game$Partner == user_id & score_game$send_proportion >= 0,]
        cur_data <- score_game[score_game$Type ==type & score_game$Subject == user_id & score_game$send_proportion >= 0,]
        new_row <- c((exp_id - 1) * num_users + user_id,
                     exp_id,
                     1,
                     0,
                     # summary(lm(PartnerDecision ~ showing_trust_score, data = score_game[score_game$Type ==type & score_game$Partner == user_id,]))$r.squared
                     # cor(cur_data$PartnerDecision, cur_data$showing_trust_score)
                     cor(cur_data$Contribution, cur_data$showing_trust_score)
        )
        df_score [nrow(df_score) + 1,] <- new_row
        # print (summary(lm(PartnerDecision ~ showing_trust_score, data = id_game[id_game$Type ==type & id_game$Partner == user_id,]))$r.squared)
        # print (cor(score_game[score_game$Type ==type & score_game$Partner == user_id,]$PartnerDecision, score_game[score_game$Type ==type & score_game$Partner == user_id,]$showing_trust_score))
        
        # cur_data <- combine_game[combine_game$Type ==type & combine_game$Partner == user_id & combine_game$send_proportion >= 0,]
        cur_data <- combine_game[combine_game$Type ==type & combine_game$Subject == user_id & combine_game$send_proportion >= 0,]
        new_row <- c((exp_id - 1) * num_users + user_id,
                     exp_id,
                     1,
                     1,
                     # summary(lm(PartnerDecision ~ showing_trust_score, data = combine_game[combine_game$Type ==type & combine_game$Partner == user_id,]))$r.squared
                     # cor(cur_data$PartnerDecision, cur_data$showing_trust_score)
                     cor(cur_data$Contribution, cur_data$showing_trust_score)
        )
        df_combine [nrow(df_combine) + 1,] <- new_row
        # print (summary(lm(PartnerDecision ~ showing_trust_score, data = combine_game[combine_game$Type ==type & combine_game$Partner == user_id,]))$r.squared)
        # print (cor(combine_game[combine_game$Type ==type & combine_game$Partner == user_id,]$PartnerDecision, combine_game[combine_game$Type ==type & combine_game$Partner == user_id,]$showing_trust_score))
      }
    }
    df_total <- rbind (df_simple, df_id)
    df_total <- rbind (df_total, df_score)
    df_total <- rbind (df_total, df_combine)
    df_total$id <- as.factor (df_total$id)
    df_total$GroupID <- as.factor (df_total$GroupID)
    df_total$SHOW_TRUST <- as.factor (df_total$SHOW_TRUST)
    df_total$SHOW_ID <- as.factor (df_total$SHOW_ID)
    # df_total$score <- sqrt(df_total$R_value)
    df_total$score <- df_total$R_value
    if (any(is.na(df_total$score))) {
      df_total[is.na(df_total$score),]$score <- 0
    }
    # print (df_total)
    df_total$R_value <- NULL
    
    if (any (is.na(df_simple$R_value))) {
      df_simple[is.na(df_simple$R_value),]$R_value <- 0
    }
    if (any (is.na(df_id$R_value))) {
      df_id[is.na(df_id$R_value),]$R_value <- 0
    }
    if (any (is.na(df_score$R_value))) {
      df_score[is.na(df_score$R_value),]$R_value <- 0
    }
    if (any (is.na(df_combine$R_value))) {
      df_combine[is.na(df_combine$R_value),]$R_value <- 0
    }
    
  }
  
  
  print ("---")
  # Create graphs of R_value
  for (type in 0:1) {
    if (type == 1) {next()}
    for (game_id in 1:4) {
      cur_data <- NULL
      if (game_id == 1) {
        cur_data <- simple_games[simple_games$Type == type,]
      }
      if (game_id == 2) {
        cur_data <- id_games[id_games$Type == type,]
      }
      if (game_id == 3) {
        cur_data <- score_games[score_games$Type == type,]
      }
      if (game_id == 4) {
        cur_data <- combine_games[combine_games$Type == type,]
      }
      png (paste("./all_data/R_value_Game_",game_id,"_type_", Type_names [(type + 1)],".png"))
      ys = c (1,2,3,4,5)
      xs = numeric()
      
      # to store confidence interval
      lowers = numeric()
      uppers = numeric()
      
      all_r = numeric ()
      all_grp_id = numeric ()
      for (exp_id in 1:num_exp) {
        num_per_rounds = num_users * (num_users - 1) * average_rounds / 2
        first_round = num_per_rounds * (exp_id - 1) + 1
        last_round = num_per_rounds * exp_id
        sub_data = cur_data[first_round:last_round,]
        
        x <- 0
        lower <- 0
        upper <- 0
        
        rs <- numeric()
        
        for (user_id in 1:num_users) {
          user_data = sub_data[sub_data$Subject == user_id & sub_data$send_proportion >= 0,]
          
          # r = cor (user_data$PartnerDecision, user_data$showing_trust_score)
          r = user_data$Contribution
          if (is.na(r)) {
            r = 0
          }
          rs <- c(rs, r)
          all_r <- c (all_r, r)
          for (i in 1:length(r)) {
            all_grp_id <- c(all_grp_id, exp_id)
          }
        }
        
        x = mean (rs)
        lower = x - pop.sd (rs)
        upper = x + pop.sd (rs)
        
        if (is.na(x)) {
          x = 0
        }
        xs <- c(xs, x)
        if (is.na(lower)) {
          lower = 0
        }
        if (is.na(upper)) {
          upper = 0
        }
        lowers <- c(lowers, lower)
        uppers <- c(uppers, upper)
      }
      if (type == 0) {
        # Do Tukey test
        print (paste ("Absolute sending - Game: ", game_id, " for SENDERs"))
        print ("*********")
#         print (all_r)
#         print (all_grp_id)
        aov1 <- aov (lm (all_r ~ as.factor(all_grp_id)))
        # print (TukeyHSD(aov1))
      }
      plot(xs, main = paste("r_value of Game ", game_id, " type: ", Type_names [(type + 1)]), xlab = "Group ID", ylab = "r value", ylim = c(0,15))
      for (exp_id in 1:num_exp) {
        segments (x0 = exp_id, y0 = lowers[exp_id], x1 = exp_id, y1 = uppers[exp_id])
        epsilon <- 0.02
        segments (x0 = exp_id - epsilon, y0 = lowers[exp_id], x1 = exp_id + epsilon, y1 = lowers[exp_id])
        segments (x0 = exp_id - epsilon, y0 = uppers[exp_id], x1 = exp_id + epsilon, y1 = uppers[exp_id])
      }
      dev.off()
    }
  }
    
  df
}

processMultiSBJ <- function (directory, num_users = 6, average_rounds = 5, num_games = 4) {
  # source("http://www.kirchkamp.de/lab/zTree.R")
  sessions <- sessions<-list.files(directory,"[0-9]{6}_[0-9]{4}.sbj",recursive=TRUE, full.names = TRUE)
  SBJs <- zTreeSbj (files = sessions, zTree.silent=TRUE)
  
  # You are sender, what game you receive back most, with same amount of sending
  SBJs$receive_back_most <- as.factor(SBJs$receive_back_most)
  
  SBJs$best_personal <- as.factor(SBJs$best_personal)
  SBJs$worst_personal <- as.factor(SBJs$worst_personal)
  SBJs$best_total <- as.factor(SBJs$best_total)
  SBJs$worst_total <- as.factor(SBJs$worst_total)
  
  # In Simple Game, profit is higher if you send more
  SBJs$send_more_for_profit <- as.numeric(SBJs$send_more_for_profit)
  # In Simple Game, profit is higher if you send less
  SBJs$send_less_for_profit <- as.numeric(SBJs$send_less_for_profit)
  # In Simple Game, you are receive, you send back more if your sender send more
  SBJs$trust_help_receiver <- as.numeric(SBJs$trust_help_receiver)
  
  # Show ID and Score help to realize behavior of partners in the history
  SBJs$show_id_help <- as.numeric(SBJs$show_id_help)
  SBJs$show_score_help <- as.numeric(SBJs$show_score_help)
  SBJs$show_combine_help <- as.numeric(SBJs$show_combine_help)
  
  # In Combine game, trust score reflects correct behavior of the partner
  SBJs$trust_score_correctness <- as.numeric(SBJs$trust_score_correctness)
  
  # In Score game, you send more if your partner has higher trust score
  SBJs$trust_help_sender <- as.numeric(SBJs$trust_help_sender)
  
  # Showing ID and score help you decide how to behave
  SBJs$identity_help_decide <- as.numeric(SBJs$identity_help_decide)
  SBJs$trust_score_help_decide <- as.numeric(SBJs$trust_score_help_decide)
  SBJs$combine_help_decide <- as.numeric(SBJs$combine_help_decide)
  
  # What factor is most important
  SBJs$important_factor <- as.factor(SBJs$important_factor)
  
  SBJs$partner_fair <- as.numeric(SBJs$partner_fair)
  SBJs
}
