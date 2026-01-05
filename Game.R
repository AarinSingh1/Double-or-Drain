#Game . R

library(tidyverse)


play_game = function(p = .6, target = 64){
  if(!is.numeric(p) || length(p) != 1 || p > 1 || p < 0 || is.na(p)){
    stop("Invalid response type. Try the game again. P must be a value in between 0 and 1 inclusive")
  }
  cat(str_glue(
    "Win probability each round: {p}\n",
    "Lose probability each round: {1 - p}\n"
  ))
  
  money = 1
  ev_hint = FALSE
  repeat{
    cat("\nYour current balance is", money, "dollars\n")
    
    if (ev_hint) {
      if (money >= target) {
        cat("You are at/above your desired target of ", target, ". Pressing B locks it in.\n")
      } else {
        k <- log2(target / money)          # integer in your game
        prob_reach <- p^k                  # chance to reach target before busting
        cat(str_glue(
          "P(reach {target} before busting) = {round(prob_reach, 4)}\n",
          "Median rule (aim for > 0.5): {ifelse(prob_reach >= 0.5, 'DOUBLE', 'BANK')}\n"
        ))
      }
    }
    
    choice = toupper(trimws(readline("[D] to double or [B] to bank or [E] to get and EV Hint: ")))
    
    
    while (!(choice %in% c("D", "B", "E"))) {
      warning("invalid answer choice")
      choice = toupper(trimws(readline("[D] to double or [B] to bank or [E] to get and EV Hint: ")))
    }
    
    if (choice == "E"){
      ev_hint = !ev_hint
      cat(str_glue("[D] Double  [B] Bank  [E] EV Hint ({ifelse(ev_hint, 'ON', 'OFF')})\n"))
      next }
    
    
    if (choice == "B"){
      message(paste("The game is over, you finished with :", money, "dollars"))
      break
    }
    
    else {
      x = runif(1,0,1)
      if (x < p){
        money = money * 2
      }
      else {
        money = 0
        message(paste("YOU LOST YOU LOSER. The game is over, you finished with : ", money, "dollars"))
        break
      }
    }
  }
}