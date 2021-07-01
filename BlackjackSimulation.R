# Blackjack Simulation
# Written by Andrew Cate


 # Writing Shuffle, Checksplit, dealerHit, and Think functions

shuffle <- function(shoe) {
  newdeck <- sample(shoe,length(shoe))
  return(newdeck)
}

checksplit <- function(hands,dhand) {
  for (i in 1:nrow(hands)) {
    hand <- c(hands[i,1],hands[i,2])
    if(think(hand,dhand) == "Split"){return(c(TRUE,i))}
    return("")
  }
}

# Does dealer hit?
dealerHit <- function(hand){
  if((sum(hand) < 17) | ((sum(hand) == 17) & (11 %in% hand))){
    return("Hit")
  }
  return("")
}


# Decision making time
think <- function(phand,dhand) {
  
  
  card <- dhand[1]
  hand <- sum(phand)
  
  
  
  
  # Strategy Based on https://wizardofodds.com/blackjack/images/bj_4d_h17.gif
  
  # If Player does not have ace and cannot split
  if(((11 %in% hand) == FALSE) &  (phand[1] != phand[2]) ){
    
    # If Player Should Hit
    if(
      (hand >= 4 & hand <= 8) |
      ((hand == 9) & (card == 2 | card >= 7)) |
      ((hand == 10) & (card >= 10)) |
      ((hand == 12) & ((card >= 2 & card < 4) | (card >= 7))) |
      (((hand == 13) | (hand == 14)) & (card >= 7)) |
      ((hand == 15) & (card >= 7)) |
      ((hand == 16) & (card >= 7))
    ){return("Hit")}
    
    # If Player Should Stay
    if(
      (hand >= 18) |
      (hand == 17) |
      (((hand == 16) | (hand == 15) | (hand == 14) | (hand == 13)) & (card <= 6)) |
      ((hand == 12) & ((card >= 4) & (card <= 6)))
    ){return("Stay")}
    
    # If Player Should Double Down
    if(length(phand) == 2){
      if(
        ((hand == 9) & ((card >= 3) & (card <= 6))) |
        ((hand == 10) & (card <= 9)) |
        (hand == 11)
      ){return("Double")}
    }
    
    # Player can't double down, should hit
    if(length(phand) != 2){
      if(
        ((hand == 9) & ((card >= 3) & (card <= 6))) |
        ((hand == 10) & (card <= 9)) |
        (hand == 11)
      ){return("Hit")}
    }
  } # End if player does not have an ace, cannot split
  
  
  
  
  
  
  
  # If player has double 5s or double 10s, strategy is the same as above...
  if( (phand[1] == 5 & phand[2] == 5) | (phand[1] == 10 & phand[2] == 10)){
    
    # If Player Should Hit
    if(
      (hand >= 4 & hand <= 8) |
      ((hand == 9) & (card == 2 | card >= 7)) |
      ((hand == 10) & (card >= 10)) |
      ((hand == 12) & ((card >= 2 & card < 4) & (card >= 7))) |
      (((hand == 13) | (hand == 14)) & (card >= 7)) |
      ((hand == 15) & (card >= 7)) |
      ((hand == 16) & (card >= 7))
    ){return("Hit")}
    
    # If Player Should Stay
    if(
      (hand >= 18) |
      (hand == 17) |
      (((hand == 16) | (hand == 15) | (hand == 14) | (hand == 13)) & (card <= 6)) |
      ((hand == 12) & ((card >= 4) & (card <= 6)))
    ){return("Stay")}
    
    # If Player Should Double Down
    if(length(phand) == 2){
      if(
        ((hand == 9) & ((card >= 3) & (card <= 6))) |
        ((hand == 10) & (card <= 9)) |
        (hand == 11)
      ){return("Double")}
    }
    
    # Player can't double down, should hit
    if(length(phand) != 2){
      if(
        ((hand == 9) & ((card >= 3) & (card <= 6))) |
        ((hand == 10) & (card <= 9)) |
        (hand == 11)
      ){return("Hit")}
    }
    
  } # End if 55 or 1010 
  
  
  
  
  
  
  
  # If Player has doubles
  if(phand[1] == phand[2]){
    
    # If Player Should Hit
    if(
      ((hand < 16) & (card > 7)) |
      ((hand == 8) & (card %in% c(2,3,4,7,8,9,10,11))) |
      ((hand == 12) & (card == 7))
    ){return("Hit")}
    
    # If Player Should Stay
    if(
      ((hand == 18) & (card %in% c(7,10,11)))
    ){return("Stay")}
    
    # If Player Should Split
    if(
      ((hand == 22)) |
      ((hand == 18) & (card %in% c(2,3,4,5,6,8,9))) |
      ((hand == 16)) |
      ((hand == 14) & (card < 8)) |
      ((hand == 12) & (card < 7)) |
      ((hand %in% c(6,4)) & (card < 8)) |
      ((hand == 8) & (card %in% c(5,6)))
    ){return("Split")}
    
    # Player can't double down, should hit
    if(length(phand) != 2){
      if(
        ((hand == 9) & ((card >= 3) & (card <= 6))) |
        ((hand == 10) & (card <= 9)) |
        (hand == 11)
      ){return("Hit")}
    }
    
  } # End if player has doubles
  
  
  
  
  
  
  
  ####################################################################################
  ### PLAYER IS SOFT #################################################################
  ####################################################################################
  
  
  # If Player should Hit
  if(
    ((hand < 15) & ((card %in% c(5,6)) == FALSE)) |
    ((hand %in% c(15,16)) & (card < 4 | card > 6)) |
    ((hand == 17) & ((card == 2) | (card > 6))) |
    ((hand == 18) & (card > 8))
  ){return("Hit")}
  
  # If Player Should Stay
  if((hand >= 20) |
     ((hand == 19) & (card != 6)) |
     ((hand == 18) & card %in% c(7,8))
  ){return("Stay")}
  
  # If Player Should Double Down
  if(
    ((hand %in% c(13,14)) & (card %in% c(5,6))) |
    ((hand %in% c(15,16)) & (card %in% c(4,5,6))) |
    ((hand == 17) & (card %in% c(3,4,5,6))) |
    ((hand == 18) & (card %in% c(2,3,4,5,6))) |
    ((hand == 19) & (card == 6))
  ){return("Double")}
  
  return("")
} # end think()






# Begin Playing Blackjack










playBlackjack <- function(numHands = 50,decks = 8,shuffThresh = 100) {
  
  
  numdecks <- decks
  shoe <- c(rep(2,4*numdecks),rep(3,4*numdecks),rep(4,4*numdecks),rep(5,4*numdecks),rep(6,4*numdecks),rep(7,4*numdecks),rep(8,4*numdecks),rep(9,4*numdecks),rep(10,16*numdecks),rep(11,4*numdecks))
  playdeck <- shuffle(shoe)
  shufflethresh <- shuffThresh
  
  # Matrix to hold multiple split hands
  SPLIT <- matrix(0, nrow = 2, ncol = 11)
  doubledSplit <- c()
  
  hands <- numHands
  
  # Creating Outcomes data frame to Collect Data
  
  outcomes <- matrix(0, nrow = hands, ncol = 18)
  outcomes <- data.frame(outcomes)
  
  names(outcomes)[1] <- "DealerBlackjack"
  names(outcomes)[2] <- "PlayerBlackjack"
  names(outcomes)[3] <- "PlayerSplit"
  names(outcomes)[4] <- "DoubleDown"
  names(outcomes)[5] <- "PlayerBust"
  names(outcomes)[6] <- "DealerBust"
  names(outcomes)[7] <- "PlayerWin"
  names(outcomes)[8] <- "DealerWin"
  names(outcomes)[9] <- "PlayerCard1"
  names(outcomes)[10] <- "PlayerCard2"
  names(outcomes)[11] <- "DealerShowCard"
  names(outcomes)[12] <- "DealerHiddenCard"
  names(outcomes)[13] <- "Shuffle"
  names(outcomes)[14] <- "NumPlayerHits"
  names(outcomes)[15] <- "NumDealerHits"
  names(outcomes)[16] <- "Push"
  names(outcomes)[17] <- "DoubledWin"
  names(outcomes)[18] <- "DoubledLoss"
  
  player <- 0
  dealer <- 0
  outcomes[1:hands,1:ncol(outcomes)] <- 0
  playdeck <- shuffle(shoe)
  
  
  # Begin Simulation ---------------------------------------------------------------------------------------------
  
  for (j in 1:hands) {
    
    # Shuffle when there are fewer than shufflethresh cards left in the deck
    if(length(playdeck) < shufflethresh){playdeck <- shuffle(shoe) ; outcomes[j,13] <- 1}
    
    # Dealing the Player and Dealer two cards
    dealerHand <- playdeck[1:2]
    playerHand <- playdeck[3:4]
    
    # Adding dealt cards to data collection
    outcomes[j,9] <- playdeck[3]
    outcomes[j,10] <- playdeck[4]
    outcomes[j,11] <- playdeck[1]
    outcomes[j,12] <- playdeck[2]
    
    # Removing Dealt Cards
    playdeck <- playdeck[-c(1:4)]
    
    # Read Player Hand
    sum(playerHand)
    
    # Read Dealer Hand
    sum(dealerHand)
    
    # Dealer Black Jack Wins
    if(sum(dealerHand) == 21 & sum(playerHand) != 21){
      dealer <- dealer + 1
      outcomes[j,1] <- 1
      outcomes[j,8] <- outcomes[j,8] + 1
      next
    }
    
    # Player Black Jack Wins
    if(sum(playerHand) == 21 & sum(dealerHand) != 21){
      player <- player + 1
      outcomes[j,2] <- 1
      outcomes[j,7] <- outcomes[j,7] + 1
      next
    }
    
    
    
    # If they Hit
    while(think(playerHand,dealerHand) == "Hit"){
      outcomes[j,14] <- outcomes[j,14] + 1
      playerHand <- c(playerHand, playdeck[1])
      playdeck <- playdeck[-1]
      
      # Did they bust and have an Ace?
      if((sum(playerHand) > 21) & (11 %in% playerHand)){
        playerHand[which(playerHand == 11)[1]] <- 1
      }
    }
    
    # If they split
    
    if(length(playerHand) == 2){
      if(think(playerHand,dealerHand) == "Split"){
        
        # Adding this outcome
        outcomes[j,3] <- 1
        
        # Declaring a matrix to hold 11 possible split hands
        SPLIT <- matrix(0, nrow = 2, ncol = 11)
        
        # Dealing cards to split hands
        outcomes[j,14] <- outcomes[j,14] + 2
        SPLIT[1,1] <- playerHand[1]
        SPLIT[2,1] <- playerHand[2]
        SPLIT[1,2] <- playdeck[1]
        SPLIT[2,2] <- playdeck[2]
        playdeck <- playdeck[c(-1,-2)]
        
        # Splitting until it is no longer recommended
        while (checksplit(SPLIT,dealerHand)[1] == 1) {
          splitrow <- checksplit(SPLIT,dealerHand)[2]
          SPLIT <- rbind(SPLIT,c(SPLIT[splitrow,1],playdeck[1],rep(0,9)))
          SPLIT[splitrow,2] <- playdeck[2]
          playdeck <- playdeck[c(-1,-2)]
          outcomes[j,3] <- outcomes[j,3] + 1
          outcomes[j,14] <- outcomes[j,14] + 2
        }
        
        # Iterating through the number of split hands to deal new cards
        for (i in 1:nrow(SPLIT)) {
          count <- 3
          while((think(SPLIT[i,],dealerHand)) == "Hit"){
            outcomes[j,14] <- outcomes[j,14] + 1
            SPLIT[i,count] <- playdeck[1]
            playdeck <- playdeck[-1]
            count <- count + 1
            
            # Did they bust and have an Ace?
            if((sum(SPLIT[i,]) > 21) & (11 %in% SPLIT[i,])){
              SPLIT[i,which(SPLIT[1,] == 11)[1]] <- 1
            }
          }
          if((think(SPLIT[i,],dealerHand)) == "Double"){
            SPLIT[i,count] <- playdeck[1]
            playdeck <- playdeck[-1]
            outcomes[j,4] <- outcomes[j,4] + 1
            outcomes[j,14] <- outcomes[j,14] + 1
            doubledSplit <- c(doubledSplit,i)
          }
        }
      }
    }
    
    # If they double down
    if(think(playerHand,dealerHand) == "Double"){
      playerHand <- c(playerHand, playdeck[1])
      playdeck <- playdeck[-1]
      outcomes[j,4] <- 1
      outcomes[j,14] <- outcomes[j,14] + 1
    }
    
    
    
    # Did they bust?
    if(sum(playerHand) > 21 & (11 %in% playerHand == FALSE)){
      dealer <- dealer + 1
      outcomes[j,5] <- 1
      outcomes[j,8] <- outcomes[j,8] + 1
      # Did they double and bust?
      if(outcomes[j,4] > 0){outcomes[j,18] <- 1}
      next
    }
    
    # Split hands
    
    if(SPLIT[1,1] != 0){
      
      bust <- 0
      # Did they split and bust?
      for (i in 1:nrow(SPLIT)) {
        if(sum(SPLIT[i,]) > 21){
          dealer <- dealer + 1
          outcomes[j,5] <- outcomes[j,5] + 1
          outcomes[j,8] <- outcomes[j,8] + 1
          bust <- bust + 1
          # Did they split, then double, then bust?
          if((outcomes[j,4] > 0) & (i %in% doubledSplit)){outcomes[j,18] <- outcomes[j,18] + 1}
        }
      }
      
      # Did a split player bust on all hands?
      if(bust == nrow(SPLIT)){next}
    }
    
    while (dealerHit(dealerHand) == "Hit") {
      outcomes[j,15] <- outcomes[j,15] + 1
      dealerHand <- c(dealerHand,playdeck[1])
      playdeck <- playdeck[-1]
      
      # Did dealer bust and have an Ace?
      if((sum(dealerHand) > 21) & (11 %in% dealerHand)){
        dealerHand[which(dealerHand == 11)[1]] <- 1
      }
    }
    
    # Did Dealer bust?
    if((sum(dealerHand) > 21) & (SPLIT[1,1] == 0)){
      player <- player + 1
      outcomes[j,7] <- 1
      outcomes[j,6] <- 1
      # Did player double?
      if(outcomes[j,4] > 0){outcomes[j,17] <- outcomes[j,17] + 1}
    }
    
    if(SPLIT[1,1] != 0){
      for (i in 1:nrow(SPLIT)) {
        
        # Dealer busts
        if((sum(dealerHand) > 21) & (sum(SPLIT[i,]) < 22)){player <- player + 1 ; outcomes[j,7] <- outcomes[j,7] + 1 ; outcomes[j,6] <- outcomes[j,6]}
        
        # No one busts, dealer wins!
        if((sum(SPLIT[i,]) < sum(dealerHand)) & ((sum(dealerHand) < 22) & (sum(SPLIT[i,]) < 22))){dealer <- dealer + 1 ; outcomes[j,8] <- outcomes[j,8] + 1}
        
        # No one busts, player wins!
        if((sum(dealerHand) < sum(SPLIT[i,])) & ((sum(dealerHand) < 22) & (sum(SPLIT[i,]) < 22))){player <- player + 1 ; outcomes[j,7] <- outcomes[j,7] + 1}
        
        # Did they double?
        if(outcomes[j,4] > 0 & (i %in% doubledSplit)){
          # Dealer busts
          if((sum(dealerHand) > 21) & (sum(SPLIT[i,]) < 22)){outcomes[j,17] <- outcomes[j,17] + 1}
          
          # No one busts, dealer wins!
          if((sum(SPLIT[i,]) < sum(dealerHand)) & ((sum(dealerHand) < 22) & (sum(SPLIT[i,]) < 22))){outcomes[j,18] <- outcomes[j,18] + 1}
          
          # No one busts, player wins!
          if((sum(dealerHand) < sum(SPLIT[i,])) & ((sum(dealerHand) < 22) & (sum(SPLIT[i,]) < 22))){outcomes[j,17] <- outcomes[j,17] + 1}
        }
        
        # Did they push?
        if(sum(dealerHand) == sum(SPLIT[i,])){outcomes[j,16] <- outcomes[j,16] + 1}
      }
      # reset SPLIT
      SPLIT <- matrix(0, nrow = 2, ncol = 11)
      doubledSplit <- c()
      next
    }
    
    
    # No one busts, dealer wins!
    if((sum(playerHand) < sum(dealerHand)) & ((sum(dealerHand) < 22) & (sum(playerHand) < 22))){dealer <- dealer + 1 ; outcomes[j,8] <- 1}
    
    # No one busts, player wins!
    if((sum(dealerHand) < sum(playerHand)) & ((sum(dealerHand) < 22) & (sum(playerHand) < 22))){player <- player + 1 ; outcomes[j,7] <- 1}
    
    # Did they double?
    if(outcomes[j,4] > 0){
      # No one busts, dealer wins!
      if((sum(playerHand) < sum(dealerHand)) & ((sum(dealerHand) < 22) & (sum(playerHand) < 22))){outcomes[j,18] <- outcomes[j,18] + 1}
      
      # No one busts, player wins!
      if((sum(dealerHand) < sum(playerHand)) & ((sum(dealerHand) < 22) & (sum(playerHand) < 22))){outcomes[j,17] <- outcomes[j,17] + 1}
    }
    
    # Did they push?
    if(sum(dealerHand) == sum(playerHand)){outcomes[j,16] <- 1}
    
  } # End Blackjack Simulation------------------------------------------------------------------------------------
  
  
  return(outcomes)
}# End playBlackjack()

simulateBlackjack <- function(wager = 5, hands = 25, numdecks = 8, shufflepoint = 100)
{

player <- c()
dealer <- c()
push <- c()
splits <- c()
doubles <- c()
doubleWins <- c()
doubleLosses <- c()
pBust <- c()
dBust <- c()
dBJ <- c()
pBJ <- c()
shuffles <- c()
unitPL <- c()

for (i in 1:10) {
  results <- playBlackjack(hands,numdecks,shufflepoint)
  player[i] <- sum(results$PlayerWin)
  dealer[i] <- sum(results$DealerWin)
  push[i] <- sum(results$Push)
  splits[i] <- sum(results$PlayerSplit)
  doubles[i] <- sum(results$DoubleDown)
  doubleWins[i] <- sum(results$DoubledWin)
  doubleLosses[i] <- sum(results$DoubledLoss)
  pBust[i] <- sum(results$PlayerBust)
  dBust[i] <- sum(results$DealerBust)
  pBJ[i] <- sum(results$PlayerBlackjack)
  dBJ[i] <- sum(results$DealerBlackjack)
  shuffles[i] <- sum(results$Shuffle)
  unitPL[i] <- ((sum(results$PlayerWin) - sum(results$DealerWin))*wager) + (0.5*(wager*sum(results$PlayerBlackjack))) + ((sum(results$DoubledWin)-sum(results$DoubledLoss))*wager)
}
simResults <- rbind(player,dealer,push,splits,doubles,doubleWins,doubleLosses,pBust,dBust,pBJ,dBJ,shuffles,unitPL)

simResults <- data.frame(simResults, row.names = c("PlayerWins","DealerWins","Push","Splits","Doubles","DoubledWins","DoubledLosses","PlayerBusts","DealerBusts", "PlayerBlackjack","DealerBlackjack","NumberShuffles","Profit/Loss"))
names(simResults)[1:10] <- 1:10

return(simResults)
} # end simBlackjack

simulateBlackjack(1,100,8,25) # this line will simulate Blackjack with Perfect Strategy using a wager of $1 per hand, playing 100 hands, using 8 decks in play and shuffling the shoe when 25 cards remain.


