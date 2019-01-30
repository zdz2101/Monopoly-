Monopoly
================
Zelos Zhu
April 6, 2016

Rules for movement
------------------

The Monopoly Board is basicaly a circle with 40 spaces on which a player can land. The number of spaces a player moves is determined by the roll of 2 dice. Most often, the player will roll the dice, land on a space, and end his turn there.

However, several exceptions which provide the primary source of variation in space landing:

One space sends players directly to jail. This space never counts as having been "landed upon." As soon as the player lands here, he is immediately sent to jail, and the jail space gets counted as landed upon. This is the only space on the game board that moves a player's piece.

If a player rolls doubles (two of the same number), the player moves his piece, and then gets to roll the dice again for another move. However, if a player rolls doubles three times in a row, he is sent directly to jail. (The third space that the player would have 'landed on' does not count, but the jail space gets counted as landed on.)

### Card Decks

A player can land on a "Chance" or "Community Chest" space. When a player lands on these spaces, he draws a card from the respective deck and follows its instructions. The instructions will sometimes give money to or take money from the player with no change in the player's position on the board. Other times, the card will instruct the player to move to another space on the board. The list of cards that can be drawn from each deck is provided below.

There are nine cards in the Chance deck that move the player's token. There are two cards in the Community Chest deck that move the player's token. All other cards do not move the player's token.

A card may say 'move to the nearest railroad' or 'move to the nearest utility' or even 'go to property xxx'. In these cases, the player always moves forward. So if a player is on 'Oriental Avenue,' the nearest railroad is 'Pennsylvania Railroad' and NOT 'Reading Railroad.'

The Chance and Community Chest spaces always get counted as "landed on" even if the card drawn moves the player to another space or sends him to jail. In those cases, a tally is counted for the Chance/Community Chest space, the token is moved, and then a tally is counted for the space where the player ends his turn.

### Jail

Jail is the most complicated aspect of this simulation.

If a player lands on space 11 (Jail), he is not in Jail. He is 'just visiting.' His play continues on as normal.

A player can be placed in jail in several ways: he can roll doubles three times in a row. He can land on the "go to jail space." He can draw a card that sends hims to jail.

When in jail, the player has the option to pay a fee to 'get out,' or he can choose not to pay the fee.

If he pays the fee, he is out of jail, and his play continues normally as before.

If he chooses not to pay the fee, he rolls the dice. If he rolls doubles on the dice, he gets out of jail and move the number of spaces the dice show. However, despite rolling doubles, he does not roll again. He takes his move out of jail and his turn ends. If he does not roll doubles, he stays in jail.

A player cannot stay in jail for more than three turns. On his third turn in jail, he rolls the dice and moves the number of spaces the dice show no matter what. If they are doubles, he moves those spaces for free. If he does not roll doubles, he moves those spaces, but must also pay a fee.

Play then continues as normal.

<http://monopoly.wikia.com/wiki/Jail>

For this simulation, each time a player ends his turn in Jail, a tally will be counted as having been 'landed upon.'

We will simulate a 'long stay' strategy for Jail. This effectively means that the player will never pay the fee to get out jail unless forced to do so. Effectively, this means that he will roll the dice and only leave jail if he gets double or it is his third turn in jail.

The Simulation
--------------

This runs 5,000 simulations of a two-player game that lasts 200 rolls/100 turns each. This is a total of 2 million dice rolls - 5000 games x 200 rolls x 2 dice.

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.1.0     ✔ purrr   0.2.5
    ## ✔ tibble  1.4.2     ✔ dplyr   0.7.7
    ## ✔ tidyr   0.8.2     ✔ stringr 1.3.1
    ## ✔ readr   1.1.1     ✔ forcats 0.3.0

    ## ── Conflicts ────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
gameboard <- data.frame(space = 1:40, title = c("Go" , "Mediterranean Avenue" , "Community Chest" , "Baltic Avenue" , "Income Tax" , "Reading Railroad" , "Oriental Avenue" , "Chance" , "Vermont Avenue" , "Connecticut Avenue" , "Jail" , "St. Charles Place" , "Electric Company" , "States Avenue" , "Virginia Avenue" , "Pennsylvania Railroad" , "St. James Place" , "Community Chest" , "Tennessee Avenue" , "New York Avenue" , "Free Parking" , "Kentucky Avenue" , "Chance" , "Indiana Avenue" , "Illinois Avenue" , "B & O Railroad" , "Atlantic Avenue" , "Ventnor Avenue" , "Water Works" , "Marvin Gardens" , "Go to jail" , "Pacific Avenue" , "North Carolina Avenue" , "Community Chest" , "Pennsylvania Avenue" , "Short Line Railroad" , "Chance" , "Park Place" , "Luxury Tax" , "Boardwalk"))

chancedeck <- data.frame(index = 1:15, card = c("Advance to Go" , "Advance to Illinois Ave." , "Advance to St. Charles Place" , "Advance token to nearest Utility" , "Advance token to the nearest Railroad" , "Take a ride on the Reading Railroad" , "Take a walk on the Boardwalk" , "Go to Jail" , "Go Back 3 Spaces" , "Bank pays you dividend of $50" , "Get out of Jail Free" , "Make general repairs on all your property" , "Pay poor tax of $15" , "You have been elected Chairman of the Board" , "Your building loan matures"))

communitydeck <- data.frame(index = 1:16, card = c("Advance to Go" , "Go to Jail" , "Bank error in your favor ??? Collect $200" , "Doctor's fees Pay $50" , "From sale of stock you get $45" , "Get Out of Jail Free" , "Grand Opera Night Opening" , "Xmas Fund matures" , "Income tax refund" , "Life insurance matures ??? Collect $100" , "Pay hospital fees of $100" , "Pay school tax of $150" , "Receive for services $25" , "You are assessed for street repairs" , "You have won second prize in a beauty contest" , "You inherit $100"))
```

``` r
dice <- function(){
    faces <- sample(1:6, 2, replace=TRUE)
    if(faces[1] == faces[2]) doubles = TRUE
    else doubles = FALSE
    movement = sum(faces)
    return(list(faces=faces, doubles=doubles, movement=movement))
}
```

Create Player Class
-------------------

``` r
setClass(Class="Player",
         representation=list(position="numeric",Jail_Status="logical",Turns_In_Jail="numeric",Doubles_Count="numeric"),
         prototype=list(position=1,Jail_Status=FALSE,Turns_In_Jail=0,Doubles_Count=0))
```

Create Chance Deck
------------------

``` r
chance<-function(player){
    draw<-sample(1:15,1,replace=TRUE)
    if(draw == 1)  #Advance to go
    {
      player@position<-1
    } 
    if(draw == 2) #Advance to Illinois Ave
    {  
      player@position<-25
    }
    if(draw == 3) #Advance to St. Charles Place
    {  
      player@position<-17
    }
    if(draw == 4) #Advance to nearest utility
    {  
      if(player@position <= 12 | player@position >=30)
      {
        player@position<-13
      }
      else
      {
        player@position<-29
      }
    }
    if(draw == 5) #Advance to nearest railroad
    {  
      if(player@position ==8)
      {
        player@position<-16
      }
      if(player@position == 23)
      {
        player@position<-26
      }
      if(player@position == 37)
      {
        player@position<-6
      }
    }
    if(draw==6) #Advance to Reading Railroad
    {
      player@position<-6
    }
    if(draw==7) #Take a walk on Boardwalk
    {
      player@position<-40
    }
    if(draw==8) #Go to Jail
    {
      player@position<-11
      player@Jail_Status<-TRUE
      player@Doubles_Count<-0
      player@Turns_In_Jail<-0
    }
    if(draw==9) #Take 3 steps back
    {
      player@position<-player@position-3
    }
    return(player)
}
```

Create Community Chest

``` r
communitychest<-function(player){
    draw<-sample(1:16,1,replace=TRUE)
    if(draw == 1)  #Advance to go
    {
      player@position<-1
    } 
    if(draw==2) #Go to Jail
    {
      player@position<-11
      player@Jail_Status<-TRUE
      player@Doubles_Count<-0
      player@Turns_In_Jail<-0
    }
    return(player)
}
```

Create Turn function
--------------------

``` r
turn<-function(player){
  
  #regular turn, not in jail
  player@Doubles_Count<-0
  if(player@Jail_Status==FALSE)
  {
    roll<-dice()
    player@position<-player@position + roll$movement
    #Lands on Jail
    if (player@position==31)
    {
      player@position<-11
      player@Jail_Status<-TRUE
      player@Doubles_Count<-0
      player@Turns_In_Jail<-0
    }
    
    #Draw Chance Card
    if(player@position==8 | player@position==23 | player@position==37)
    {
      chance(player)
    }
    
    #Draw Community
    if(player@position==3 | player@position==18 | player@position==34)
    {
      communitychest(player)
    }
    #Passes GO
    if(player@position>40)
    {
      player@position<-player@position%%40
    }
    
    #if doubles
    if(roll$doubles==TRUE)
    {
      player@Doubles_Count<-1
      roll2<-dice()
      player@position<-player@position + roll$movement
      #land in jail
      if (player@position==31)
      {
        player@position<-11
        player@Jail_Status<-TRUE
        player@Doubles_Count<-0
        player@Turns_In_Jail<-0
      }
      #draw from chance
      if(player@position==8 | player@position==23 | player@position==37)
      {
        chance(player)
      }
      #draw from community chest
      if(player@position==3 | player@position==18 | player@position==34)
      {
        communitychest(player)
      }
      #Passes GO
      if(player@position>40)
      {
        player@position<-player@position%%40
      }
      #if doubles 2x in a row
      if(roll2$doubles==TRUE)
      {
        player@Doubles_Count<-2
        roll3<-dice()
        player@position<-player@position + roll$movement
        #land in jail
        if (player@position==31 | roll3$doubles==TRUE)
        {
          player@position<-11
          player@Jail_Status<-TRUE
          player@Doubles_Count<-0
          player@Turns_In_Jail<-0
        }
        #draw from chance
        if(player@position==8 | player@position==23 | player@position==37)
        {
          chance(player)
        }
        #draw from community chest
        if(player@position==3 | player@position==18 | player@position==34)
        {
          communitychest(player)
        }
        #Passes GO
        if(player@position>40)
        {
          player@position<-player@position%%40
        }
      }
    }
  }
  
  #Jail Turn
  if(player@Jail_Status==TRUE)
  {
    roll<-dice()
     #Stay in Jail
    if(roll$doubles==FALSE & player@Turns_In_Jail <=2)
    {
      player@Turns_In_Jail<-player@Turns_In_Jail+1
    }
    #Get out of Jail
    if(roll$doubles==TRUE | player@Turns_In_Jail==3)
    {
      player@position<-player@position + roll$movement
      player@Jail_Status<-FALSE
      player@Turns_In_Jail<-0
    }
  }  
return(player)
}
```

Run the simluation (for two players):
=====================================

``` r
gameboard$frequency<-rep(0,40)
for(i in 1:5000){
  player1<-new("Player")
  player2<-new("Player")
  for(i in 1:100){
    player1<-turn(player1)
    if(player1@position>40)
    {
      player1@position<-player1@position%%40
    }
    gameboard$frequency[player1@position]<-gameboard$frequency[player1@position]+1
    player2<-turn(player2)
    if(player2@position>40)
    {
      player2@position<-player2@position%%40
    }
    gameboard$frequency[player2@position]<-gameboard$frequency[player2@position]+1
  }
}  
```

``` r
gameboard$prob<-gameboard$frequency/sum(gameboard$frequency)
arrange(gameboard, desc(frequency))
```

    ##    space                 title frequency     prob
    ## 1     11                  Jail     68169 0.068169
    ## 2     21          Free Parking     28879 0.028879
    ## 3     17       St. James Place     28748 0.028748
    ## 4     19      Tennessee Avenue     28616 0.028616
    ## 5     23                Chance     28486 0.028486
    ## 6     25       Illinois Avenue     26476 0.026476
    ## 7     18       Community Chest     26372 0.026372
    ## 8     15       Virginia Avenue     26282 0.026282
    ## 9     27       Atlantic Avenue     26243 0.026243
    ## 10    29           Water Works     26206 0.026206
    ## 11    28        Ventnor Avenue     26205 0.026205
    ## 12    26        B & O Railroad     26200 0.026200
    ## 13    13      Electric Company     26095 0.026095
    ## 14    20       New York Avenue     26077 0.026077
    ## 15    30        Marvin Gardens     26015 0.026015
    ## 16    32        Pacific Avenue     25935 0.025935
    ## 17    22       Kentucky Avenue     25833 0.025833
    ## 18    24        Indiana Avenue     25651 0.025651
    ## 19    33 North Carolina Avenue     25187 0.025187
    ## 20    16 Pennsylvania Railroad     25159 0.025159
    ## 21    34       Community Chest     24191 0.024191
    ## 22    14         States Avenue     23558 0.023558
    ## 23     8                Chance     23408 0.023408
    ## 24     9        Vermont Avenue     23116 0.023116
    ## 25     4         Baltic Avenue     22944 0.022944
    ## 26    36   Short Line Railroad     22934 0.022934
    ## 27     6      Reading Railroad     22884 0.022884
    ## 28    10    Connecticut Avenue     22823 0.022823
    ## 29    12     St. Charles Place     22816 0.022816
    ## 30     5            Income Tax     22766 0.022766
    ## 31    35   Pennsylvania Avenue     22758 0.022758
    ## 32     7       Oriental Avenue     22500 0.022500
    ## 33    37                Chance     21941 0.021941
    ## 34     1                    Go     21866 0.021866
    ## 35     2  Mediterranean Avenue     21777 0.021777
    ## 36    40             Boardwalk     21590 0.021590
    ## 37     3       Community Chest     21255 0.021255
    ## 38    38            Park Place     20988 0.020988
    ## 39    39            Luxury Tax     20786 0.020786
    ## 40    31            Go to jail       265 0.000265

Above investigates the frequency of which certain spots in the game are landed on.
