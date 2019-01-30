Monopoly
================
Miles Chen, Zelos Zhu
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
    ## 1     11                  Jail     68383 0.068383
    ## 2     19      Tennessee Avenue     29005 0.029005
    ## 3     21          Free Parking     28763 0.028763
    ## 4     17       St. James Place     28478 0.028478
    ## 5     23                Chance     28249 0.028249
    ## 6     28        Ventnor Avenue     26474 0.026474
    ## 7     15       Virginia Avenue     26333 0.026333
    ## 8     29           Water Works     26308 0.026308
    ## 9     27       Atlantic Avenue     26272 0.026272
    ## 10    25       Illinois Avenue     26187 0.026187
    ## 11    18       Community Chest     26157 0.026157
    ## 12    26        B & O Railroad     26148 0.026148
    ## 13    20       New York Avenue     26037 0.026037
    ## 14    22       Kentucky Avenue     25942 0.025942
    ## 15    13      Electric Company     25892 0.025892
    ## 16    30        Marvin Gardens     25834 0.025834
    ## 17    32        Pacific Avenue     25758 0.025758
    ## 18    24        Indiana Avenue     25693 0.025693
    ## 19    16 Pennsylvania Railroad     25131 0.025131
    ## 20    33 North Carolina Avenue     24964 0.024964
    ## 21    34       Community Chest     24291 0.024291
    ## 22    14         States Avenue     23508 0.023508
    ## 23    10    Connecticut Avenue     23337 0.023337
    ## 24     8                Chance     23298 0.023298
    ## 25    35   Pennsylvania Avenue     23229 0.023229
    ## 26    12     St. Charles Place     23083 0.023083
    ## 27     5            Income Tax     23042 0.023042
    ## 28     4         Baltic Avenue     22969 0.022969
    ## 29     9        Vermont Avenue     22880 0.022880
    ## 30    36   Short Line Railroad     22769 0.022769
    ## 31     6      Reading Railroad     22588 0.022588
    ## 32     7       Oriental Avenue     22435 0.022435
    ## 33    37                Chance     22004 0.022004
    ## 34     1                    Go     21864 0.021864
    ## 35     2  Mediterranean Avenue     21862 0.021862
    ## 36    40             Boardwalk     21487 0.021487
    ## 37     3       Community Chest     21346 0.021346
    ## 38    38            Park Place     21062 0.021062
    ## 39    39            Luxury Tax     20678 0.020678
    ## 40    31            Go to jail       260 0.000260

Above investigates the frequency of which certain spots in the game are landed on.
