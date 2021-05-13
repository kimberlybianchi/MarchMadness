library(tidyverse)
library(readr)
library(numbers)

games_raw<-                                                                        # Importing raw game data into R
  read_csv("GameData.csv")

games<-                                                                            # Reformatting the data to to be Home and Road variables
  games_raw %>% mutate(HTeam=ifelse(WLoc=="A",LTeamID,WTeamID),                    # Home Team       
                       RTeam=ifelse(WLoc=="A",WTeamID,LTeamID),                # Road Team
                       HScore=ifelse(WLoc=="A",LScore,WScore),                 # Home Team Score
                       RScore=ifelse(WLoc=="A",WScore,LScore),                 # Road Team Score
                       HConf=ifelse(WLoc=="A",L_Conf,W_Conf),                  # Home Team Conference
                       RConf=ifelse(WLoc=="A",W_Conf,L_Conf)) %>%              # Road Team Conference
  dplyr::select(-c(1:7)) %>% dplyr::select(c(1,3,5,2,4,6))                         # Removing excess variables and reordering

rteam=games$RTeam                                                                  # Creating variable for Road Teams
rscore=games$RScore                                                                # Creating variable for Road Team Scores
hteam=games$HTeam                                                                  # Creating variable for Home Teams
hscore=games$HScore                                                                # Creating variable for Home Team Scores

teams=unique(rteam)                                                                # Assigning each team an ID number
ng=length(rteam)                                                                   # Creating variable for total number of games
rnum=double(ng)                                                                    # Empty vector for Road Team IDs
hnum=double(ng)                                                                    # Empty vector for Home Team IDs
for (g in 1:ng){                                                                   # Loop to create vectors of team IDs for each game
  rnum[g]=which(rteam[g]==teams)                                                   # Road Team ID for each game
  hnum[g]=which(hteam[g]==teams)}                                                  # Home Team ID for each game
hwin=ifelse(hscore>rscore,1,-1)                                                    # Creating vector indicating who won the game
nt=length(teams)                                                                   # Creating variable for total number of teams

# ----------------------------------------------------------------------------------------------------------------------------------------------------
# Creating Markov Chain for model based on Wins and Losses (Log Scale)
# ----------------------------------------------------------------------------------------------------------------------------------------------------

set.seed(0)                                                                        # Setting the seed to make the code repeatable
nsteps=1000000                                                                     # Number of times the Markov chain should be run
theta=double(nt)                                                                   # Vector for starting Team Power Ratings (theta)
h=0                                                                                # Variable for starting Home Court Advantage (h)
thetamat=matrix(0,nsteps,nt)                                                       # Matrix for storing all Team Power Ratings
hvec=double(nsteps)                                                                # Vector for storing all Home Court Advantage
eps=0.031                                                                          # Assigning epsilon value
logprior=sum(dnorm(theta,log=T))+dnorm(h,log=T)                                    # Calculating prior distribution
loglike=sum(log(pnorm(hwin*(theta[hnum]-theta[rnum]+h))))                          # Calculating likelihood distribution
logpost=logprior+loglike                                                           # Calculating posterior distribution
for (step in 1:nsteps){                                                            # Loop to simulate Markov chain
  newtheta=theta+runif(nt,-eps,eps)                                                # Calculating proposed theta values
  newh=h+runif(1,-eps,eps)                                                         # Calculating proposed h value
  newlogprior=sum(dnorm(newtheta,log=T))+dnorm(newh,log=T)                         # Calculating new prior with proposed theta and h values
  newloglike=sum(log(pnorm(hwin*(newtheta[hnum]-newtheta[rnum]+newh))))            # Calculating new likelihood with proposed theta and h values
  newlogpost=newlogprior+newloglike                                                # Calculating new posterior with new prior and likelihood   
  acc=min(1,exp(newlogpost-logpost))                                               # Calculating acceptance probability 
  if (runif(1)<acc){                                                               # If proposed move is accepted
    h=newh                                                                         # Updating h to new value
    theta=newtheta                                                                 # Updating theta to new theta value
    logpost=newlogpost}                                                            # Updating posterior to new posterior distribution
  thetamat[step,]=theta                                                            # Storing theta value in storage matrix
  hvec[step]=h}                                                                    # Storing h value in storage vector

# mean(abs(diff(hvec))>1.0e-15)                                                    # Calculating acceptance rate. Want to be close to 27%

# epsilon = 0.01, total moves = 0.7298467                                          # Tuning epsilon value to maximize the number of moves
# epsilon = 0.05, total moves = 0.09419409
# epsilon = 0.03, total moves = 0.3070853
# epsilon = 0.031, total moves = 0.2907993

# ggplot()+geom_line(aes(x=1:nsteps,y=thetamat[,182]),color="midnightblue")+       # Plot of Theta Values
#   labs(x="Step",y="Power Rating")+
#   scale_x_continuous(labels=scales::comma)

# ggplot()+geom_line(aes(x=1:nsteps,y=hvec),color="midnightblue")+                 # Plot of Home Court Advantage
#   labs(x="Step",y="Home Court Advantage")+
#   scale_x_continuous(labels=scales::comma)

c=100000                                                                           # Assigning burn in value
b_nsteps=nsteps-c+1                                                                # Calculating new nsteps with burn in included

b_thetamat<-thetamat[c:nsteps,]                                                    # Subsetting storage matrix to include burn in 
b_hvec<-hvec[c:nsteps]                                                             # Subsetting storage vector to include burn in

# ggplot()+geom_line(aes(x=1:b_nsteps,y=b_thetamat[,182]),color="midnightblue")+   # Plot of Theta Values with burn in
#   labs(x="Step",y="Power Rating")+
#   scale_x_continuous(labels=scales::comma)

# ggplot()+geom_line(aes(x=1:b_nsteps,y=b_hvec),color="midnightblue")+             # Plot of Home Court Advantage with burn in
#   labs(x="Step",y="Home Court Advantage")+
#   scale_x_continuous(labels=scales::comma)

postmean=apply(b_thetamat,2,mean)                                                  # Calculating estimated posterior mean (power ranking)
postsd=apply(b_thetamat,2,sd)                                                      # Calcualting estimated posterior standard deviations

# mean(b_hvec)                                                                     # Estimated Home Court Advantage value  - 0.4729326
# mean(b_hvec>0)                                                                   # Estimated chance that Home Court Advantage is positive  - 1

credint<-matrix(0,nt,2)                                                            # Creating empty matrix to store Credible Intervals
for (team in 1:nt){
  credint[team,]<-quantile(rankmat[,team],c(0.025,0.975))}                         # Calculating a 95% Credible Interval for each teams rank

# cbind(teams,credint)                                                             # Creating table to show Credible Intervals for each team             

# -------------------------------------------------------------------------
# Calculating the chance of each team being the best (highest power rating)
# -------------------------------------------------------------------------

bestct=double(nt)                                                                  # Vector to store counts                                         
for (step in 1:b_nsteps){                                                          # Loop through all steps post burn in
  best=which.max(b_thetamat[step,])                                                # Finding which team has the highest power ranking
  bestct[best]=bestct[best]+1                                                      # Incrementing count for team
}                                                     

# cbind(as.character(teams[which(bestct!=0)]),bestct[which(bestct!=0)]/nsteps)     # Creating table to show teams that have a chance to be the best

results<-                                                                          # Combining results into one dataframe
  data.frame(ID=seq(1:nt),Team=as.character(teams),
             PostMean=postmean,PostSD=postsd) %>% arrange(desc(PostMean))

rankmat<-matrix(0,b_nsteps,nt)                                                     # Empty matrix to store ranks of teams
for(rank in 1:b_nsteps){
  rankmat[rank,]<-rank(-b_thetamat[rank,])                                         # Calculating rank of each team for every run
}

# sum(b_thetamat[,182]>b_thetamat[,189])/b_nsteps                                  # Probability Villanova is better than Kansas  - 0.01150665
# mean(rankmat[,182])                                                              # Mean Rank of Villanova  - 8.966581
# sum(rankmat[,184]<=20)/b_nsteps                                                  # Probability of Villanova being a Top 20 team  - 0.97351

# ggplot()+geom_histogram(aes(rankmat[,182]),                                      # Plot of distribution of ranks for Villanova
#       fill="midnightblue",
#       color="white",
#       bins=(max(rankmat[,182])-min(rankmat[,182])+1))+
#   geom_segment(aes(x = mean(rankmat[,182]),
#       y=0,xend=mean(rankmat[,182]),
#       yend=120000),color="red")+
#   labs(x="Power Ranking",y="Frequency")+
#   scale_y_continuous(labels=scales::comma)


# ---------------------------------------------------
# Setting up the bracket - Bids, Ranking, and Seeding 
# ---------------------------------------------------

conf<-games %>% count(HTeam,HConf) %>% dplyr::select(-n)                           # Dataset of each team and their respective conference
colnames(conf)<-c("Team","Conf")

confwinners<-results %>% left_join(conf) %>% group_by(Conf) %>%                    # Assuming highest ranked team in each conference won their tournaments
  summarise(max(PostMean)) %>% mutate(NCAA=1,AutoBid=1)                            # Creating AutoBid variable to track which teams automatically received bids to tournament
colnames(confwinners)<-c("Conf","PostMean","NCAA","AutoBid")                       # Renaming column variables

confwinners<-confwinners %>% arrange(desc(PostMean)) %>%                           # Combining Team names with respective power ratings
  left_join(select(results,Team,PostMean),by="PostMean")
confwinners$FirstFour<-c(rep(NA,28),"A1","A1","A2","A2")                           # Assigning the four lowest ranked AutoBids to go to the First Four
# first two make one pair, last two make a second pair

seeds<-results %>%                                                                 # Combining list of conference winners into full team results
  left_join(select(confwinners,
                   c("NCAA","Team","AutoBid","FirstFour")),by="Team") %>%   
  mutate(AtLarge = NA)                                                             # Creating variable to keep track of AtLarge Teams

top<-1
while(sum(na.omit(seeds$NCAA)) < 68){                                              # Choosing the top 36 ranked teams that are not AutoBids for the AtLarge Bids
  if(is.na(seeds$NCAA[top]==TRUE)){
    seeds$NCAA[top]<-1
    seeds$AtLarge[top]<-1
  }
  top<-top+1
}

atlarge<-seeds %>% filter(AtLarge==1) %>%                                          # Making dataset of just AtLarge teams
  mutate(FirstFour=c(rep(NA,32),"L1","L1","L2","L2"))                              # Assigning the four lowest ranked AtLarge bids to go to the First Four, 
# first two make one pair, last two make a second pair

seeds<-seeds %>% 
  left_join(select(atlarge,"Team","FirstFour"),by="Team") %>% 
  filter(!is.na(NCAA)) %>%
  mutate(FirstFour=coalesce(FirstFour.x,FirstFour.y)) %>%                          # Combing the First Four columns from AtLarge Bids and AutoBids datasets
  select(-c(FirstFour.x,FirstFour.y)) %>% 
  mutate(Rank=seq(1:68))

rank<-1
seed<-1
while(rank < 68){
  if(sum(is.na(seeds$FirstFour[rank:(rank+3)]))==4){                               # Loop through for teams at a time to seed
    seeds$Seed[rank:(rank+3)]<-seed                                                # Top four ranks assigned to seed 1, next four assigned to seed 2 and so on
    rank<-rank+4
    seed<-seed+1
  } else{                                                                          # Teams assigned to first four get same ranking as pair
    if(length(unique(seeds$FirstFour[rank:(rank+4)]))==2){                         # If one or two teams in block of four is assigned to First Four 
      seeds$Seed[rank:(rank+4)]<-seed                                              # seed an additional team to give pairs the same ranking
      rank<-rank+5
      seed<-seed+1
    } else{
      seeds$Seed[rank:(rank+5)]<-seed                                              # If three or four teams in block of four is assigned to First Four 
      rank<-rank+6                                                                 # seed two additional teams to give pairs the same ranking
      seed<-seed+1
    }
  }
}


set.seed(0)                                                                        # Setting seed for repetition
ntrial<-10000                                                                      # Number of times to repeat entire newbracket
champvec<-character(ntrial)                                                        # Storage vector of who wins the entire tournament
upsetvec<-double(ntrial)                                                           # Storage vector of how many upsets occur in the tournament
rankvec<-double(ntrial)                                                            # Storage vector of what seed wins the tournanent
numbwins<-matrix(0,68,ntrial)                                                      # Storage matrix for number of times each team wins a game (excluding first four wins)
place<-1:4                                                                         # Region assignments (1 - East, 2 - South, 3 - Midwest, 4 - West)

# ----------------------
# Running the tournament
# ----------------------

for(trial in 1:ntrial){ 
  firstfour<-seeds %>% filter(!is.na(FirstFour)) %>%                               # Teams sent to the first four
    dplyr::select("Team","FirstFour","Seed") %>% mutate(Region=NA)
  
  placement<-seeds %>% filter(is.na(FirstFour)) %>%                                # Select teams that are not in the first four
    dplyr::select("Team","FirstFour","Seed") %>%
    group_by(Seed) %>%                                                             # Group teams by their predetermined seed
    mutate(Region=sample(place,replace=F,size=n())) %>% ungroup()                  # Randomly assign each team from the same seed to a different region
  
  firstfour<-merge(placement,firstfour,all=T) %>% arrange(Seed,FirstFour) %>%      # Merging teams assigned to a region with the first four team
    filter(Seed==12|Seed==16) %>% group_by(Seed) %>%                               # Filtering by seeds that contain a First Four team in it (12 and 16) 
    mutate(Leftover=rep(which(!(place %in% Region)),n()/2)) %>%                    # Creating a leftover variable that determines which regions have not been assigned yet
    filter(is.na(Region)) %>% 
    mutate(Region=rep(sample(unique(Leftover),size=2,replace=F),each=2)) %>%       # Randomly assign first four teams to leftover regions 
    dplyr::select(-Leftover)
  
  placement<-merge(placement,firstfour,all=T) %>%                                  # Merging first four teams with the rest of the teams
    mutate(Team=as.character(Team),
           Seed=factor(                                                            # Ordering the Seeds to match layout of the bracket,
             Seed,levels=c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15))) %>%           # each consecutive pair plays one another to move on
    arrange(Region,Seed)
  
  g<-0
  for(t in 1:nrow(placement)){                                                     # Looping through teams to find first four teams
    if(!is.na(placement$FirstFour[t])){                                            # since there are two of the same seed, 
      g<-g+1                                                                       # delete one row and rename the other "PLAYIN#"
      placement<-placement[-t,]                                                    # where # is a number 1-4 signifying the 4 play in games
      placement$Team[t]<-paste("PLAYIN",g,sep="")
    }
  }
  
  # -------------------------------------------
  # Putting the teams into the bracket for play
  # -------------------------------------------
  
  first4<-c(as.character(firstfour$Team),rep(NA,56))                               # Vector for first four teams
  round64<-placement$Team                                                          # Vector for first round
  round32<-c(rep("PLACEHOLDER",32),rep(NA,32))                                     # Vector for second round
  sweet16<-c(rep("PLACEHOLDER",16),rep(NA,48))                                     # Vector for sweet sixteen
  elite8<-c(rep("PLACEHOLDER",8),rep(NA,56))                                       # Vector for elite 8
  final4<-c(rep("PLACEHOLDER",4),rep(NA,60))                                       # Vector for final four
  championship<-c(rep("PLACEHOLDER",2),rep(NA,62))                                 # Vector for championship
  champion<-c(rep("PLACEHOLDER",1),rep(NA,63))                                     # Vector for champion
  newbracket<-data.frame(first4,round64,round32,sweet16,                           # Combining all rounds to a dataframe to create bracket
                         elite8,final4,championship,champion,stringsAsFactors = F)
  
  # --------------------------
  # Simulating tournament play
  # --------------------------
  
  nrounds<-length(newbracket)                                                      # Number of rounds in the tournament 
  winprob<-matrix(NA,64,nrounds)                                                   # Empty matrix to track win probabilies
  ranks<-matrix(NA,64,nrounds)                                                     # Empty matrix to track the seeds of the teams that move on
  upsets<-0                                                                        # Tracking the number of upsets in the tournament (defined as difference of 2 or more in seeds)
  rand<-sample(1:b_nsteps,size=1)                                                  # Number generated to randomly select a row from markov chain to calculate teams win probabilites
  
  # ----------
  # First four
  # ----------
  numteam<-length(na.omit(newbracket[[1]]))                                        # Number of teams in first four
  game<-1                                                                          # Tracks the game number of the round
  i<-1
  while(i < numteam){                                                              # Loop to calculate win probabilities between teams in first four
    winprob[i,1]<-pnorm(b_thetamat[rand,which(teams==newbracket[[1]][i])]-         # using a randomly generated row
                          b_thetamat[rand,which(teams==newbracket[[1]][i+1])])
    winprob[i+1,1]<-pnorm(b_thetamat[rand,which(teams==newbracket[[1]][i+1])]-
                            b_thetamat[rand,which(teams==newbracket[[1]][i])])
    
    
    simulation<-runif(1)                                                           # Generating random numbers to simulate game play
    if(simulation <= winprob[i,1]){                                                # If Team i wins
      newbracket[[2]][which(newbracket[[2]]==
                              paste("PLAYIN",as.character(game),sep=""))]<-newbracket[[1]][i]      # Team i moves on to next round
      placement$Team[which(placement$Team==
                             paste("PLAYIN",as.character(game),sep=""))]<-newbracket[[1]][i]
      game<-game+1
      numbwins[which(seeds$Team==newbracket[[1]][i]),trial]<-                      # Adding to the number of wins by team i+1
        numbwins[which(seeds$Team==newbracket[[1]][i]),trial]-1
    } else{                                                                        # If Team i+1 wins
      newbracket[[2]][which(newbracket[[2]]==
                              paste("PLAYIN",as.character(game),sep=""))]<-newbracket[[1]][i+1]    # Team i+1 moves on to the next round
      placement$Team[which(placement$Team==
                             paste("PLAYIN",as.character(game),sep=""))]<-newbracket[[1]][i+1]
      game<-game+1
      numbwins[which(seeds$Team==newbracket[[1]][i+1]),trial]<-                    # Adding to the number of wins by team i+1
        numbwins[which(seeds$Team==newbracket[[1]][i+1]),trial]-1
    }
    i<-i+2
  }
  
  # ------------
  # Other rounds
  # ------------
  for(round in 2:nrounds){                                                         # Loop for each round in the rest of the tournament
    numteam<-length(na.omit(newbracket[[round]]))                                  # Number of teams in the round
    game<-1                                                                        # Tracks the game number of the round
    i<-1
    while(i < numteam){                                                            # Loop to calculate win probabilities between teams in the round
      winprob[i,round]<-                                                           # using a randomly generated row
        pnorm(b_thetamat[rand,which(teams==newbracket[[round]][i])]-
                b_thetamat[rand,which(teams==newbracket[[round]][i+1])])
      winprob[i+1,round]<-
        pnorm(b_thetamat[rand,which(teams==newbracket[[round]][i+1])]-
                b_thetamat[rand,which(teams==newbracket[[round]][i])])
      
      ranks[i,round]<-as.numeric(as.character(                                     # Records rank of team i
        placement$Seed[placement$Team==newbracket[[round]][i]]))
      ranks[i+1,round]<-as.numeric(as.character(                                   # Records rank of team i+1
        placement$Seed[placement$Team==newbracket[[round]][i+1]]))
      
      simulation<-runif(1)                                                         # Generating random numbers to simulate game play
      if(simulation <= winprob[i,round]){                                          # If Team i wins
        newbracket[[round+1]][game]<-newbracket[[round]][i]                        # Team i moves on to the next round
        ifelse(ranks[i,round] > ranks[i+1,round]+1,                                # Adding to the upset counter if win is an upset
               upsets<-upsets+1,upsets<-upsets) 
        numbwins[which(seeds$Team==newbracket[[round]][i]),trial]<-                # Adding to the number of wins by team i
          numbwins[which(seeds$Team==newbracket[[round]][i]),trial]+1
        game<-game+1
      } else{                                                                      # If Team i+1 wins
        newbracket[[round+1]][game]<-newbracket[[round]][i+1]                      # Team i+1 moves on to the next round
        ifelse(ranks[i,round]+1 < ranks[i+1,round],                                # Adding to the upset counter if win is an upset
               upsets<-upsets+1,upsets<-upsets)
        numbwins[which(seeds$Team==newbracket[[round]][i+1]),trial]<-              # Adding to the number of wins by team i+1
          numbwins[which(seeds$Team==newbracket[[round]][i+1]),trial]+1
        game<-game+1
      }
      i<-i+2
    }
    if(round==8){                                                                  # Recording the rank of the champion team
      ranks[1,8]<-as.numeric(as.character(
        placement$Seed[placement$Team==newbracket[[8]][1]]))
    }
  }
  champvec[trial]<-newbracket[[8]][1]                                              # Assigning champion to storage vector
  rankvec[trial]<-ranks[1,8]                                                       # Assigning champion rank to rank vector
  upsetvec[trial]<-upsets                                                          # Assigning number of upsets to storage vector
}

champs<-as.data.frame(table(champvec)/ntrial) %>% arrange(desc(Freq))              # Of the teams the are champions, what is the probability they win the tournament
# mean(upsetvec)                                                                   # 13.6173 upsets

rownames(numbwins)<-seeds$Team

wincount<-matrix(NA,7,68)                                                          # Empty matrix to record the number of wins for each team
for(i in 1:68){                                                                    # Calculating number of wins for each round
  wincount[1,i]<-sum(numbwins[i,]>=0)/ntrial                                       # 0 - First Round
  wincount[2,i]<-sum(numbwins[i,]>=1)/ntrial                                       # 1 - Second Round
  wincount[3,i]<-sum(numbwins[i,]>=2)/ntrial                                       # 2 - Sweet Sixteen
  wincount[4,i]<-sum(numbwins[i,]>=3)/ntrial                                       # 3 - Elite Eight
  wincount[5,i]<-sum(numbwins[i,]>=4)/ntrial                                       # 4 - First Four
  wincount[6,i]<-sum(numbwins[i,]>=5)/ntrial                                       # 5 - Championship
  wincount[7,i]<-sum(numbwins[i,]==6)/ntrial                                       # 6 - Champion
}
colnames(wincount)<-seeds$Team

# cbind(Round=c("First Round","Second Round","Sweet 16","Elite 8",                 # Creating table of teams and probabilities of going to each round
# "Final Four","Championship","Champion"),Prob=wincount[,7])

# ggplot()+geom_bar(aes(factor(numbwins[7,],levels=0:6)),                          # Plot of farthest Villanova makes it in the tournament
#     bins=(max(numbwins[7,])-min(winprob[7,]+1)),
#     color="white",fill="midnightblue")+
#  labs(x="Farthest Round in Tournament",y="Frequency")+coord_flip()+
#  scale_x_discrete(breaks=factor(0:6),drop=FALSE,
#     labels=c("First Round","Second Round","Sweet 16",
#         "Elite 8","Final Four","Championship","Champion"))
#  geom_text(aes(x=factor(numbwins[7,],levels=0:6),y=n()))



