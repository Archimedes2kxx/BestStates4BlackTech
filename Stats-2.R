### Generate tables and graphics to be included in report based on the data from Data-1A and Data-2B

###################################################
### use map_dat() as described on this URL
### http://is-r.tumblr.com/post/37708137014/us-state-maps-using-mapdata
###################################################

setwd("/Users/roylbeasley/Google Drive/Diversity/Census-Bureau/BestStates4BlackTech")
load(file="dfEmploymentAndShares.RData")
load(file="dfStatesPop3.RData") 
load("dfCensus2.RData")


### Table 1 (top 2 rows). Context -- How many people in the US in 2014 -- total, black, white, ### asian, hispanic, and OTHERS and percentages of total, white, black, asian, hispanic, 
### and OTHERs in second row
tail(dfStatesPop3,1) ### Full dfStatesPop3 in Appendix so readers can see racial shares of pop

### Table 1 (bottom 2 rows). How many Techs in the US? How many total, white, black, asian, ### and hispanic techs in U.S.
tail(dfEmploymentAndShares, 1) ### ... second two rows of Table 1


### Tables 2A, 2B, 2C, 2D. How many members of racial groups  in each state ... totals, white, ### black, asian, hispanic and percents racial shares of total population in each state
### ... sorted in decreasing order for racial groups ... so users can see "Top 10"
### ... Only show top 10 in report, show full tables 2AA, 2BB, 2CC, 2DD in appendices on GitHub
dfStatesPop3

### Maps 2W, 2B, 2A, 2H ... maps of white, black, asian, hispanics in states.
### Maps 2.1W, 2.1B, 2.1A, 2.1H ... maps of % white, black, asian, hispanics in states.

### Tables 3W, 3B, 3A, 3H ... How many techs in each state and shares (%) of tech employmnet 
### for white, black, asian, and hispanic techs sorted in order of decreasing tech employment for 
### each total pop in the state .... allow identification of "top 10" in terms of most number of 
### minority employees .. only show top 10; full tables 3WW, 3BB, 3AA, 3HH in appendices on GitHub
dfEmploymentAndShares

### Maps 3W, 3B, 3A, 3H ... maps of white, black, asian, hispanic techs in each state
### Maps 3.1W, 3.1B, 3.1A, 3.1H ... maps of  white, black, asian, hispanic % of techs in each state


### Tables 4W, 4B, 4A, 4H ... parity ratios, i.e., tech share/populaton shares for white, black, 
### asian, and hispanic ratios sortin states in decreasing order for each groups... only show
### top ten ...  full 4WW, 4BB, 4AA, 4HH in appendices on GitHub

### Maps 4W, 4B, 4A, 4H ... maps of parity ratios for each racial group


### Tables 5W, 5B, 5A, 5H ... regressions of Betas for racial shares of tech in each state vs. white
### blacks, asians, and hispanic population in each state ... show that a 1 percent increase in 
### share of pop yields an X percent increase in share of tech employment ... sorted in order of 
###decreasing size of ratios ... enable picking the 10 fairest states 
### ... only show top 10;  full 5WW, 5BB, 5AA, 5HH in appendices on GitHub

### Plots 5W, 5B, 5A, 5H ... regression lines for racial component ot each state's tech sector vs. ### the racial component of each state's population. The Beta slopes are in Tables 5A, 5B, 5C, 5D 
### ... ALL ON THE SAME PLOT FRAME so user can see that asian slope is much steeper than white, 
### black, and hispanic ... Betas show that a 1 percent increase in a group's population yields a 
### Beta percent increase in the size of the group's tech employment


