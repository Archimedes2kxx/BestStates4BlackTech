### Generate tables and graphics to be included in report based on the data from Data-1A and Data-2B

###################################################
### use map_dat() as described on this URL
### http://is-r.tumblr.com/post/37708137014/us-state-maps-using-mapdata
###################################################

setwd("/Users/roylbeasley/Google Drive/Diversity/Census-Bureau/BestStates4BlackTech")
load(file="dfEmploymentAndShares.RData")
load(file = "dfStatesPop3.RData") 

### Table 1 (top). Context -- How many people in the US in 2014 -- total, black, white, asian, hispanic, and OTHERS
###   and percentages of total, white, black, asian, hispanic, and OTHERs in second row
tail(dfStatesPop3,1) ### ... first two rows ... Full dfStatesPop3 in Appendix so readers can see racial shares of pop

### Table 1 (bottom). How many Techs in the US? How many total, white, black, asian, and hispanic techs in U.S.
tail(dfEmploymentAndShares, 1) ### ... second two rows of Table 1


### Table 2A, 2B, 2C, 2D. How many techs in each state ... totals, white, black, asian, hispanic and percents racial shares
### ... sorted in decreasing order for racial groups ... so users can see "Top 10"
### ... Only show top 10 in report, show full tables 2AA, 2BB, 2CC, 2DD in appendices
dfEmploymentAndShares

### Maps 2A, 2B, 2C, 2D ... maps of white, black, asian, and hispanic techs in U.S.


### Tables 3A, 3B, 3C, 3D ... shares (%) of tech employmnet for white, black, asian, and hispanic techs sorted in order of decreasing shares for each group .... allow identification of "top 10" in terms of most integrated

### Maps 3A, 3B, 3C, 3D ... maps of shares of tech employment for white, black, asian, and hispanic techs


### Tables 4A, 4B, 4C, 4D ... parity ratios, i.e., tech share/populaton shares for white, black, asian, and hispanic sorted in decreasing order for each groups

### Maps 4A, 4B, 4C, 4D ... maps of parity ratios for each racial group

### Tables 5A, 5B, 5C, 5D ... regressions of Betas for racial shares of tech in each state vs. whites, blacks, asians, and hispanic population in each state ... show that a 1 percent increase in share of pop yields an X percent increase in share of tech employment ... sorted in order of decreasing size of ratios ... enable picking the 10 fairest states

### Plots 5A, 5B, 5C, 5D ... regression lines for racial shares of tech vs. racial shares of population whose slopes are the Betas in Tables 5A, 5B, 5C, 5D ... ALL ONE THE SAME PLOT FRAME so user can see that asian is much steeper than white, black, and hispanic



