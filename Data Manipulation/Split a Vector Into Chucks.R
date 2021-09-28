#Split a vector into chucks

#https://stackoverflow.com/questions/3318333/split-a-vector-into-chunks

d <- rpois(73,5)
D_Splt <- split(d, ceiling(seq_along(d)/20))
V <- D_Splt[1]
V



C_Fips_Split <- split(unique(County_FIPS.1$state_county_fips), ceiling(seq_along(unique(County_FIPS.1$state_county_fips))/20))
C_Fips_Split
length(C_Fips_Split)
