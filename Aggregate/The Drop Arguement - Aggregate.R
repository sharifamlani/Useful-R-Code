####################### Aggregate - Keep All Donors #########################
Bonica.1$Count <- 1
#************************* In/Out Distict ********************
Bonica.2a <- plyr::join_all(list(
  setNames(aggregate(amount ~ ID_Cycle + Donor_Location.District, data = Bonica.1, drop = F, sum), c("ID_Cycle", "Donor_Location.District", "Amount")),
  setNames(aggregate(Count ~ ID_Cycle + Donor_Location.District, data = Bonica.1,drop = F, sum), c("ID_Cycle", "Donor_Location.District", "Count"))
),
by = c("ID_Cycle", "Donor_Location.District"))

#*********************** In/Out Distict Detail********************
Bonica.2b <- plyr::join_all(list(
  setNames(aggregate(amount ~ ID_Cycle + Donor_Location.Location, data = Bonica.1,drop = F, sum), c("ID_Cycle", "Donor_Location.Location", "Amount")),
  setNames(aggregate(Count ~ ID_Cycle + Donor_Location.Location, data = Bonica.1,drop = F, sum), c("ID_Cycle", "Donor_Location.Location", "Count"))
),
by = c("ID_Cycle", "Donor_Location.Location"))

table(Bonica.2b$Donor_Location.Location)
table(Bonica.2a$Donor_Location.District)


####################### Aggregate - Keep Donors In each location only #########################

#************************* In/Out Distict ********************
Bonica.2a <- plyr::join_all(list(
  setNames(aggregate(amount ~ bonica.cid + Donor_Location.District + reelection_year, data = Bonica.1, sum), c("bonica.cid", "Donor_Location.District", "reelection_year", "Amount")),
  setNames(aggregate(Count ~ bonica.cid + Donor_Location.District + reelection_year, data = Bonica.1, sum), c("bonica.cid", "Donor_Location.District", "reelection_year", "Count"))
),
by = c("bonica.cid", "Donor_Location.District", "reelection_year"))

#*********************** In/Out Distict Detail********************
Bonica.2b <- plyr::join_all(list(
  setNames(aggregate(amount ~ bonica.cid + Donor_Location.Location + reelection_year, data = Bonica.1, sum), c("bonica.cid", "Donor_Location.Location", "reelection_year", "Amount")),
  setNames(aggregate(Count ~ bonica.cid + Donor_Location.Location + reelection_year, data = Bonica.1, sum), c("bonica.cid", "Donor_Location.Location", "reelection_year", "Count"))
),
by = c("bonica.cid", "Donor_Location.Location", "reelection_year"))

table(Bonica.2b$Donor_Location.Location)
table(Bonica.2a$Donor_Location.District)
