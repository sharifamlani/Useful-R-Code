
#Reorder the factor levels

levels(ANES2016.9$PS.Camp)

ANES2016.9$PS.Camp <- factor(ANES2016.9$PS.Camp,
                             levels = c( "Not much interested", #(1)
                                         "Somewhat interested", #(2)
                                         "Very much interested" #(3)
                             ))