
#Changing a Long Dataset into a Wide Dataset


Vestimate.Reshape <- dcast(MVestimate.New, #Data Set
                           .id ~  #ID Variables (Rownames)
                            stimuli, #Varibales to Swing into a Column
                            value.var = "point" #name of column which stores values
)

#See the figure toward the bottom of the page:

      #http://seananderson.ca/2013/10/19/reshape.html


#Multiple Row Names
aqw <- dcast(aql, month + day ~ variable)
