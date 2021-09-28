#How to Drop an Unused Level?

# Drop an Unused Level

#Use Factor()
ANES2016.11$Race <- factor(ANES2016.11$Race)

#Boom! The level without any data will be droped
levels(ANES2016.11$Race)
