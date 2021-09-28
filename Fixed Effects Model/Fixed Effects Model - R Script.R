#Fixed Effects Model

FEM.1 <- lm(DV ~ IV + Control Variable +
            as.factor(country) + #Accounts for exogenous differences between countires
            year #accounts for a general trend over time, like the heat~death of the universe
            )


FEM.2 <- lm(DV ~ IV + Control Variable +
            as.factor(country) + #Accounts for exogenous differences between countires
            as.factor(year)) #accounts for a differences between years, like wine vintage
