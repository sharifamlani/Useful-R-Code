
rm(list=ls(all=TRUE))

setwd("C:/Users/Sharif/OneDrive/R-Scripts/Discource Network Analysis/Start Up")

library("rJava")
# 1. initialize JVM
.jinit()

# 2. retrieve the Java-version
.jcall("java/lang/System", "S", "getProperty", "java.version")


# 3. retrieve JAVA_HOME location
.jcall("java/lang/System", "S", "getProperty", "java.home")

# 4. retrieve Java architecture
.jcall("java/lang/System", "S", "getProperty", "sun.arch.data.model")

# 5. retreive architecture of OS (This should have 64 in it if step 4 displays
# "64")
.jcall("java/lang/System", "S", "getProperty", "os.arch")

# 6. retrieve architecture of R as well (This should again have 64 in it if
# step 4 and 5 display 64)
R.Version()$arch

Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jre1.8.0_221")

#Check to see if rDNA works

library("rDNA")
dna_init()
dna_gui(infile = dna_sample(overwrite = TRUE)) 


#Update rDNA
# install.packages("remotes")
remotes::install_github("leifeld/dna/rDNA@*release",
                        INSTALL_opts = "--no-multiarch")
dna_downloadJar(force = TRUE)
