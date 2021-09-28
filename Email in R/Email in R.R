#Email in R using Google Mail

library(gmailr)


setwd("C:/Users/Sharif/OneDrive/R-Scripts/API/Creditentials")
gm_auth_configure(path ="Google API.json")

my_email_message <- gm_mime() %>%
  gm_to("sharifocc@gmail.com") %>%
  gm_from("sharifocc@gmail.com") %>%
  gm_subject("Finished Running Member's Donor Network") %>%
  gm_text_body("Nice Work!")

gm_send_message(my_email_message)


#More info here: https://github.com/r-lib/gmailr