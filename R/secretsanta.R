secretsanta <- function(sender_email, sender_pwd) {
  # determine number of participants
  num_santas <- as.integer(readline(prompt = "How many participants in your secret santa group? "))

  # initialize name and email vectors
  names <- c()
  emails <- c()

  # for each santa, assign name and email
  for (i in 1:num_santas) {
    names <- c(names, assign(paste("santa", i, "_name", sep=""), readline(prompt = paste("What is santa #", i, "'s name? ", sep=""))))
    emails <- c(emails, assign(paste("santa", i, "_email", sep=""), readline(prompt = paste("What is santa #", i, "'s email? ", sep=""))))
  }

  # randomly assign secret santas until no one is their own secret santa
  santa <- sample(names)
  santee <- sample(names)

  i <- 1
  while (any(santa == santee)) {
    santee <- sample(names)
    i <- i + 1
  }
  assignments <- data.frame(santa,santee)

  # prepare variables for email
  # consisent across recipients
  sender <- user.name <- sender_email
  subject <- "secret santa assignment"
  hostname <- "smtp.gmail.com"
  port <- 465
  passwd <- sender_pwd

  # unique to recipient


}
