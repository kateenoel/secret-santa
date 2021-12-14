#' secretsanta
#'
#' Facilitate secret santa gift exchange with user input of participant names and email addresses
#' @param sender_email Email address from which the secret santa assignments will be communicated
#' @param sender_pwd Password to above email address
#' @export
secretsanta <- function(sender_email, sender_pwd) {
  # determine number of participants
  num_santas <- as.integer(readline(prompt = "How many participants in your secret santa group? "))

  # initialize name and email vectors
  names <- c()
  emails <- c()
  addresses <- c()
  wants <- c()
  notwants <- c()

  # for each santa, assign name and email
  for (i in 1:num_santas) {
    names <- c(names, assign(paste("santa", i, "_name", sep=""), readline(prompt = paste("What is santa #", i, "'s name? ", sep=""))))
    emails <- c(emails, assign(paste("santa", i, "_email", sep=""), readline(prompt = paste("What is santa #", i, "'s email? ", sep=""))))
    addresses <- c(addresses, assign(paste("santa", i, "_address", sep=""), readline(prompt = paste("What is santa #", i, "'s shipping address? ", sep=""))))
    wants <- c(wants, assign(paste("santa", i, "_want", sep=""), readline(prompt = paste("What are santa #", i, "'s wants? ", sep=""))))
    notwants <- c(notwants, assign(paste("santa", i, "_notwant", sep=""), readline(prompt = paste("What are santa #", i, "'s not wants? ", sep=""))))
  }

  # for each santa, create a dictionary with all relevant information
  people <- vector(mode='list', length=num_santas)

  for (i in 1:num_santas) {
    name = names[i]
    email = emails[i]
    address = addresses[i]
    want = wants[i]
    notwant = notwants[i]

    person = c("name" = name, "email" = email, "address" = address, "want" = want, "notwant" = notwant)

    people[[i]] <- person
  }

  # randomly assign secret santas until no one is their own secret santa
  santa <- names
  santee <- sample(santa)

  while (any(santa == santee)) {
    santee <- sample(santa)
  }

  # prepare variables for email
  # consistent across recipients
  sender <- sender_email
  subj <- "secret santa assignment :)"
  host <- "smtp.gmail.com"
  portnum <- 465
  username <- sender_email
  pwd <- sender_pwd

  # unique to recipient
  for (i in 1:num_santas) {
    recipient <- emails[i]
    text <- paste("hi ", santa[i], ', your secret santee is ', santee[i], ".", ' in other words, you are the secret santa of ', santee[i], ". have fun!", sep="")
    send.mail(from = sender,
              to = recipient,
              subject = subj,
              body = text,
              html = T,
              smtp = list(host.name = host,
                          port = portnum,
                          user.name = username,
                          passwd = pwd,
                          ssl = T),
              authenticate = T)
  }
}
