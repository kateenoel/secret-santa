#' secretsanta
#'
#' Facilitate secret santa gift exchange with user input of participant names and email addresses
#' @param sender_email Email address from which the secret santa assignments will be communicated
#' @param sender_pwd Password to above email address
#' @examples
#' example1
#' @export
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
  santa <- names
  santee <- sample(names)

  while (any(santa == santee)) {
    santee <- sample(names)
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
    text <- paste("hi ", names[i], ', your secret santee is ', santee[i], ".", ' in other words, you are the secret santa of ', santee[i], ". have fun!", sep="")
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
