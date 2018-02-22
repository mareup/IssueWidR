card_utf <- read.csv("card_utf.csv",stringsAsFactors = FALSE)
setup <- function(card_utf) {
  CARD_UTF <- card_utf
  card_rc <- c( 0, 0)
  card_ans <- 0
  
  RANDOM <- function() {
    card_row <- sample(1:332,size = 1)
    card_col <- sample(c(3,4),size = 1)
    assign("card_rc", c(card_row,card_col), envir = parent.env(environment()))
  }
  
  PRINTCARD <- function() {
    CARD_UTF[card_rc[1],card_rc[2]]
  }
  
  SCAN <- function() {
    assign("card_ans", readline("解答:"),envir = parent.env(environment()))
  }
  
  ANS_TF <- function() {
    if(card_rc[2] == 3) {
      card_ans == CARD_UTF[card_rc[1], 4]
    } else {
      card_ans == CARD_UTF[card_rc[1], 3]
    }
  }
  
  PRINTANS <- function() {
    CARD_UTF[card_rc[1],]

###########ランダム化した後のデータを順に表示する
##    assign("card_utf",CARD_UTF[-1,],envir = parent.env(environment()))
  }

  list(random = RANDOM , printcard = PRINTCARD , scan = SCAN , ans_tf = ANS_TF , printans = PRINTANS)
}

cardSystem <- setup(card_utf)
random <- cardSystem$random
printcard <- cardSystem$printcard
scan <- cardSystem$scan
ans_tf <- cardSystem$ans_tf
printans <- cardSystem$printans

startCS <- function(n) {
  for(i in 1:n) {
  random()
  print(printcard())
  scan()
  print(ans_tf())
  print(printans())

#################練習用に入力できるよ##########
  scan()
  print(ans_tf())
  }
}