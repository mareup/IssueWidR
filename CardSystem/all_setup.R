### import data
### データ読み込み
card_utf <- read.csv("~/work/share/CardSystem/CS_shell/card_utf.csv",stringsAsFactors = FALSE)

### create environment
### 環境の立ち上げ
setup <- function(card_utf) {

### cp parent environmental data child environmental data
### 親のデータを子の環境にコピーする
  CARD_UTF <- card_utf

### initialize data
### データの初期化 ------------------------------------------------------------------------------------
#### 行と列
	card_col <- 0
	card_row <- matrix(0,1:333,1)
  card_ans <- 0
	ans_f <- read.csv("~/work/share/CardSystem/CS_shell/ans_f.csv",stringsAsFactors = FALSE)
#### セクション
  section_num <- 0
	section_first <- c(0,72,139,217,287)
	section_row <- 0
#### 確率
	prob_f <- ans_f
		prob_f[,2] <- prob_f[,2] + 1
	prob_section <- matrix(0,1:333,1)
#  check_data <- matrix(0,1:333,1)
### ----------------------------------------------------------------------------------------------------

### check the section's number -------------------------------------------------------------------------
### section番号の確認
#	CHECK_SECTION<-function(section_num = NULL) {
#	  if(!is.null(section_num)) {
#		  section_row <- section_first[section_num - 1]
#		  assign("section_row", section_row, envir = parent.env(environment()))
#	  }
#	}
### ----------------------------------------------------------------------------------------------------

### calculate probability ------------------------------------------------------------------------------ 
### 確率の計算
	CALC_PROB <- function(section_num = NULL, fail = NULL) {
	  if(!is.null(fail)) {
  		prob_section <- prob_f[,1] == section_num
	  	prob_f[prob_section,2] <- prob_f[prob_section,2] / sum(prob_f[prob_section,2])
		  assign("prob_f", prob_f , envir = parent.env(environment()))
		  assign("prob_section", prob_section, envir = parent.env(environment()))
	  }
	}
### ----------------------------------------------------------------------------------------------------
	
### randomized element ---------------------------------------------------------------------------------
### 要素のランダム化
  RANDOM_COL <- function(col_num = 3) {
		if(any(col_num %in% c(3,4))) {
  	  assign("card_col", col_num, envir = parent.env(environment()))
		} else {
	    card_col <- sample(c(3,4),size = 1)
  	  assign("card_col", card_col, envir = parent.env(environment()))
		}
	}

	RANDOM_ROW <- function(section_num = NULL, fail = NULL) {
		section_row_add <- c(0,71,138,216,286)
	  if(is.null(section_num)){
	    card_row <- sample(1:333,size = 333)
	  } else {
	    randomnumber <- sum(CARD_UTF == section_num)
	    if(is.null(fail)) {
  	    card_row <- sample(1:randomnumber ,size = randomnumber)
	    } else {
	      card_row <- sample(1:randomnumber ,size = randomnumber, prob = prob_f[prob_section,2])
	    }
			card_row <- card_row + section_row_add[section_num]
	  }
    assign("card_row", card_row, envir = parent.env(environment()))
  }
### ---------------------------------------------------------------------------------------------------

### print selected data
### 指定された部分を表示
  PRINTCARD <- function() {
    print(CARD_UTF[card_row[1],card_col])
  }
  
### as like scanf , input answer
### 解答の入力
  SCAN <- function() {
    assign("card_ans", readline("解答:"),envir = parent.env(environment()))
  }
  
### judge answer whether that is T or F
### 答えがあってるかどうかをTRUEとFALSEでかえす。
  ANS_TF <- function() {
		teach <- c("間違ってるよ！！","正解だよ！！")

    if(card_col == 3) {
      ifans <- card_ans == CARD_UTF[card_row[1], 4]
			print(teach[ifans + 1])
    } else {
      ifans <- card_ans == CARD_UTF[card_row[1], 3]
			print(teach[ifans + 1])
    }

		if(ifans == FALSE) {
      ans_f[card_row[1],2] <- ans_f[card_row[1],2] + 1
			assign("ans_f", ans_f , envir = parent.env(environment()))
		}
  }
  
### print row as answer
### 行を表示する
  PRINTANS <- function() {
    print(CARD_UTF[card_row[1],])
  }
  RESETTING <- function() {
###########ランダム化した後のデータを順に表示する
    assign("card_row", card_row[-1], envir = parent.env(environment()))
  }

	PRAC_SCAN <- function() {
		assign("card_ans", readline("もう一度入力しよう:"), envir = parent.env(environment()))
	}

	PRINTFAIL <- function() {
#	print(ans_f)
	  write.csv(ans_f , file = "~/work/share/CardSystem/CS_shell/ans_f.csv",row.names = FALSE)
	}

  list( random_col = RANDOM_COL, random_row = RANDOM_ROW, calc_prob = CALC_PROB, printcard = PRINTCARD, scan = SCAN, ans_tf = ANS_TF, printans = PRINTANS, prac_scan = PRAC_SCAN,resetting = RESETTING, printfail = PRINTFAIL)
}

cardSystem <- setup(card_utf)
#check_section <- cardSystem$check_section
random_col <- cardSystem$random_col
random_row <- cardSystem$random_row
calc_prob <- cardSystem$calc_prob
printcard <- cardSystem$printcard
scan <- cardSystem$scan
ans_tf <- cardSystem$ans_tf
printans <- cardSystem$printans
prac_scan <- cardSystem$prac_scan
resetting <- cardSystem$resetting
printfail <- cardSystem$printfail

startCS <- function(n,section_num = NULL,col_num = 3, fail = NULL) {
#  check_section(section_num)
  random_col(col_num)
  random_row(section_num)
  calc_prob(section_num,fail)
  for(i in 1:n) {
  printcard()
  scan()
  ans_tf()
  printans()

#################練習用に入力できるよ##########
  prac_scan()
  ans_tf()
  resetting()
  }
	printfail()
}
