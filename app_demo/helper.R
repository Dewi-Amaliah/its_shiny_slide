calculate_framingham_risk <- function(age, sex, totalChol, hdlChol, systolic, smoker) {
  # Initialize risk score
  risk_score <- 0

  # Age points
  if (sex == "Male") {
    if (age >= 20 & age <= 34) risk_score <- risk_score - 9
    else if (age >= 35 & age <= 39) risk_score <- risk_score - 4
    else if (age >= 45 & age <= 49) risk_score <- risk_score + 3
    else if (age >= 50 & age <= 54) risk_score <- risk_score + 6
    else if (age >= 55 & age <= 59) risk_score <- risk_score + 8
    else if (age >= 60 & age <= 64) risk_score <- risk_score + 10
    else if (age >= 65 & age <= 69) risk_score <- risk_score + 11
    else if (age >= 70 & age <= 74) risk_score <- risk_score + 12
    else if (age >= 75 & age <= 79) risk_score <- risk_score + 13
  } else { # Female
    if (age >= 20 & age <= 34) risk_score <- risk_score - 7
    else if (age >= 35 & age <= 39) risk_score <- risk_score - 3
    else if (age >= 45 & age <= 49) risk_score <- risk_score + 3
    else if (age >= 50 & age <= 54) risk_score <- risk_score + 6
    else if (age >= 55 & age <= 59) risk_score <- risk_score + 8
    else if (age >= 60 & age <= 64) risk_score <- risk_score + 10
    else if (age >= 65 & age <= 69) risk_score <- risk_score + 12
    else if (age >= 70 & age <= 74) risk_score <- risk_score + 14
    else if (age >= 75 & age <= 79) risk_score <- risk_score + 16
  }

  # Total Cholesterol points
  if (age >= 20 & age <= 39) {
    if (totalChol < 160) risk_score <- risk_score + 0
    else if (totalChol >= 160 & totalChol <= 199) risk_score <- risk_score + 4
    else if (totalChol >= 200 & totalChol <= 239) risk_score <- risk_score + 8
    else if (totalChol >= 240 & totalChol <= 279) risk_score <- risk_score + 11
    else risk_score <- risk_score + 13
  } else if (age >= 40 & age <= 49) {
    if (totalChol < 160) risk_score <- risk_score + 0
    else if (totalChol >= 160 & totalChol <= 199) risk_score <- risk_score + 3
    else if (totalChol >= 200 & totalChol <= 239) risk_score <- risk_score + 6
    else if (totalChol >= 240 & totalChol <= 279) risk_score <- risk_score + 8
    else risk_score <- risk_score + 10
  } else if (age >= 50 & age <= 59) {
    if (totalChol < 160) risk_score <- risk_score + 0
    else if (totalChol >= 160 & totalChol <= 199) risk_score <- risk_score + 2
    else if (totalChol >= 200 & totalChol <= 239) risk_score <- risk_score + 4
    else if (totalChol >= 240 & totalChol <= 279) risk_score <- risk_score + 5
    else risk_score <- risk_score + 7
  } else if (age >= 60 & age <= 69) {
    if (totalChol < 160) risk_score <- risk_score + 0
    else if (totalChol >= 160 & totalChol <= 199) risk_score <- risk_score + 1
    else if (totalChol >= 200 & totalChol <= 239) risk_score <- risk_score + 2
    else if (totalChol >= 240 & totalChol <= 279) risk_score <- risk_score + 3
    else risk_score <- risk_score + 4
  } else { # age >= 70 & age <= 79
    if (totalChol < 160) risk_score <- risk_score + 0
    else if (totalChol >= 160 & totalChol <= 199) risk_score <- risk_score + 1
    else if (totalChol >= 200 & totalChol <= 239) risk_score <- risk_score + 1
    else if (totalChol >= 240 & totalChol <= 279) risk_score <- risk_score + 2
    else risk_score <- risk_score + 2
  }

  # HDL Cholesterol points
  if (hdlChol >= 60) risk_score <- risk_score - 1
  else if (hdlChol >= 50 & hdlChol <= 59) risk_score <- risk_score + 0
  else if (hdlChol >= 40 & hdlChol <= 49) risk_score <- risk_score + 1
  else risk_score <- risk_score + 2

  # Systolic blood pressure points (Untreated)
  if (systolic >= 120 & systolic <= 129) risk_score <- risk_score + 1
  else if (systolic >= 130 & systolic <= 139) risk_score <- risk_score + 2
  else if (systolic >= 140 & systolic <= 159) risk_score <- risk_score + 3
  else if (systolic >= 160) risk_score <- risk_score + 4

  # If smoker
  if (smoker == "Yes") {
    if (age >= 20 & age <= 39) risk_score <- risk_score + 8
    else if (age >= 40 & age <= 49) risk_score <- risk_score + 5
    else if (age >= 50 & age <= 59) risk_score <- risk_score + 3
    else if (age >= 60 & age <= 69) risk_score <- risk_score + 1
    else if (age >= 70 & age <= 79) risk_score <- risk_score + 1
  }

  res <- case_when(risk_score == 0 ~ "<1%",
                   risk_score >= 1 & risk_score <= 4 ~ "1%",
                   risk_score >= 5 & risk_score <= 6 ~ "2%",
                   risk_score == 7 ~ "3%",
                   risk_score == 8 ~ "4%",
                   risk_score == 9 ~ "5%",
                   risk_score == 10 ~ "6%",
                   risk_score == 11 ~ "8%",
                   risk_score == 12 ~ "10%",
                   risk_score == 13 ~ "12%",
                   risk_score == 14 ~ "16%",
                   risk_score == 15 ~ "20%",
                   risk_score == 16 ~ "25%",
                   risk_score >= 17 ~ "30%")

  return(res)
}
