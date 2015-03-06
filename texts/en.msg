Calculator: Loan Calculator
CalculatorValidation: Loan Calculator - Validation
Loan: Type of loan
Principal: Principal
Duration: Duration
InterestRate:Interest rate
Deferrment: First installment deferrment
Balloon: Balloon
MaxExtDur: Maximal Extended Duration
ExtDur: Extended Duration
Freq: Installment frequency
RoundingType: Type of rounding
Initial: Populate fields you want and submit the form to get the result...
ShowParameters: Show parameters
Calculate: Calculate
DownloadPaper: Download paper
YALC: Yet another Loan Calculator?
NotExactly: Not exactly.
LongText: Well, it is  a loan calculator - which addresses one specific thing. If you ever thought what is happening with the roundings you might find here an answer. Shortly: usually roundings disappear under the carpet. In the end they do not make more than couple of cents difference, so noone cares. But to do things right, one has to cope with them. For more details look into the provided paper.
Monthly: Monthly
Truncate: Truncate
AmtTooltip: You can enter an arithmetic statment!

RecEffRate: Recalculated effective interest rate
Months n@Int: #{plural n "Month" "Months" "Months"}
Installment: Installment Amount
NbrInst: Number of installments
NomRate: Nominal monthly interest rate
FullInstPlan: Full Installment Plan
Repayment: Repayment 
InterestCalc: Interest calculated
InterestPaid: Interest paid
PrincipalAfterPayment: Principal after payment
LateInterest: Late interest
Total: Total
FIP: Folded Installment Plan

AmountToolTip: Key in amount or an arithmetic expression. Use dot or comma as decimal point
PrincipalLowerBoundary amt@Integer: Loan amount has to be >= #{show amt}
DurationLowerBoundary dur@Int: Duration has to be >= #{show dur}
RateLowerBoundary r@Double: Rate has to be >= #{show r}
RateUpperBoundary r@Double: Rate has to be <= #{show r}
Populate: This field has to be populated
InstallmentUpperBoundary a@Double: Installment amount has to be <= #{show a}
BalloonUpperBoundary: Balloon has to be <= Principal
ExtDurLowerBoundary n@Int: Extended duration has to be >= #{show n}
ExtDurUpperBoundary n@Int: Extended duration has to be <= #{show n}

ResidualBalloon: Residual balloon

LoanExplanation l@GUIClassic: #{loanExplanation l "Each installment has equal amount and duration." "Last installment (balloon) is given, the others are equal." "All installments are qual except the last one, which is greater than each other by given balloon amount." "Each installment is equal to given amount but the last one which is calculated." "Like Balloon, where all installments equal 0 except last one which contains full capital and first one which contains only interest." "Two periods product with given duration of the 1st one and residual balloon amount (RBA). RBA serves to calculate first period installment amount like for Balloon loan. 2nd period amount is equal to the 1st one, its duration is calculated (last installment may differ then). In case 2nd perion duration would be greater than given maximum duration of extended period, the duration is limited and installment amount of 2nd period is recalculated (and greater than one of 1st period)." "Two periods product with given duration of the 1st one and residual balloon amount (RBA). RBA serves to calculate first period installment amount like for Balloon Plus loan. 2nd period amount is equal to the 1st one, its duration is calculated (last installment may differ then). In case 2nd perion duration would be greater than given maximum duration of extended period, the duration is limited and installment amount of 2nd period is recalculated (and greater than one of 1st period)."}
UnexpectedError: An unexpected error occured:
DownloadCsv: Download CSV