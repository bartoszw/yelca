Calculator: Kreditrechner
CalculatorValidation: Kreditrechner - Korrektur
Loan: Kreditart
Principal: Kreditsumme
Duration: Laufzeit
InterestRate:Zinssatz
Deferrment: Aufschub der ersten Rate
Balloon: Balloon
MaxExtDur: Maximale erweitete Laufzeit
ExtDur: Erweitete Laufzeit
Freq: Ratenfrequenz
RoundingType: Rundungsart
Initial: Geben sie die Felder ein um Resultat zu bekommen...
ShowParameters: Parameter zeigen
Calculate: Berechne
DownloadPaper: Dokument herunterladen
YALC: Schon wieder ein Kreditrechner?
NotExactly: Nicht wirklich.
LongText: Well, it is  a loan calculator - which addresses one specific thing. If you ever thought what is happening with the roundings you might find here an answer. Shortly: usually roundings disappear under the carpet. In the end they do not make more than couple of cents difference, so noone cares. But to do things right, one has to cope with them. For more details look into the provided paper.
Monthly: Monatlich
Truncate: Abrunden
AmtTooltip: Arithmetische Satz ist möglich!

RecEffRate: Nachgerechneter effektiver Zinssatz
Months n@Int: #{plural n "Monat" "Monate" "Monate"}
Installment: Ratenbetrag
NbrInst: Laufzeit
NomRate: Nenn monatlichen Zinssatz
FullInstPlan: Voller Ratenplan
Repayment: Tilgung
InterestCalc: Berechneter Zinsen
InterestPaid: Bezahlte Zinsen
PrincipalAfterPayment: Saldo nach Ratenzahlung
LateInterest: Verschobene Zinsen
Total: Total
FIP: Gefalteter Ratenplan

AmountToolTip: Key in amount or an arithmetic expression. Use dot or comma as decimal point
PrincipalLowerBoundary amt: Ratenbetrag muss >= #{show amt} sein
DurationLowerBoundary dur: Laufzeit muss >= #{show dur} sein
RateLowerBoundary r@Double: Zinssatz muss >= #{show r} sein
RateUpperBoundary r@Double: Zinssatz muss <= #{show r} sein
Populate: Dieses Feld muss befüllt werden
InstallmentUpperBoundary a: Ratenbetrag muss <= #{show a} sein
BalloonUpperBoundary: Balloon muss <= Kreditbetrag sein
ExtDurLowerBoundary n: Erweitete Laufzei muss >= #{show n} sein
ExtDurUpperBoundary n: Erweitete Laufzei muss <= #{show n} sein

ResidualBalloon: Restlicher Saldo

LoanExplanation l: #{loanExplanation l "Jeder Ratenbetrag ist gleich." "Letzte Rate (Ballonrate) ist gegeben, die anderen sind alle gleich." "Alle Raten sind gleich außer der letzten, die ist um gegebenen Ballonbetrag großer." "Alle Raten sind gleich dem gegebenem Ratenbetrag, außer der letzten, die ist berechnet." "Ist wie Balloon, mit Einschränkung, dass all Raten sind Null, außer erster, die enthält alle Zinsen und der letzten, die enthält ganzes Saldo." "Prodult mit zwei Laufzeiten mit gegebener ersten Laufzeit, maximaler zweiten Laufzeit und Restlichen Saldo (RS). RS dient der Berechnung der Ratenbetrag wie beim Balloon. Ratenbetrag in zweiter Laufzeit gleicht der von erster, die zweite Laufzeit ist berechnet (last installment may differ then). In case 2nd perion duration would be greater than given maximum duration of extended period, the duration is limited and installment amount of 2nd period is recalculated (and greater than one of 1st period)." "Two periods product with given duration of the 1st one and residual balloon amount (RBA). RBA serves to calculate first period installment amount like for Balloon loan. 2nd period amount is equal to the 1st one, its duration is calculated (last installment may differ then). In case 2nd perion duration would be greater than given maximum duration of extended period, the duration is limited and installment amount of 2nd period is recalculated (and greater than one of 1st period)." }
UnexpectedError: Ein unerwarteter Fehler ist aufgetreten:
DownloadCsv: CSV herunterladen