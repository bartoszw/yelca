module Handler.Loan where
import Import
import Handler.Home (loanForm
                    ,displayInputForm
                    ,initLoan
                    )
import Handler.Util (Loan (..)
                    ,initErrors
                    ,loanValidation
                    ,anyError
                    ,presentLoan
                    ,total
                    ,ClassicLoan (..)
                    ,Classical (..)
                    ,Balloon (..)
                    ,BalloonPlus (..)
                    ,ReversBalloon (..)
                    ,Bullet (..)
                    ,UnfdBalloon (..)
                    ,UnfdBalloonPlus (..)
                    ,InstalmentLoanData (..)
                    ,runWithIPP
                    ,InstalmentPlanParam (..)
                    ,RoundingType (..)
                    ,Freq (..)
                    ,ValidMonad (..)
                    ,InstalmentPlan 
                    ,InstalmentPlanLine (..)
                    ,Instalment (..)
                    ,Amount
                    ,Interest
                    ,Rate
                    ,FoldedInstalmentPlan
                    ,FoldedInstalmentPlanLine (..)
                    ,showAmt
                    ,isBalloon
                    ,isUnfoldedBalloon
                    ,recalculateEffectiveRate
                    ,foldIP
                    ) 
import qualified Data.Csv as CSV

-- The POST handler processes the form. If it is successful, it displays the
-- parsed person. Otherwise, it displays the form again with error messages.
postLoanR :: Handler Html
postLoanR = do
    ((result, widget), enctype) <- runFormPost $ loanForm (initLoan) initErrors
    case result of
        FormSuccess loan -> do
            let ip = presentLoan loan
            setLoanToSession loan
            (widget, enctype) <- generateFormPost $ loanForm (loan) (loanValidation loan)
            defaultLayout $ case (anyError $ loanValidation loan) of
                    True -> displayInputForm MsgCalculatorValidation widget enctype
                    False -> do
                        displayInputForm MsgCalculator widget enctype
                        case presentLoan loan >>= recalculateEffectiveRate Monthly of
                            Right rate -> renderLoanOverview loan rate
                            Left err -> return () -- TODO: improve this
                        renderFoldedInstalmentPlan ip
                        renderIP $ presentLoan loan

        _ -> defaultLayout $ displayInputForm MsgCalculatorValidation widget enctype

-- | Storying loan details in section for purpose of retreiving calculated details via GET (CSV,XSL,...)
setLoanToSession :: MonadHandler m => Loan -> m ()
setLoanToSession loan = do
            setSession "Loan" (pack $ show $ fromEnum $ loanS loan)
            setSession "Principal" (pack $ show $ principalS loan)
            setSession "Duration"  (pack $ show $ durationS loan)
            setSession "Rate" (pack $ show $ rateS loan)
            setSession "Delay" (pack $ show $ delayS loan)
            setSession "Balloon" (pack $ show $ balloonS loan)
            setSession "ExtDur" (pack $ show $ extDurS loan)
            setSession "Freq" (pack $ show $ fromEnum $ freqS loan)
            setSession "Rounding" (pack $ show $ fromEnum $ roundingS loan)

renderLoanOverview :: Loan -> Rate -> Widget
renderLoanOverview l rate = [whamlet|
     <p .my-text-right> 
        <a href=@{HomeR}>Home
     <h2>_{MsgLoan}: #{show $ loanS l} 
     <table .table .table-bordered .table-layout-fixed>
            <tr>
                <td .strong .my-text-center>#{showAmtWithLen 10 $ principalS l}
                <td .strong .my-text-center>#{show $ durationS l} _{MsgMonths $ durationS l}
                <td .strong .my-text-center>#{showWithLenDec 7 3 $ rateS l * 100} %
            <tr>
                <td .small .my-text-right colspan=2>_{MsgRecEffRate}
                <td .strong .my-text-center>#{showWithLenDec 7 3 (rate * 100)} %
            <tr>
                $maybe del <- delayS l
                    $if del > 0
                        <td .small .my-text-right>_{MsgDeferrment}
                        <td .strong colspan=2>#{show del} _{MsgMonths del}
            <tr>
                $maybe bal <- balloonS l
                    $if isBalloon (loanS l)
                        <td .small .my-text-right colspan=2>_{MsgBalloon}
                        <td .strong>#{showAmtWithLen 10 $ bal}
            <tr>
                $maybe ext <- extDurS l
                    $if isUnfoldedBalloon (loanS l)
                        <td .small .my-text-right colspan=2>_{MsgExtDur}
                        <td .strong>#{show ext} _{MsgMonths ext}
            <tr>
                <td .small .my-text-right colspan=2>_{MsgFreq}
                <td .strong>#{show $ freqS l}
            <tr>
                <td .small .my-text-right colspan=2>_{MsgRoundingType}
                <td .strong>#{show $ roundingS l}
       |]

renderFoldedInstalmentPlan :: ValidMonad InstalmentPlan -> Widget
renderFoldedInstalmentPlan vip = case vip of
    Right ip -> do
        let fip = foldIP ip
        [whamlet|
        <h2>_{MsgFIP}
        <table .table .table-hover>
            <tr>
                <th .my-text-right>_{MsgInstallment}
                <th .my-text-right>_{MsgNbrInst}
                <th .my-text-right>_{MsgNomRate}
            $forall fipl <- fip
                <tr>
                    <td .my-text-amount>#{showAmtWithLen 10 (fiplAmt fipl)}
                    <td .my-text-amount>#{show $ fiplDur fipl}
                    <td .my-text-amount>#{showWithLenDec 13 9 $ (fiplRate fipl * 100)} %
        |]
    Left err -> [whamlet|
        <p .errors>_{MsgUnexpectedError} #
           <span .monospace>#{show err}
        |]


-- | Render a form into a series of tr tags. Note that, in order to allow
-- you to add extra rows to the table, this function does /not/ wrap up
-- the resulting HTML in a table tag; you must do that yourself.
renderIP :: ValidMonad InstalmentPlan -> Widget
renderIP vip =  case vip of
    Right ip -> do
        let ipc = zip ip [1::(Int)..]
        let (tiAmt,tiRep,tiIP,tiI) = total ip
        [whamlet|
        <h2>_{MsgFullInstPlan}
        <table .table .table-hover>
            <tr>
                <th .my-text-right> ##
                <th .my-text-right>_{MsgInstallment}
                <th .my-text-right>_{MsgRepayment}
                <th .my-text-right>_{MsgInterestCalc}
                <th .my-text-right>_{MsgInterestPaid}
                <th .my-text-right>_{MsgPrincipalAfterPayment}
                <th .my-text-right>_{MsgLateInterest}
            $forall (ipl,counter) <- ipc
                <tr>
                    <td .my-text-amount>#{counter}
                    <td .my-text-amount>#{showAmtWithLen 10 (iAmt $ iplInst ipl)}
                    <td .my-text-amount>#{showAmtWithLen 10 (iRepayment $ iplInst ipl)}
                    <td .my-text-amount>#{showWithLenDec 10 4 (iInterest (iplInst ipl) / 100)}
                    <td .my-text-amount>#{showAmtWithLen 10 (iIntPaid $ iplInst ipl)}
                    <td .my-text-amount>#{showAmtWithLen 10 (iplPrincipal ipl)}
                    <td .my-text-amount>#{showWithLenDec 15 4 (iplIntLate ipl / 100)}
            <tfoot>
               <tr .footer>
                   <td .my-text-right>_{MsgTotal}
                   <td .my-text-amount>#{showAmtWithLen 10 tiAmt}
                   <td .my-text-amount>#{showAmtWithLen 10 tiRep}
                   <td .my-text-amount>#{showWithLenDec 14 4 (tiI / 100)}
                   <td .my-text-amount>#{showAmtWithLen 10 tiIP}
                   <td .my-text-amount>
                   <td .my-text-amount>
        |]
    Left err -> [whamlet|        |]