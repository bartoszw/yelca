{-# LANGUAGE NoImplicitPrelude #-}
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
                    ,newAbsLoan
                    ,presentLoanWithoutFee
                    ,total
                    ,feeAmor
                    ,RoundingType (..)
                    ,Freq (..)
                    ,Amount
                    ,Rate
                    ,showAmt
                    ,isBalloon
                    ,isUnfoldedBalloon
                    ,simpleLoanHash
                    ,Financing (..)
                    ,LoanErrors
                    ,fieldInstDay
                    ,fieldFinDate
                    ,instDayField
                    ,financingForm
                    ,initFinancing
                    ,displayInputFormFin
                    ,getLoanFromSession
                    ) 
--import qualified Data.Csv as CSV
--import qualified Data.List as List (head)
import qualified Data.Map.Strict as Map


-- The POST handler processes the form. If it is successful, it displays the
-- parsed person. Otherwise, it displays the form again with error messages.
postLoanR :: Handler Html
postLoanR = do
    ((result, widget), enctype) <- runFormPost $ loanForm (initLoan) initErrors
    case result of
        FormSuccess loan -> do
            let ip = presentLoan loan
                ipWF = presentLoanWithoutFee loan
                cl = calc $ newAbsLoan loan
            setLoanToSession loan
            (widget, enctype) <- generateFormPost $ loanForm (loan) (loanValidation loan)
            (widgetFin, enctypeFin) <- generateFormPost $ financingForm initFinancing initErrors
            defaultLayout $ case (anyError $ loanValidation loan) of
                    True -> displayInputForm MsgCalculatorValidation widget enctype
                    False -> do
                        let isFee = feeAmountS loan /= Nothing && feeAmountS loan /= Just 0 ||
                                    feePercentS loan /= Nothing && feePercentS loan /= Just 0
                        displayInputForm MsgLoanScreen widget enctype
                        renderLoanOverview loan  (fee cl)
                        displayInputFormFin loan MsgFinancing widgetFin enctypeFin
                        renderFoldedInstalmentPlan (installments cl) (rNom cl) (rEffRec cl)
                        if isFee 
                            then renderIPTwice ip ipWF (feeAmor (fee cl) (map trdOf5 ip) (map trdOf5 ipWF))
                            else renderIP ip
        _ -> defaultLayout $ displayInputForm MsgCalculatorValidation widget enctype

getLoanR :: Handler Html
getLoanR = getLoanFromSession >>=  \l ->
                case l of
                    Just loan -> do
                            let ip = presentLoan loan
                                ipWF = presentLoanWithoutFee loan
                                cl = calc $ newAbsLoan loan
                            (widget, enctype) <- generateFormPost $ loanForm (loan) (loanValidation loan)
                            (widgetFin, enctypeFin) <- generateFormPost $ financingForm initFinancing initErrors
                            defaultLayout $ case (anyError $ loanValidation loan) of
                                                True -> displayInputForm MsgCalculatorValidation widget enctype
                                                False -> do
                                                    let isFee = feeAmountS loan /= Nothing && feeAmountS loan /= Just 0 ||
                                                                feePercentS loan /= Nothing && feePercentS loan /= Just 0
                                                    displayInputForm MsgLoanScreen widget enctype
                                                    renderLoanOverview loan  (fee cl)
                                                    displayInputFormFin loan MsgFinancing widgetFin enctypeFin
                                                    renderFoldedInstalmentPlan (installments cl) (rNom cl) (rEffRec cl)
                                                    if isFee 
                                                        then renderIPTwice ip ipWF (feeAmor (fee cl) (map trdOf5 ip) (map trdOf5 ipWF))
                                                        else renderIP ip
                    _ -> redirect HomeR --defaultLayout $ displayInputForm MsgCalculatorValidation widget enctype


-- | Storying loan details in section for purpose of retreiving calculated details via GET (CSV,XSL,...)
setLoanToSession :: MonadHandler m => Loan -> m ()
setLoanToSession loan = do
            setSession "Loan" (pack $ ccConfFun $ loanS loan)
            setSession "Principal" (pack $ show $ principalS loan)
            setSession "Duration"  (pack $ show $ durationS loan)
            setSession "Rate" (pack $ show $ rateS loan)
            setSession "Delay" (pack $ show $ delayS loan)
            setSession "Balloon" (pack $ show $ balloonS loan)
            setSession "ExtDur" (pack $ show $ extDurS loan)
            setSession "ExtRate" (pack $ show $ extRateS loan)
            setSession "FeeAmt" (pack $ show $ feeAmountS loan)
            setSession "FeePer" (pack $ show $ feePercentS loan)


renderLoanOverview :: Loan -> Amount -> Widget
renderLoanOverview l feee = do
    let isFee = feee > 0
    [whamlet|
     <h2 #result>_{MsgLoan}: #{ccConfName $ loanS l} 
     <table .table .table-bordered .table-layout-fixed>
            <tr>
                <td .strong .my-text-center>#{showAmtWithLen 10 $ principalS l}
                <td .strong .my-text-center>#{show $ durationS l} _{MsgMonths $ durationS l}
                <td .strong .my-text-center>#{showWithLenDec 7 3 $ rateS l * 100} %
            <tr>
                $maybe del <- delayS l
                    $if del > 0
                        <td .small .my-text-right>_{MsgDeferrment}
                        <td .strong colspan=2>#{show del} _{MsgMonths del}
            <tr>
                $maybe bal <- balloonS l
                    $if isBalloon (clType $ loanS l)
                        <td .small .my-text-right colspan=2>_{MsgBalloon}
                        <td .strong>#{showAmtWithLen 10 $ bal}
            <tr>
                $maybe ext <- extDurS l
                    $if isUnfoldedBalloon (clType $ loanS l)
                        <td .small .my-text-right colspan=2>_{MsgMaxExtDur}
                        <td .strong>#{show ext} _{MsgMonths ext}
            <tr>
                $maybe extR <- extRateS l
                    $if isUnfoldedBalloon (clType $ loanS l)
                        <td .small .my-text-right colspan=2>_{MsgExtRate}
                        <td .strong>#{show (100 * extR)} %
            <tr>
                $maybe fA <- feeAmountS l
                        <td .small .my-text-right colspan=2>_{MsgFeeAmt}
                        <td .strong>#{showAmtWithLen 10 fA}
            <tr>
                $maybe fP <- feePercentS l
                        <td .small .my-text-right colspan=2>_{MsgFeePercent}
                        <td .strong>#{showWithLenDec 7 3 (100 * fP)} %
            <tr>
                $if isFee
                        <td .small .my-text-right colspan=2>_{MsgFeeAmt}
                        <td .strong>#{showAmtWithLen 10 feee}
       |]


renderFoldedInstalmentPlan :: InstPlan -> [Double] -> [Double] -> Widget
renderFoldedInstalmentPlan ip rs ers = do
        let fip = zip3 ip rs ers 
        [whamlet|
        <h2>_{MsgFIP}
        <table .table .table-hover>
            <tr>
                <th .my-text-right>_{MsgInstallment}
                <th .my-text-right>_{MsgNbrInst}
                <th .my-text-right>_{MsgNomRate}
                <th .my-text-right>_{MsgRecEffRate}
            $forall (fipl,rN, rE) <- fip
                <tr>
                    <td .my-text-amount>#{showAmtWithLen 10 (snd fipl)}
                    <td .my-text-amount>#{show $ fst fipl}
                    <td .my-text-amount>#{showWithLenDec 13 9 $ (rN * 100)} %
                    <td .my-text-amount>#{showWithLenDec 7 3 $ (cN2E rE * 100)} %
        |]

-- | Render a form into a series of tr tags. Note that, in order to allow
-- you to add extra rows to the table, this function does /not/ wrap up
-- the resulting HTML in a table tag; you must do that yourself.
renderIP :: AmorPlan -> Widget
renderIP ip = do
        let ipc = zip ip [1::(Int)..]
        let (tiAmt,tiRep,tiI) = total ip
        [whamlet|
        <h2>_{MsgFullInstPlan}
        <table .table .table-hover>
            <tr>
                <th .my-text-right> ##
                <th .my-text-right>_{MsgInstallment}
                <th .my-text-right>_{MsgRepayment}
                <th .my-text-right>_{MsgInterestPaid}
                <th .my-text-right>_{MsgPrincipalAfterPayment}
                <th .my-text-right>_{MsgLateInterest}
            $forall (ipl,counter) <- ipc
                <tr>
                    <td .my-text-amount>#{counter}
                    <td .my-text-amount>#{showAmtWithLen 10 (fstOf5 ipl)}
                    <td .my-text-amount>#{showAmtWithLen 10 (sndOf5 ipl)}
                    <td .my-text-amount>#{showAmtWithLen 10 (trdOf5 ipl)}
                    <td .my-text-amount>#{showAmtWithLen 10 (frthOf5 ipl)}
                    <td .my-text-amount>#{showAmtWithLen 10 (fvthOf5 ipl)}
            <tfoot>
               <tr .footer>
                   <td .my-text-right>_{MsgTotal}
                   <td .my-text-amount>#{showAmtWithLen 10 tiAmt}
                   <td .my-text-amount>#{showAmtWithLen 10 tiRep}
                   <td .my-text-amount>#{showAmtWithLen 10 tiI}
                   <td .my-text-amount>
                   <td .my-text-amount>
        |]

renderIPTwice :: AmorPlan -> AmorPlan -> [Amount] -> Widget
renderIPTwice ip ipWF fees = do
        let ipc = zip4 ip ipWF fees [1::(Int)..]
        let (tiAmt,tiRep,tiI) = total ip
        let (tiAmtWF,tiRepWF,tiIWF) = total ipWF
        [whamlet|
        <h2>_{MsgFullInstPlan}
        <table .table .table-hover>
            <tr>
                <th colspan="6" .td-border-right .my-text-center>_{MsgSelectedProduct}
                <th colspan="5" .td-border-right .my-text-center>_{MsgReferenceProductWithoutFee}
                <th .my-text-center>
            <tr>
                <th .my-text-right> ##
                <th .my-text-right>_{MsgInstallment}
                <th .my-text-right>_{MsgRepayment}
                <th .my-text-right>_{MsgInterestPaid}
                <th .my-text-right>_{MsgPrincipalAfterPayment}
                <th .my-text-right .td-border-right>_{MsgLateInterest}
                <th .my-text-right>_{MsgInstallment}
                <th .my-text-right>_{MsgRepayment}
                <th .my-text-right>_{MsgInterestPaid}
                <th .my-text-right>_{MsgPrincipalAfterPayment}
                <th .my-text-right .td-border-right>_{MsgLateInterest}
                <th .my-text-right>_{MsgFeeAmortisation}
            $forall (ipl, iplWF, fee, counter) <- ipc
                <tr>
                    <td .my-text-amount>#{counter}
                    <td .my-text-amount>#{showAmtWithLen 10 (fstOf5 ipl)}
                    <td .my-text-amount>#{showAmtWithLen 10 (sndOf5 ipl)}
                    <td .my-text-amount>#{showAmtWithLen 10 (trdOf5 ipl)}
                    <td .my-text-amount>#{showAmtWithLen 10 (frthOf5 ipl)}
                    <td .my-text-amount .td-border-right>#{showAmtWithLen 10 (fvthOf5 ipl)}
                    <td .my-text-amount>#{showAmtWithLen 10 (fstOf5 iplWF)}
                    <td .my-text-amount>#{showAmtWithLen 10 (sndOf5 iplWF)}
                    <td .my-text-amount>#{showAmtWithLen 10 (trdOf5 iplWF)}
                    <td .my-text-amount>#{showAmtWithLen 10 (frthOf5 iplWF)}
                    <td .my-text-amount .td-border-right>#{showAmtWithLen 10 (fvthOf5 iplWF)}
                    <td .my-text-amount>#{showAmtWithLen 10 fee}
            <tfoot>
               <tr .footer>
                   <td .my-text-right>_{MsgTotal}
                   <td .my-text-amount>#{showAmtWithLen 10 tiAmt}
                   <td .my-text-amount>#{showAmtWithLen 10 tiRep}
                   <td .my-text-amount>#{showAmtWithLen 10 tiI}
                   <td .my-text-amount>
                   <td .my-text-amount .td-border-right>
                   <td .my-text-amount>#{showAmtWithLen 10 tiAmtWF}
                   <td .my-text-amount>#{showAmtWithLen 10 tiRepWF}
                   <td .my-text-amount>#{showAmtWithLen 10 tiIWF}
                   <td .my-text-amount>
                   <td .my-text-amount .td-border-right>
                   <td .my-text-amount>
        |]

