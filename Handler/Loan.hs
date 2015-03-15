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
                    ,RoundingType (..)
                    ,Freq (..)
                    ,Amount
                    ,Rate
                    ,showAmt
                    ,isBalloon
                    ,isUnfoldedBalloon
                    ) 
import qualified Data.Csv as CSV
import qualified Data.List as List (head)

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
                        let isFee = feeAmountS loan /= Nothing && feeAmountS loan /= Just 0 ||
                                    feePercentS loan /= Nothing && feePercentS loan /= Just 0
                        displayInputForm MsgCalculator widget enctype
                        renderLoanOverview loan (cN2E $ List.head $ rEffRec $ calc $ newAbsLoan loan)
                    --    case presentLoan loan >>= recalculateEffectiveRate Monthly of
                    --        Right rate -> renderLoanOverview loan rate
                    --        Left err -> return () -- TODO: improve this
                        renderFoldedInstalmentPlan (installments $ calc $ newAbsLoan loan) (rNom $ calc $ newAbsLoan loan)
                        if isFee 
                            then renderIPTwice (presentLoan loan) (presentLoanWithoutFee loan)
                            else renderIP (presentLoan loan)

        _ -> defaultLayout $ displayInputForm MsgCalculatorValidation widget enctype

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


renderLoanOverview :: Loan -> Rate -> Widget
renderLoanOverview l rate = [whamlet|
     <p .my-text-right> 
        <a href=@{HomeR}>Home
     <h2>_{MsgLoan}: #{ccConfName $ loanS l} 
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
       |]

{-
            <tr>
                <td .small .my-text-right colspan=2>_{MsgFreq}
                <td .strong>#{show $ freqS l}
            <tr>
                <td .small .my-text-right colspan=2>_{MsgRoundingType}
                <td .strong>#{show $ roundingS l}
-}




renderFoldedInstalmentPlan :: InstPlan -> [Double] -> Widget
renderFoldedInstalmentPlan ip rs = do
--    case vip of
--    Right ip -> do
        let fip = zip ip rs -- foldIP ip
        [whamlet|
        <h2>_{MsgFIP}
        <table .table .table-hover>
            <tr>
                <th .my-text-right>_{MsgInstallment}
                <th .my-text-right>_{MsgNbrInst}
                <th .my-text-right>_{MsgNomRate}
            $forall (fipl,rN) <- fip
                <tr>
                    <td .my-text-amount>#{showAmtWithLen 10 (snd fipl)}
                    <td .my-text-amount>#{show $ fst fipl}
                    <td .my-text-amount>#{showWithLenDec 13 9 $ (rN * 100)} %
        |]
--    Left err -> [whamlet|
--        <p .errors>_{MsgUnexpectedError} #
--           <span .monospace>#{show err}
--        |]


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

renderIPTwice :: AmorPlan -> AmorPlan -> Widget
renderIPTwice ip ipWF = do
        let ipc = zip3 ip ipWF [1::(Int)..]
        let (tiAmt,tiRep,tiI) = total ip
        let (tiAmtWF,tiRepWF,tiIWF) = total ipWF
        [whamlet|
        <h2>_{MsgFullInstPlan}
        <table .table .table-hover>
            <tr>
                <th colspan="6" .td-border-right .my-text-center>_{MsgSelectedProduct}
                <th colspan="5" .my-text-center>_{MsgReferenceProductWithoutFee}
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
                <th .my-text-right>_{MsgLateInterest}
            $forall (ipl, iplWF, counter) <- ipc
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
                    <td .my-text-amount>#{showAmtWithLen 10 (fvthOf5 iplWF)}
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
                   <td .my-text-amount>
        |]
