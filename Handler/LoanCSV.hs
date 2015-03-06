module Handler.LoanCSV where
import Handler.Home (loanForm
                    ,displayInputForm
                    ,initLoan
                    )
import Handler.Util (showAmt
                    ,initErrors
                    ,presentLoan
                    ,total
                    ,ValidMonad (..)
                    ,InstalmentPlan 
                    ,InstalmentPlanLine (..)
                    ,Instalment (..)
                    ,Loan (..)
                    )
import Import
import qualified Data.Csv as CSV
import qualified Data.ByteString.Lazy as BS
--import Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy as TL
import qualified Data.Map as Map
import Control.Monad.Trans.Maybe

{-
postLoanCSVR :: Handler TL.Text
postLoanCSVR = do
    ((result, widget), enctype) <- runFormPost $ loanForm initLoan initErrors
    case result of
        FormSuccess loan -> do
            renderIP $ presentLoan loan
      --  _ -> defaultLayout $ displayInputForm MsgCalculatorValidation widget enctype
-}
    
getLoanCSVR :: Handler TL.Text
getLoanCSVR = getLoanFromSession >>=  \l ->
                case l of
                    Just loan -> renderIP $ presentLoan loan
                    Nothing   -> notFound

-- | Render a form into a series of tr tags. Note that, in order to allow
-- you to add extra rows to the table, this function does /not/ wrap up
-- the resulting HTML in a table tag; you must do that yourself.
renderIP :: ValidMonad InstalmentPlan -> Handler TL.Text
renderIP vip =  case vip of
    Right ip -> do
        let ipc = zip ip [1::(Int)..]
        let (tiAmt,tiRep,tiIP,tiI) = total ip
        mR <- getMessageRender
        let sA a = (fromIntegral a :: Double) / 100
        let header = decodeUtf8 $ CSV.encode $ [(("#"::Text),mR MsgInstallment,mR MsgRepayment,mR MsgInterestCalc,mR MsgInterestPaid,mR MsgPrincipalAfterPayment,mR MsgLateInterest)]
        let line tail (ipl,i) = (i,sA $ iAmt $ iplInst ipl,sA $ iRepayment $ iplInst ipl,iInterest (iplInst ipl) / 100,sA $ iIntPaid $ iplInst ipl,sA $ iplPrincipal ipl,iplIntLate ipl / 100):tail
        let result = header <> (decodeUtf8 $ CSV.encode $ reverse $ foldl' line [] ipc)
        return result

{-            
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
-}
    Left err -> return ""

getLoanFromSession :: Handler (Maybe Loan)
getLoanFromSession = do
        runMaybeT $ do
            l <- helperFS "Loan" toEnum
            p <- helperFS "Principal" id
            d <- helperFS "Duration" id
            r <- helperFS "Rate" id
            de <- helperFS "Delay" id
            b <- helperFS "Balloon" id
            x <- helperFS "ExtDur" id
            f <- helperFS "Freq" toEnum
            rd <- helperFS "Rounding" toEnum
            return $ Loan l p d  r de b x f rd
    where   helperFS :: Read a => Text -> (a -> b) -> MaybeT Handler b
            helperFS key f = MaybeT $ (lookupSession key) >>= return . fmap (f . read . unpack)
