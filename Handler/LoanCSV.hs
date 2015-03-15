{-# LANGUAGE NoImplicitPrelude #-}
module Handler.LoanCSV where
import Handler.Home (loanForm
                    ,displayInputForm
                    ,initLoan
                    )
import Handler.Util (showAmt
                    ,initErrors
                    ,presentLoan
                    ,total
--                    ,ValidMonad (..)
--                    ,InstalmentPlan 
--                    ,InstalmentPlanLine (..)
--                    ,Instalment (..)
                    ,Loan (..)
                    )
import Import
import qualified Data.Csv as CSV
import qualified Data.ByteString.Lazy as BS
--import Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy as TL
import qualified Data.Map as Map
import Control.Monad.Trans.Maybe
import qualified Data.List as List (head)

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

-- renderIP $ presentLoan testL

-- | Render a form into a series of tr tags. Note that, in order to allow
-- you to add extra rows to the table, this function does /not/ wrap up
-- the resulting HTML in a table tag; you must do that yourself.
renderIP :: AmorPlan -> Handler TL.Text
renderIP ip = do
    --case vip of
    --Right ip -> do
        let ipc = zip ip [1::(Int)..]
        --let (tiAmt,tiRep,tiI) = total ip
        mR <- getMessageRender
        let sA a = (fromIntegral a :: Double) / 100
        let header = decodeUtf8 $ CSV.encode $ [(("#"::Text),mR MsgInstallment,mR MsgRepayment,mR MsgInterestPaid,mR MsgPrincipalAfterPayment,mR MsgLateInterest)]
        let line tail (ipl,i) = (i,sA $ fstOf5 ipl,sA $ sndOf5 ipl,sA $ trdOf5 ipl,sA $ frthOf5 ipl,sA $ fvthOf5 ipl):tail
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
    --Left err -> return ""

getLoanFromSession :: Handler (Maybe Loan)
getLoanFromSession = do
        runMaybeT $ do
            l <- helperFS' "Loan" toClassicalCalcConf
            p <- helperFS "Principal" id
            d <- helperFS "Duration" id
            r <- helperFS "Rate" id
            de <- helperFS "Delay" id
            b <- helperFS "Balloon" id
            x <- helperFS "ExtDur" id
            xr <- helperFS "ExtRate" id
            fA <- helperFS "FeeAmt" id
            fP <- helperFS "FeePer" id
            return $ Loan l p d  r de b x xr fA fP 
    where   helperFS :: Read a => Text -> (a -> b) -> MaybeT Handler b
            helperFS key f = MaybeT $ (lookupSession key) >>= return . fmap (f . read . unpack)
            -- Loan's type id is a String, so read on it causes bad behaviour.
            helperFS' key f = MaybeT $ (lookupSession key) >>= return . fmap (f . unpack)
            toClassicalCalcConf a = List.head $  filter (\x -> ccConfFun x == a) confList


