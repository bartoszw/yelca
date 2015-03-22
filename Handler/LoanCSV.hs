{-# LANGUAGE NoImplicitPrelude #-}
module Handler.LoanCSV where
import Handler.Home (loanForm
                    ,displayInputForm
                    ,initLoan
                    )
import Handler.Util (showAmt
                    ,initErrors
                    ,presentLoan
                    ,presentLoanWithoutFee
                    ,total
                    ,newAbsLoan
                    ,feeAmor
                    ,Amount
                    ,Loan (..)
                    ,getLoanFromSession
                    )
import Import
import qualified Data.Csv as CSV
import qualified Data.ByteString.Lazy as BS
--import Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy as TL
import qualified Data.Map as Map

getLoanCSVR :: Handler TL.Text
getLoanCSVR = getLoanFromSession >>=  \l ->
                case l of
                    Just loan -> renderCSV loan
                    Nothing   -> notFound

    where renderCSV loan | isFee     = renderIPTwice ip ipWF (feeAmor (fee cl) (map trdOf5 ip) (map trdOf5 ipWF))
                         | otherwise = renderIP ip
                    where isFee = feeAmountS loan /= Nothing && feeAmountS loan /= Just 0 ||
                                  feePercentS loan /= Nothing && feePercentS loan /= Just 0
                          ip = presentLoan loan
                          ipWF = presentLoanWithoutFee loan
                          cl = calc $ newAbsLoan loan

-- | Render a form into a series of tr tags. Note that, in order to allow
-- you to add extra rows to the table, this function does /not/ wrap up
-- the resulting HTML in a table tag; you must do that yourself.
renderIP :: AmorPlan -> Handler TL.Text
renderIP ip = do
        let ipc = zip ip [1::(Int)..]
        mR <- getMessageRender
        let sA a = (fromIntegral a :: Double) / 100
            header = decodeUtf8 $ CSV.encode $ [(("#"::Text),mR MsgInstallment,mR MsgRepayment,mR MsgInterestPaid,mR MsgPrincipalAfterPayment,mR MsgLateInterest)]
            line tail (ipl,i) = (i,sA $ fstOf5 ipl,sA $ sndOf5 ipl,sA $ trdOf5 ipl,sA $ frthOf5 ipl,sA $ fvthOf5 ipl):tail
            result = header <> (decodeUtf8 $ CSV.encode $ reverse $ foldl' line [] ipc)
        return result

renderIPTwice :: AmorPlan -> AmorPlan -> [Amount] -> Handler TL.Text
renderIPTwice ip ipWF fees = do
        let ipc = zip4 ip ipWF fees [1::(Int)..]
        mR <- getMessageRender
        let sA a = (fromIntegral a :: Double) / 100
            header = decodeUtf8 $ CSV.encode $ [(("#"::Text),mR MsgInstallment,mR MsgRepayment,mR MsgInterestPaid,mR MsgPrincipalAfterPayment,mR MsgLateInterest)]
            header2 = decodeUtf8 $ CSV.encode $ [(("#"::Text),mR MsgInstallment,mR MsgRepayment,mR MsgInterestPaid,mR MsgPrincipalAfterPayment,mR MsgLateInterest,mR MsgFeeAmortisation)]
            line tail (ipl,iplWF,fee,i) = (i,sA $ fstOf5 ipl,sA $ sndOf5 ipl,sA $ trdOf5 ipl,sA $ frthOf5 ipl,sA $ fvthOf5 ipl) : tail
            line2 tail (ipl,iplWF,fee,i) = (i,sA $ fstOf5 iplWF,sA $ sndOf5 iplWF,sA $ trdOf5 iplWF,sA $ frthOf5 iplWF,sA $ fvthOf5 iplWF,sA fee):tail
            result = header <> (decodeUtf8 $ CSV.encode $ reverse $ foldl' line [] ipc) <> header2 <> (decodeUtf8 $ CSV.encode $ reverse $ foldl' line2 [] ipc) 
        return result




