{-# LANGUAGE NoImplicitPrelude #-}
module Handler.ShowLoanExplanation where
import Handler.Util (showAmt, niceShowClassicType, niceShowInstallmentAdjustment, niceShowEarlyRepaymentType)
import Import
import qualified Data.List as List (head,drop)

getShowLoanExplanationR :: Int -> Handler Value
getShowLoanExplanationR n = getMessageRender >>= \messageRender ->
                            (return $ object $ ["value" .= (messageRender $ MsgLoanExplanation l)
                                               ,"title" .= ccConfName l
                                               ,"loanKind" .= (niceShowClassicType $ clType l)
                                               ,"minFstInstDur" .= (show $ minFstInstDur l)
                                               ,"erType" .= (niceShowEarlyRepaymentType $ cccERType l)
                                               ,"maxDur" .= (show $ cccMaxDur l)
                                               ,"minInstAmt" .= (showAmtWithLen 10 $ cccMinInstAmt l)
                                               ,"days" .= messageRender MsgDays
                                               ,"instAdj" .= (niceShowInstallmentAdjustment $ cccInstAdj l)])
                            where l = List.head $ List.drop (n-1) confList
