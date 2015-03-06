module Handler.ShowLoanExplanation where
import Handler.Util (showAmt)
import Import

getShowLoanExplanationR :: Int -> Handler Value
getShowLoanExplanationR n = getMessageRender >>= \messageRender ->
                            (return $ object $ ["value" .= (messageRender $ MsgLoanExplanation $ toEnum $ n-1)
                                               ,"title" .= show (toEnum $ n-1 :: GUIClassic)])
