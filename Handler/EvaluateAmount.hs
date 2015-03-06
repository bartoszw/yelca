module Handler.EvaluateAmount where
import Handler.Util (showAmt)
import Import

getEvaluateAmountR :: Text -> Handler Value
getEvaluateAmountR s = return $ object $ case readEitherPlus $ unpack s of
                                Right a -> ["value" .=  showAmt (a::Double)]
                                Left err -> ["error" .= Just ((pack err)::Text) ]

