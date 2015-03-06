module Handler.BalloonText where
import Handler.Util (showAmt)
import Import

getBalloonTextR :: Int -> Handler Value
getBalloonTextR n = getMessageRender >>= \messageRender ->
                            (return $ object $ case toEnum (n-1) of
                                ClBalloon               -> ["value" .= messageRender MsgBalloon]
                                ClUnfoldedBalloon       -> ["value" .= messageRender MsgResidualBalloon]
                                ClUnfoldedBalloonPlus   -> ["value" .= messageRender MsgResidualBalloon]
                                ClReversBalloon         -> ["value" .= messageRender MsgInstallment]
                                _ -> []
                            )
