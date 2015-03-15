module Handler.BalloonText where
import Handler.Util (showAmt)
import Import
import qualified Data.List as List (head,drop)

getBalloonTextR :: Int -> Handler Value
getBalloonTextR n = getMessageRender >>= \messageRender ->
                            (return $ object $ case clType $ List.head $ List.drop (n-1) confList  of
                                Balloon         -> ["value" .= messageRender MsgBalloon]
                                Vario2          -> ["value" .= messageRender MsgResidualBalloon]
                                Vario3          -> ["value" .= messageRender MsgResidualBalloon]
                                SecuredBalloon  -> ["value" .= messageRender MsgInstallment]
                                _ -> []
                            )
