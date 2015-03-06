{-# LANGUAGE FlexibleContexts #-}
module Handler.Util (module Handler.Util
                    ,module Haslo
       )
       where
import Import
import qualified Data.Text.Read      (signed,decimal,double)
import qualified Data.Map.Strict as Map
import Haslo

data Loan = Loan
               { loanS        :: GUIClassic
               , principalS   :: Amount
               , durationS    :: Duration
               , rateS        :: Rate
               , delayS       :: Maybe Duration
               , balloonS     :: Maybe Amount
               , extDurS      :: Maybe Duration
               , freqS        :: Freq
               , roundingS    :: RoundingType
               }
               deriving (Show,Eq)

type LoanErrors = Map.Map Text AppMessage

instance Show GUIClassic where 
    show ClClassical = "Classical"
    show ClBalloon = "Balloon"
    show ClBalloonPlus = "Balloon Plus"
    show ClReversBalloon = "Reversal Balloon"
    show ClBullet = "Bullet"
    show ClUnfoldedBalloon = "Unfolded Balloon"
    show ClUnfoldedBalloonPlus = "Unfolded Balloon Plus"

showAmt :: Double -> String
showAmt a = showAmtWithLen (3+l) a
    where l = length $ show (truncate a :: Integer)

-- | Allows human friendly show of loan type.
niceShow :: GUIClassic -> String
niceShow = drop 2 . show

confList = [minBound .. maxBound::GUIClassic]
loanList = map niceShow confList

nonBalloon = [ClClassical, ClBullet]
balloonsList = ClBalloon : ClBalloonPlus : ClReversBalloon : unfoldedBalloonList
isBalloon l = l `elem` balloonsList
unfoldedBalloonList = [ClUnfoldedBalloon,ClUnfoldedBalloonPlus]
isUnfoldedBalloon l = l `elem` unfoldedBalloonList

roundingList = [Rounded,Truncated]
roundingShowList = map show roundingList
freqList = [Monthly,Yearly]
freqShowList = map show freqList

fieldLoan,fieldFreq,fieldRound,fieldDeferrment :: Text
fieldLoan = "fieldLoan"
fieldPrincipal = "fieldPrincipal"
fieldDuration = "fieldDuration"
fieldRate = "fieldRate"
fieldDeferrment = "fieldDeferrment"
fieldBalloon = "fieldBalloon"
fieldExtDur = "fieldExtDur"
fieldFreq = "fieldFreq"
fieldRound = "fieldRound"
fieldLoanExplanation = "fieldLoanExplanation"

initErrors = Map.empty
anyError errs = not $ Map.null errs

-- | Contextual validation - context is Loan record, validated are its all fields.
loanValidation :: Loan -> LoanErrors
loanValidation l = (extDurValidation l . balloonValidation l . rateValidation l . durationValidation l . principalValidation l) 
                    initErrors

principalValidation :: Loan -> LoanErrors -> LoanErrors
principalValidation l err | principalS l < a * 100 = Map.insert fieldPrincipal (MsgPrincipalLowerBoundary a) err 
                          | otherwise              = err
       where a = 100

durationValidation l err | durationS l < n = Map.insert fieldDuration (MsgDurationLowerBoundary 1) err
                         | loanS l == ClBalloon &&
                           durationS l < nBal = Map.insert fieldDuration (MsgDurationLowerBoundary nBal) err
                         | otherwise       = err
       where n = 1
             nBal = 2

rateValidation l err | rateS l < rMin     = Map.insert fieldRate (MsgRateLowerBoundary rMin) err --"Rate has to be >= 0" err
                     | rateS l > rMax     = Map.insert fieldRate (MsgRateUpperBoundary rMax) err -- "Rate has to be <= 100" err
                     | otherwise          = err
       where rMin = 0
             rMax = 100

balloonValidation l err | loanS l `elem` nonBalloon   = err 
                        | balloonS l == Nothing       = Map.insert fieldBalloon MsgPopulate err -- ("This field has to be populated") err
                        | loanS l == ClReversBalloon  && 
                           bal > maxI                 = Map.insert fieldBalloon (MsgInstallmentUpperBoundary $ maxI / 100) err -- ("Installment amount has to be <=" <> pack (showAmt $ maxI / 100)) err
                        | isBalloon (loanS l) &&
                           bal > pri                  = Map.insert fieldBalloon MsgBalloonUpperBoundary err -- "Balloon has to be <= Principal" err
                        | otherwise                   = err
       where pri = fromIntegral $ principalS l
             dur = fromIntegral $ durationS l
             maxI = pri / dur
             bal = fromIntegral $ fromJust $ balloonS l

extDurValidation l err | not $ loanS l `elem` unfoldedBalloonList   = err
                       | extDurS l == Nothing = Map.insert fieldExtDur MsgPopulate err -- "Extended duration has to be populated" err
                       | eD < edMin    = Map.insert fieldExtDur (MsgExtDurLowerBoundary edMin) err --"Extended duration has to be >= 0" err
                       | eD > edMax    = Map.insert fieldExtDur (MsgExtDurUpperBoundary edMax) err -- "Extended duration has to be <= 100" err
                       | otherwise            = err
       where eD = fromJust $ extDurS l
             edMin = 0
             edMax = 100

-- | Instantiation of Loan data type is sufficient for loan calculation.
instance ClassicLoan Loan where
   newLoanI st | l == ClClassical            = newLoanI $ Classical p n d r
               | l == ClBalloon              = newLoanI $ Balloon p n d r b
               | l == ClBalloonPlus          = newLoanI $ BalloonPlus p n d r b
               | l == ClReversBalloon        = newLoanI $ ReversBalloon p n d r b
               | l == ClBullet               = newLoanI $ Bullet p n d r
               | l == ClUnfoldedBalloon      = newLoanI $ UnfdBalloon p n d r b x
               | l == ClUnfoldedBalloonPlus  = newLoanI $ UnfdBalloonPlus p n d r b x
       where l = loanS st
             p = principalS st
             n = durationS st
             r = rateS st
             d = case delayS st of
                  Just x  -> x
                  Nothing -> 0
             b = fromJust $ balloonS st
             x = fromJust $ extDurS st

   extract st = InstalmentLoanData p n d r
       where p = principalS st
             n = durationS st
             r = rateS st
             d = case delayS st of
                  Just x  -> x
                  Nothing -> 0

presentLoan :: Loan -> ValidMonad InstalmentPlan
presentLoan l = (runWithIPP (IPP (freqS l) (roundingS l)) . newLoanI) l

-- | Sum of four columns of installment plan - these 4 which makes sense to sum up.
total :: InstalmentPlan -> (Amount,Amount,Amount,Interest)
total = foldl' (\(a1,a2,a3,a4) ipl -> (a1 + (iAmt $ iplInst ipl)
                                             ,a2 + (iRepayment $ iplInst ipl)
                                             ,a3 + (iIntPaid $ iplInst ipl)
                                             ,a4 + (iInterest $ iplInst ipl))) (0,0,0,0)

-- | Creates a input which is evaluated arithmetical expression with @type="number"@, min value 0 and @step=0.01@.
amountField :: Field Handler Amount
amountField = Field
    { fieldParse = parseHelper $ \s ->
          case readEitherPlus $ unpack s of
              Right a -> Right $ round $ 100 * (a::Double)
              Left err -> Left $ MsgInvalidNumber $ pack err

    , fieldView = \theId name attrs val isReq -> do
        [whamlet|
            $newline never
            <input id="#{theId}" name="#{name}" *{attrs} type="text" :isReq:required="" value="#{showVal val}" min=0 title=_{MsgAmtTooltip}>
        |]
        toWidget [julius|
        $("##{rawJS theId}").change(function() {
            eurl = encodeURIComponent ($("##{rawJS theId}").val());
            console.log ("sending:" + eurl);
            
            $.getJSON("/evaluateAmount/" + eurl, function (o) {
                console.log (o);
                if (o.error)
                    $("##{rawJS theId}output").text(o.error);
                else {
                   $("##{rawJS theId}").val(o.value)
                   $("##{rawJS theId}output").text(""); 
                }
             });
         });
        |]
    , fieldEnctype = UrlEncoded
    }
  where
    showVal = either id (pack . showI)
    showI x = show (fromIntegral x / 100)


-- | Creates a input which is evaluated arithmetical expression with @type="number"@, min value 0 and @step=0.01@.
rateField :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m Rate
rateField = Field
    { fieldParse = parseHelper $ \s ->
          case Data.Text.Read.signed Data.Text.Read.double s of
            Right (a, "") -> Right $ a/100
            _ -> Left $ MsgInvalidNumber s

    , fieldView = \theId name attrs val isReq -> toWidget [hamlet|
$newline never
<input id="#{theId}" name="#{name}" *{attrs} type="number" step="0.001" :isReq:required="" value="#{showVal val}" min="0" max="100" .input-medium> %
|]
    , fieldEnctype = UrlEncoded
    }
  where
    showVal = either id (pack . show . (100*))


-- | Creates a input with @type="number"@ and @step=1@.
durationField :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m Duration
durationField = Field
    { fieldParse = parseHelper $ \s ->
        case Data.Text.Read.signed Data.Text.Read.decimal s of
            Right (a, "") -> Right a
            _ -> Left $ MsgInvalidInteger s

    , fieldView = \theId name attrs val isReq -> toWidget [hamlet|
$newline never
<input id="#{theId}" name="#{name}" *{attrs} type="number" step="1" :isReq:required="" value="#{showVal val}" min="0">
|]
    , fieldEnctype = UrlEncoded
    }
  where
    showVal = either id (pack . showI)
    showI x = show (fromIntegral x :: Integer)

simpleLoanHash :: Loan -> Text
simpleLoanHash loan = (pack $ show $ loanS loan) <> 
                      " P" <> (pack $ show $ principalS loan) <>
                      " D" <> (pack $ show $ durationS loan) <>
                      " R" <> (pack $ showWithLenDec 6 2 $ 100 * rateS loan)
