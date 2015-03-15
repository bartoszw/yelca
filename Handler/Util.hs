{-# LANGUAGE NoImplicitPrelude #-}
module Handler.Util (module Handler.Util
                    ,module ElcaUI
       )
       where
import Import
import qualified Data.Text.Read      (signed,decimal,double)
import qualified Data.Map.Strict as Map
import ElcaUI

type Amount = Int
type Duration = Int
type Rate = Double
type Freq = InstalmentFreq

data Loan = Loan
               { loanS        :: ClassicCalcConf
               , principalS   :: Amount
               , durationS    :: Duration
               , rateS        :: Rate
               , delayS       :: Maybe Duration
               , balloonS     :: Maybe Amount
               , extDurS      :: Maybe Duration
               , extRateS     :: Maybe Rate
               , feeAmountS   :: Maybe Amount
               , feePercentS  :: Maybe Rate
--               , freqS        :: Freq
--               , roundingS    :: RoundingType
               }
               deriving (Show,Eq)

type LoanErrors = Map.Map Text AppMessage

showAmt :: Double -> String
showAmt a = showAmtWithLen (3+l) a
    where l = length $ show (truncate a :: Integer)

-- | Allows human friendly show of loan type.
niceShow :: ClassicCalcConf -> String
niceShow =  show . ccConfName

niceShowClassicType ReversBalloon = "Reversed Balloon"
niceShowClassicType SecuredBalloon = "Secured Balloon"
niceShowClassicType ClassicalOneFreeInterest = "Classical One Free Interest"
niceShowClassicType OneFreeInterest = "First Instalment Interest Free"
niceShowClassicType x = show x

niceShowEarlyRepaymentType ERProportional = "Proportional"
niceShowEarlyRepaymentType ERNoInstInc = "No Installment Increase"
niceShowEarlyRepaymentType x = show x

niceShowInstallmentAdjustment FstInstallmentAdjusted = "First Installment Adjusted"
niceShowInstallmentAdjustment NoAdjustment = "No Adjustment"
niceShowInstallmentAdjustment x = show x

--confList = [minBound .. maxBound::GUIClassic]
loanList = map niceShow confList

nonBalloon = [Classical, Zielkauf,ClassicalOneFreeInterest,OneFreeInterest]
balloonsList = Balloon : ReversBalloon : SecuredBalloon : unfoldedBalloonList
isBalloon l = l `elem` balloonsList
unfoldedBalloonList = [Vario2,Vario3]
isUnfoldedBalloon l = l `elem` unfoldedBalloonList

roundingList = [Rounded,Truncated]
roundingShowList = map show roundingList
freqList = [Monthly,Yearly]
freqShowList = map show freqList

fieldLoan,fieldFreq,fieldRound,fieldDeferrment,fieldExtRate,fieldFeeAmt,fieldFeePercent :: Text
fieldLoan = "fieldLoan"
fieldPrincipal = "fieldPrincipal"
fieldDuration = "fieldDuration"
fieldRate = "fieldRate"
fieldDeferrment = "fieldDeferrment"
fieldBalloon = "fieldBalloon"
fieldExtDur = "fieldExtDur"
fieldExtRate = "fieldExtRate"
fieldFreq = "fieldFreq"
fieldRound = "fieldRound"
fieldLoanExplanation = "fieldLoanExplanation"
fieldFeeAmt = "fieldFeeAmt"
fieldFeePercent = "fieldFeePercent"

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

durationValidation l err | dur < n = Map.insert fieldDuration (MsgDurationLowerBoundary 1) err
                         | clType (loanS l) == Balloon &&
                           dur < nBal = Map.insert fieldDuration (MsgDurationLowerBoundary nBal) err
                         | maxDur > 0 && dur > maxDur = Map.insert fieldDuration (MsgDurationUpperBoundary maxDur) err
                         | otherwise       = err
       where n = 1
             nBal = 2
             dur = durationS l
             maxDur = cccMaxDur $ loanS l

rateValidation l err | rateS l < rMin     = Map.insert fieldRate (MsgRateLowerBoundary rMin) err --"Rate has to be >= 0" err
                     | rateS l > rMax     = Map.insert fieldRate (MsgRateUpperBoundary rMax) err -- "Rate has to be <= 100" err
                     | otherwise          = err
       where rMin = 0
             rMax = 100

balloonValidation l err | (clType $ loanS l) `elem` nonBalloon   = err 
                        | balloonS l == Nothing       = Map.insert fieldBalloon MsgPopulate err -- ("This field has to be populated") err
                        -- | loanS l == ClReversBalloon  && 
                        --   bal > maxI                 = Map.insert fieldBalloon (MsgInstallmentUpperBoundary $ maxI / 100) err -- ("Installment amount has to be <=" <> pack (showAmt $ maxI / 100)) err
                        | isBalloon (clType $ loanS l) &&
                           bal > pri                  = Map.insert fieldBalloon MsgBalloonUpperBoundary err -- "Balloon has to be <= Principal" err
                        | otherwise                   = err
       where pri = fromIntegral $ principalS l
             dur = fromIntegral $ durationS l
             maxI = pri / dur
             bal = fromIntegral $ fromJust $ balloonS l

extDurValidation l err | not $ (clType $ loanS l) `elem` unfoldedBalloonList   = err
                       | extDurS l == Nothing = Map.insert fieldExtDur MsgPopulate err -- "Extended duration has to be populated" err
                       | eD < edMin    = Map.insert fieldExtDur (MsgExtDurLowerBoundary edMin) err --"Extended duration has to be >= 0" err
                       | eD > edMax    = Map.insert fieldExtDur (MsgExtDurUpperBoundary edMax) err -- "Extended duration has to be <= 100" err
                       | otherwise            = err
       where eD = fromJust $ extDurS l
             edMin = 0
             edMax = 100

-- | Sum of four columns of installment plan - these 4 which makes sense to sum up.
total :: AmorPlan -> (Amount,Amount,Amount)
total = foldl' (\(a1,a2,a3) ipl -> (a1 + (fstOf5 ipl)
                                   ,a2 + (sndOf5 ipl)
                                   ,a3 + (trdOf5 ipl)
                                   )) (0,0,0)


presentLoan :: Loan -> AmorPlan
presentLoan = easyAmorPlan . newAbsLoan

presentLoanWithoutFee :: Loan -> AmorPlan
presentLoanWithoutFee = easyAmorPlan . newAbsLoanWithoutFee

feeAmor fee is iWFs = feeAmor' fee is iWFs []
    where feeAmor' _ [] _ result = result
          feeAmor' _ _ [] result = result
          feeAmor' fee (i:is) (iWF:iWFs) result = feeAmor' newFee is iWFs (newFee:result)
            where newFee = fee - i + iWF

easyAmorPlan :: ClassicLoan -> AmorPlan
easyAmorPlan cll = amorPlan (capital cl) 0 (rNomAmor cl) (nbrInst cl) ((fromIntegral.delay) cl)
                           30 0 (ip2Il [] (installments cl)) ((amorType.conf) cl) []
       where cl = calc cll

newAbsLoanWithoutFee l = calcALZ rALZ $ mkClassLoan c n r 0 b d cf
   where c = principalS l
         b | clType cf `elem` [ReversBalloon,Balloon,Vario2,Vario3,Zielkauf] = fromJust $ balloonS l
           | clType cf == SecuredBalloon                = calcInstCl (rounding cf) (fromIntegral c) (n-1) rALZ d
           | otherwise                                  = 0
         n = durationS l
         rALZ | any (== clType cf) [Vario2,Vario3,SecuredBalloon] = fromJust $ extRateS l
              | otherwise                                         = (-1)
         d | delayS l == Nothing = 0
           | otherwise             = fromJust $ delayS l
         r = rateS l
         cf = loanS l

newAbsLoan :: Loan -> ClassicLoan
newAbsLoan l = calcALZ rALZ $ mkClassLoan c n r (fA+fP) b d cf
   where c = principalS l
         b | clType cf `elem` [ReversBalloon,Balloon,Vario2,Vario3,Zielkauf] = fromJust $ balloonS l
           | clType cf == SecuredBalloon                = calcInstCl (rounding cf) (fromIntegral c) (n-1) rALZ d
           | otherwise                                  = 0
         n = durationS l
         rALZ | any (== clType cf) [Vario2,Vario3,SecuredBalloon] = fromJust $ extRateS l
              | otherwise                                         = (-1)
         d | delayS l == Nothing = 0
           | otherwise             = fromJust $ delayS l
         r = rateS l
         cf = loanS l
         fA = case feeAmountS l of
               Nothing -> 0
               Just x  -> fromIntegral x / fromIntegral c -- x
         fP = case feePercentS l of
               Nothing -> 0
               Just x  -> x 

showClassicCalcConf :: ClassicCalcConf -> [(AppMessage, String)]
showClassicCalcConf c = (MsgLoanKind, show $ clType c) : 
                        (MsgMinFstInstDur,show $ minFstInstDur c) :
                        (MsgERType, show $ cccERType c) :
                        (MsgMaxDur, show $ cccMaxDur c) :
                        (MsgMinInstAmt, show $ cccMinInstAmt c) :
                        (MsgInstAdj, show $ cccInstAdj c) : []

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
