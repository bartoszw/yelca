{-# LANGUAGE NoImplicitPrelude #-}
module Handler.Util (module Handler.Util
                    ,module ElcaUI
       )
       where
import Import
import           Yesod.Form.Jquery
import qualified Data.Text.Read      (signed,decimal,double)
import qualified Data.Map.Strict as Map
import           Data.Time           (Day)
import Control.Monad.Trans.Maybe
import qualified Data.List as List (head)
import ElcaUI

type Amount = Int
type Duration = Int
type Rate = Double
type Freq = InstalmentFreq

data Loan = Loan
               { loanS        :: ClassicCalcConf
               , principalS   :: Amount
               , durationS    :: Duration
               , balloonS     :: Maybe Amount
               , rateS        :: Rate
               , delayS       :: Maybe Duration
               , extDurS      :: Maybe Duration
               , extRateS     :: Maybe Rate
               , feeAmountS   :: Maybe Amount
               , feePercentS  :: Maybe Rate
               }
               deriving (Show,Eq)

data Financing = Financing
                { finDate       :: Day
                , paymentDay    :: Int
                }
                deriving (Show, Eq)

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

loanList = map niceShow confList

nonBalloon = [Classical, Zielkauf, SecuredBalloon, ClassicalOneFreeInterest, OneFreeInterest]
balloonsList = Balloon : ReversBalloon : unfoldedBalloonList
isBalloon l = l `elem` balloonsList
unfoldedBalloonList = [Vario2,Vario3]
isUnfoldedBalloon l = l `elem` unfoldedBalloonList
isSecuredBalloon l = l == SecuredBalloon

roundingList = [Rounded,Truncated]
roundingShowList = map show roundingList
freqList = [Monthly,Yearly]
freqShowList = map show freqList

fieldLoan,fieldFreq,fieldRound,fieldDeferrment,fieldExtRate,fieldFeeAmt,fieldFeePercent,fieldExtDur,fieldFinDate,fieldInstDay :: Text
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
fieldFinDate = "fieldFinDate"
fieldInstDay = "fieldInstDay"

initErrors = Map.empty
anyError errs = not $ Map.null errs

-- | Contextual validation - context is Loan record, validated are its all fields.
loanValidation :: Loan -> LoanErrors
loanValidation l = (balloonValidation l . rateValidation l . durationValidation l . principalValidation l) 
                    initErrors

finValidation :: Financing -> LoanErrors
finValidation fin = instDayValidation fin initErrors

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

instDayValidation fin err | i < 1 || i > 31 = Map.insert fieldInstDay MsgInstDayBoundary err
                          | otherwise = err
       where i = paymentDay fin

-- | Sum of four columns of installment plan - these 4 which makes sense to sum up.
total :: AmorPlan -> (Amount,Amount,Amount)
total = foldl' (\(a1,a2,a3) ipl -> (a1 + (fstOf5 ipl)
                                   ,a2 + (sndOf5 ipl)
                                   ,a3 + (trdOf5 ipl)
                                   )) (0,0,0)

presentFullLoan :: Financing -> Loan -> AmorPlan
presentFullLoan fin = (calcAmorPlan . calc) . newFullLoan fin

presentFullLoanWithoutFee :: Financing -> Loan -> AmorPlan
presentFullLoanWithoutFee fin = (calcAmorPlan . calc) . newFullLoanWithoutFee fin

presentLoan :: Loan -> AmorPlan
presentLoan = easyAmorPlan . newAbsLoan

presentLoanWithoutFee :: Loan -> AmorPlan
presentLoanWithoutFee = easyAmorPlan . newAbsLoanWithoutFee

feeAmor :: Amount -> [Amount] -> [Amount] -> [Amount]
feeAmor fee is iWFs = feeAmor' fee is iWFs []
    where feeAmor' _ [] _ result = reverse result
          feeAmor' _ _ [] result = reverse result
          feeAmor' fee (i:is) (iWF:iWFs) result = feeAmor' newFee is iWFs (newFee:result)
            where newFee =   fee + i - iWF

easyAmorPlan :: ClassicLoan -> AmorPlan
easyAmorPlan cll = amorPlan (capital cl) 0 (rNomAmor cl) (nbrInst cl) ((fromIntegral.delay) cl)
                           30 0 (ip2Il [] (installments cl)) ((amorType.conf) cl) []
       where cl = calc cll


newFullLoan :: Financing -> Loan -> ClassicLoan
newFullLoan fin = newAbsLoan' False (Just fin)

newFullLoanWithoutFee :: Financing -> Loan -> ClassicLoan
newFullLoanWithoutFee fin = newAbsLoan' True (Just fin)

newAbsLoanWithoutFee :: Loan -> ClassicLoan
newAbsLoanWithoutFee = newAbsLoan' True Nothing

newAbsLoan :: Loan -> ClassicLoan
newAbsLoan = newAbsLoan' False Nothing

-- | Main interface to ELCA backend.
newAbsLoan' :: Bool -> Maybe Financing -> Loan -> ClassicLoan
newAbsLoan' isPure mFin l | isPure    = case mFin of
                                            Just fin -> do
                                                        let q = paymentDay fin
                                                            (yyyy,mm,dd) = toGregorian $ finDate fin
                                                        nextInstallment $ calcALZ rALZ $ finClassLoan (fromIntegral yyyy) mm dd $ setDueDay q $ mkClassLoan c n r 0       b d cf
                                            Nothing  -> calcALZ rALZ $ mkClassLoan c n r 0       b d cf
                          | otherwise = case mFin of
                                            Just fin -> do
                                                        let q = paymentDay fin
                                                            (yyyy,mm,dd) = toGregorian $ finDate fin
                                                        nextInstallment $ calcALZ rALZ $ finClassLoan (fromIntegral yyyy) mm dd $ setDueDay q $ mkClassLoan c n r (fA+fP) b d cf
                                            Nothing  -> calcALZ rALZ $ mkClassLoan c n r (fA+fP) b d cf
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
               Just x  -> fromIntegral x / fromIntegral c 
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

-- | Creates a input with @type="number"@ and @step=1@.
instDayField :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m Int
instDayField = Field
    { fieldParse = parseHelper $ \s ->
        case Data.Text.Read.signed Data.Text.Read.decimal s of
            Right (a, "") -> Right a
            _ -> Left $ MsgInvalidInteger s

    , fieldView = \theId name attrs val isReq -> toWidget [hamlet|
$newline never
<input id="#{theId}" name="#{name}" *{attrs} type="number" step="1" :isReq:required="" value="#{showVal val}" min="1" max="31">
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
            return $ Loan l p d b r de x xr fA fP 
    where   helperFS :: Read a => Text -> (a -> b) -> MaybeT Handler b
            helperFS key f = MaybeT $ (lookupSession key) >>= return . fmap (f . read . unpack)
            -- Loan's type id is a String, so read on it causes bad behaviour.
            helperFS' key f = MaybeT $ (lookupSession key) >>= return . fmap (f . unpack)
            toClassicalCalcConf a = List.head $  filter (\x -> ccConfFun x == a) confList

initFinancing :: Financing
initFinancing = Financing (fromGregorian 2015 1 1) 1

financingForm :: Financing -> LoanErrors -> Html -> MForm Handler (FormResult Financing, Widget)
financingForm fin le = renderFinancing fin le $ Financing
    <$> areq (jqueryDayField def
                { jdsChangeYear = True -- give a year dropdown
                , jdsYearRange = "2000:" -- 1900 till five years ago
                }) (mkFieldSettings MsgFinDate fieldFinDate) (Just $ finDate fin)
    <*> areq instDayField (mkFieldSettings MsgInstDay fieldInstDay) (Just $ paymentDay fin)
    where
        mkFieldSettings :: AppMessage -> Text -> FieldSettings App
        mkFieldSettings msg field = "" {fsLabel = SomeMessage msg
                                       ,fsId = Just field}


--renderFinancing :: (Show a) => Financing -> LoanErrors -> FormRender Handler a
-- | Render a form into a series of tr tags. Note that, in order to allow
-- you to add extra rows to the table, this function does /not/ wrap up
-- the resulting HTML in a table tag; you must do that yourself.
renderFinancing fin lErr aform fragment = do
    (res, views') <- aFormToForm aform
    let views = views' []
    let widget = [whamlet|
        $newline never
        $if null views
            \#{fragment}
        <div>
            <table .table> 
                $forall (isFirst, view) <- addIsFirst views
                    <tr ##{fvId view <> "tr"} :fvRequired view:.required :not $ fvRequired view:.optional :isJust $ fvErrors view:.errors>
                        <td>
                            $if isFirst
                                \#{fragment}
                            <label ##{fvId view <> "Label"} for=#{fvId view}>#{fvLabel view}
                            $maybe tt <- fvTooltip view
                                <div .tooltip>#{tt}
                        <td>^{fvInput view}
                            $maybe err <- Map.lookup (fvId view) lErr
                                <p .errors>_{err}
                        $maybe err <- fvErrors view
                                <td>#{err}
                        $nothing
                                <td ##{fvId view <> "output"} .warnings>
                <tr>
                    <td>
                    <td>
                        <button .btn .btn-primary .btn-large>_{MsgFinance}
                    <td>
        |]
    return (res, widget)
    where
        addIsFirst [] = []
        addIsFirst (x:y) = (True, x) : map (False, ) y

displayInputFormFin :: Loan -> AppMessage -> Widget -> Enctype -> Widget
displayInputFormFin l title widget enctype = do
        setTitleI title

        [whamlet|
       <table .table>
            <tr>
                <td>
                    <a href=@{LoanCSVR} .btn .btn-icon download="#{simpleLoanHash l}.csv"><img src=@{StaticR csv_png}> _{MsgDownloadCsv}
                <td>
                    <h2>
                        _{title}
                    <form method=post action=@{FinancingR} enctype=#{enctype}>
                        <div .row-fluid .show-gird>
                                ^{widget}
                <td .my-text-right>
                    <a href=@{HomeR}>_{MsgHome}
                    <br>
                    <a href="#">_{MsgTop}
                    <br>
                    <a href=@{LoanR}>_{MsgToLoan}
         |]