{-# LANGUAGE TupleSections #-}
module Handler.Home where
import Import
import Handler.Util
import qualified Data.Map.Strict as Map
    
-- The GET handler displays the form
getHomeR :: Handler Html
getHomeR = do
    -- Generate the form to be displayed
    (widget, enctype) <- generateFormPost $ loanForm initLoan initErrors
    defaultLayout $ displayInputForm MsgCalculator widget enctype >> abstractWidget

initLoan = Loan ClClassical 0 0 0 (Just 0) Nothing Nothing Monthly Truncated

loanForm :: Loan -> LoanErrors -> Html -> MForm Handler (FormResult Loan, Widget)
loanForm l le = renderLoan l le $ Loan
    <$> areq (selectFieldList loans) (mkFieldSettings MsgLoan fieldLoan) (Just $ loanS l)
    <*> areq amountField (mkFieldSettings MsgPrincipal fieldPrincipal) (Just $ principalS l) 
    <*> areq durationField (mkFieldSettings MsgDuration fieldDuration) (Just $ durationS l)
    <*> areq rateField (mkFieldSettings MsgInterestRate fieldRate) (Just $ rateS l)
    <*> aopt durationField (mkFieldSettings MsgDeferrment fieldDeferrment) (Just $ delayS l)
    <*> aopt amountField (mkFieldSettings MsgBalloon fieldBalloon) (Just $ balloonS l)
    <*> aopt durationField (mkFieldSettings MsgMaxExtDur fieldExtDur) (Just $ extDurS l)
    <*> areq (radioFieldList freqs) (mkFieldSettings MsgFreq fieldFreq) (Just $ freqS l)
    <*> areq (radioFieldList roundings) (mkFieldSettings MsgRoundingType fieldRound) (Just $ roundingS l)
    where
        loans :: [(Text, GUIClassic)]
        loans = map (pack . show &&& id) confList
        freqs :: [(Text,Freq)]
        freqs = map (pack . show &&& id) freqList
        roundings :: [(Text,RoundingType)]
        roundings = map (pack . show &&& id) roundingList 
        mkFieldSettings :: AppMessage -> Text -> FieldSettings App
        mkFieldSettings msg field = "" {fsLabel = SomeMessage msg
                                       ,fsId = Just field}



isHidden :: GUIClassic -> Text -> Bool -> Bool
isHidden l id isErr | not (isUnfoldedBalloon l) && id == fieldExtDur && not isErr = True
                    | not (isBalloon l) && id == fieldBalloon && not isErr = True
                    | id `elem` [fieldFreq,fieldRound] = True
                    | otherwise = False

renderLoan :: (Show a) => Loan -> LoanErrors -> FormRender Handler a
-- | Render a form into a series of tr tags. Note that, in order to allow
-- you to add extra rows to the table, this function does /not/ wrap up
-- the resulting HTML in a table tag; you must do that yourself.
renderLoan l lErr aform fragment = do
    (res, views') <- aFormToForm aform
    let views = views' []
        -- loan = fromMaybe ClClassical $ loanS <$> l
        loan =  loanS l
        showCSVButton = Map.null lErr && l /= initLoan
    --let isError = any (isJust . fvErrors) views
    let widget = [whamlet|
        $newline never
        $if null views
            \#{fragment}
        <div .span9>
            <table .table>
                $forall (isFirst, view) <- addIsFirst views
                    <tr ##{fvId view <> "tr"} :fvRequired view:.required :not $ fvRequired view:.optional :isJust $ fvErrors view:.errors :isHidden loan (fvId view) (isJust $ fvErrors view):.hide>
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
                        <a href=# #showParameters>_{MsgShowParameters}
                    <td>
                        <button .btn .btn-primary .btn-large>_{MsgCalculate}
                    <td>
                        $if showCSVButton
                            <a href=@{LoanCSVR} .btn .btn-icon download="#{simpleLoanHash l}.csv"><img src=@{StaticR csv_png}> _{MsgDownloadCsv}
        <div .span3>
                        <div .loan-info-box>
                           <h5>
                                <img src=@{StaticR help_about_png}> #
                                <span #fieldLoanExplanationTitle> #
                           <p #fieldLoanExplanation .small> #
        |]
    return (res, widget)
    where
        addIsFirst [] = []
        addIsFirst (x:y) = (True, x) : map (False, ) y


displayInputForm :: AppMessage -> Widget -> Enctype -> Widget
displayInputForm title widget enctype = do
        setTitleI title

        [whamlet|
            <h1>
                _{title}
            <p>
                _{MsgInitial}
            <form method=post action=@{LoanR} enctype=#{enctype}>
                <div .row-fluid .show-gird>
                                ^{widget}
         |]

abstractWidget :: Widget
abstractWidget = [whamlet|
                        <h3>_{MsgYALC}
                        <p>_{MsgNotExactly}
                        <p>_{MsgLongText}
                        <p>
                            <a href=@{StaticR haslo_pdf} .btn .btn-large> 
                               <img src=@{StaticR application_pdf_png}>
                               _{MsgDownloadPaper}
        |]
