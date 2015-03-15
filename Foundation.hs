{-# LANGUAGE NoImplicitPrelude #-}
module Foundation where

import           Import.NoFoundation
--import           Yesod
import           Yesod.Default.Util
--import           Yesod.Static
import           Yesod.Form.Jquery
import           Yesod.Core.Types (Logger)
--import qualified Yesod.Core.Unsafe as Unsafe
import           Text.Hamlet
import           Data.Text
import           ElcaUI

{-

Every Yesod application has a foundation data type. This is a data type
which represents the application itself. It can include any initialization
data necessary for running the application, such as a database connection pool.

In our case, our application has no such initialization data, and therefore
our datatype is very simplistic. By convention, we call this datatype "App" with
a single data constructor, also called "App".

-}
data App = App
    { --appSettings :: AppSettings
    appStatic :: Static -- ^ Settings for static file serving.
    --, appConnPool :: ConnectionPool -- ^ Database connection pool.
    --, appHttpManager :: Manager
    --, appLogger :: Logger
    }

--instance HasHttpManager App where
--    getHttpManager = appHttpManager



{-

The Yesod typeclass allows us to alter a number of the behaviors of Yesod, such
as the default page layout, error handler functions, and how URLs are rendered.
We're going to accept all of the defaults in our application, so our instance
is pretty boring.

-}
instance Yesod App where

 -- Controls the base of generated URLs. For more information on modifying,
 -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
--  approot = ApprootMaster $ appRoot . appSettings
 -- Store session data on the client in encrypted cookies,
 -- default session idle timeout is 120 minutes
  makeSessionBackend _ = Just <$> defaultClientSessionBackend
    120 -- timeout in minutes
    "config/client_session_key.aes"

  defaultLayout widget = do
    pc <- widgetToPageContent $ do
        -- Include some CDN-hosted Javascript and CSS to make our page a little nicer.
        addScriptRemote "//ajax.googleapis.com/ajax/libs/jquery/1.7/jquery.min.js"
        addStylesheetRemote "//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/css/bootstrap-combined.min.css"
        $(widgetFileReload def "default-layout")
        
    giveUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

 -- What messages should be logged. The following includes all messages when
 -- in development, and warnings and errors in production.
--  shouldLog app _source level =
--      appShouldLogAll (appSettings app)
--          || level == LevelWarn
--          || level == LevelError

  --makeLogger = return . appLogger

-- Tells our application to use the standard English messages.
-- If you want i18n, then you can supply a translating function instead.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

--unsafeHandler :: App -> Handler a -> IO a
--unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- And tell us where to find the jQuery libraries. We'll just use the defaults,
-- which point to the Google CDN.
instance YesodJquery App

{-
-- | All selectable loan types are collected under one data type.
data GUIClassic = ClClassical
                | ClBalloon
                | ClBalloonPlus
                | ClReversBalloon
                | ClBullet
                | ClUnfoldedBalloon
                | ClUnfoldedBalloonPlus
     deriving (Eq, Ord, Bounded, Enum)
-}

{-

Every Yesod application has a set of routes associated with its foundation datatype.
These routes represent all of the URLs that can be accessed in an application.
Instead of manually defining the route datatype and its parse/render/dispatch functions,
Yesod uses code generation to avoid boilerplate and ensure that the parse/render combination
is both correct and efficient.

parseRoutes parses Yesod's routing Domain Specific Language (DSL), and mkYesodData generates
the route datatype and the parse/render functions. We'll see the generation of the dispatch
function when we get to the Main module.

-}

mkYesodData "App" $(parseRoutesFile "config/routes")

$(staticFiles "static/")

mkMessage "App" "texts" "en"

plural :: Int -> String -> String -> String -> String
plural 1 x _ _ = x  
plural n _ y z | n == 12 = z
               | n' >= 2 && n' <= 4 = y
               | otherwise = z
               where n' = n `rem` 10

loanExplanation :: ClassicCalcConf -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text
loanExplanation conf t1 t2 t3 t4 t5 t6 t7 
    |  ccConfFun conf == "confClassic" = t1
    |  ccConfFun conf == "confBalloon" = t2
    |  ccConfFun conf == "confClassicDreba" = t3
    |  ccConfFun conf == "confClassicDrebaCar" = t4
    |  ccConfFun conf == "confVario" = t5
    |  ccConfFun conf == "confVario2" = t6
    |  ccConfFun conf == "confVario3" = t7
    |  otherwise                       = "???"

