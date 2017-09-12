{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.GI.Base (AttrOp ((:=)), get, new, on, set)
import           Data.List    (intercalate)
import           Data.Maybe   (fromJust)
import qualified Data.Text    as T
import qualified GI.Gtk       as Gtk
import qualified Pilya.Lex    as Lex

data AppUI = AppUI
    { uiWindow     :: Gtk.Window
    , uiSourceEdit :: Gtk.TextView
    , uiModeTabs   :: Gtk.Notebook
    , uiLexOutput  :: Gtk.TextView
    , uiLexButton  :: Gtk.Button
    }

buildUI :: IO AppUI
buildUI = do
    window <- new Gtk.Window
        [ #title := "Pilya Lang"
        , #widthRequest := 800
        , #heightRequest := 600
        ]

    rootPaned <- new Gtk.Paned
        [ #wideHandle := True
        , #orientation := Gtk.OrientationHorizontal
        ]

    sourceEdit <- new Gtk.TextView
        [ #monospace := True
        , #topMargin := 12
        , #rightMargin := 12
        , #bottomMargin := 12
        , #leftMargin := 12
        ]
    #pack1 rootPaned sourceEdit True True

    modeTabs <- new Gtk.Notebook
        [ #widthRequest := 300
        ]
    #pack2 rootPaned modeTabs False True

    lexTabContainer <- new Gtk.Box
        [ #orientation := Gtk.OrientationVertical
        , #margin := 6
        , #spacing := 6
        ]
    lexOutput <- new Gtk.TextView
        [ #editable := False
        , #cursorVisible := False
        , #topMargin := 6
        , #rightMargin := 6
        , #bottomMargin := 6
        , #leftMargin := 6
        ]
    #packStart lexTabContainer lexOutput True True 0
    lexButton <- new Gtk.Button
        [ #label := "Split into tokens"
        ]
    #packEnd lexTabContainer lexButton False False 0
    lexTabLabel <- new Gtk.Label
        [ #label := "Lexics"
        ]
    #appendPage modeTabs lexTabContainer (Just lexTabLabel)

    #add window rootPaned

    return AppUI
        { uiWindow = window
        , uiSourceEdit = sourceEdit
        , uiModeTabs = modeTabs
        , uiLexOutput = lexOutput
        , uiLexButton = lexButton
        }

tokensIntoReport :: [Lex.Token] -> T.Text
tokensIntoReport tokens = T.pack $ intercalate "\n" $ fmap
    (\(Lex.Token ttype tline tpos) ->
        show tline ++ ":" ++ show tpos ++ " " ++ show ttype
    ) tokens

onLexButtonClicked :: AppUI -> IO ()
onLexButtonClicked appUI = do
    srcBuffer <- uiSourceEdit appUI `get` #buffer
    source <- fmap fromJust $ srcBuffer `get` #text

    outBuffer <- uiLexOutput appUI `get` #buffer
    case Lex.parse source of
        Left (Lex.ParserError line pos msg) -> do
            let errText = "Error on " ++ show line ++ ":" ++ show pos ++ ":\n" ++ msg
            outBuffer `set` [ #text := T.pack errText ]
            return ()
        Right tokens -> do
            outBuffer `set` [ #text := tokensIntoReport tokens ]
            return ()

main :: IO ()
main = do
    Gtk.init Nothing
    appUI <- buildUI
    on (uiWindow appUI) #destroy Gtk.mainQuit
    on (uiLexButton appUI) #clicked (onLexButtonClicked appUI)
    #showAll (uiWindow appUI)
    Gtk.main
