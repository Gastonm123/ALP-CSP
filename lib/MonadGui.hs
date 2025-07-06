module MonadGui(MonadGui(..)) where

-- import ThreepennyFudgets
import qualified Graphics.UI.Threepenny         as UI
import Graphics.UI.Threepenny.Core(
    UI,
    Window,
    Config(..),
    getBody,
    getHead,
    element,
    startGUI,
    defaultConfig,
    text,
    on,
    (#+),
    (#),
    (#.),
    set,
    style,
    children
    )
import Paths

---------------------------------------------------------

type CSP = () -- TBD

data MonadGui a = MonadGui {
    filename :: String, -- read write
    history :: [CSP], -- read write
    trace :: [String], -- read write
    traceIdx :: Int, -- read write
    window :: Window, -- read
    ui :: UI a
}

-- instance Functor MonadGui where

-- instance Monad MonadGui where
