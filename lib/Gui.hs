module Gui (gui) where

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

site = "#"
siteTitle = "Simulador CSP"

gui :: IO ()
gui = do
    static <- getStaticDir
    startGUI defaultConfig {
        jsPort = Just 8080,
        jsStatic = Just static
    } setup

setup :: Window -> UI ()
setup window = do

    return window # set UI.title siteTitle

    UI.addStyleSheet window "css-reset.css"
    UI.addStyleSheet window "styles.css"
    UI.addStyleSheet window "branding.css"
    
    favicon <- UI.link
        # set UI.rel "icon"
        # set UI.href "/static/images/Favicon.ico"
        # set UI.type_ "image/x-icon"
    getHead window #+ [element favicon]

    button <- UI.button # set text "apretame!" #. "press-me"
    getBody window #+ [element button]
    on UI.click button (\_ -> do
        element button # set text "me apretaron")
    
    arena <- UI.new #. "arena flex"
    getBody window # set children [arena]

    landing window

render window page = case page of
    "landing" -> landing window
    "simulator" -> simulator window

landing :: Window -> UI ()
landing window = do
    arena <- UI.new #. "arena flex"
    getBody window # set children [arena]

    branding_ <- branding

    fileSelector_ <- fileSelector window

    element arena #+ [
        element branding_,
        element fileSelector_]

    return ()

branding = do
    title <- UI.h1 #. "branding-title" # set text "SimCSP"

    logo <- UI.img #. "inline-block logo" # set UI.src "/static/images/logo.svg"

    UI.a #. "branding"
        # set UI.href site
        #+ [element title, element logo]

fileSelector window = do
    button <- UI.div #. "file-selector-btn btn" #+ [
        UI.span
            # set text "Subir archivo",
            -- # set style [("display", "inline")],
        UI.img # set UI.src "/static/images/cloud-upload_4945753.png"]

    title <- UI.h1 #. "file-selector-title"
        # set text "Seleccione un archivo csp"

    fileInput <- UI.input #. "file-selector-input" 
        # set UI.type_ "file" 
        # set UI.name "csp_file"
        # set UI.id_ "file-input"

    -- weird hack to make the button act as the file input
    label <- UI.label # set UI.for "file-input" #+ [
        element button]

    -- on UI.change input (\_ -> do
        -- check input
        -- simulator window)
    
    UI.new #. "file-selector" #+ [
        element title,
        element label,
        element fileInput]

simulator :: Window -> UI ()
simulator window = do
    arena <- UI.new #. "arena flex"
    getBody window # set children [arena]

    branding_ <- branding

    tFile <- UI.button #. "btn" #+ [
        UI.span # set text "Archivo",
        UI.img # set UI.src "/static/images/folder_1383970.png"]

    tReload <- UI.button #. "btn rounded" #+ [
        UI.img # set UI.src "/static/images/arrow_14361741.png"]

    -- backwards is a forward icon rotated
    let forwardIcon = "/static/images/fast-forward_4250967.png"
    tBackwards <- UI.button #. "btn tBackwards" #+ [
        UI.img # set UI.src forwardIcon]
    tForward <- UI.button #. "btn rounded" #+ [
        UI.img # set UI.src forwardIcon]

    tPlay <- UI.button #. "btn rounded" #+ [
        UI.img # set UI.src "/static/images/play_9694582.png"]
    
    toolbar <- UI.new #. "toolbar" #+ [
        element tFile,
        element tReload,
        element tBackwards,
        element tPlay,
        element tForward]

    let filename = ""
    uiFilename <- UI.span #. "uiFilename" # set text filename

    element arena #+ [
        element branding_,
        element toolbar]

    return ()