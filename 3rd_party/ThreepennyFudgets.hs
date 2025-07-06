{-|
Module      : ThreepennyFudgets
Description : Fudgets on top of threepenny-gui
Maintainer  : Thomas Hallgren
Stability   : Experimental

Threepenny Fudgets is a prototype library for writing GUI applications in a
functional style in Haskell, using a web browser for the user interface.

Threepenny Fudgets is based on <http://www.altocumulus.org/Fudgets Fudgets>
(a Graphical User Interface Libary for Haskell developed in the early 1990s)
but has a completely separate implementation
on top of <http://hackage.haskell.org/package/threepenny-gui threepenny-gui>.
-}
{-# LANGUAGE TypeOperators #-}
module ThreepennyFudgets(-- * The Fudget type
                  F,
                  -- ** Other types
                  type (+),fromLeft,fromRight,
                  URL,
                  -- ** Running a fudget
                  runF,runF',
                  -- * User interface elements
                  buttonF,Click(..),buttonGroupF,
                  toggleButtonF,checkboxF,
                  radioGroupF,radioGroupF',selectF,Options(..),
                  dynSelectF,SelectF,ListRequest(..),
                  sliderF,progressF,meterF,
                  stringDisplayF,htmlDisplayF,showF,
                  numberF,readShowF,stringF,passwordF,
                  canvasF,canvasF',imgF,imgF',
                  --alertF,
                  -- ** Interaction control
                  focusF,
                  disableF,
                  eventF,
                  -- * Static content
                  textF,htmlF,ahrefF,
                  -- * Web page layout
                --blockF, BlockTag(..),
                  h1F,h2F,h3F,h4F,pF,tableF,divF,boxF,ulF,olF,liF,preF,
                  permuteF,
                  -- ** Traditional Fudgets compatibility
                  shellF,vBoxF,hBoxF,
                  -- ** Changing style and other properties
                  classF,withF,dynWithF,dynF,
                  -- * Fudget plumbing
                  -- | <<P/fudget_plumbing2.jpg>>

                  -- ** Parallel composition
                  (>+<),(>+),(+<),listF,
                  -- ** Serial composition
                  (=<=),(=>=),
                  -- ** Loops
                  loopLeftF,loopF,loopThroughRightF,
                  -- * Adding application specific functionality
                  -- ** Stateless
                  mapF,filterF,mapMaybeF,concatMapF,
                  -- ** Stateful
                  stateF,persistentStateF,localStorageF,
                  -- ** Stream manipulation
                  putF,putsF,nullF,
                  idF,concatF,toBothF,throughF,splitF,gatherF,gatherF',
                  -- * Timing
                  timerF,Tick(..),
                  -- * Debugging
                  writeLogF,
                  -- * Internal
                  -- | These definitions reveals implementation details that
                  -- might change.
                  initF,ioF,elemDisplayF,modifyF,H,
                  -- * Threepenny extras
                  -- ** Attributes
                  Attribute,(=:),attr,style,setMany,
                  -- ** Drawing on a canvas
                  -- | See also "Graphics.UI.Threepenny.Canvas"
                  Picture(..),UI.Point,circle,line,strokePath,fillPath,
                  -- * Other utilities
                  chop,readM
                  ) where

import qualified Data.Aeson             as JSON
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

-- Overkill, since we are creating single-threaded programs
import Control.Concurrent.MVar--(newMVar,takeMVar,readMVar,putMVar)

import Data.List(findIndex)
import Control.Monad(unless,when,zipWithM,(<=<))
import System.Directory
import System.Environment
import System.IO.Error

-- | @F hi ho@ is the type of a fudget that
-- consumes an high-level input stream of values of type @hi@ and
-- produces an high-level output stream of values of type @ho@.
-- It can also generate a number of user interface elements,
-- and can read input from and send output to those user interface elements.
-- <<P/the_fudget2.jpg>>
newtype F hi ho = F (H ho->UI (H hi,[Element]))

-- | The type of an event handler
type H a = a -> UI ()

-- | 'runF' is typically used only once in the @main@ function of a program.
-- It runs the fudget and adds any user interface elements it generates
-- to the 'documentBody' of the web page.
runF :: F hi ho -> IO ()
runF = runF' (const (return ()))

runF' init (F fud) =
  startGUI config $ \ window ->
  do init window
     (_,es) <- fud ignore
     getBody window #+ map pure es
     return ()
  where
    config = defaultConfig {jsStatic=Just "static"}

initF init fud = F $ \ oh -> do a <- init
                                let F f = fud a
                                f oh
{-
instance Monoid (F hi ho) where
  mempty = nullF
  mappend (F f1) (F f2) = F $ \ oh -> do (_,es1) <- f1 oh
                                         (_,es2) <- f2 oh
                                         return (ignore,es1++es2)
-}

instance Functor (F hi) where
  fmap f (F fud) = F $ \ oh -> fud (oh . f)

instance Applicative (F hi) where
  pure ho = putF ho nullF
  ff <*> fa = mapF (uncurry id) =<= gatherF =<= (ff>+<fa) =<= toBothF
  fl <* fr = mapMaybeF fromLeft =<= (fl>+<fr) =<= toBothF
  fl *> fr =  mapMaybeF fromRight =<= (fl>+<fr) =<= toBothF

absF f = F $ \ oh -> return (f oh,[])

ioF io = F $ \ oh -> return (oh <=< (liftIO . io),[])

-- | @putF x fud@ outputs @x@, then behaves like fud
putF x (F fud) = F $ \ oh -> do oh x; fud oh

-- | @putF xs fud@ outputs @xs@, then behaves like fud
putsF :: [ho] -> F hi ho -> F hi ho
putsF xs (F fud) = F $ \ oh -> do mapM_ oh xs; fud oh

nullF = absF (const ignore)
-- ^ Ignores all input. Doesn't produce any output.

idF   = absF id
-- ^ @mapF id@, propagates all input directly to the output

mapF f = absF (. f)
-- ^ Like 'map' for lists. @mapF f@ outputs @f x@ for every @x@ in the input stream

mapMaybeF f = absF (\oh->maybe (return ()) oh . f)
-- ^ Like 'mapMaybe' for lists. A combination of 'mapF' and 'filterF'.

filterF p = mapMaybeF (\x->if p x then Just x else Nothing)
-- ^ Like 'filter' for lists. Propagates values from the input stream to
-- the output stream if they pass a test.

stateF :: (s -> hi -> (s,[ho])) -> s -> F hi ho
stateF f init = F $ \ oh -> do state <- liftIO $ newMVar init
                               let ih i = do old <- liftIO $ takeMVar state
                                             let (new,out) = f old i
                                             liftIO $ putMVar state new
                                             mapM_ oh out
                               return (ih,[])
-- ^ @stateF@ is used to maintain an internal state.
-- Given a state transition function @f@ and an initial state @s@,
-- @stateF f s@ responds to input by applying @f@ to it to update the
-- internal state and generate zero or more output messages.

-- | Like 'concat' for lists, flattens a stream of lists.
concatF :: F [i] i 
concatF = absF mapM_

-- | Like 'concatMap' for lists. A combination of 'concatF' and 'mapF'.
concatMapF :: (i->[o]) -> F i o
concatMapF f = absF (\oh -> mapM_ oh  . f)

toBothF :: F a (a+a)
toBothF = concatMapF (\x->[Left x,Right x])

throughF fud = either id id <$> (fud >+< idF) =<= toBothF

splitF :: F (a,b) (a+b)
splitF = concatMapF (\(x,y)->[Left x,Right y])

-- | After the first @Left a@ and @Right b@ has arrived on the input, @gatherF@
-- output pairs @(a,b)@ with the most recent @a@ and @b@ values received.
gatherF :: F (a+b) (a,b)
gatherF = gatherF'' (Nothing,Nothing)

-- | 'gatherF' with initial values.
gatherF' :: (a,b) -> F (a+b) (a,b)
gatherF' (a,b) = gatherF'' (Just a,Just b)

gatherF'' :: (Maybe a,Maybe b) -> F (a+b) (a,b)
gatherF'' = stateF gather
  where
    gather (_,Just b) (Left a) = ((Just a,Just b),[(a,b)])
    gather (_,n) (Left a) = ((Just a,n),[])
    gather (Just a,_) (Right b) = ((Just a,Just b),[(a,b)])
    gather (n,_) (Right b) = ((n,Just b),[])

--------------------------------------------------------------------------------

infixr 3 =<=,=>=

-- | Right-to-left serial composition. The output stream of the right fudget
-- is connected to the input stream of the left fudget. This was
-- originally called '>==<' in Fudgets.
F f1 =<= F f2 = F $ \ oh -> do (ih1,es1) <- f1 oh
                               (ih2,es2) <- f2 ih1
                               return (ih2,es1++es2)

-- | Left-to-right serial composition. The output stream of the left fudget
-- is connected to the input stream of the right fudget. (This was not
-- included in the original implementation of Fudgets. Using both '=>='
-- and '=<=' in the same expression might lead to confusion...)
F f1 =>= F f2 = F $ \ oh -> do (ih2,es2) <- f2 oh
                               (ih1,es1) <- f1 ih2
                               return (ih1,es1++es2)

-- | We use the notation @a+b@ for @'Either' a b@,
-- the standard disjoint union type in Haskell.
type a+b = Either a b

infixl 5 >+<,+<,>+
-- | Tagged parallel composition. Messages to/from the left fudget are
-- tagged 'Left'. Messages to/from the right fudget are tagged 'Right'.
(>+<) :: F i1 o1 -> F i2 o2 -> F (i1+i2) (o1+o2)
F f1 >+< F f2 = F $ \ oh -> do  (ih1,es1) <- f1 (oh . Left)
                                (ih2,es2) <- f2 (oh . Right)
                                return (either ih1 ih2,es1++es2)

-- | Parallel composition where only the right fudget is connected.
-- The left fudget is typically static content.
F f1 +< F f2 = F $ \ oh -> do  (ih1,es1) <- f1 ignore
                               (ih2,es2) <- f2 oh
                               return (ih2,es1++es2)

-- | Parallel composition where only the left fudget is connected.
-- The right fudget is typically static content.
F f1 >+ F f2 = F $ \ oh -> do  (ih1,es1) <- f1 oh
                               (ih2,es2) <- f2 ignore
                               return (ih1,es1++es2)


-- | Tagged parallel composition of a list of fudgets
listF tfs = F $ \ oh ->
            do (ihs,ess) <- unzip <$> sequence [f (oh . (,) t) | (t,F f)<-tfs]
               let tihs = zip (map fst tfs) ihs
                   ih (t,i) = maybe (return ()) ($ i) (lookup t tihs)
               return (ih,concat ess)

--------------------------------------------------------------------------------

-- | Creates a feedback loop. @loopLeftF fud@ behaves as follows:
-- output from @fud@ tagged @Left@ will be sent back to
-- the input of @fud@. Output from @fud@ tagged @Right@ will be sent to the
-- output of @loopLeftF fud@. Input to @loopLeftF fud@ will be tagged
-- @Right@ and delivered to @fud@.
loopLeftF :: F (loop+hi) (loop+ho) -> F hi ho
loopLeftF (F fud) = 
  F $ \ oh  -> do (ev,ih) <- liftIO newEvent
                  --let ih x = do putStrLn "ih"; ih0 x
                  q <- liftIO $ newMVar (Left [])
                  let eih x = takeMVar q >>=
                              either (putMVar q . Left . (x:))
                                     (\ ih->putMVar q (Right ih)>>ih x)
                  --let oh x = do liftIO (putStrLn "oh "); oh0 x
                  (ih',es) <- fud (either (liftIO . eih . Left) oh)
                  --let ih' x = do liftIO (putStrLn "ih' "); ih0' x
                  --liftIO $ putStrLn "onEvent"
                  onEvent ev ih'
                  Left xs <- liftIO $ takeMVar q
                  liftIO $ putMVar q (Right ih)
                  liftIO $ mapM_ ih (reverse xs)
                  return (liftIO . ih . Right,es)
                 
-- | Copy output back to the input. The fudget needs to send on average
-- strictly less than one output message per input message, otherwise it
-- will become busy reacting to its own messages.
loopF fud = loopLeftF (toBothF =<= fud =<= mapF (either id id))

-- | @loopThroughRightF master slave@ is similar to loopLeftF master, but
-- the loop goes through the @slave@ fudget. (A better name might be
-- @encapsulateF@ since all communication with the @slave@ has to go via
-- the @master@, so the @slave@ is encapsulated in this sense.)
--loopThroughRightF :: F (ro+hi) (ri+ho) -> F ri ro -> F hi ho
loopThroughRightF lfud rfud = loopCompThroughRightF (lfud>+<rfud)


--loopCompThroughRightF :: F ((ro+hi)+ri) ((ri+ho)+ro) -> F hi ho
loopCompThroughRightF w =
    let post (Left (Left x)) = Left (Right x)
        post (Left (Right x)) = Right x
        post (Right x) = Left (Left (Left x))
        pre (Left x) = x
        pre (Right x) = Left (Right x)
    in  loopLeftF (prepostMapHigh pre post w)

prepostMapHigh pre post fud = post <$> fud =<= mapF pre

--------------------------------------------------------------------------------
{-
data BlockTag = DIV | P | H1 | H2 | H3 | H4 | H5 | H6 | UL | OL | PRE
  deriving (Eq,Ord,Enum,Bounded,Show)

blockF t = wrapF (show (t::BlockTag))
-}
divF = wrapF "div"
-- ^ A div element @\<div>...\</div>@
pF = wrapF "p"
-- ^ Paragraph @\<p>...\</p>@
h1F = wrapF "h1"
-- ^ Level 1 header @\<h1>...\</h1>@
h2F = wrapF "h2"
-- ^ Level 2 header @\<h2>...\</h2>@
h3F = wrapF "h3"
-- ^ Level 3 header @\<h3>...\</h3>@
h4F = wrapF "h4"
-- ^ Level 4 header @\<h4>...\</h4>@

ulF = wrapF "ul"
-- ^ Unordered list @\<ul>...\</ul>@

olF = wrapF "ol"
-- ^ Ordered list @\<ol>...\</ol>@

liF = wrapF "li"
-- ^ List item @\<li>...\</li>@

-- | A div element with a black border and some padding
boxF = wrapF' "div" (set style [("border","1px solid black"),
                                ("padding","1ex"),
                                ("margin","1ex")])

preF = wrapF "pre"
-- ^ A pre element @\<pre>...\</pre>@

wrapF = layoutF . wrap

wrapF' tagname as = layoutF (wrap' tagname as)

layoutF layout = modifyElemsF (fmap (:[]) . layout)

permuteF perm = modifyElemsF (pure . perm)
-- ^ Rearrange the elements generated by a fudget. Note that 'Element's
-- can not be duplicated.

modifyElemsF f = modifyF (const f)

modifyF modify (F fud) = F $ \ oh ->
                         do (ih,es) <- fud oh
                            es' <- modify oh es
                            return (ih,es')

-- | Add event handlers to the elements generated by a fudget.
-- Event types can be imported from "Graphics.UI.Threepenny.Events".
eventF evs h = modifyF modify
  where
    modify oh es =
       do sequence_ [on ev el (oh . curry h ev )|el<-es,ev<-evs]
          return es

-- | Set the @class@ attribute of the elements generated by a fudget
classF fud cls = withF fud [attr "class"=:cls]

-- | Apply attributes to the elements generated by a fudget
withF fud as = modifyElemsF (mapM (setMany as.pure)) fud

-- | Dynamically change element attributes
dynWithF :: F hi ho -> F ([Attribute]+hi) ho
dynWithF = dynModF dynWith
  where
    dynWith es as = mapM_ (setMany as.pure) es

-- | Disable and enable buttons and other input elements.
disableF :: F hi ho -> F (Bool+hi) ho
disableF = dynModF disable
  where
    disable es b = sequence_ [e # set' UI.enabled (not b) | e<-es]


-- | Allows you to observe and control the focus of a fudget. (Focus
-- determines where keyboard input goes.)
focusF :: F hi ho -> F (Bool+hi) (Bool+ho)
focusF (F fud) =
  F $ \ oh ->
  do (ih,es@(el:_)) <- fud (oh . Right)
     on UI.focus el $ \ _ -> oh (Left True)
     on UI.blur  el  $ \ _ -> oh (Left False)
     let --fb True = focus el
         --fb False = blur el
         fb _ = return () -- !!!
     return (either fb ih,es)

dynModF mod (F fud) =
  F $ \ oh ->
  do (ih,es) <- fud oh
     return (either (mod es) ih,es)

-- | A fudget that can be replaced dynamically
dynF :: F i o -> F (F i o + i) o
dynF (F fud) =
  F $ \ oh ->
  do el <- mkElement "span"
     (ih0,els0) <- fud oh
     pure el #+ (map pure els0)
     ihvar <- liftIO $ newMVar ih0
     let ih = either new input
         input i = ($ i) =<< liftIO (readMVar ihvar)
         new (F fud) = do _ <- liftIO $ takeMVar ihvar
                          (ih',els') <- fud oh
                          el # set' UI.children els'
                          liftIO $ putMVar ihvar ih'
     return (ih,[el])

-- | With traditional Fudgets, 'shellF' creates top-level application windows.
-- With WebFudgets, using 'shellF' is entierly optional. It just puts a
-- title above another fudget and adds a couple of @\<div>@ elements that
-- can be styled to look like a traditional application window with a
-- title bar, if you wish.
-- @\<div class="shellF">\<h4>/title/\<\/h4>\<div>...\<\/div>\<\/div>@
shellF title fud = divF (h4F (textF title) +< divF fud)
                   `classF` "shellF"

-- | Place elements vertically
vBoxF = layoutF vBoxL

-- | Place elements horizontally
hBoxF = layoutF hBoxL

-- | A table with @n@ columns. The elements generated by the
-- argument fudget are placed in separate table cells.
tableF n fud = layoutF (tableL n) fud `classF` "table"

tableL n es = do tds <- mapM (wrap1 "td") es
                 rows <- mapM (wrap "tr") (chop n tds)
                 wrap "table" rows

vBoxL es = tableL 1 es #. "vbox"
hBoxL es = tableL (length es) es #. "hbox"

-- | Plain text
textF = staticF . string

-- | Text with HTML markup
htmlF s = staticF (htmlElem s)

htmlElem s = mkElement "span" # set html s

staticF haste = F $ const ((,) ignore . (:[]) <$> haste)

-- | A hyperlink @\<a href="url">...\</a>@
ahrefF :: URL -> F i o -> F i o
ahrefF url = layoutF (wrap' "a" (set UI.href url))

--------------------------------------------------------------------------------

-- | An image, @\<img src="/url/" alt="">@
-- You can change the image dynamically by sending in the URL of another
-- image.
imgF :: URL -> F URL o
imgF = imgF' (set UI.alt "")

-- | An image with extra attributes, @\<img src="/url/" ...>@.
-- You can change the image dynamically by sending in the URL of another
-- image.
imgF' :: Attribute -> URL -> F URL o
imgF' as url = F $ \ oh ->
               do el <- mkElement "img"
                  let ih url = do pure el # set UI.src url
                                  return ()
                  ih url
                  pure el # as
                  return (ih,[el])

-- | 'stringF' combined with 'show', marked read-only
showF :: Show i => F i o
showF = nullF =<= stringF `withF` as =<= mapF show
  where
    as = [attr "readonly"=:"readonly"]

-- | An output-only element displaying text, @\<span>...\</span>@
stringDisplayF = elemDisplayF Nothing =<= mapF (set text)

-- | An output-only element displaying HTML content
htmlDisplayF = elemDisplayF Nothing =<= mapF (set html)

elemDisplayF :: Maybe (UI Element->UI a) -> F (UI Element->UI b) o
elemDisplayF init =
    F $ \ oh ->
    do el <- mkElement "span"
       let ih m = do pure el # m
                     return ()
       maybe (return ()) ih init
       return (ih,[el])

-- | 'stringF' combined with 'show' and 'read'
readShowF :: (Show a,Read a) => F a a
readShowF = mapMaybeF readM =<= stringF =<= mapF show

-- | 'readShowF' restricted to numbers, @\<input type="number">@
numberF :: (Show a,Read a,Num a) => F a a
numberF = mapMaybeF readM =<= inputF "number" =<= mapF show

-- | A string input/output field, @\<input type="text">@
stringF = inputF "text"

-- | A string input/output field that shows @****@ instead of the actual
-- input, @\<input type="password">@
passwordF = inputF "password"

-- | A slider which lets you choose a value from an enumeration,
--  @\<input type="range">@
sliderF :: Enum a => (a,a) -> F a a
sliderF (low,high) =
    toEnum.read <$> qInputF' "range" attrs =<= mapF (show.fromEnum)
  where
    attrs = [set (attr "min") (show (fromEnum low)),
             set (attr "max") (show (fromEnum high))]


-- | A progress meter
--  @\<progress max=...>\<\/meter>@
progressF :: (Num i,Ord i,Show i) => i -> F i o
progressF max =
  F $ \ oh ->
  do el <- mkElement "progress" # set (attr "max") (show max)
     let ih v = do pure el # set value (show v)
                   return ()
     return (ih,[el])

-- | A meter for scalar value between given minimum and maximum.
--  @\<meter min=... max=...>\<\/meter>@
meterF :: (Num i,Ord i,Show i) => (i,i) -> F i o
meterF (min,max) =
  F $ \ oh ->
  do el <- mkElement "meter" # set (attr "min") (show min)
                             # set (attr "max") (show max)
     let ih v = do pure el # set value (show v)
                   return ()
     return (ih,[el])

{- -- Meter implemented as a canvasF
--meterF :: F Double x
meterF = nullF =<= canvasF (width,height) `withF` attrs =<= mapF bar
  where
    attrs = [style "border"=:"1px solid black"]
    width,height :: Num a => a
    width = 150
    height = 18

    bar d = color (RGB 0 0 240) $ fill $ rect (0,0) (d*width,height)
-}

inputF ty = inputF' ty []

inputF' = inputF'' valueChange' (const True)
qInputF' = inputF'' valueInput' (const True)

inputF'' ev p ty ps =
  F $ \ oh ->
  do inp <- mkElement "input" # set UI.type_ ty
                              # setMany ps
     on ev inp $ \ed -> do when (p ed) (oh =<< get value inp)
     let ih s = do pure inp # set value s
                   --when isText (select inp)
                   return ()
     return (ih,[inp])
  where
    isText = ty `elem` ["number","text","password"]

data Click = Click deriving (Eq,Show,Read)

-- | Creates a button, like @\<input type="button" value="/label/">@.
-- It outputs 'Click' when pressed. ('Click' received on the input
-- is propagated directly to the output, allowing button clicks to
-- be simulated programmatically.)
buttonF lbl =
  F $ \ oh ->
  do btn <- mkElement "input" # set UI.type_ "button" # set value lbl
     on UI.click btn $ \_ -> oh Click
     return (oh,[btn])

-- | Creates a button with another fudget inside (which
-- should be something simple, e.g. an 'imgF' or a 'stringDisplayF'...),
-- @\<button>...\<\/button>@.
buttonGroupF :: F hi ho -> F (Click+hi) (Click+ho)
buttonGroupF (F fud) =
  F $ \ oh ->
  do (ih,els) <- fud (oh . Right)
     btn <- wrap "button" els
     on UI.click btn $ \_ -> oh (Left Click)
     return (either (oh . Left) ih,[btn])
  
-- | A 'checkboxF' with a label
toggleButtonF lbl = wrapF "label" (checkboxF >+ textF lbl)

-- | A checkbox, @\<input type=checkbox>@.
checkboxF =
  F $ \ oh ->
  do inp <- mkElement "input" # set UI.type_ "checkbox"
     let checked = get UI.checked inp
     on UI.checkedChange inp oh
     let ih b = do old <- checked
                   when (b/=old) $ do pure inp # set UI.checked b
                                    --oh b
                                      return ()
     return (ih,[inp])

-- | A group of checkboxes allowing you to select one of several alternatives
radioGroupF :: Eq alt => Options alt -> F alt alt
radioGroupF opts = vBoxF (radioGroupF' opts)

-- | A 'radioGroupF' without the built-in vertical layout
radioGroupF' :: Eq alt => Options alt -> F alt alt
radioGroupF' (Options alts start_alt) =
    loopThroughRightF controlF altsF
  where
    altsF = listF [(alt,toggleButtonF lbl)|(alt,lbl)<-alts]
    
    controlF = putsF init (stateF control start_alt)
       where init = [Left (start_alt,True),Right start_alt]

    control alt = either click select
       where
         click (a,False) = if a==alt
                           then (alt,[Left (a,True)])
                           else (alt,[])
         click (a,True) = select a

         select a = if a==alt
                    then (alt,[])
                    else (a,[Left (alt,False),Right a])

--data ListRequest alt = Splice Int Int [alt] | Pick alt
data ListRequest alt = New (Options alt) | Pick alt
data Options     alt = Options [(alt,String)] alt

-- | A menu of options, @\<select>\<option>...\<\/option>...\<\/select>@
selectF :: Eq alt => Options alt -> F alt alt
selectF os = dynSelectF os =<= mapF Pick

type SelectF alt = F (ListRequest alt) alt

-- | A menu of options that can be modified dynamically
dynSelectF :: Eq alt => Options alt -> SelectF alt
dynSelectF os@(Options alts start_alt) =
    F $ \ oh -> 
    do altsVar <- liftIO $ newMVar alts
       el <- mkElement "select"
       let change v = pure el # set value (show v)
           pick alt alts = maybeM (\ix->do change ix;oh alt) (alt_ix alt)
              where alt_ix alt = findIndex ((==alt).fst) alts
           ih i = do alts <- liftIO $ takeMVar altsVar
                     case i of
                       Pick alt -> do pick alt alts; liftIO $ putMVar altsVar alts
                       New (Options alts alt) ->
                                   do os <- zipWithM option [0..] alts
                                      pure el # set UI.children os
                                      pick alt alts
                                      liftIO $ putMVar altsVar alts
       ih (New os)
       on valueChange' el $ \_ ->
           do ixs <- get value el
              case readM ixs of
                Nothing -> liftIO $ putStrLn $ "ixs="++ixs
                Just ix -> do alts <- liftIO $ readMVar altsVar
                              oh (fst (alts!!ix))
       return (ih,[el])
  where
    option n (_,lbl) =
      mkElement "option" # set text lbl # set (attr "value") (show n)

-- | Creates a canvas of given width and height. Use the functions
-- from "Graphics.UI.Threepenny.Canvas" to draw things. The canvas
-- produces output on 'UI.mouseup' events
-- (becase 'UI.mouseup' up seems to work for both the left and
-- the right mouse button, while 'UI.click' only works for the left mouse
-- button).
canvasF :: (Int,Int) -> F (Picture ()) (Int,Int)
canvasF size = canvasF' {-devicePixelRatio-} size =<= mapF ((,) False)

--devicePixelRatio = 1
-- a custom scaling factor and

-- | 'canvasF' with an option to render on top of
-- or replace the current picture
canvasF' :: (Int,Int) -> F (Bool,Picture ()) (Int,Int)
canvasF' {-k-} size@(w,h) =
  F $ \ oh ->
  do el <- mkElement "canvas" # set (attr "width") (show w)
                              # set (attr "height") (show h)
    --http://stackoverflow.com/questions/10864249/disabling-right-click-context-menu-on-a-html-canvas
     pure el # set (attr "oncontextmenu") "return false"
     let canvas = el
     on mouseup' el $ \(x,y) -> oh (round x,round y)
     let ih (ontop,p) = do unless ontop $ UI.clearCanvas canvas
                           p canvas
     return (ih,[el])
--where
--  k' = fromIntegral k
{-
-- | Pops up a message that the user must acknowledge before continuing
alertF = F $ \ _ -> return (alert . fromString,[])
-}
--------------------------------------------------------------------------------

-- | Outputs one message read from LocalStorage on startup. Writes any input
-- to LocalStorage.
localStorageF :: (Read a,Show a) => String -> a -> F a a
localStorageF key a0 =
  initF (readLocalStorage key a0) $ \ a -> putF a $ writeLocalStorageF key
     
writeLocalStorageF key = nullF =<= ioF (writeXdgFile key . show)

readLocalStorage key d =
  either (const d) (maybe d id) . fmap readM <$> liftIO (readXdgFile key)

readXdgFile path = tryIOError $
                   do dir <- getAppXdgDir XdgData
                      s <- readFile (dir++"/"++path)
                      seq (length s) $ return s

writeXdgFile path s = do dir <- getAppXdgDir XdgData
                         createDirectoryIfMissing True dir
                         writeFile (dir++"/"++path) s

getAppXdgDir xdg = getXdgDirectory xdg =<< getProgName
--{-
-- | Like 'stateF', but also uses LocalStorage to retain the state between
-- activations of the web application. The first argument is a key that should
-- be unique among all web applications on the same server.
persistentStateF :: (Read s,Show s) => String -> (s->i->(s,[o])) -> s -> F i o
persistentStateF key f s0 =
    initF (readLocalStorage key s0) $ \ s1 ->
    loopThroughRightF (stateF pf s1) (writeLocalStorageF key)
  where
    pf s (Left ()) = (s,[]) -- doesn't happen
    pf s (Right i) = (s',Left s':map Right os)
      where (s',os) = f s i
--}
--------------------------------------------------------------------------------

data Tick = Tick deriving (Eq,Show)

-- | 'timerF' outputs 'Tick' at regular intervals.
-- The timer starts when it receives 'Just' /interval/ on its input.
-- The timer stops when it receives 'Nothing' in its input.
timerF = F $ \ oh ->
         do t <- UI.timer
            let start d = do pure t # set UI.interval d
                             UI.start t
                ih Nothing = UI.stop t
                ih (Just d) = do UI.stop t; start d
            on UI.tick t $ \ _ -> oh Tick
            return (ih,[])

--------------------------------------------------------------------------------
{-
type AjaxRequest k v = (Method,URL,[(k,v)])

ajaxF :: (JSType k,JSType v,JSType r) =>
         F (AjaxRequest k v) (AjaxRequest k v,Maybe r)
ajaxF =
    F $ \ oh ->
    do let ih req@(m,url,ps) = do resp <- ajaxRequest m url ps
                                  oh (req,resp)
       return (ih,[])
-}
--------------------------------------------------------------------------------

-- | Like 'idF' but also writes messages to stdout
writeLogF show =
    F $ \ oh ->
    let ih x = do liftIO $ putStrLn (show x); oh x
    in pure (ih,[])

logF tag = writeLogF (((tag++": ")++).show)


--------------------------------------------------------------------------------
ignore _ = return ()

chop n [] = []
chop n xs = xs1:chop n xs2
     where (xs1,xs2) = splitAt n xs

fromLeft = either Just (const Nothing)
fromRight = either (const Nothing) Just

maybeM m = maybe (return ()) m

readM x = case reads x of
            (x,s):_ | lex s == [("","")] -> Just x
            _ -> Nothing

type URL = String

--------------------------------------------------------------------------------
-- * Threepenny extras

-- ** Web page

-- | Link to a style sheet at a given URL (not just a local FilePath)
addStyleSheet w url =
  do el <- mkElement "link"
             # set (attr "rel" ) "stylesheet"
             # set (attr "type") "text/css"
             # set (attr "href") url
     getHead w #+ [element el]
     return ()

-- ** Elements

wrap1 tagname = wrap tagname . (:[])
wrap tagname = wrap' tagname id
wrap' tagname as es = mkElement tagname # as #+ map pure es


-- ** Attributes

-- | An attribute is a function that modifies a user interface element.
-- See also "Graphics.UI.Threepenny.Attributes".
type Attribute = UI Element -> UI Element

-- | @a=:v@ is the same as @v # 'set' a@
(=:) :: ReadWriteAttr Element i o -> i -> Attribute
a=:v = set a v

setMany :: [Attribute] -> Attribute
setMany = foldr (.) id

--------------------------------------------------------------------------------

-- ** Workaround for buggy mousedown,mouseup :: Element -> Event (Int,Int)

-- | Mouse down event.
-- The mouse coordinates are relative to the element, with (0,0) in the
-- top left corner.
mousedown' :: Element -> Event (Double,Double)
mousedown' = fmap readCoordinates . domEvent "mousedown"
mouseup' :: Element -> Event (Double,Double)
mouseup' = fmap readCoordinates . domEvent "mouseup"


readCoordinates :: EventData -> (Double,Double)
readCoordinates json =
   case JSON.fromJSON json of
     JSON.Success [x,y] -> (x,y)
     _ -> error (show json) -- Array [Number 316.0,Number 249.125]

--------------------------------------------------------------------------------
-- ** Input events

-- | Event that occurs when the /user/ has changed the value of the
-- input element.
valueChange' :: Element -> Event String
valueChange'= valueChange'' "change"
valueInput' = valueChange'' "input"
valueChange'' ev el = unsafeMapUI el (const $ get value el) (domEvent ev el)
  where
    unsafeMapUI el f = unsafeMapIO (\a -> getWindow el >>= \w -> runUI w (f a))


--------------------------------------------------------------------------------
-- ** Functions for drawing on a canvas

type Picture a = UI.Canvas -> UI a

-- | Draw a straight line between to points
line :: UI.Point -> UI.Point -> Picture ()
line p1 p2 c = do UI.beginPath c
                  UI.moveTo p1 c
                  UI.lineTo p2 c
                  UI.stroke c

-- | Connect a sequence of points with lines.
path :: [UI.Point] -> Picture ()
path (p:ps) c = do c # UI.beginPath
                   c # UI.moveTo p
                   sequence_ [c # UI.lineTo p|p<-ps]
path _ _ = return ()

strokePath :: String -> [UI.Point] -> Picture ()
strokePath color ps c = do c # set' UI.strokeStyle color
                           c # path ps
                           c # UI.stroke

fillPath :: UI.FillStyle -> [UI.Point] -> Picture ()
fillPath color ps c = do c # set' UI.fillStyle color
                         c # path ps
                         c # UI.fill

circle p r = UI.arc p r 0 (2*pi)

instance MonadFail UI where
  fail _ = undefined