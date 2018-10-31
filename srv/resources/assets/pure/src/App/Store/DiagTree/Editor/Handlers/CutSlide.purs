module App.Store.DiagTree.Editor.Handlers.CutSlide
     ( cutSlide
     ) where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (error, message, stack)
import Control.Monad.Error.Class (catchError, throwError)
import Network.HTTP.Affjax (AJAX)
import Data.Maybe (maybe, fromMaybe)

import App.Store (AppContext)
import App.Store.DiagTree.Editor.Handlers.Helpers (errLog, sendAction)
import App.Store.DiagTree.Editor.Reducers (DiagTreeEditorState)
import App.Store.DiagTree.Editor.Actions
     ( DiagTreeEditorAction ( CutSlideSuccess
                            , CutSlideFailure
                            )
     )


cutSlide
  :: forall eff
   . AppContext
  -> DiagTreeEditorState
  -> Aff (avar :: AVAR, console :: CONSOLE, ajax :: AJAX | eff) Unit

cutSlide appCtx state = flip catchError handleError $ do
  case slidePath of
    [ ] -> throwError $ error "Slide path is empty"
    _   -> act $ CutSlideSuccess slidePath

  where
    slidePath = fromMaybe [] state.copyPasteBuffer.branch

    act = sendAction appCtx

    reportErr err = errLog $
      "Cutting slide " <> show slidePath <> " failed: " <> message err
      # \x -> maybe x (\y -> x <> "\nStacktrace:\n" <> y) (stack err)

    handleError err = do
      reportErr err
      act $ CutSlideFailure slidePath
