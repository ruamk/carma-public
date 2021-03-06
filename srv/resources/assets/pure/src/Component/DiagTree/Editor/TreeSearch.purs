module Component.DiagTree.Editor.TreeSearch
     ( diagTreeEditorTreeSearch
     ) where

import Prelude hiding (div)

import Data.Maybe (Maybe (..), maybe)
import Data.String (trim, null)
import Data.String.NonEmpty (NonEmptyString, fromString, toString)

import Record.Builder (merge)

import Effect (Effect)
import Effect.Aff (launchAff_)

import React
     ( ReactClass, component, getProps, getState, modifyState
     )

import React.SyntheticEvent (preventDefault, altKey, ctrlKey, shiftKey, key)
import React.DOM (div, input, button, i)

import React.DOM.Props
     ( className, value, _type, placeholder, title, disabled
     , onChange, onClick, onKeyUp
     )

import Utils ((<.>), storeConnect, eventInputValue)

import Utils.Debouncer
     ( DebouncerSubscription
     , newDebouncer
     , subscribeToDebouncer
     , unsubscribeFromDebouncer
     , sendToDebouncer
     )

import App.Store (Store, dispatch)
import App.Store.Reducers (AppState)
import App.Store.Actions (AppAction (DiagTree))
import App.Store.DiagTree.Actions (DiagTreeAction (Editor))

import App.Store.DiagTree.Editor.Actions
     ( DiagTreeEditorAction (TreeSearch)
     )

import App.Store.DiagTree.Editor.TreeSearch.Actions
     ( DiagTreeEditorTreeSearchAction (SearchByQuery, ResetSearch)
     )


diagTreeEditorTreeSearchRender
  :: forall state
   . ReactClass { store       :: Store state AppAction
                , isDisabled  :: Boolean
                , searchQuery :: Maybe NonEmptyString
                }

diagTreeEditorTreeSearchRender = defineComponent $
  \ { changeHandler, clearHandler, keyHandler } { isDisabled } { query } ->

  [ input
      [ className $ classSfx "search-input"
      , _type "text"
      , placeholder "Поиск"
      , value query
      , disabled isDisabled
      , onChange changeHandler
      , onKeyUp keyHandler
      ]

  , button
      [ className $ classSfx "clear"
      , disabled $ isDisabled || null query
      , onClick clearHandler
      , title "Очистить строку поиска"
      ]
      [ i [className $ "glyphicon" <.> "glyphicon-remove"] mempty ]
  ]

  where
    name = "DiagTreeEditorTreeSearch"
    classSfx s = name <> "--" <> s
    wrapper = div [className name]

    onChangeHandler this changeDebouncer event = do
      query <- eventInputValue event
      modifyState this _ { query = query }
      sendToDebouncer changeDebouncer query

    onClearHandler store this changeDebouncer event = do
      preventDefault event
      resetSearch store this changeDebouncer

    onKeyHandler store this changeDebouncer event = go where
      condition a b c d = a && b && c && d
      f x = if x then resetSearch store this changeDebouncer else pure unit

      go  =  f
         =<< condition
         <$> (not <$> altKey   event)
         <*> (not <$> ctrlKey  event)
         <*> (not <$> shiftKey event)
         <*> (key event <#> (_ == "Escape"))

    resetSearch store this changeDebouncer = do
      act store ResetSearch

      -- In case escape pressed before debounced request
      sendToDebouncer changeDebouncer ""
      modifyState this _ { query = "" }

    searchHandler store query = act store $
      case fromString $ trim query of
           Nothing -> ResetSearch
           Just x  -> SearchByQuery x

    defineComponent renderFn = component name \this -> do
      { store, searchQuery } <- getProps this
      changeDebouncer <- newDebouncer 500
      let search = searchHandler store

      let preBound =
            { changeHandler : onChangeHandler       this changeDebouncer
            , clearHandler  : onClearHandler  store this changeDebouncer
            , keyHandler    : onKeyHandler    store this changeDebouncer
            }

      let state =
            { changeSubscription: (Nothing :: Maybe DebouncerSubscription)
            , query: maybe "" toString searchQuery
            }

      let r = renderFn preBound

      pure
        { state
        , render: map wrapper $ r <$> getProps this <*> getState this

        , unsafeComponentWillMount: do
            subscription <- subscribeToDebouncer changeDebouncer search
            modifyState this _ { changeSubscription = Just subscription }

        , unsafeComponentWillReceiveProps: \ { searchQuery: newQuery } -> do
            { searchQuery: oldQuery } <- getProps this

            if newQuery == oldQuery
               then pure unit
               else case newQuery <#> toString of
                         Nothing -> modifyState this _ { query = "" }
                         Just x  -> do
                           { query } <- getState this
                           if trim query /= x
                              then modifyState this _ { query = x }
                              else pure unit

        , componentWillUnmount: do
            { changeSubscription } <- getState this
            maybe (pure unit) unsubscribeFromDebouncer changeSubscription
        }


diagTreeEditorTreeSearch
  :: ReactClass { store      :: Store AppState AppAction
                , isDisabled :: Boolean
                }

diagTreeEditorTreeSearch = storeConnect f diagTreeEditorTreeSearchRender where
  f appState = merge { searchQuery } where
    { searchQuery } = appState.diagTree.editor.treeSearch


act
  :: forall state
   . Store state AppAction
  -> DiagTreeEditorTreeSearchAction
  -> Effect Unit

act store = launchAff_ <<< dispatch store <<< DiagTree <<< Editor <<< TreeSearch
