{ko} = require "carma/vendor"
{data} = require "carma/data"
{store} = require "carma/neoComponents/store"
{closeSmsForm} = require "carma/neoComponents/store/smsForm/actions"
require "./styles.less"

smsTemplates = data.model.SmsTemplate.filter (x) => x.isActive

hasMatch = (q, x) ->
  q = q.toLowerCase()
  x = x.toLowerCase()
  (w.trim() for w in q.split /\s+/g).every (w) -> ~ x.indexOf w


class SmsFormViewModel
  constructor: () ->
    # Connector to store
    @appState = ko.observable store.getState()
    @unsubscribeFromAppState = store.subscribe => @appState store.getState()

    # Pure value from store
    isShown = ko.pureComputed => @appState().smsForm.isShown

    @subscriptions = [] # Mutable

    @phone = ko.observable @appState().smsForm.phone
      .extend validate: (x) -> true unless /^\+\d{11}$/.test x

    @caseId      = ko.observable @appState().smsForm.caseId
    @caseCity    = ko.observable @appState().smsForm.caseCity
    @caseAddress = ko.observable @appState().smsForm.caseAddress

    # Overwrite form's values with ones from store when form is just shown
    @subscriptions.push isShown.subscribe (value) =>
      return unless value # if form is just close with don' have to do anything
      @phone       @appState().smsForm.phone
      @caseId      @appState().smsForm.caseId
      @caseCity    @appState().smsForm.caseCity
      @caseAddress @appState().smsForm.caseAddress

    @message = ko.observable("").extend validate: (x) -> true if x.trim() is ""

    # Delaying just for a moment to start animation after element is shown
    @fadeIn = ko.pureComputed isShown
      .extend rateLimit: {method: "notifyWhenChangesStop", timeout: 1}

    # Some gap for fade-out animation
    fadeOut = ko.pureComputed @fadeIn
      .extend rateLimit: {method: "notifyWhenChangesStop", timeout: 500}

    # Property that indicates if root elements must be shown,
    # not really visible for user, but visible for browser (display: block).
    @isVisible = ko.computed => isShown() || fadeOut()

    @smsTemplate = ko.observable null

    @subscriptions.push @smsTemplate.subscribe (x) =>
      {text} = _.find smsTemplates, ({label}) -> label is x

      x = text
        .replace /\$phone\$/g,                     @phone()
        .replace /\$case\.id\$/g,                  @caseId()
        .replace /\$case\.city\$/g,                @caseCity()
        .replace /\$case\.caseAddress_address\$/g, @caseAddress()

      @message x

    @sendIsBlocked = ko.pureComputed =>
      Boolean @phone.validationError() || @message.validationError()

  dispose: =>
    do @unsubscribeFromAppState
    do x.dispose for x in @subscriptions

  closeForm: (model, e) ->
    do e?.preventDefault
    do e?.stopPropagation
    store.dispatch closeSmsForm()

  handleOverlayClick: (model, {target}) =>
    do @closeForm if target.classList.contains "is-overlay"

  smsTplFuzzySearchHandler: (q, cb) ->
    cb (x for {label: x} in smsTemplates when hasMatch q, x)

  send: =>
    console.error "TODO: sms form sending"


module.exports =
  componentName: "sms-form"

  component:
    template:  require "./template.pug"
    viewModel: SmsFormViewModel
