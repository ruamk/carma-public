define [ "utils"
       , "hotkeys"
       , "text!tpl/screens/case.html"
       , "model/utils"
       , "model/main"
       ],
  (utils, hotkeys, tpl, mu, main) ->
    # Case view (renders to #left, #center and #right as well)
    setupCaseMain = (viewName, args) -> setupCaseModel viewName, args

    setupCaseModel = (viewName, args) ->

      # Default values
      # FIXME: User's name and creation date are better to be assigned by
      # the server.


      # Render list of required fields in right pane
      #
      # bbInstance is available only after model has been loaded. The
      # only way to execute custom code inside modelSetup is using
      # fetchCb option. By the time slotsee's are bound, fetchCb may
      # not have been called yet, thus we explicitly use applyBindings
      # here.
      fetchCb =  () ->
        instance = global.viewsWare[viewName].bbInstance
        ctx =
          "fields": _.map(instance.requiredFields, (f) -> instance.fieldHash[f])
        setCommentsHandler()

        $("#empty-fields-placeholder").html(
          Mustache.render($("#empty-fields-template").html(), ctx))

        ko.applyBindings(global.viewsWare[viewName].knockVM,
                         el("empty-fields"))

      main.modelSetup("case") viewName, args,
                         permEl       : "case-permissions"
                         focusClass   : "focusable"
                         slotsee      : ["case-number"]
                         groupsForest : "center"
                         fetchCb      : fetchCb

      # Render service picker
      #
      # We use Bootstrap's glyphs if "icon" key is set in dictionary
      # entry.
      $("#service-picker-container").html(
        Mustache.render($("#service-picker-template").html(),
                        {dictionary: global.dictionaries["Services"]}))

      $("body").on("change.input", ".redirectOnChange", () ->
          setTimeout(( -> window.location.hash = "back"), 500))

      utils.mkDataTable $('#call-searchtable')
      hotkeys.setup()
      kvm = global.viewsWare[viewName].knockVM
      for i of kvm when /.*Not$/.test(i) or i == 'actions'
        do (i) -> kvm[i].subscribe -> mbEnableActionResult(kvm)

    mbEnableActionResult = (kvm) ->
      nots = (i for i of kvm when /.*Not$/.test i)
      if (_.any nots, (e) -> kvm[e]())
        $("[name=result]").attr('disabled', 'disabled')
        $("[name=result]").next().find("i").removeAttr("data-provide")
      else
        $("[name=result]").removeAttr 'disabled'
        $("[name=result]").next().find("i")
          .attr("data-provide", "typeahead-toggle")

    setCommentsHandler = ->
      $("#case-comments-b").on 'click', ->
        i = $("#case-comments-i")
        return if _.isEmpty i.val()
        comment =
          date: (new Date()).toString('dd.MM.yyyy HH:mm')
          user: global.user.login
          comment: i.val()
        k = global.viewsWare['case-form'].knockVM
        if _.isEmpty k['comments']()
          k['comments'] [comment]
        else
          k['comments'] k['comments']().concat comment
        i.val("")


    # Top-level wrapper for storeService
    addService = (name) ->
      kvm = global.viewsWare["case-form"].knockVM
      mu.addReference kvm,
                   'services',
                   { modelName : name },
                   (k) ->
                      e = $('#' + k['view'])
                      e.parent().prev()[0].scrollIntoView()
                      e.find('input')[0].focus()

    utils.build_global_fn 'addService', ['screens/case']

    makeCase = () ->
      v = global.viewsWare['call-form'].knockVM
      args =
        contact_name:   v['callerName_name']()
        contact_phone1: v['callerName_phone1']()
        contact_phone2: v['callerName_phone2']()
        contact_phone3: v['callerName_phone3']()
        contact_phone4: v['callerName_phone4']()
        contact_email:  v['callerName_email']()
        contact_contactOwner: v['callerName_contactOwner']()
        contact_ownerName:    v['callerName_ownerName']()
        contact_ownerPhone1:  v['callerName_ownerPhone1']()
        contact_ownerPhone2:  v['callerName_ownerPhone2']()
        contact_ownerPhone3:  v['callerName_ownerPhone3']()
        contact_ownerPhone4:  v['callerName_ownerPhone4']()
        contact_ownerEmail:   v['callerName_ownerEmail']()
        program:        v['program']()
        city:           v['city']()
        car_make:       v['make']()
        comment:        v['wazzup']()
        callTaker: global.user.meta.realName
      main.buildNewModel 'case', args, {},
        (a, b, k) ->
          global.router.navigate("case/#{k.id()}", { trigger: true })



    removeCaseMain = ->
      $("body").off "change.input"

    # get partners and show them in table
    # this is called from local.coffe:showCase
    initPartnerTables = ($view,parentView) ->
      m = $view[0].id.match(/(\w*)_partner-view/)
      partnerType = m[1]
      table = $view.find("table##{partnerType}_partnerTable")
      kase = global.viewsWare["case-form"].knockVM
      svc = utils.findCaseOrReferenceVM(parentView)
      # this options for datatable will hide priorities columns for dealer table
      tblOpts = if partnerType is "contractor"
                  {}
               else
                  { aoColumns: utils
                      .repeat(5, null)
                      .concat(utils.repeat(3, { bVisible: false}))
                  }

      unless table.hasClass("dataTable")
        utils.mkDataTable table, tblOpts
        table.on "click.datatable", "tr", ->
          name = this.children[0].innerText
          city = this.children[1].innerText
          addr = this.children[2].innerText
          svc["#{partnerType}_partner"](name)
          svc["#{partnerType}_address"]("#{city}, #{addr}")
          svc["#{partnerType}_partnerId"]($(this).attr('partnerid'))

      table = table.dataTable()
      # hope that contractor_partner is the only partner
      dealer = if partnerType is "contractor" then 0 else 1
      select = ["isActive=1", "isDealer=#{dealer}"]
      select.push("city=#{kase.city()}") if kase.city()
      select.push("makes=#{kase.car_make()}")  if kase.car_make()
      url    = if partnerType is "contractor"
                  "/partnersFor/#{svc.modelName()}?#{select.join('&')}"
               else
                  "/allPartners?#{select.join('&')}"
      dict = global.dictValueCache['DealerCities']
      $.getJSON url, (objs) ->
        # Store partner cache for use with maps
        cache = {}
        rows = for p in objs
          p.name = p.name.trim()
          cache[p.id] = p
          [p.name        || '',
           dict[p.city]  || '',
           p.addrDeFacto || '',
           p.phone1      || '',
           p.workingTime || '',
           p.priority1   || '',
           p.priority2   || '',
           p.priority3   || '',
           p.id]
        # this last id will never be shown, but I need this, to add
        # partnerid as attribute of the row to pass it then to
        # the service kvm
        table.data("cache", cache)
        table.fnClearTable()
        r = table.fnAddData(rows)
        n = table.fnSettings().aoData[ r[0] ]
        # this will set partnerid attribute to each row
        # FIXME: find better way to do this
        for i in r
          s  = table.fnSettings().aoData[ i ]
          tr = s.nTr
          id = s._aData[8]
          $(tr).attr('partnerid', "partner:#{id}")

    #############################################################################
    # kb hooks


    { constructor       : setupCaseMain
    , destructor        : removeCaseMain
    , template          : tpl
    , addService        : addService
    , makeCase          : makeCase
    , initPartnerTables : initPartnerTables
    }