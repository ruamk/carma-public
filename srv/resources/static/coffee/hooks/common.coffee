define [ "utils"
       , "dictionaries"
       ], (u, d) ->
  distanceQuery = (coord1, coord2) ->
    u.stripWs "/geo/distance/#{coord1}/#{coord2}/"

  # Transform distance in meters to km
  formatDistance = (dist) -> Math.round ((parseInt dist) / 1000)

  dictionaryKbHook: (instance, knockVM) ->
    for n of instance.dictionaryFields
      do (n) ->
        fieldName  = instance.dictionaryFields[n]
        dictName   = instance.fieldHash[fieldName].meta.dictionaryName
        parent     = instance.fieldHash[fieldName].meta.dictionaryParent
        bounded    = instance.fieldHash[fieldName].meta.bounded
        dictType   = instance.fieldHash[fieldName].meta.dictionaryType

        dict = d.dictFromMeta knockVM, instance.fieldHash[fieldName].meta

        # Perform label-value transformation
        knockVM["#{fieldName}Local"] =
          kb.observable instance,
                        key: fieldName
                        read: (k) ->
                          # Read label by real value
                          val = knockVM[k]()
                          lab = dict.getLab(val)
                          return (lab || val)
                        write: (lab) ->
                          # Set real value by label
                          return if knockVM["#{fieldName}Disabled"]
                          val = dict.getVal(lab)
                          # drop value if can't find one for bounded dict
                          if bounded and not val
                          then  knockVM[fieldName]("")
                          else  knockVM[fieldName](val || lab)
                        ,
                        knockVM

        knockVM["#{fieldName}Typeahead"] =
          new ThMenu
            select: (v) ->
              knockVM[fieldName](dict.id2val(v))
              knockVM[fieldName].valueHasMutated()
            dict  : dict

        knockVM["#{fieldName}Disabled"].subscribe (v) ->
          dict.disabled = v

  regexpKbHook: (instance, knockVM) ->
    # Set observable with name <fieldName>Regexp for inverse of
    # result of regexp checking for every field with meta.regexp
    # annotation. Observable is True when regexp fails.
    for n of instance.regexpFields
      fieldName = instance.regexpFields[n]
      regexp = instance.fieldHash[fieldName].meta.regexp
      ((f, r) ->
        knockVM["#{fieldName}Regexp"] =
              kb.observable instance,
                            key: f
                            read: (k) -> not r.test instance.get(k)
      )(fieldName, new RegExp(global.dictLabelCache["_regexps"][regexp]))

  filesKbHook: (instance, knockVM) ->
    _.each instance.filesFields, (n) ->
      upl = "/upload"
      p   = "/s/fileupload"
      knockVM["#{n}UploadUrl"] = ko.computed
        read: ->
          # some strange magick, if remove knockVM['maybeId']()
          # then this won't be recomputed when id will be defined
          # in program model still works well on case and others
          knockVM['maybeId']()
          return unless knockVM['id']
          path = "#{instance.model.name}/#{knockVM['id']()}/#{n}"
          "#{upl}/#{path}"
      knockVM["#{n}Info"] = ko.computed
        read: ->
          knockVM['maybeId']()
          return unless knockVM['id']
          path = "#{instance.model.name}/#{knockVM['id']()}/#{n}"
          fs = knockVM[n]()
          return [] unless fs
          for i in fs.split(',')
            do (i) ->
              url: "#{p}/#{path}/#{i.trim()}"
              name: i.trim()
              ctrl: "#{upl}/#{path}/#{i.trim()}"

  # Clear dependant dictionary fields when parent is changed
  # this.dictionaryHook = (elName) ->
  #   instance = global.viewsWare[elName].bbInstance
  #   for n of instance.dictionaryFields
  #     fieldName = instance.dictionaryFields[n]
  #     parent    = instance.fieldHash[fieldName].meta.dictionaryParent

  #     if parent
  #       ((f) ->
  #         instance.bind("change:" + parent, (v) -> instance.set(f, ""))
  #       )(fieldName)

  dateTimeHook: (i, k) ->
    for n in i.dateTimeFields
      do (n) ->
        k["#{n}DateTime"] = ko.computed
          read :       -> k[n]()
          write: (val) -> if Date.parse(val) then k[n](val) else k[n]("")

  tarifOptNameDef: (i, k) ->
    k["nameOrDef"] = ko.computed
      read: -> k["optionName"]() or "Тарифная опция…"

  # Update a field with the distance between two coordinates whenever
  # they change
  distHook: (instance, knockVM) ->
    for n in instance.distFields
      do (n) ->
        m = instance.fieldHash[n].meta

        # Find VMs and fields to watch for coordinates
        d1_meta = u.splitFieldInView m.distanceTo1
        if not d1_meta.view?
          vm1 = knockVM
        else
          vm1 = u.findVM d1_meta.view

        d2_meta = u.splitFieldInView m.distanceTo2
        if not d2_meta.view?
          vm2 = knockVM
        else
          vm2 = u.findVM d2_meta.view

        # Subscribe to change in either of coordinates
        vm1[d1_meta.field].subscribe (new_coord) ->
          other_coord = vm2[d2_meta.field]()
          if other_coord
            $.get distanceQuery(new_coord, other_coord), (resp) ->
              knockVM[n](formatDistance(resp).toString())

        vm2[d2_meta.field].subscribe (new_coord) ->
          other_coord = vm1[d1_meta.field]()
          if other_coord
            $.get distanceQuery(new_coord, other_coord), (resp) ->
              knockVM[n](formatDistance(resp).toString())

  dictManyHook: (i, k) ->
    for n in i.dictManyFields
      do (n) ->
        dictName  = i.fieldHash[n].meta.dictionaryName
        parent    = i.fieldHash[n].meta.dictionaryParent
        bounded   = i.fieldHash[n].meta.bounded
        dictType  = i.fieldHash[n].meta.dictionaryType

        dict = d.dictFromMeta k, i.fieldHash[n].meta

        k["#{n}Many"] = ko.computed
          # we don't need any value here
          # I have to retrieve something, to make ko refresh view
          read: -> k[n](); return ""

          write: (lab) ->
            return if lab == ""
            return if k["#{n}Disabled"]
            val = dict.getVal(lab)
            c = u.splitVals k[n]()
            return if _.contains c, val
            c.push val
            if (bounded and val) or (not bounded)
              k[n](c.sort().join(','))

        k["#{n}Locals"] = ko.computed
          read: ->
            for val in u.splitVals k[n]()
              do (val) ->
                lab = dict.getLab(val)
                {label: lab || val, value: val}

        k["#{n}Remove"] = (el) ->
          return if k["#{n}Disabled"]
          # FIXME: I think, this should be made with bb observable
          # arrays, so we can make them in metamodel and use normal
          # collections, without splitting it manually
          c = u.splitVals(k[n]())
          k[n] _.without(c, el.value).join(',')

        k["#{n}Typeahead"] =
          new ThMenu
            select: (v) ->
              # FIXME: find more appropriate way to set values here
              k["#{n}Many"](dict.getLab(dict.id2val(v)))
            dict  : dict

  # Standard element callback which will scroll model into view and
  # focus on first field
  stdElCb: (elName) ->
    e = $el(elName)
    # Scroll group to the top of the screen
    if e.hasClass("accordion-inner")
      e.parents(".accordion-group")[0].scrollIntoView()
    f = e.find(".focusable")[0]
    f and f.focus()
