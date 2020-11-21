{$, _} = require "carma/vendor"
m = require "carma/dictionaries/meta-dict"

debounce = (wait, fn) -> _.debounce fn, wait

class AddressesDict extends m.dict

  # options object:
  # { kvm: kvm_object
  # , coords_field: "name of coordinates field in kvm, defaults to caseAddress_coords"
  # , address_field: "name of address field in kvm, defaults to caseAddress_address"
  # , map_name: "name of HTML element, defaults to #case-form-caseAddress_map"
  # , search_radius_meters = #self-explanatory, by default 100
  # }
  constructor: (@opts)->
    # will not set coords field if it is not specified.
    @coords_field = if @opts.coords_field? then @opts.coords_field else "caseAddress_coords"
    @address_field = if @opts.address_field? then @opts.address_field else "caseAddress_address"
    @search_radius_meters = if @opts.search_radius_meters? then @opts.search_radius_meters else 100
    @map_name = if @opts.map_name? then @opts.map_name else "#case-form-caseAddress_map"
    @kvm = @opts.kvm
    @Dict = require "carma/dictionaries"
    @addresses = []
    @suggestions = []
    @revsuggestions = []
    @kvm[@address_field].subscribe(((newValue) -> this.trySetCoords(newValue)), this)
    @kvm[@coords_field].subscribe(((newValue) -> this.askForAddressFromCoords(newValue)), this)
    @mapModule = null # to require/load afterwards.
    port = if @opts.proxy_port? then @opts.proxy_port else "8167"
    @search_url = "https://" + window.location.hostname + "/geosearch"
    @revsearch_url = "https://" + window.location.hostname + "/revgeosearch"

  trySetCoords: (newValue) ->
    foundExact = (x for x in @suggestions when x.value == newValue)
    if foundExact.length == 1
      exactData = foundExact[0].data
      coordsString = exactData.geo_lon + "," + exactData.geo_lat
      @kvm[@coords_field](coordsString) if @coords_field != null
      if !@mapModule
        @mapModule = require "carma/map"
      osMap = $(@map_name).data("osmap")
      lonLatObj = @mapModule.lonlatFromShortString(coordsString)
      @mapModule.setPlace osMap, {coords: lonLatObj}
      @mapModule.spliceCoords lonLatObj, @kvm,
          osmap: osMap
          addr_field: @address_field
          city_field: @coords_field
          current_blip_type: "default"

  askForAddressFromCoords: (newValue) ->
    if !@mapModule
      @mapModule = require "carma/map"
    osMap = $(@map_name).data("osmap")
    lonLat = @mapModule.lonlatFromShortString(newValue)
    if !lonLat
      return
    asStr = @mapModule.shortStringFromLonlat(lonLat)
    asStrs = asStr.split(",")
    lon = asStrs[0]
    lat = asStrs[1]
    objForDD =
      url: @revsearch_url + "?lat=" + encodeURIComponent(lat)+"&lon="+encodeURIComponent(lon)+"&radius="+encodeURIComponent(@search_radius_meters)
      type: "post"
      crossDomain: true
      dataType: "json"
      success:  (data) => @setupAddress data
    $.ajax (objForDD)
  setupAddress: (data) ->
    curr_addr = @kvm[@address_field]
    if data.length > 0 && curr_addr.length < 1
      @kvm[@address_field](data[0].value) # we set address from reverse suggestions only when it is empty

  find: debounce 1200, (q, cb, opt) ->
    # too short a query
    return cb({}) if q.length < 4 and not opt?.force

    # query is not short.
    objForDD =
           url: @search_url + "?query=" + encodeURIComponent(q)
           type: "post"
           crossDomain: true
           #contentType: "application/json"
           dataType: "json"
           success:  (data) => # https://coffeescript.org/#fat-arrow as Max suggested.
                               @processAnswer data, cb
    $.ajax (objForDD)

  processAnswer: (data, cb) ->
    @addresses = (x.value for x in data)
    @suggestions = data
    return cb(@addresses)

  id2val: (i) ->
    return @addresses[i]

module.exports =
  dict: AddressesDict
  name: 'AddressesDict'
