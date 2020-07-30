{$, _} = require "carma/vendor"
m = require "carma/dictionaries/meta-dict"

debounce = (wait, fn) -> _.debounce fn, wait

class AddressesDict extends m.dict

  # options object:
  # { kvm: kvm_object
  # , coords_field: "name of coordinates field in kvm, defaults to caseAddress_coords"
  # , address_field: "name of address field in kvm, defaults to caseAddress_address"
  # , map_name: "name of HTML element, defaults to #case-form-caseAddress_map"
  # }
  constructor: (@opts)->
    # will not set coords field if it is not specified.
    @coords_field = if @opts.coords_field? then @opts.coords_field else "caseAddress_coords"
    @address_field = if @opts.address_field? then @opts.address_field else "caseAddress_address"
    @map_name = if @opts.map_name? then @opts.map_name else "#case-form-caseAddress_map"
    @kvm = @opts.kvm
    @Dict = require "carma/dictionaries"
    @addresses = []
    @suggestions = []
    @kvm[@address_field].subscribe(((newValue) -> this.trySetCoords(newValue)), this)
    @mapModule = null

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

  find: debounce 1200, (q, cb, opt) ->
    # too short a query
    return cb({}) if q.length < 4 and not opt?.force

    # query is not short and not in suggestions.
    dataForDD =
           query: q
    objForDD =
           url: "https://suggestions.dadata.ru/suggestions/api/4_1/rs/suggest/address"
           type: "post"
           contentType: "application/json"
           data: JSON.stringify(dataForDD)
           headers:
                    Authorization: "Token e0fb6d9a7a7920405c3eeefde7e7d6b529b2b2b9"
           dataType: "json"
           success:  (data) => # https://coffeescript.org/#fat-arrow as Max suggested.
                               @processAnswer data, cb
    $.ajax (objForDD)

  processAnswer: (data, cb) ->
    @addresses = (x.value for x in data.suggestions)
    @suggestions = data.suggestions
    return cb(@addresses)

  id2val: (i) ->
    return @addresses[i]

module.exports =
  dict: AddressesDict
  name: 'AddressesDict'
