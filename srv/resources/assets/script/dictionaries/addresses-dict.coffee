{$, _} = require "carma/vendor"
m = require "carma/dictionaries/meta-dict"

debounce = (wait, fn) -> _.debounce fn, wait

class AddressesDict extends m.dict
  constructor: (@opts)->
    @kvm = @opts.kvm
    @Dict = require "carma/dictionaries"
    @addresses = []
    @suggestions = []
    @kvm.caseAddress_address.subscribe(((newValue) -> this.trySetCoords(newValue)), this)
    @mapModule = null

  trySetCoords: (newValue) ->
    foundExact = (x for x in @suggestions when x.value == newValue)
    if foundExact.length == 1
      exactData = foundExact[0].data
      coordsString = exactData.geo_lon + "," + exactData.geo_lat
      @kvm.caseAddress_coords(coordsString)
      if !@mapModule
        @mapModule = require "carma/map"
      osMap = $("#case-form-caseAddress_map").data("osmap")
      lonLatObj = @mapModule.lonlatFromShortString(coordsString)
      @mapModule.setPlace osMap, {coords: lonLatObj}
      @mapModule.spliceCoords lonLatObj, @kvm,
          osmap: osMap
          addr_field: "caseAddress_address"
          city_field: "caseAddress_city"
          current_blip_type: "default"

  find: debounce 1200, (q, cb, opt) ->
    # too short a query
    return cb({}) if q.length < 4 and not opt?.force

    # query is not short and not in suggestions.
    dataForDD =
           query: q
    originalThis = this # this way it will be captured.
    objForDD =
           url: "https://suggestions.dadata.ru/suggestions/api/4_1/rs/suggest/address"
           type: "post"
           contentType: "application/json"
           data: JSON.stringify(dataForDD)
           headers:
                    Authorization: "Token e0fb6d9a7a7920405c3eeefde7e7d6b529b2b2b9"
           dataType: "json"
           success:  (data) ->
                               originalThis.processAnswer data, cb
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
