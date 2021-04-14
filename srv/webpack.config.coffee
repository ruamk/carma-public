path = require "path"

webpack                 = require "webpack"
UglifyJSPlugin          = require "uglifyjs-webpack-plugin"
ExtractTextPlugin       = require "extract-text-webpack-plugin"
OptimizeCssAssetsPlugin = require "optimize-css-assets-webpack-plugin"
{vendorNames} = require "./frontend.data.json"

RES_DIR = path.join __dirname, "resources"
SRC_DIR = path.join RES_DIR, "assets", "script"

BS_WYSIHTML5 =
  "bootstrap3-wysihtml5-bower/dist/bootstrap3-wysihtml5.all"

BS_WYSIHTML5_LOC_RU =
  "bootstrap3-wysihtml5-bower/dist/locales/bootstrap-wysihtml5.ru-RU"

cssExtractor = new ExtractTextPlugin "bundle.[name].css"

module.exports =
  devtool: "source-map"

  entry: do ->
    f = (acc, x) ->
      acc[x] = path.join SRC_DIR, "entryPoints", x
      acc

    vendorNames.reduce f,
      carma:     path.join SRC_DIR, "entryPoints", "main"
      resources: path.join SRC_DIR, "entryPoints", "resources"

  node:
    __filename: true
    __dirname:  true

  resolve:
    alias:
      carma:         path.resolve SRC_DIR
      "carma-img":   path.join RES_DIR, "static", "img"
      "./carma-img": path.join RES_DIR, "static", "img" # for urls in css
      "carma-tpl":   path.join RES_DIR, "assets", "template"
      "./carma-tpl": path.join RES_DIR, "assets", "template" # pug includes
      "carma-css":   path.join RES_DIR, "assets", "style", "style.less"
      elm:           path.join RES_DIR, "assets", "elm", "photos-in-carma", "src"

      oldLegacy3p:   "carma/oldLegacy3p"

      "jquery": "oldLegacy3p/jquery-2.2.4"
      # Not using it right now:
      # "jquery": "oldLegacy3p/myJQuery"
      # "jquery-original": path.resolve __dirname, "node_modules", "jquery"

      "jquery.knob": "jquery-knob/js/jquery.knob"
      "jquery.notify": "notify/dist/notify-combined"
      "jquery.datatables": "datatables"
      "jquery.typeahead": "typeahead.js/dist/typeahead.jquery"
      # "bloodhound": "typeahead.js/dist/bloodhound"

      "moment.tz":
        "moment-timezone/builds/moment-timezone-with-data-2010-2020"

      "bootstrap-datepicker.ru":
        "bootstrap-datepicker/js/locales/bootstrap-datepicker.ru"

      "bootstrap.jasny": "jasny-bootstrap/dist/js/jasny-bootstrap"
      "bootstrap.wysihtml5": BS_WYSIHTML5
      "bootstrap.wysihtml5.ru-RU": BS_WYSIHTML5_LOC_RU

      spin: "spin.js"
      ol2: "openlayers-2-build"
      finch: "finchjs/coffee/finch"
      "normalize-css": "normalize.css/normalize.css"

    extensions: [".js", ".coffee"]

  output:
    path: path.join RES_DIR, "static", "build", "frontend"
    filename: "bundle.[name].js"
    publicPath: "/s/frontend/"

  resolveLoader:
    alias:
      "precompile-code-loader": path.resolve __dirname,
        "webpackLoaders/precompile-code-loader.coffee"
      "precompile-template-loader": path.resolve __dirname,
        "webpackLoaders/precompile-template-loader.coffee"

  module:
    rules: [
      {
        test: require.resolve BS_WYSIHTML5
        use:  [
                {
                  loader: "imports-loader"
                  options: define: ">false", "this": ">window"
                }
                {
                  loader: "exports-loader"
                  options: wysihtml5: true
                }
              ]
      }

      {
        test: require.resolve BS_WYSIHTML5_LOC_RU
        use:  {
                loader: "imports-loader"
                options: define: ">false", "this": ">window"
              }
      }

      {
        test: require.resolve path.join "jasny-bootstrap",
                                        "dist", "js", "jasny-bootstrap"
        use:  {
                loader: "imports-loader"
                options: define: ">false", "this": ">window"
              }
      }

      {
        test: require.resolve "bootstrap-daterangepicker"
        use:  loader: "imports-loader", options: define: ">false"
      }

      # Not using it right now:
      # {
      #   test: require.resolve "jquery-migrate"
      #   use:  loader: "imports-loader", options: define: ">false"
      # }

      {
        test: require.resolve path.join SRC_DIR, "oldLegacy3p", "d3-v3.5.17"
        use:  loader: "imports-loader", options: define: ">false"
      }

      {
        test: require.resolve "openlayers-2-build"
        use:  loader: "exports-loader", options: "OpenLayers": true
      }

      { test: /\.coffee$/, use: "coffee-loader" }

      {
        test: /([a-zA-Z0-9]P|\/p|-p)recompiled\.coffee$/
        use:  [
                {
                  loader: "precompile-code-loader"
                  options:
                    requirableLibs: [
                      "underscore"
                      "immutable"
                      "blueimp-md5"
                      "js-base64"
                    ]
                }

                { loader: "coffee-loader" }
              ]
      }

      { test: /\.json$/, use: "json-loader" }

      {
        test: /\.(css|less)$/
        use: cssExtractor.extract [
          "css-loader"

          {
            loader: "less-loader"
            options: paths: [path.resolve(__dirname, "node_modules")]
          }
        ]
      }

      {
        test: /\.pug$/
        use:  [
                { loader: "precompile-template-loader" }
                { loader: "pug-loader" }

                {
                  loader:  "pug-lint-loader"
                  options: require "./.pug-lintrc.js"
                }
              ]
      }

      {
        test: /\.(png|jpg|jpeg|gif)$/
        use:  { loader: "url-loader", options: limit: 8192 }
      }

      { test: /\.(eot|svg|ttf|woff|woff2)$/, use: "file-loader" }

      {
        test: /\.elm$/
        exclude: [/elm-stuff/, /node_modules/]
        use:
          loader: 'elm-webpack-loader'
          options:
            cwd: RES_DIR + '/assets/elm/photos-in-carma'
            pathToElm: __dirname + '/node_modules/.bin/elm'
      }
    ]

  plugins: do ->
    plugins = [
      new webpack.optimize.CommonsChunkPlugin
        # Last in the list contains `webpackJsonp` function that others depends
        # on, so we need to reverse `vendorNames` to make first vendor to be
        # last in the list, because in page template it goes first.
        names: ["carma", "resources"].concat vendorNames.reverse()
        minChunks: Infinity

      new webpack.ProvidePlugin
        $: "jquery"
        jQuery: "jquery"
        "window.jQuery": "jquery"

      new webpack.EnvironmentPlugin NODE_ENV: "development"
      cssExtractor
    ]

    if process.env.NODE_ENV is "production"
      plugins.push new UglifyJSPlugin, new OptimizeCssAssetsPlugin

    plugins
