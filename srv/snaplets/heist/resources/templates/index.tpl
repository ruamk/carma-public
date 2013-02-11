<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8" />
    <title>CaRMa</title>
    <link rel="stylesheet" href="/s/css/bootstrap.min.css" />
    <link rel="stylesheet" href="/s/css/datepicker.css" />
    <link rel="stylesheet" href="/s/css/jquery.dataTables.css" />

    <!-- Additional set of icons -->
    <link rel="stylesheet" href="/s/css/stolen-icons.css" />

    <link rel="stylesheet" href="/s/css/local.css" />

    <!-- DOM manipulation -->
    <script src="/s/js/3p/jquery-1.7.1.min.js" />

    <!-- Rich UI -->
    <script src="/s/js/3p/bootstrap.min.js" />
    <script src="/s/js/3p/bootstrap-datepicker.js" />
    <script src="/s/js/3p/bootstrap-typeahead.js" />
    <script src="/s/js/3p/bootstrap-popover.js" />

    <!-- Tabular display -->
    <script src="/s/js/3p/jquery.dataTables.min.js" />

    <!-- Responsive UI javascript library -->
    <script src="/s/js/3p/knockout-2.0.0.js" />

    <!-- Utility library, Backbone dependency -->
    <script src="/s/js/3p/underscore-1.3.1.min.js" />

    <!-- Loose MVC -->
    <script src="/s/js/3p/backbone-0.9.1.min.js" />

    <!-- Knockback is a Knockout + Backbone glue -->
    <script src="/s/js/3p/knockback-0.13.min.js" />

    <!-- Simple templates -->
    <script src="/s/js/3p/mustache.js" />

    <!-- OpenLayers library allows map rendering -->
    <script src="/s/js/3p/OpenLayers-2.11/OpenLayers.js" />

    <!-- 25Kb of date parsing and formatting -->
    <script src="/s/js/3p/date-ru-RU.js" />

    <!-- masked input for datetime fields -->
    <script src="/s/js/3p/jquery.maskedinput-1.3.js" />

    <script src="/s/js/gen/customKoHandlers.js" />

    <!-- Model processing -->
    <script src="/s/js/search.js" />
    <script src="/s/js/gen/dictionaries.js" />
    <!-- <script src="/s/js/gen/metamodel.js" /> -->
    <script src="/s/js/gen/map.js" />
    <!-- <script src="/s/js/gen/main.js" /> -->
    <script src="/s/js/gen/viewsware.js" />
    <!-- <script src="/s/js/gen/hooks.js" /> -->
    <!-- <script src="/s/js/gen/local.js" /> -->
    <!-- <script src="/s/js/gen/case.js" /> -->
    <!-- <script src="/s/js/gen/vin.js" /> -->
    <!-- <script src="/s/js/gen/editVin.js" /> -->
    <!-- <script src="/s/js/gen/partners.js" /> -->
    <!-- <script src="/s/js/gen/call.js" /> -->
    <!-- <script src="/s/js/gen/backoffice.js" /> -->
    <!-- <script src="/s/js/gen/supervisors.js" /> -->
    <!-- <script src="/s/js/gen/rkc.js" /> -->
    <!-- <script src="/s/js/gen/report.js" /> -->
    <!-- <script src="/s/js/gen/hotkeys.js" /> -->
    <!-- <script src="/s/js/gen/fileupload.js" /> -->
    <script src="/s/js/gen/avaya.js" />
    <!-- <script src="/s/js/gen/editSms.js" /> -->
    <script src="/s/js/gen/sendSms.js" />
    <!-- <script src="/s/js/gen/rkcOps.js" /> -->
    <!-- <script src="/s/js/gen/rkcFront.js" /> -->
    <!-- <script src="/s/js/gen/printService.js" /> -->

    <script src="/s/js/3p/require.js" />
    <script src="/s/js/gen/requireCfg.js" />
    <script src="/s/js/gen/local.js" />

  </head>
  <body>
    <!-- Navigation bar on top -->
    <div class="navbar navbar-fixed-top">
      <div class="navbar-inner">
        <div class="container">
          <ul class="nav" id="nav">
            <a class="brand" href="/">
              CaRMa
            </a>
            <li class="divider-vertical" />
            <li id="avaya-panel" class="dropdown" style="display: none">
              <form class="navbar-search pull-left">
                <input type="text" class="search-query" placeholder="Avaya">
                <button id="avaya-call" class="btn">
                  <i class="icon stolen-icon-phone"></i>
                </button>
              </form>
              <ul class="dropdown-menu">
                <li id="avaya-info" class="nav-header"></li>
                <li><a id="avaya-accept" href="#">Принять звонок</a></li>
              </ul>
            </li>
            <li>
              <a href="#sms-send-modal" data-toggle="modal">
                <i class="icon icon-envelope icon-white"></i>
              </a>
            </li>
            <li class="divider-vertical" />
            <!-- ko template: { name: 'nav-li-template' }-->
            <!-- /ko -->
          </ul>
          <ifLoggedIn>
            <ul class="nav pull-right">
              <li class="divider-vertical" />
              <li class="dropdown">
                <a href="#"
                   class="dropdown-toggle"
                   data-toggle="dropdown">
                  <i class="icon-user icon-white" />&nbsp;<loggedInUser />
                  <b class="caret"></b>
                </a>
                <ul class="dropdown-menu">
                  <li>
                    <a href="/logout/">
                      <i class="icon-off icon-black" />&nbsp;Выход
                    </a>
                </ul>
              </li>
            </ul>
          </ifLoggedIn>
        </div>
      </div>
    </div>

    <!-- Main container for dynamically rendered layouts -->
    <div class="container-fluid" id="main-container">
      <div class="row-fluid" id="layout" />
    </div>

    <!-- Search screen -->
    <script type="text/template"
            id="search-screen-template"
            class="screen-template">
      <!-- Can't use offsetN class here due to fluid layout. -->
      <div id="tableView" />
    </script>

    <!-- SMS send form -->
    <div id="sms-send-modal" class="modal hide fade">
      <div class="modal-header">
        <button type="button" class="close" data-dismiss="modal" aria-hidden="true">&times;</button>
        <h3>Отправка СМС</h3>
      </div>
      <div id="sms-send-form" class="modal-body"/>
      <div class="modal-footer">
        <button id="do-send-sms" class="btn btn-primary">Отправить</button>
      </div>
    </div>

    <script type="text/template"
            id="search-table-template"
            class="view-template">
      <div style="text-align:center;">
      <fieldset style="width:50%; margin-left:25%;">
        <legend>Поиск</legend>
        <form onsubmit="gotoCase(); return false;">
          <div data-date-format="dd.mm.yyyy"
               id="search-datepicker"
               data-provide="datepicker"
               data-date-weekstart="1"
               class="input-append date">
            <input type="text"
                   style="width: 90%;"
                   id="table-query"
                   size="16" class="span2" id="acpro_inp2">
            <span class="add-on"><i class="icon-calendar"></i></span>
          </div>
          <button class="btn btn-success" type="submit">
            Поиск
          </button>
        </form>
      </fieldset>
      </div>
      <table id="searchtable" class="table table-striped table-bordered">
        <thead>
          <tr>
            <th>ID</th>
            <th>ФИО</th>
            <th>Дата звонка</th>
            <th>Телефон</th>
            <th>Номер машины</th>
            <th>Программа</th>
          </tr>
        </thead>
        <tbody/>
      </table>
    </script>

    <script type="text/template"
            id="vin-form-template"
            class="view-template">
      <div style="text-align:center;">
      <fieldset>
        <legend>Импорт VIN</legend>
        <form id="vin-import-form" onsubmit="doVin(); return false;">
          <p>
            <select name="program" id="vin-program-select" data-bind="foreach: $data">
              <option data-bind="value: id, text: name" />
            </select>
            <input type="file"
                   name="file"
                   accept="text/csv|application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" />
          </p>
          <button class="btn btn-success" type="submit">
            Отправить
          </button>
        </form>
      </fieldset>
      </div>
      <div id="vin-alert-container" />
    </script>

    <script type="text/template"
            id="vin-alert-template">
      <!-- TODO Should be row-fluid when fluid containers are
                fixed in Bootstrap upstream. -->
      <div class="container">
        <div class="row">
          <div class="span6 offset3">
            {{# alerts}}
              <div class="alert alert-{{alertType}}" style="margin-bottom: 2px;">
                <button class="close"
			data-dismiss="alert"
			onclick="removeVinAlert('{{alertId}}'); return false;">×</button>
                {{alertVinFile}}: {{ alertMessage }}
                {{# alertErrorFile }}
                  <a href="{{alertErrorFile}}">Файл</a> с необработанными записями.
                {{/ alertErrorFile }}
                {{# alertErrorLogFile }}
                  <a href="{{alertErrorLogFile}}">Файл</a> с описанием ошибок.
                {{/ alertErrorLogFile }}
              </div>
            {{/ alerts}}
          </div>
        </div>
      </div>
    </script>

    <!--
         Form field templates.

         Field template must have id in form of <type>-field-template,
         where <type> is field type to be rendered using this
         template, or <name>-<type>-field-template, where <name> is
         the name of field of given type which will be rendered with
         this template. Client code must prefer named templates to
         only-typed ones.

      -->

    <script type="text/template"
            class="field-template"
            id="textarea-field-template">
      <div class="control-group"
           {{# meta.required }}data-bind="css: { error: {{name}}Not }"{{/ meta.required}}
           {{# meta.regexp }}data-bind="css: { warning: {{name}}Regexp }"{{/ meta.regexp}}
           >
        <div class="control-label">
          <label>{{ meta.label }}
            {{# meta.infoText1 }}
              <i class="icon icon-question-sign"
                 data-provide="popover"
                 data-content="{{ meta.infoText1 }}" />
            {{/ meta.infoText1 }}
          </label>
        </div>
        <div class="controls">
          <textarea class="pane-span focusable"
                    name="{{ name }}"
                    {{# readonly }}disabled{{/ readonly }}
                    rows="7"
                    data-bind="value: {{ name }},
                               valueUpdate: 'afterkeydown'" />
        </div>
      </div>
    </script>

    <script type="text/template"
            class="field-template"
            id="statictext-field-template">
      <div class="control-group">
          <span data-bind="text: {{ name }}" />
      </div>
    </script>

    <script type="text/template"
            class="field-template"
            id="text-field-template">
      <div class="control-group"
           {{# meta.required }}data-bind="css: { error: {{name}}Not }"{{/ meta.required}}
           {{# meta.regexp }}data-bind="css: { warning: {{name}}Regexp }"{{/ meta.regexp}}
           >
        <div class="control-label">
          <label>{{ meta.label }}
            {{# meta.infoText1 }}
              <i class="icon icon-question-sign"
                 data-provide="popover"
                 data-content="{{ meta.infoText1 }}" />
            {{/ meta.infoText1 }}
          </label>
        </div>
        <div class="controls">
          <input type="text"
                 class="pane-span focusable"
                 autocomplete="off"
                 name="{{ name }}"
                 {{# meta.transform }}
                    style="text-transform:{{meta.transform}};"
                 {{/ meta.transform }}
                 {{# readonly }}readonly{{/ readonly }}
                 data-bind="value: {{ name }},
                            valueUpdate: 'afterkeydown'" />
        </div>
      </div>
    </script>

    <script type="text/template"
            class="field-template"
            id="datetime-field-template">
      <div class="control-group"
           {{# meta.required }}data-bind="css: { error: {{name}}Not }"{{/ meta.required}}
           {{# meta.regexp }}data-bind="css: { warning: {{name}}Regexp }"{{/ meta.regexp}}
           >
        <div class="control-label">
          <label>{{ meta.label }}
            {{# meta.infoText1 }}
              <i class="icon icon-question-sign"
                 data-provide="popover"
                 data-content="{{ meta.infoText1 }}" />
            {{/ meta.infoText1 }}
          </label>
        </div>
        <div class="controls">
          <input type="text"
                 class="datetime-field pane-span focusable"
                 autocomplete="off"
                 name="{{ name }}"
                 {{# readonly }}readonly{{/ readonly }}
                 data-bind="value: {{ name }}DateTime,
                            valueUpdate: 'change'" />
        </div>
      </div>
    </script>

    <!-- Like text-field-template, but with datepicker -->
    <script type="text/template"
            class="field-template"
            id="date-field-template">
      <div class="control-group"
           {{# meta.required }}data-bind="css: { error: {{name}}Not }"{{/ meta.required}}
           {{# meta.regexp }}data-bind="css: { warning: {{name}}Regexp }"{{/ meta.regexp}}
           >
        <div class="control-label">
          <label>{{ meta.label }}
            {{# meta.infoText1 }}
              <i class="icon icon-question-sign"
                 data-provide="popover"
                 data-content="{{ meta.infoText1 }}" />
            {{/ meta.infoText1 }}
          </label>
        </div>
        <div class="controls">
          <div class="input-append date"
               {{^ readonly }}
               data-provide="datepicker"
               data-autoshow-datepicker="true"
               data-date-format="dd.mm.yyyy"
               data-date-weekstart="1"
               {{/ readonly }}>
            <input type="text"
                   class="pane-span focusable"
                   autocomplete="off"
                   name="{{ name }}"
                   {{# readonly }}readonly{{/ readonly }}
                   data-bind="value: {{ name }},
                              valueUpdate: 'afterkeydown'" />
            <span class="add-on"><i class="icon icon-calendar" /></span>
          </div>
        </div>
      </div>
    </script>

    <!-- Like text-field-template but with call button -->
    <!-- FIXME: this template differs from the picker-field-template
         only in icon class. Seems that it is reasonable to parametrize it.
    -->
    <script type="text/template"
            class="field-template"
            id="phone-field-template">
      <div class="control-group"
           {{# meta.required }}data-bind="css: { error: {{name}}Not }"{{/ meta.required}}
           {{# meta.regexp }}data-bind="css: { warning: {{name}}Regexp }"{{/ meta.regexp}}
           >
        <div class="control-label">
          <label>{{ meta.label }}</label>
        </div>
        <div class="controls">
          <div class="input-append">
            <input type="text"
                   class="pane-span focusable"
                   autocomplete="off"
                   name="{{ name }}"
                   data-bind="value: {{ name }},
                              valueUpdate: 'afterkeydown'"
                   onkeyDown="kdoPick('{{ meta.picker }}',
                                      '{{ name }}',
                                      73, event);"
                   />
            <span class="add-on">
              <i class="icon stolen-icon-phone"
                 onclick="doPick('{{ meta.picker }}', '{{ name }}');"/>
            </span>
          </div>
        </div>
      </div>
    </script>


    <script type="text/template"
            class="field-template"
            id="dictionary-field-template">
      <div class="control-group"
           {{# meta.required }}
             data-bind="css: { error: {{name}}Not }"
           {{/ meta.required }}
           {{# meta.regexp   }}
             data-bind="css: { warning: {{name}}Regexp }"
           {{/ meta.regexp   }}
           >
        <div class="control-label">
          <label>{{ meta.label }}
            {{# meta.infoText1 }}
              <i class="icon icon-question-sign"
                 data-provide="popover"
                 data-content="{{ meta.infoText1 }}" />
            {{/ meta.infoText1 }}
          </label>
        </div>
        <div class="controls">
          <div class="input-append">
            <!--

            Note the difference between readonly attribute and
            disabled class from Bootstrap.

            -->

            <input type="text"
                   class="pane-span
                          focusable
                          {{# meta.addClass }}{{meta.addClass}}{{/ meta.addClass }}
                          {{# readonly }}disabled{{/ readonly }}"
                   {{# readonly }}readonly{{/ readonly }}
                   autocomplete="off"
                   name="{{ name }}"
                   data-source="global.dictionaries['{{meta.dictionaryName}}']"
                   data-bind=" value: {{ name }}Local,
                              valueUpdate: 'change'
                              {{# meta.dictionaryParent }},
                              attr: { 'data-parent': {{ meta.dictionaryParent }} }
                              {{/ meta.dictionaryParent }}"
                   {{^readonly}}
                   data-provide="typeahead"
                   {{/readonly}}
                   />
            <span class="add-on">
              <i class="icon icon-chevron-down"
                {{^readonly}}data-provide="typeahead-toggle"{{/readonly}}
              />
            </span>
          </div>
          {{# meta.targetCategory }}
          <ul data-depends="{{ name }}"
              data-source="{{ meta.targetCategory }}"
              data-provide="checklist" />
          {{/ meta.targetCategory }}
        </div>
      </div>
    </script>

    <!-- like usual dic, but allow sort of multiple selection -->
    <script type="text/template"
            class="field-template"
            id="dictionary-many-field-template">
      <div class="control-group"
           {{# meta.required }}
             data-bind="css: { error: {{name}}Not }"
           {{/ meta.required }}
           {{# meta.regexp   }}
             data-bind="css: { warning: {{name}}Regexp }"
           {{/ meta.regexp   }}
           >
        <div class="control-label">
          <label>{{ meta.label }}
            {{# meta.infoText1 }}
              <i class="icon icon-question-sign"
                 data-provide="popover"
                 data-content="{{ meta.infoText1 }}" />
            {{/ meta.infoText1 }}
          </label>
        </div>
        <div class="controls">
          <div class="input-append">
            <!--

            Note the difference between readonly attribute and
            disabled class from Bootstrap.

            -->

            <input type="text"
                   class="pane-span
                          focusable
                          {{# meta.addClass }}{{meta.addClass}}{{/ meta.addClass }}
                          {{# readonly }}disabled{{/ readonly }}"
                   {{# readonly }}readonly{{/ readonly }}
                   autocomplete="off"
                   name="{{ name }}"
                   data-source="global.dictionaries['{{meta.dictionaryName}}']"
                   data-bind="value: {{ name }}Many,
                              valueUpdate: 'change'
                              {{# meta.dictionaryParent }},
                              attr: { 'data-parent': {{ meta.dictionaryParent }} }
                              {{/ meta.dictionaryParent }}"
                   {{^readonly}}
                   data-provide="typeahead"
                   {{/readonly}}
                   />
            <span class="add-on">
              <i class="icon icon-chevron-down"
                {{^readonly}}data-provide="typeahead-toggle"{{/readonly}}
              />
            </span>
          </div>
          <!-- ko if: {{ name }}Locals().length -->
          <ul data-bind="foreach: {{ name }}Locals">
            <li>
              <span data-bind="text: $data.label" />
              <a href="" data-bind="click: $parent.{{ name }}Remove" >
                x
              </a>
            </li>
          </ul>
          <!-- /ko -->
          {{# meta.targetCategory }}
          <ul data-depends="{{ name }}"
              data-source="{{ meta.targetCategory }}"
              data-provide="checklist" />
          {{/ meta.targetCategory }}
        </div>
      </div>
    </script>

    <!-- Picker which fills fields with stored data -->
    <script type="text/template"
            class="field-template"
            id="picker-field-template">
      <div class="control-group"
           {{# meta.required }}data-bind="css: { error: {{name}}Not }"{{/ meta.required}}
           {{# meta.regexp }}data-bind="css: { warning: {{name}}Regexp }"{{/ meta.regexp}}
           >
        <div class="control-label">
          <label>{{ meta.label }}
            {{# meta.infoText1 }}
              <i class="icon icon-question-sign"
                 data-provide="popover"
                 data-content="{{ meta.infoText1 }}" />
            {{/ meta.infoText1 }}
          </label>
        </div>
        <div class="controls">
          <div class="input-append">
            <input type="text"
                   class="pane-span focusable {{# readonly }}disabled{{/ readonly }}"
                   autocomplete="off"
                   {{# readonly }}readonly{{/ readonly }}
                   {{# meta.transform }}
                      style="text-transform:{{meta.transform}};"
                   {{/ meta.transform }}
                   name="{{ name }}"
                   data-bind="value: {{ name }},
                              valueUpdate: 'afterkeydown'"
                   onkeyDown="kdoPick('{{ meta.picker }}',
                                      '{{ name }}',
                                      66, event);"
                   />
            <span class="add-on"><i class="icon icon-search"
               onclick="doPick('{{ meta.picker }}', '{{ name }}', event.target);"/>
            </span>
          </div>
        </div>
      </div>
    </script>

    <!-- radio widget for flat dictionary fields -->
    <script type="text/template"
            class="field-template"
            id="radio-field-template">
      <div class="control-group">
        <div class="control-label">
          <label>{{ meta.label }}
            {{# meta.infoText1 }}
              <i class="icon icon-question-sign"
                 data-provide="popover"
                 data-content="{{ meta.infoText1 }}" />
            {{/ meta.infoText1 }}
          </label>
        </div>
        <div class="controls">
          {{# dictionary.entries }}
            <label class="radio">
              <!-- Mustache.js contexts support bubbling -->
              <input type="radio"
                     name="{{ name }}"
                     value="{{ value }}"
                     data-bind="checked: {{ name }}"></input>
              {{ label }}
            </label>
          {{/ dictionary.entries }}
        </div>
      </div>
    </script>

    <!-- May be used for plain rendering of flat dictionaries as well -->
    <script type="text/template"
            class="field-template"
            id="select-field-template">
      <div class="control-group">
        <div class="control-label">
          <label>{{ meta.label }}
            {{# meta.infoText1 }}
              <i class="icon icon-question-sign"
                 data-provide="popover"
                 data-content="{{ meta.infoText1 }}" />
            {{/ meta.infoText1 }}
          </label>
        </div>
        <div class="controls">
          <select name="{{ name }}"
                  {{# readonly }}disabled{{/ readonly }}
                  data-bind="value: {{ name }},
                             valueUpdate: 'change'">
            {{# dictionary.entries }}
            <option value="{{value}}">{{meta.label}}</option>
            {{/ dictionary.entries }}
          </select>
        </div>
      </div>
    </script>

    <script type="text/template"
            class="field-template"
            id="checkbox-field-template">
      <div class="control-group">
        <div class="controls">
          <label class="checkbox inline">
            <input type="checkbox"
                   name="{{ name }}"
                   {{# readonly }}disabled{{/ readonly }}
                   data-bind="checked: {{ name }},
                              valueUpdate: 'change'" />
          {{ meta.label }}
          {{# meta.infoText1 }}
            <i class="icon icon-question-sign"
               data-provide="popover"
               data-content="{{ meta.infoText1 }}" />
          {{/ meta.infoText1 }}
          </label>
        </div>
      </div>
    </script>


    <script type="text/template"
            class="field-template"
            id="map-field-template">
      <div class="control-group">
        <div class="controls">
          <div style="height:600px;" id="{{ viewName }}-{{ name }}"
               name="{{ name }}"
               class="osMap"></div>
        </div>
      </div>
    </script>

    <script type="text/template"
            class="field-template"
            id="table-field-template">
      <div class="control-group">
        <div class="controls">
          <table id="{{ name }}" class="dataTable table table-striped table-bordered">
            <thead>
              <tr>
                {{# meta.columns }}<th>{{ label }}</th>{{/ meta.columns }}
              </tr>
            </thead>
            <tbody/>
          </table>
        </div>
      </div>
    </script>

    <script type="text/template" class="field-template"
            id="partnerTable-field-template">
      <table id="{{name}}" class="table table-striped table-bordered">
        <thead>
          <tr>
            <th>Название</th>
            <th>Город</th>
            <th>Адрес</th>
            <th>Телефоны</th>
            <th>Время работы</th>
            <th> ПН </th>
            <th> ПБГ </th>
            <th> ПБЗ </th>
          </tr>
        </thead>
        <tbody/>
      </table>
    </script>

    <!-- NOP here — references are rendered after model has loaded -->
    <script type="text/template"
            class="field-template"
            id="reference-field-template">
      <div class="controls">
        <span class="accordion"
              id="{{ modelName }}-{{ cid }}-{{ name }}-references" />
      </div>
    </script>

    <!--

         Special template used to render first field of group in
         parent view.
    -->
    <script type="text/template"
            class="field-template"
            id="group-field-template">
      <div class="control-group">
        <div class="control-label">
          <label>{{ meta.label }}
            {{# meta.infoText1 }}
              <i class="icon icon-question-sign"
                 data-provide="popover"
                 data-content="{{ meta.infoText1 }}" />
            {{/ meta.infoText1 }}
          </label>
        </div>
        <div class="controls">
          <div class="input-append">
            <input type="text"
                   name= {{ name }}
                   class="pane-span"
                   autocomplete="off"
                   {{# meta.transform }}
                      style="text-transform:{{meta.transform}};"
                   {{/ meta.transform }}
                   onfocus="showComplex('{{ viewName }}', '{{ name }}');"
                   {{# readonly }}readonly{{/ readonly }}
                   data-bind="value: {{ name }},
                              valueUpdate: 'afterkeydown'" />
            <span class="add-on">
              <i onclick="showComplex('{{ viewName }}', '{{ name }}');"
                 class="icon icon-share" />
            </span>
          </div>
        </div>
      </div>
    </script>

    <!--
         Template for one of references.

         Must generate id="{{ refView }}" element which
         will hold contents of referenced model. Its class must be is
         {{ refClass }}.

         "{{ refView }}-perms" will be used for instance permissions.

         May setup on-demand loading function.
    -->
    <script type="text/template"
            class="reference-template"
            id="services-reference-template">
      <div class="accordion-group">
        <div class="accordion-heading">
          <div class="accordion-toggle"
               data-target="#{{ refView }}-head"
               data-toggle="collapse"
               id="{{ refView }}-link">
            <a class="icon icon-remove" />
            <a
               data-bind="text: modelTitle">
               Услуга…
            </a>
            <a data-bind="attr: { href: '#printSrv/'+modelName()+'/'+id() }"
               target="_blank">
              [Печать]
          </a>

          </div>
        </div>

        <div id="{{ refView }}-head"
             class="accordion-body collapse in">
          <div class="accordion-inner {{ refClass }}"
               id="{{ refView }}">
            <!-- Instance contents are rendered here -->

          </div>
        </div>
      </div>
    </script>

    <script type="text/template"
            class="reference-template"
            id="actions-reference-template">
      <div class="accordion-group">
        <div class="accordion-heading"
             id="{{ refView }}-link">
          <a class="accordion-toggle"
             data-bind="text: actionNameLocal"
             data-target="#{{ refView }}-head"
             data-toggle="collapse">Действие…</a>
        </div>

        <div id="{{ refView }}-head"
             class="accordion-body collapse {{^refId}}in{{/refId}}">
          <div class="accordion-inner {{ refClass }}"
               id="{{ refView }}">
            <!-- Instance contents are rendered here -->
          </div>
        </div>
      </div>
    </script>

    <script type="text/template"
            class="reference-template"
            id="tarifOptions-reference-template">
      <div class="accordion-group">
        <div class="accordion-heading">
          <div class="accordion-toggle"
               data-target="#{{ refView }}-head"
               data-toggle="collapse">
            <a class="icon icon-remove" />
            <a id="{{ refView }}-link"
               data-bind="text: nameOrDef">
               Тарифная опция…
            </a>
          </div>
        </div>

        <div id="{{ refView }}-head"
             class="accordion-body collapse {{^refId}}in{{/refId}}">
          <div class="accordion-inner {{ refClass }}"
               id="{{ refView }}">
            <!-- Instance contents are rendered here -->

          </div>
        </div>
      </div>
    </script>

    <script type="text/template"
            class="reference-template"
            id="cost_serviceTarifOptions-reference-template">
      <div class="accordion-group">
        <div class="accordion-heading">
          <div class="accordion-toggle"
               data-target="#{{ refView }}-head"
               data-toggle="collapse">
            <a class="icon icon-remove" />
            <a id="{{ refView }}-link"
               data-bind="text: nameOrDef">
               Тарифная опция…
            </a>
          </div>
        </div>

        <div id="{{ refView }}-head"
             class="accordion-body collapse {{^refId}}in{{/refId}}">
          <div class="accordion-inner {{ refClass }}"
               id="{{ refView }}">
            <!-- Instance contents are rendered here -->

          </div>
        </div>
      </div>
    </script>

    <!-- Group view container -->
    <script type="text/template"
            class="group-template"
            id="-group-template">
      <fieldset class="complex-field"
                id="{{ refView }}"
                style="display: none;">
          <i class="icon icon-remove complex-field-close"
             onclick="hideComplex()"/>
          <form class="content form-vertical"/>
      </fieldset>
    </script>

    <!-- Template for fields with unknown type -->
    <script type="text/template"
            class="field-template"
            id="unknown-field-template">
      <div class="control-group">
        <div class="controls">
          <span class="label label-important">
            (Ошибка — поле {{ name }} неизвестного типа)
          </span>
        </div>
      </div>
    </script>

    <!-- Form controls wrt user permissions -->
    <script type="text/template"
            id="permission-template">
        {{# readonly }}
        <button class="btn disabled" type="button">
          <i class="icon-ban-circle" /> Только для чтения</button>
        {{/ readonly }}
        {{^ readonly }}
        <button class="btn btn-success" type="button"
                onClick="saveInstance('{{ viewName }}');successfulSave.call(this);">
          <i class="icon-pencil icon-white" /> Сохранить</button>
          <span class="save-result"/>
        {{/ readonly }}
    </script>

    <!-- List of empty required fields -->
    <script type="text/template"
            id="empty-fields-template">
      <ul id="empty-fields">
      {{# fields }}
      <li onclick="focusField('{{name}}'); return false;"
          data-bind="css: { lierror: {{name}}Not }, visible: {{name}}Not">{{meta.label}}</li>
      {{/ fields }}
      </ul>
    </script>

    <!-- Render service picker with services dictionary -->
    <script type="text/template"
            id="service-picker-template">
      <ul class="nav nav-pills">
        <li class="dropdown">
          <button class="dropdown-toggle btn btn-action"
                  type="button"
                  data-toggle="dropdown">
            <i class="icon icon-plus" />Добавить услугу
          </button>
          <ul class="dropdown-menu">
            {{# dictionary.entries }}
            <li>
              <a href="#"
                 onclick="addService('{{value}}'); return false;">
                <i class="icon-{{icon}} icon-black" />
                {{ label }}
              </a>
            </li>
            {{/ dictionary.entries }}
          </ul>
        </li>
      </ul>
    </script>

    <script type="text/template"
            id="check-list-item-template">
      <li><input type="checkbox" /> {{ label }}</li>
    </script>

    <!-- Fallback template for pickTemplate failures -->
    <script type="text/template"
            id="unknown-template">
      <span class="label label-important">
        Не удалось найти ни один из шаблонов:
        {{#names}}{{.}}&nbsp;{{/names}}
      </span>
    </script>

    <!-- File upload template -->
    <script type="text/template"
            class="field-template"
            id="files-field-template">
      <label>{{ meta.label }}</label>
      <form data-bind="attr: { action: {{name}}UploadUrl }, setdata: {{name}}"
            method="post"
            enctype="multipart/form-data">
        <input type="file" name="files" />
        <input type="button" value="Загрузить" onClick="uploadFile(this)" />
      </form>
    </script>

    <script type="text/template"
            id="add-ref-button-template">
      <button class="dropdown-toggle btn btn-action"
              onclick="{{ fn }}"
              type="button">
        <i class="icon icon-plus"></i>{{ label }}
      </button>
    </script>


    <!-- Default case group view template -->
    <script type="text/template"
            class="group-template"
            id="default-case-group-template">
      <fieldset class="complex-field default-complex-field"
                id="default-case">
        <p>
          <b>Кто звонил:</b>
          <span data-bind="text: contact_name"/>&nbsp;
          <span data-bind="text: contact_phone1"/>
        </p>
        <p data-bind="visible: car_make">
          <b>Машина:</b>
          <span data-bind="text: car_makeLocal"/>&nbsp;
          <span data-bind="text: car_modelLocal"/>&nbsp;
          <span data-bind="text: car_plateNum"/>
        </p>
        <p data-bind="visible: caseAddress_address">
          <b>Адрес кейса:</b>
          <span data-bind="text: caseAddress_address"/><br/>
          <span data-bind="text: caseAddress_comment"/>
        </p>
        <div class="program">
          <h1 data-bind="text: programLocal"></h1>
          <div data-bind="html:programDesc"></div>
          <br />
          <br />
          <div data-bind="foreach: servicesDescs">
            <dt data-bind="text: title"></dt>
            <dd data-bind="html: description"></dd>
          </div>
          <br />
          <h4 data-bind="visible: filesInfo">Загруженные файлы:</h4>

          <ul class="unstyled"
              data-bind="foreach: filesInfo, setdata: files">
            <li>
              <a data-bind="attr: { href: url }, text: name" />
              <i class="icon icon-remove"
                 data-bind="setdata: name"
                 onclick="deleteFile(this)"/>
            </li>
          </ul>

          <dl data-bind="foreach: servicesReference">
            <dt data-bind="text: modelTitle">
              <dd>
                <ul class="unstyled"
                    data-bind="foreach: filesInfo, setdata: files">
                  <li>
                    <a data-bind="attr: { href: url }, text: name" />
                    <i class="icon icon-remove"
                       data-bind="setdata: name"
                       onclick="deleteFile(this)"/>
                  </li>
                </ul>
              </dd>
            </dt>
          </dl>

          <h4>История звонков</h4>
          <table id="call-searchtable" class="table table-striped table-bordered">
            <thead>
              <tr>
                <th>Дата</th>
                <th>Сотрудник РАМК</th>
                <th>Тип коммуникации</th>
                <th>Комментарий</th>
                <th>Результат действия</th>
              </tr>
            </thead>
            <tbody/>
          </table>

        </div>
        <div id="case-comments">
          <label> Комментарий </label>
          <textarea  id="case-comments-i" rows="7"  />
          <br />
          <button    id="case-comments-b" class="btn">
            Добавить комментарий
          </button>
        </div>
      </fieldset>
    </script>


    <!-- navigation menu templates -->
    <script type="text/html" id="nav-li-template">
      <!-- ko foreach: screens -->
        <!-- ko if: type == 'li' -->
          <li data-bind="if: type == 'li',
                         attr: { id: name + '-screen-nav' }">
            <a data-bind="attr: { href: '#' + name}, text: label"/>
          </li>
        <!-- /ko -->
        <!-- ko if: type == 'dropdown' -->
          <li class="dropdown"
              data-bind="if: type == 'dropdown'">
            <a href="#"
               class="dropdown-toggle"
               data-toggle="dropdown"
               data-bind="html: label + '<b class=\'caret\'></b>'">
              <b class="caret"></b>
            </a>
            <ul class="dropdown-menu"
                data-bind="template: { name: 'nav-li-template' }">
            </ul>
          </li>

        <!-- /ko -->
      <!-- /ko -->
    </script>

    <script type="text/html" id="tarif-opts-template">
      <div class="add-opt-btn" id="{{ modelName }}-{{ cid }}-tarif-select">
        <input type="button" class="btn" value="Добавить">
      </div>
    </script>

    <script type="text/html" id="tarif-opt-sel-template">
      <div class="add-opt-btn">
        <input type="button" class="btn add" value="Добавить">
        <select>
          {{# opts }}
          <option value="{{ id }}"> {{ optionName }} </option>
          {{/ opts }}
        </select>
        <input type="button" class="btn reload" value="Обновить стоимость" />
      </div>
    </script>

    <script type="text/template" id="partner-popup-template">
      <div><strong>{{ name }}</strong></div>
      <div>{{ addrDeFacto }}</div>
      <div>{{ phone1 }}</div>
      <div>{{ workingTime }}</div>
      <div><a class="btn btn-mini btn-primary"
              onclick="pickPartnerBlip('{{ parentView }}', '{{ mapId }}', '{{ id }}', '{{ name }}', '{{ addrDeFacto }}', '{{ coords }}', '{{ partnerIdField }}', '{{ partnerField }}', '{{ partnerAddrField }}', '{{ partnerCoordsField }}');">Выбрать</a></div>
    </script>

  </body>
</html>
