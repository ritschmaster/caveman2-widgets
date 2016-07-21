$(document).ready(function() {
  var restBaseUrl = "/rest/";
  var restJavaScriptCheckerUrl = "javascript-checker";
  var restTableWidgetUrl = "table-widget";
  var dirtyObjectsUrl = "/widgets/dirty";

  /**
   * This function must be called each time HTML code is changed
   * within the document. Otherwise the buttons, links etc. won't do
   * what they are supposed to do.
   *
   * @param context The newly added HTML. Default the context is the
   * entire context. Only use 'document' once - when the page is
   * loaded initially.
   */
  function registerOperationsOnTags(context=document) {
    $('form', context).submit(function(e) {
      e.preventDefault();

      var parent = $(this).parent();

      if (parent.attr("class").indexOf("button-widget")) {
        var action = $(this).attr('action');
        $.ajax({
          url: action,
          type: "post"
        }).done(function(data) {
          processDirty();
        });
      }
    });

    $('.navigation-widget-links li a', context).click(function(e) {
      e.preventDefault();
      var URL = $(this).attr('href');
      $.ajax({
        url: URL,
        type: "POST",
        error: function(jqXHR, status, errorMsg) {
        },
        success: function(data, status, jqXHR) {
          var stateObj = { };
          var title = data;
          var url = data;
          history.pushState(stateObj, title, url);
          processDirty();
        }
      });
    });

    $('.link-widget a', context).click(function(e) {
      e.preventDefault();
      var URL = $(this).attr('href');
      $.ajax({
        url: URL,
        type: "POST",
        error: function(jqXHR, status, errorMsg) {
        },
        success: function(data, status, jqXHR) {
          var stateObj = { };
          var title = data;
          var url = data;
          if (("/" + url) == window.location.pathname) {

          } else {
            window.location.href = url;
          }
          processDirty();
        }
      });
    });
  }

  registerOperationsOnTags();

  $.ajax({
    url: restBaseUrl + restJavaScriptCheckerUrl,
    dataType: "json",
    type: "post",
    data: {
      available: "true"
    },
    error: function(jqXHR, status, errorMsg) {
    },
    success: function(data, status, jqXHR) {
    }
  });

  var processDirty = function() {
    $.ajax({
      url: dirtyObjectsUrl,
      dataType: "json",
      type: "GET",
      error: function(jqXHR, status, errorMsg) {
      },
      success: function(data, status, jqXHR) {
        var dirtyObjectIds = data.dirtyObjects;
        dirtyObjectIds.forEach(function(dirtyObjectId) {
          var dirtyObjectIdTag = '#' + dirtyObjectId;
          var dirtyHtml = $(dirtyObjectIdTag);
          var className = dirtyHtml.attr("class").trim();
          console.log(dirtyHtml.attr("class"));
          var classNames = className.split(' ');
          className = classNames[classNames.length - 1];
          console.log(className);

          var dirtyUrl = restBaseUrl + className;
          $.ajax({
            url: dirtyUrl,
            type: "get",
            data: {
              "id": dirtyObjectId
            },
            error: function(jqXHR, status, errorMsg) {
            },
            success: function(dirtyData, dirtyStatus, dirtyJqXHR) {
              var parsedHtml = $.parseHTML(dirtyData, document, true);
              if (dirtyData.indexOf(dirtyObjectId)) {
                parsedHtml = $(parsedHtml).children();
              }
              dirtyHtml.empty();
              dirtyHtml.append(parsedHtml);
              registerOperationsOnTags(parsedHtml);
            }
          });
        });
      }
    });
  };

  setInterval(function() {
    processDirty();
  }, 10000);

  var tableWidgetsUpdating = [];
  $(window).scroll(function () {
    $('.limited-table-widget').each(function() {
      var tableId = $(this).attr('id');
      if (tableWidgetsUpdating[tableId] != true) {
        var currentTableLength = $('#' + tableId + ' tr').length;
        if ($('#' + tableId + ' th').length)
          currentTableLength--;

        $.ajax({
          url: restBaseUrl + restTableWidgetUrl,
          dataType: "html",
          type: "post",
          data: {
            id: tableId,
            "length_p": "true"
          },
          error: function(jqXHR, status, errorMsg) {
            tableWidgetsUpdating[tableId] = false;
          },
          success: function(data, status, jqXHR) {
            if (currentTableLength < data) {
              var loadAmount = 10;
              if (currentTableLength + loadAmount > data)
                loadAmount = data - currentTableLength;
              $.ajax({
                url: restBaseUrl + restTableWidgetUrl,
                dataType: "html",
                type: "post",
                data: {
                  id: tableId,
                  already: currentTableLength,
                  amount: loadAmount
                },
                error: function(jqXHR, status, errorMsg) {
                  tableWidgetsUpdating[tableId] = false;
                },
                success: function(data, status, jqXHR) {
                  tableWidgetsUpdating[tableId] = false;
                }
              });
            }
          }
        });
      }
    });
  });
});
