$(document).ready(function() {
  var restBaseUrl = "/rest/";
  var restJavaScriptCheckerUrl = "javascript-checker";
  var restTableWidgetUrl = "table-widget";
  var dirtyObjectsUrl = "/widgets/dirty";

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
          var className = dirtyHtml.attr("class");
          className = className.substring(7 ,className.length);

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
            }
          });
        });
      }
    });
  };

  setInterval(function() {
    processDirty();
  }, 10000);

  $('.button-widget form').submit(function(e) {
    e.preventDefault();

    var action = $(this).attr('action');
    $.ajax({
      url: action,
      type: "post"
    }).done(function(data) {
      processDirty();
    });
  });

  $('.navigation-widget-links li a').click(function(e) {
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
