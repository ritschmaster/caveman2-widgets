$(document).ready(function() {
  var restBaseUrl = "/rest/";
  var dirtyObjectsUrl = "/widgets/dirty";

  var processDirty = function() {
    $.ajax({
      url: dirtyObjectsUrl,
      dataType: "json",
      type: "GET",
      error: function(jqXHR, status, errorMsg) {
      },
      success: function(data, status, jqXHR) {
        var test = eval(data);
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
              var parsedHtml = $.parseHTML(dirtyData);
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

  $('.navigation-widget ul .link-widget a').click(function(e) {
    e.preventDefault();
    var URL = $(this).attr('href');
    $.ajax({
      url: URL,
      type: "POST",
      error: function(jqXHR, status, errorMsg) {
      },
      success: function(data, status, jqXHR) {
        processDirty();
      }
    });
  });
});
