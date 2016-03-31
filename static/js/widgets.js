$(document).ready(function() {
  $('.button-widget form').submit(function(e) {
    e.preventDefault();
    var action = $(this).attr('action');
    $.ajax({
      url: action,
        type: "post"
    });
  });
});
