// allows us to replace hamburger menu context button with a different button
// in our case we are using a download button
Highcharts.SVGRenderer.prototype.symbols.download = function (x, y, w, h) {
  var path = [
    // Arrow stem
    'M', x + w * 0.5, y,
    'L', x + w * 0.5, y + h * 0.7,
    // Arrow head
    'M', x + w * 0.3, y + h * 0.5,
    'L', x + w * 0.5, y + h * 0.7,
    'L', x + w * 0.7, y + h * 0.5,
    // Box
    'M', x, y + h * 0.9,
    'L', x, y + h,
    'L', x + w, y + h,
    'L', x + w, y + h * 0.9
  ];
  return path;
};

var num_row_choices = [
  "10", "25", "50", "100", "All"
]
var have_cells_changed = false;
Shiny.addCustomMessageHandler(
  "have_cells_changed",
  function(message) {
    have_cells_changed = message
  }
)

$(document).on("click", ".num_rows", function(event) {
  event.preventDefault()
  if (have_cells_changed === true) {
   swal("Changes Detected", "Please save or discard changes before changing the number of rows displayed");
  } else {
    num_row_choices.forEach(function(choice) {
      $("#" + choice).removeClass("bold")
    })

    $("#" + this.id).addClass("bold")

    Shiny.setInputValue("num_rows", this.id)
  }

})