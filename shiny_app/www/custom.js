
// used to change the number of rows that can be viewed in the hours table
$(document).on("click", ".num_rows", function(event) {
  event.preventDefault()
  Shiny.setInputValue("num_rows", this.id)
})
