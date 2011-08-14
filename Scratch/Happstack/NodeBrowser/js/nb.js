function echo(val) {
  $.ajax({
    type: "POST",
    url:  "/echo?val=" + val
  });
}

function n_free(nid) {
  $.ajax({
    type: "POST",
    url:  "/n_free/" + nid
  });
  $('#node_'+nid).remove();
}

function n_set(me) {
  var nid = $(me).parent().parent().parent().parent()[0].id
    .replace(/[a-zA-Z_]+/, "");
  var key = $(me)[0].name;
  var val = $(me)[0].value;
  $.ajax({
    type: "POST",
    url:  "/n_set/" + nid + "?" + key + "=" + val
  });
}