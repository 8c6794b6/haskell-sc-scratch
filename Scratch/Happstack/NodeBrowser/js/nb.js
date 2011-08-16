function echo(val) {
  $.ajax({
    type: "POST",
    url: "/echo?val=" + val
  });
}

function n_free(nid) {
  $.ajax({
    type: "POST",
    url: "/n_free/" + nid,
    success: function() {
      $('#node_'+nid).remove();
    }
  });
}

function n_set(me) {
  var nid = $(me).parent().parent().parent().parent()[0].id
    .replace(/[a-zA-Z_]+/, "");
  var key = $(me)[0].name;
  var val = $(me)[0].value;
  $.ajax({
    type: "POST",
    url: "/n_set/" + nid + "?" + key + "=" + val
  });
}

function param_changed(me) {
  var nid = $(me).parent().parent().parent().parent().parent()[0].id
    .replace(/[a-zA-Z_]+/, "");
  var val = $(me)[0].value;
  var key = $(me)[0].name;
  var act = "";
  if (val[0] == "c") { act = "/n_map/"; }
  else if (val[0] == "a") { act = "/n_mapa/"; }
  else { act = "/n_set/"; }
  $.ajax({
    type: "POST",
    url: act + nid + "?" + key + "=" + val
  });
}

function node_detail(nid) {
  $.ajax({
    type: "POST",
    url: "/nodeDetail/" + nid,
    success: function(rsp) {
      $("div.right_main").html(rsp);
      $(".synth_node").removeClass("active");
      $("#node_"+nid+"_tree").addClass("active");
    }
  });
}

function focusNodeUp(evt) {
  current = $(".synth_node.active");
  if (current.length <= 0) {
    return;
  } else {
    focused = current.prev();
    if (focused.length <= 0) {
      focused = current.parent().parent().prev().find(".synth_node").last();
    }
    if (focused.length > 0) {
      node_detail(
        focused[0].id
          .replace(/[a-zA-Z_]+/, "")
          .replace(/[a-zA-Z_]+/, "")
      );
    }
  }
}

function focusNodeDown(evt) {
  current = $(".synth_node.active");
  if (current.length <= 0) {
    focused = $(".synth_node").first();
  } else {
    focused = current.next();
  }
  if (focused.length <= 0) {
    focused = current.parent().parent().next().find(".synth_node").first();
  }
  if (focused.length > 0) {
    node_detail(
      focused[0].id
        .replace(/[a-zA-Z_]+/, "")
        .replace(/[a-zA-Z_]+/, "")
    );
  }
}

function focusFirstInput(evt) {
  $("input").first().focus();
}

function init() {
  $(document).ready(function() {
    jQuery(document).bind('keypress', 'k', focusNodeUp);
    jQuery(document).bind('keypress', 'j', focusNodeDown);
    jQuery(document).bind('keypress', 'i', focusFirstInput);
  });
}
