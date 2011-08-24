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
      $('#node_'+nid+'_tree').remove();
    }
  });
}

function n_run(nid,val) {
  $.ajax({
    type: "POST",
    url: "/n_run/" + nid + "/" + val,
    success: function() { }
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
  focusNodeWith(
    function(e) {return e.last();},
    function(e) {return e.prev();}
  );
}

function focusNodeDown(evt) {
  focusNodeWith(
    function(e) {return e.first();},
    function(e) {return e.next();}
  );
}

function focusNodeWith(f1, f2) {
  var current = $(".synth_node.active");
  var target;
  if (current.length <= 0) {
    target = $(".synth_node").first();
  } else {
    target = f2(current);
    if (target.length <= 0) {
      target = f1(getGroupWith(f2, current.parent().parent()).find(".synth_node"));
    }
  }
  if (target.length > 0) {
    nid = target[0].id.replace(/[^-0-9]+/,"").replace(/[^-0-9]+/,"");
    node_detail(nid);
  }
}

function getGroupWith(func,elem) {
 if (elem.length <= 0) {
    return $([]);
  } else {
    _g = func(elem);
    _n = _g.find(".synth_node").first();
    if (_n.length <= 0) {
      return getGroupWith(func,_g);
    } else {
      return _g;
    }
  }
}

function focusFirstInput(evt) {
  $("input").first().focus();
}

function freeCurrentNode(evt) {
  nid = $(".synth_node.active")[0].id
    .replace(/[^-0-9]+/,"").replace(/[^-0-9]+/,"");
  n_free(nid);
  focusNodeUp();
}

function init() {
  $(document).ready(function() {
    $(document).bind('keypress', 'k', focusNodeUp);
    $(document).bind('keypress', 'j', focusNodeDown);
    $(document).bind('keypress', 'i', focusFirstInput);
    $(document).bind('keypress', 'f', freeCurrentNode);
  });
}
