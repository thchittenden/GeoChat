var socket;
var autoscroll = true;
var autoscrollPending = false;
var haveLocation = false;
var lat;
var lon;

function sendMessage() {
    var msg = $("#message").val();
    if (socket != null && msg.length > 0) {
        socket.send(msg);
        $("#message").val("");
    }
}

function recvMessage(event) {
    var msg = JSON.parse(event.data);
    var at_bottom =
    $("#log").append("<li><strong>" + msg.alias + ": </strong>" +  msg.text + "</li>");
    if (autoscroll) {
        $("#logbox").stop().scrollTop($("#log").height() - $("#logbox").height());
    }
}

function recvClose(event) {
    $("#log").append("Connection Closed!");
}

function getWsUrl(alias, lat, lon) {
    var protocol;
    if (window.location.protocol === "https:") {
        protocol = "wss:";
    } else {
        protocol = "ws:";
    }

    var loc = protocol + "//" + window.location.host + "/";
    loc += "chat/";
    loc += alias + "/";
    loc += lat + "/";
    loc += lon + "/";
    return loc;
}

function connect() {
    if ($("#aliasFormGroup").hasClass("has-success") && $("#locationFormGroup").hasClass("has-success")) {
        var alias = $("#aliasInput").val();
        socket = new WebSocket(getWsUrl(alias, lat, lon));
        socket.onmessage = recvMessage;
        socket.onclose = recvClose;
        $("#setup_modal").modal("hide");
    }
}

function locationSuccess(pos) {
    $("#locationFormGroup").addClass("has-success");
    lat = pos.coords.latitude;
    lon = pos.coords.longitude;
    var message = lat + ", " + lon;
    $("#location").html("");
    $("#location").append("<input readonly='true' type='text' class='form-control' id='locationInput' value='" + message + "' />");

    haveLocation = true;
    validateInput();
}

function locationError(err) {
    var message;
    switch(error.code) {
        case error.PERMISSION_DENIED:
            message = "Location permission denied.";
            break;
        case error.POSITION_UNAVAILABLE:
            message = "Location unavailable.";
            break;
        case error.TIMEOUT:
            message = "Location request timed out.";
            break;
        default:
            message = "Unknown error.";
            break;
    }
    $("#locationFormGroup").addClass("has-error");
    $("#location").html("");
    $("#location").append("<input readonly='true' type='text' class='form-control' id='locationInput' value='" + message + "' />");
}

function validateInput() {
    var aliasPat = /^[a-zA-Z0-9]+$/;
    var valid = true;
    if (aliasPat.test($("#aliasInput").val())) {
        $("#aliasFormGroup").removeClass("has-error");
        $("#aliasFormGroup").addClass("has-success");
    } else {
        valid = false;
        $("#aliasFormGroup").removeClass("has-success");
        $("#aliasFormGroup").addClass("has-error");
    }

    if (!haveLocation) {
        valid = false;
    }

    $("#enterButton").attr("disabled", !valid);
}

$(function() {
    $("#setup_modal").modal({
        backdrop: 'static',
        keyboard: false
    });
    $("#aliasInput").bind('input', function() {
        validateInput();
    });
    $("#scrollprompt").bind('click', function() {
        var target = $("#log").height() - $("#logbox").height();
        $("#logbox").animate({ scrollTop: target }, 'fast');
        autoscroll = true;
        autoscrollPending = true;
        $("#scrollprompt").fadeOut(250);
    });
    $("#logbox").bind('scroll', function() {
        var topScroll = $("#log").height() - $("#logbox").height();
        var curScroll = $("#logbox").scrollTop();
        if (curScroll == topScroll) {
            // User returned to bottom of screen. Start auto-scrolling.
            autoscroll = true;
            autoscrollPending = false;
            $("#scrollprompt").fadeOut(250);
        } else if (!autoscrollPending) {
            // User moved away from bottom of screen. Don't auto-scroll.
            autoscroll = false;
            $("#scrollprompt").fadeIn(250);
        }
    });
    navigator.geolocation.getCurrentPosition(locationSuccess, locationError);
    validateInput();
});
