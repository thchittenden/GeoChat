var socket;
var autoscroll = true;
var autoscrollPending = false;
var haveLocation = false;
var map;
var mapBounds;
var lat;
var lon;
var users = {};

function updateBounds() {
    var newBounds = new google.maps.LatLngBounds();
    for (var alias in users) {
        newBounds.extend(users[alias].pos);
    }
    mapBounds = newBounds;
    map.fitBounds(newBounds);
}

function setUsers (newUsers) {
    users = {};
    var newBounds = new google.maps.LatLngBounds();
    for (var i = 0; i < newUsers.length; i++) {
        var user = newUsers[i];
        console.log("adding user " + JSON.stringify(user));

        // Add the new user to the map and users list.
        var pos = new google.maps.LatLng(user.lat, user.lon);
        var marker = new google.maps.Marker({
            position: pos,
            map: map,
        });
        users[user.alias] = { user: user, pos: pos, marker: marker };
        newBounds.extend(pos);
     }

     // Update the map bounds.
     mapBounds = newBounds;
     map.fitBounds(newBounds);
}

function addUser (user) {
    // Remove the user if they were already on the map.
    removeUser(user);
    console.log("adding user " + JSON.stringify(user));

    // Add the new user to the map and users list.
    var pos = new google.maps.LatLng(user.lat, user.lon);
    var marker = new google.maps.Marker({
        position: pos,
        map: map,
    });
    users[user.alias] = { user: user, pos: pos, marker: marker };

    // Update the map bounds.
    mapBounds.extend(pos);
    map.fitBounds(mapBounds);
}

function removeUser (user) {
    if (user.alias in users) {
        console.log("removing user " + JSON.stringify(user));

        // Clear the marker.
        users[user.alias].marker.setMap(null);

        // Delete from the array.
        delete users[user.alias];

        // Update the map bounds.
        updateBounds();
    }
}

function sendMessage() {
    var msg = $("#message").val();
    if (socket != null && msg.length > 0) {
        var packet = { clientMessage: msg }
        socket.send(JSON.stringify(packet));
        $("#message").val("");
    }
}

function recvMessage(event) {
    var msg = JSON.parse(event.data);
    switch (msg.tag) {
        case "ServerMessage":
            $("#log").append("<li><strong>" + msg.userAlias + ": </strong>" +  msg.message + "</li>");
            if (autoscroll) {
                $("#logbox").stop().scrollTop($("#log").height() - $("#logbox").height());
            }
            break;
        case "ServerInitUsers":
            setUsers(msg.users);
            break;
        case "ServerAddUser":
            addUser(msg.user);
            break;
        case "ServerRemoveUser":
            removeUser(msg.user);
            break;
        default:
            console.log("Unknown Message: " + event.data);
            break;
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
    // TODO: better way to determine if we're in a valid connect state!
    if ($("#aliasFormGroup").hasClass("has-success") && $("#locationFormGroup").hasClass("has-success")) {
        var alias = $("#aliasInput").val();
        socket = new WebSocket(getWsUrl(alias, lat, lon));
        socket.onmessage = recvMessage;
        socket.onclose = recvClose;
        $("#setup_modal").modal("hide");

        // Set location text.
        $("#locationText").html(lat + ", " + lon);

        // Setup initial map. This is just centered at our location. We set our maxZoom
        // to 12 to prevent the map from zooming excessively when we're the only one in
        // the chat room.
        var mapOptions = {
            mapTypeId: google.maps.MapTypeId.ROADMAP,
            disableDefaultUI: true, // No controls.
            center: new google.maps.LatLng(lat, lon),
            maxZoom: 12,
            zoom: 12,
        };
        var mapBox = document.getElementById("mapbox");
        map = new google.maps.Map(mapBox, mapOptions);
    }
}

function locationSuccess(pos) {
    $("#locationFormGroup").addClass("has-success");
    lat = pos.coords.latitude;
    lon = pos.coords.longitude;
    var message = lat + ", " + lon;
    $("#location").html("");
    $("#location").append("<input readonly='true' type='text' class='form-control' id='locationInput' value='" + message + "' />");

    // Re-validate the input so the "enter" button becomes available.
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
    // Display the setup modal with no way to escape it.
    $("#setup_modal").modal({
        backdrop: 'static',
        keyboard: false
    });

    // Re-validate the alias input on modification.
    $("#aliasInput").bind('input', function() {
        validateInput();
    });

    // When the scroll prompt is clicked, scroll to the bottom.
    $("#scrollprompt").bind('click', function() {
        var target = $("#log").height() - $("#logbox").height();
        $("#logbox").animate({ scrollTop: target }, 'fast');
        autoscroll = true;
        autoscrollPending = true;
        $("#scrollprompt").fadeOut(250);
    });

    // When the logbox is scrolled, display the scroll prompt and disable
    // autoscrolling.
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

    // When the window is resized, refit the map to the background.
    $(window).resize(function() {
        if (map && mapBounds) {
            map.fitBounds(mapBounds);
        }
    });

    // Kick off GeoLocation and validate any browser entered input.
    navigator.geolocation.getCurrentPosition(locationSuccess, locationError);
    validateInput();
});
