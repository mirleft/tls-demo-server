
function msg (from, to, text, ev1, tt) {
    var typ = from.secure ? Diagram.LINETYPE.SOLID : Diagram.LINETYPE.DOTTED;
    return new Diagram.Signal(from, typ | (Diagram.ARROWTYPE.FILLED << 2), to, text, ev1, tt)
}

function lookupchap (msg) {
    switch (msg) {
        case 'Change Cipher Spec'  : return '.chapter-7-1'
        case 'Hello Request'       : return '.chapter-7-4-1-1'
        case 'Client Hello'        : return '.chapter-7-4-1-2'
        case 'Server Hello'        : return '.chapter-7-4-1-3'
        case 'Certificate'         : return '.chapter-7-4-2'
        case 'Server Key Exchange' : return '.chapter-7-4-3'
        case 'Server Hello Done'   : return '.chapter-7-4-5'
        case 'Client Key Exchange' : return '.chapter-7-4-7'
        case 'Finished'            : return '.chapter-7-4-9'
        default                    : return '.chapter-10'
    }
}

function clear_element (element) {
    while (element.hasChildNodes())
        element.removeChild(element.firstChild)
}

function rfc_cb (div, msg, data) {
    var chap = lookupchap(msg);
    return function () {
        clear_element(div);
        $('.rfc-chapter').hide() ;
        $(chap).show('slow');
        var pre = document.createElement("pre");
        pre.innerHTML = data;
        div.appendChild(pre);
    }
}

function handle_sec (from, msg) {
    if (msg == 'Change Cipher Spec')
        from.secure = true
}

function process (diagram, details_div, in_record) {
    var me = diagram.getActor('Me');
    var txt = in_record.message;
    if (in_record.event == 'message') {
        //msg
        var you = diagram.getActor('You');
        var from = me ;
        var to = you ;
        if (in_record.direction == 'in') {
            from = you ; to = me ;
        }
        var cb = rfc_cb(details_div, txt, in_record.data);
        var sig = msg(from, to, txt, cb);
        handle_sec(from, txt);
        diagram.addSignal(sig);
    } else {
        //note
        var not = new Diagram.Note(me, Diagram.PLACEMENT.RIGHTOF, txt);
        diagram.addSignal(not)
    }
}

function runme(diagram_div, details_div) {
    $('.chapter-NONE').show();


    $.ajax({ url: "/diagram.txt" }).done( function( data ) {
        console.log( "initially received data:", data.length );
    });

    var req = document.getElementById("request")
    request.onclick = function () {
        $.ajax({ url: "/rekey" })
    }

    var up = document.getElementById("update")
    up.onclick = function () {
        $.ajax({ url: "/diagram.txt" }).done( function( data ) {
            console.log( "received data:", data.length );
        });
    }

    var diagram = new Diagram();

    var you = diagram.getActor('You');
    var me = diagram.getActor('Me');

    var tt = "tooltip here";

    var evn = function () { console.log("clicked note") }

    var msgs = [ { "event": "message", "message": "Client Hello", "direction": "in", "data": "version: TLS_1_2\nciphersuites: A, B, C, D\nrandom: 00112233445566778899001122334455\n\t66778899001122334455667788990011" },
                 { "event": "note",    "message": "bla" },
                 { "event": "message", "message": "Server Hello", "direction": "out" },
                 { "event": "note",    "message": "blubb" },
                 { "event": "message", "message": "Certificate", "direction": "out" },
                 { "event": "note",    "message": "blablubb" },
                 { "event": "message", "message": "Server Hello Done", "direction": "out" },
                 { "event": "message", "message": "Client Key Exchange", "direction": "in" },
                 { "event": "message", "message": "Change Cipher Spec", "direction": "in" },
                 { "event": "message", "message": "Finished", "direction": "in" },
                 { "event": "message", "message": "Change Cipher Spec", "direction": "out" },
                 { "event": "message", "message": "Finished", "direction": "out" },
                 { "event": "message", "message": "AD: GET /", "direction": "in" },
                 { "event": "message", "message": "AD: this site", "direction": "out" } ]

    for (var i = 0 ; i < msgs.length ; i++) {
        process(diagram, details_div, msgs[i]);
    }

    var options = {
        theme: 'hand',
        scale: 1.2
    };

    diagram_div.innerHTML='';

    // Draw
    diagram.drawSVG(diagram_div, options);
}

function initialise () {
    runme(document.getElementById('diagram'), document.getElementById('details'))
};
