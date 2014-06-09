
function msg (from, to, text, secret, ev1, tt) {
    var typ = secret ? Diagram.LINETYPE.SOLID : Diagram.LINETYPE.DOTTED;
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

function rfc_cb (msg) {
    var chap = lookupchap(msg);
    return function () {
        $('.rfc-chapter').hide() ;
        $(chap).show('slow');
    }
}

function process (diagram, in_record) {
    if (in_record.event = 'message') {
        //msg
    } else {
        //note
    }
}

function runme(diagram_div) {
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
    var right = Diagram.PLACEMENT.RIGHTOF;
    var left = Diagram.PLACEMENT.LEFTOF;

    var msgs = [ [ 'msg', you, me, 'Client Hello', false ] ,
                 [ 'note', me, 'bla',  right, evn ],
                 [ 'msg', me, you, 'Server Hello', false ] ,
                 [ 'note', me, 'blubb', right, evn ],
                 [ 'msg', me, you, 'Certificate', false ] ,
                 [ 'note', me, 'blablubb\nblabla', right, evn ],
                 [ 'msg', me, you, 'Server Hello Done', false ] ,
                 [ 'msg', you, me, 'Client Key Exchange', false ] ,
                 [ 'msg', you, me, 'Change Cipher Spec', false ],
                 [ 'msg', you, me, 'Finished', true ],
                 [ 'msg', me, you, 'Change Cipher Spec', false ],
                 [ 'msg', me, you, 'Finished', true ],
                 [ 'msg', you, me, 'AD: GET /', true ],
                 [ 'msg', me, you, 'AD: this site', true ] ];

    for (var i = 0 ; i < msgs.length ; i++) {
        var m = msgs[i];
        var typ = m[0];
        if (typ == 'msg') {
            var cb = rfc_cb(m[3]);
            var sig = msg(m[1], m[2], m[3], m[4], cb)
            diagram.addSignal(sig);
        } else if (typ == 'note') {
            var not = new Diagram.Note(m[1], m[3], m[2], m[4], tt);
            diagram.addSignal(not)
        } else {
            console.log("unknown msg")
        }
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
    runme(document.getElementById('diagram'))
};
