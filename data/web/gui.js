
function msg (from, to, text, secret, ev1, tt) {
    var typ = secret ? Diagram.LINETYPE.SOLID : Diagram.LINETYPE.DOTTED;
    return new Diagram.Signal(from, typ | (Diagram.ARROWTYPE.FILLED << 2), to, text, ev1, tt)
}

function runme(diagram_div) {
    $('.chapter-NONE').show();

    var diagram = new Diagram();

    var you = diagram.getActor('You');
    var me = diagram.getActor('Me');

    var tt = "tooltip here";
    var rfc_cb = function (chap) {
        return function () {
            $('.rfc-chapter').hide() ;
            $(chap).show('slow');
        }
    }
    var evn = function () { console.log("clicked note") }
    var right = Diagram.PLACEMENT.RIGHTOF;
    var left = Diagram.PLACEMENT.LEFTOF;

    var msgs = [ [ 'msg', you, me, 'Client Hello', false, rfc_cb('.chapter-7-4-1-2') ] ,
                 [ 'note', me, 'bla',  right, evn ],
                 [ 'msg', me, you, 'Server Hello', false, rfc_cb('.chapter-7-4-1-3') ] ,
                 [ 'note', me, 'blubb', right, evn ],
                 [ 'msg', me, you, 'Certificate', false, rfc_cb('.chapter-7-4-2') ] ,
                 [ 'note', me, 'blablubb\nblabla', right, evn ],
                 [ 'msg', me, you, 'Server Hello Done', false, rfc_cb('.chapter-7-4-5') ] ,
                 [ 'msg', you, me, 'Client Key Exchange', false, rfc_cb('.chapter-7-4-7') ] ,
                 [ 'msg', you, me, 'Change Cipher Spec', false, rfc_cb('.chapter-7-1') ],
                 [ 'msg', you, me, 'Finished', true, rfc_cb('.chapter-7-4-9') ],
                 [ 'msg', me, you, 'Change Cipher Spec', false, rfc_cb('.chapter-7-1') ],
                 [ 'msg', me, you, 'Finished', true, rfc_cb('.chapter-7-4-9') ],
                 [ 'msg', you, me, 'AD: GET /', true, rfc_cb('.chapter-10') ],
                 [ 'msg', me, you, 'AD: this site', true, rfc_cb('.chapter-10') ] ];

    for (var i = 0 ; i < msgs.length ; i++) {
        var m = msgs[i];
        var typ = m[0];
        if (typ == 'msg') {
            var sig = msg(m[1], m[2], m[3], m[4], m[5])
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
