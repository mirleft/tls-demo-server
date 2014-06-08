
function msg (from, to, text, secret, ev1, tt) {
    var typ = secret ? Diagram.LINETYPE.DOTTED : Diagram.LINETYPE.SOLID;
    return new Diagram.Signal(from, typ | (Diagram.ARROWTYPE.FILLED << 2), to, text, ev1, tt)
}

function runme(diagram_div) {

    var diagram = new Diagram();

    var you = diagram.getActor('You');
    var me = diagram.getActor('Me');

    var tt = "tooltip here";
    var ev1y = function () { console.log("clicked you") }
    var ev1m = function () { console.log("clicked me") }
    var evn = function () { console.log("clicked note") }
    var right = Diagram.PLACEMENT.RIGHTOF;
    var left = Diagram.PLACEMENT.LEFTOF;

    var msgs = [ [ 'msg', you, me, 'Client Hello', false, ev1y ] ,
                 [ 'note', me, 'bla',  right, evn ],
                 [ 'msg', me, you, 'Server Hello', false, ev1m ] ,
                 [ 'note', me, 'blubb', right, evn ],
                 [ 'msg', me, you, 'Certificate', false, ev1m ] ,
                 [ 'note', me, 'blablubb', right, evn ],
                 [ 'msg', me, you, 'Server Hello Done', false, ev1m ] ,
                 [ 'msg', you, me, 'Client Key Exchange', false, ev1y ] ,
                 [ 'msg', you, me, 'Change Cipher Spec', false, ev1y ],
                 [ 'msg', you, me, 'Finished', true, ev1y ],
                 [ 'msg', me, you, 'Change Cipher Spec', false, ev1m ],
                 [ 'msg', me, you, 'Finished', true, ev1m ],
                 [ 'msg', you, me, 'AD: GET /', true, ev1y ],
                 [ 'msg', me, you, 'AD: this site', true, ev1m ] ];

    for (var i = 0 ; i < msgs.length ; i++) {
        var m = msgs[i];
        var typ = m[0];
        if (typ == 'msg') {
            var sig = msg(m[1], m[2], m[3], m[4], m[5], tt)
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
        scale: 1
    };

    diagram_div.innerHTML='';

    // Draw
    diagram.drawSVG(diagram_div, options);
}

function initialise () {
    runme(document.getElementById('diagram'))
};
