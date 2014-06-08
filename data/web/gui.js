
function msg (diagram, from, to, text, secret, ev1, tt) {
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

    var msgs = [ [ you, me, 'Client Hello', false, ev1y ] ,
                 [ me, you, 'Server Hello', false, ev1m ] ,
                 [ me, you, 'Certificate', false, ev1m ] ,
                 [ me, you, 'Server Hello Done', false, ev1m ] ,
                 [ you, me, 'Client Key Exchange', false, ev1y ] ,
                 [ you, me, 'Change Cipher Spec', false, ev1y ],
                 [ you, me, 'Finished', true, ev1y ],
                 [ me, you, 'Change Cipher Spec', false, ev1m ],
                 [ me, you, 'Finished', true, ev1m ],
                 [ you, me, 'AD: GET /', true, ev1y ],
                 [ me, you, 'AD: this site', true, ev1m ] ];

    for (var i = 0 ; i < msgs.length ; i++) {
        var m = msgs[i];
        var sig = msg(diagram, m[0], m[1], m[2], m[3], m[4], tt)
        diagram.addSignal(sig);
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
