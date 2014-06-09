
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

function rfc_cb (pre, msg, data) {
    var chap = lookupchap(msg);
    return function () {
        $('.rfc-chapter').hide() ;
        $(chap).show('slow');
        pre.innerHTML = data;
    }
}

function handle_sec (from, msg) {
    if (msg == 'Change Cipher Spec')
        from.secure = true
}

var updating = false;

function process (diagram, details_pre, in_record) {
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
        var cb = rfc_cb(details_pre, txt, in_record.data);
        var sig = msg(from, to, txt, cb);
        handle_sec(from, txt);
        diagram.addSignal(sig);
    } else {
        //note
        var not = new Diagram.Note(me, Diagram.PLACEMENT.RIGHTOF, txt);
        diagram.addSignal(not)
    }
}

function getData (diagram_div, details_pre) {
  $.ajax({ url: "/diagram.txt" }).done( function( data ) {
      console.log( "received data:", data.length );
      var diagram = new Diagram();
      diagram.getActor('You'); diagram.getActor('Me');
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
          process(diagram, details_pre, msgs[i]);
      }
      var options = {
          theme: 'hand',
          scale: 1.2
      };

      diagram_div.innerHTML='';
      details_pre.innerHTML='';
      $('.rfc-chapter').hide() ;
      $('.chapter-NONE').show();

      // Draw
      diagram.drawSVG(diagram_div, options);
      updating = false;
  });
}

function initialise () {
    $('.chapter-NONE').show();

    var diagram_div = document.getElementById('diagram');
    var details_pre = document.getElementById('details-pre');

    getData(diagram_div, details_pre);

    var req = document.getElementById("request")
    request.onclick = function () {
        if (! updating) {
            updating = true;
            $.ajax({ url: "/rekey" }).done( function () {
                setTimeout(function () { return getData(diagram_div, details_pre); }, 1000)
            })
        }
    }
}
