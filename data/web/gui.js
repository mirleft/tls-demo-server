
function msg (from, to, text, ev1, tt) {
    var typ = from.secure ? Diagram.LINETYPE.SOLID : Diagram.LINETYPE.DOTTED;
    return new Diagram.Signal(from, typ | (Diagram.ARROWTYPE.FILLED << 2), to, text, ev1, tt)
}

function lookupchap (msg) {
    switch (msg) {
        case 'ChangeCipherSpec'  : return '.chapter-7-1'
        case 'HelloRequest'      : return '.chapter-7-4-1-1'
        case 'ClientHello'       : return '.chapter-7-4-1-2'
        case 'ServerHello'       : return '.chapter-7-4-1-3'
        case 'Certificate'       : return '.chapter-7-4-2'
        case 'ServerKeyExchange' : return '.chapter-7-4-3'
        case 'ServerHelloDone'   : return '.chapter-7-4-5'
        case 'ClientKeyExchange' : return '.chapter-7-4-7'
        case 'Finished'          : return '.chapter-7-4-9'
        default                  : return '.chapter-10'
    }
}

function rfc_cb (sig, msg, moredata) {
    var chap = lookupchap(msg);
    _.each (moredata, function (datum) {
        var dat = datum[0] != '' ? datum[0] + ': ' : '' ;
        var txt = dat + datum[1];
        sig.data.push (txt);
    })
    return function () {
        $('.rfc-chapter').hide() ;
        $(chap).show('fast');
        $('#details').empty().append(
          _.map (sig.data, function (d) { return $("<li>").append (d) })
        )
    }
}

function handle_sec (from, msg) {
    if (msg == 'ChangeCipherSpec')
        from.secure = true
}

var updating = false;

function process (me, you, in_record) {
    var txt = in_record.message;
    if (in_record.event == 'message') {
        //msg
        var from = me ;
        var to = you ;
        if (in_record.direction == 'in') {
            from = you ; to = me ;
        }
        var r = msg(from, to, txt);
        handle_sec(from, txt);
        return r;
    } else if (in_record.event == 'note') {
        //note
        if (in_record.data == '')
            return new Diagram.Note(me, Diagram.PLACEMENT.RIGHTOF, txt)
        return new Diagram.Note(me, Diagram.PLACEMENT.RIGHTOF, txt, null, in_record.data);
    }
}

function insertSignals (diagram, signals, data) {

    var last = null;

    for ( var i = 0 ; i < signals.length ; i++ ) {
        var now = signals[i];
        if (now.type == 'Signal') {
            var txt = now.message;
            if (last != null && last.actorA == now.actorA &&
                (last.message == txt || last.message == txt + '*' )) {
                var cb = rfc_cb(last, txt, data[i].data);
                last.ev1 = cb;
                if (last.message == txt) {
                    last.message = last.message + "*";
                }
            } else {
                var cb = rfc_cb(now, txt, data[i].data);
                now.ev1 = cb;
                diagram.addSignal(now);
                last = now;
            }
        } else {
            diagram.addSignal(now);
        }
    }
}

function getData () {
  $.ajax({ url: "/diagram.json" }).done( function( data ) {
      var diagram = new Diagram();
      var you = diagram.getActor('Client');
      var me = diagram.getActor('Server');

      var signals = _.map(data, function (x) { return process(me, you, x) });
      insertSignals(diagram, signals, data);

      var options = {
          theme: 'simple'
      };

      $('#diagram, #details').empty ();
      $('.rfc-chapter').hide() ;
      $('.chapter-NONE').show();

      // Draw
      diagram.drawSVG($('#diagram')[0], options);
      updating = false;
      $('#request').val('renegotiate!').attr('disabled', false);
  });
}

function initialise () {
    $('.chapter-NONE').show();

    getData();

    var req = document.getElementById("request")
    request.onclick = function () {
        if (! updating) {
            updating = true;
            req.value = 'renegotiating...';
            req.disabled = true;
            $.ajax({ url: "/rekey" }).done( function () {
                setTimeout(function () { return getData(); }, 300)
            })
        }
    }
}
