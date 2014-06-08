
function runme(diagram_div) {

    var data = "You->Me: Client Hello \n \
Note right of Me: bla \n \
Me->You: Server Hello \n \
Note right of Me: blubb \n \
Me->You: Certificate \n \
Note right of Me: blablubb \n \
Me->You: Server Hello Done \n \
You->Me: Client Key Exchange \n \
You->Me: Change Cipher Spec \n \
You-->Me: Finished \n \
Me->You: Change Cipher Spec \n \
Me-->You: Finished \n \
You-->Me: Application Data: GET / \n \
Me-->You: Application Data: This site"

    var diagram = Diagram.parse(data);

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
