var shapenames = ['circle', 'rect'];

var width_range = {
    min: 15,
    max: 50
};

var height_range = {
    min: 15,
    max: 50
};

var colors = ["red", "green", "blue", "purple",
    "yellow", "cyan", "magenta", "black", "grey"];

// Utility functions for randomness

function randInt(min, max) {
    return Math.floor(Math.random() * (max - min)) + min;
}

function random(a) {
    return a[randInt(0, a.length)];
}

// Visual Element (VE) helper functions
function getWidth(sh) {
    if (sh.name == 'circle') {
        return sh.radius * 2;
    } else {
        return sh.width;
    }
}

function getHeight(sh) {
    if (sh.name == 'circle') {
        return sh.radius * 2;
    } else {
        return sh.height;
    }
}

function alignBottomLeft(sh, pos) {
    if (sh.name == 'circle') {
        pos.x = pos.x + sh.radius;
        pos.y = pos.y - sh.radius;
    } else {
        pos.y = pos.y - sh.height;
    }
}

function alignCenter(sh, pos) {
    if (sh.name == 'circle') {
        // do nothing - already correct
    } else {
        pos.x = pos.x - getWidth(sh) / 2;
        pos.y = pos.y - getHeight(sh) / 2;
    }
}

// Frames
var frames = {
    "touching": {
        holes: [{
            size: 100, // % of original size
            horiz: 45, // % of frame size right
            vert: 75 // % of frame size down
        }, {
            size: 100,
            horiz: 55,
            vert: 75
        }]
    },

    "monolog": {
        holes: [{
            size: 100,
            horiz: 50,
            vert: 50
        }]
    },

    "dialog": {
        holes: [{
            size: 100,
            horiz: 20,
            vert: 75
        }, {
            size: 100,
            horiz: 80,
            vert: 75
           }]
    },

    "carry": {
        holes: [{
            size: 100,
            horiz: 50,
            vert: 50
        }, {
            size: 100,
            horiz: 55,
            vert: 45
        }]
    },

    "stack": {
        holes: [{
            size: 100,
            horiz: 50,
            vert: 60
        }, {
            size: 100,
            horiz: 50,
            vert: 40
        }]
    },

    "blank": {
        holes: []
    }
};

function frame_name_to_frame(name) {
    if (name == "touch" || name == "whisper" || name == "aid") {
        return frames["touching"];
    }
    if (name == "dialog" || name == "blank" || name == "walk" || name == "posse") {
        return frames["blank"];
    }
    if (name == "carry") {
        return frames["carry"];
    }
    if (name == "monolog" || name == "fall") {
        return frames["monolog"];
    }
    return frames["blank"];
}


// Drawing functions

function newVE() {
    var my_shapename, myshape, my_radius, my_width, my_height, mycolor; 
    my_shapename = random(shapenames);
    if (my_shapename == 'circle') {
        my_radius = randInt(width_range.min, width_range.max) / 2;
        myshape = {
            name: 'circle',
            radius: my_radius
        };
    } else {
        my_width = randInt(width_range.min, width_range.max);
        my_height = randInt(width_range.min, width_range.max);
        myshape = {
            name: 'rect',
            width: my_width,
            height: my_height
        };
    }
    // myshape = random(shapes)
    console.log("shape: " + JSON.stringify(myshape));
    mycolor = random(colors);
    console.log("color: " + mycolor);
    return {
        shape: myshape,
        color: mycolor
    };
}

function draw_rect(context, x, y, w, h, color) {
    context.fillStyle = color;
    context.fillRect(x, y, w, h);
}

function draw_circle(context, x, y, sz, color) {
    context.beginPath();
    context.arc(x, y, sz, 0, 2 * Math.PI, false);
    context.fillStyle = color;
    context.fill();
}

function draw_at(ctx, ve, x, y) {
    console.log("Drawing " + JSON.stringify(ve) + " at (" + x + ", " + y + ")");
    if (ve.shape.name == 'circle') {
        draw_circle(ctx, x, y, ve.shape.radius, ve.color);
    } else if (ve.shape.name == 'rect') {
        draw_rect(ctx,
            x, y, ve.shape.width, ve.shape.height, ve.color);
    }
}

function draw_adjacent(ctx, frame_height, frame_width, ve_set, x_offset) {
    for (var k = 0; k < ve_set.length; k++) {
        ve = ve_set[k];
        my_width = getWidth(ve.shape);
        pos = {
            x: x_offset,
            y: frame_height - 20
        }
        alignBottomLeft(ve.shape, pos);
        draw_at(ctx, ve, pos.x, pos.y);
        x_offset += my_width + 5;
    }
}

function draw_frame(ctx, ve_set, frame_name, w, h) {
    x_offset = 10;
    if (frame_name == "blank") {
        console.log("Drawing default frame " + frame_name);
        draw_adjacent(ctx, h, w, ve_set, x_offset)
    } else {
        console.log("Drawing special frame: " + frame_name);
        frame = frame_name_to_frame(frame_name);
        for (var idx = 0; idx < frame.holes.length; idx++) {
            hole = frame.holes[idx];
            ve = ve_set[idx];
            x = 0.01 * hole.horiz * w;
            y = (0.01 * hole.vert) * h;
            pos = {
                x: x,
                y: y
            };
            alignCenter(ve.shape, pos);
            draw_at(ctx, ve, pos.x, pos.y);
        } //end for loop over frame's holes
        if (idx < ve_set.length) { // more VEs to draw
            tail = [];
            for (var qq = idx; qq < ve_set.length; qq++) {
                tail.push(ve_set[qq]);
            }
            draw_adjacent(ctx, h, w, tail, x_offset);
        } //end draw leftover VEs
    } //end else (non-blank frames)
} //end function draw_frame

function draw_panel(cvs, frame_name, panel_ves, ve_table) {

    // ctx.fillRect(20,30,20,30,"green"); // TEST
    ctx = cvs.getContext('2d');

    x_offset = 5;
    y_offset = cvs.height;
    ves_to_draw = [];
    for (var j = 0; j < panel_ves.length; j++) {
        ve_index = panel_ves[j]
        if (ve_table[ve_index] == undefined) {
            console.log("Making a new VE: " + ve_index);
            // make a new VE and add it to the table
            ve_table[ve_index] = newVE();
        }
        ves_to_draw.push(ve_table[ve_index]);
    }

    // draw_adjacent(ctx, cvs.height, cvs.width, ves_to_draw, 10);
    draw_frame(ctx, ves_to_draw, frame_name, cvs.width, cvs.height);

}

function draw_comic(n, comicSpec) {

    ves = []

    for (var i = 0; i < comicSpec.length; i++) {
        panelSpec = comicSpec[i][0];
        frame_name = panelSpec.name;
        ve_set = panelSpec.elements;
        console.log("drawing panel " + i);
        draw_panel(document.getElementById('canvas' + i), frame_name, ve_set, ves);
    }
}


function add_comic_JSON() {
    nves = parseInt(document.getElementById('NVEs').value);
    min = parseInt(document.getElementById('min').value) - 1;
    max = parseInt(document.getElementById('max').value) - 1;

    comicSpecString = "";
    var constrained = document.getElementById('constrained').checked;
    if (constrained) {
        comicSpecString = comic.genconstrained(nves);
        arcSpec = JSON.parse(comicSpecString);
        comicSpec = arcSpec.comic;
    } else {
        comicSpecString = comic.gen(nves, min, max);
        comicSpec = JSON.parse(comicSpecString)
    }
    document.getElementById('output').innerHTML =
        'Source: <br>'+comicSpecString

    comicDOMelement = document.getElementById('comic');
    comicDOMelement.innerHTML = ""
    num_canvases = 0

    console.log("Comic length: " + comicSpec.length);
    console.log("First element: " + JSON.stringify(comicSpec[0]));
    // Set up comic frames
    for (var i = 0; i < comicSpec.length; i++) {
        console.log("Adding frame " + i);
        comicDOMelement.innerHTML +=
            "<canvas id='canvas" + i + "' width='200' height='200' \
        style='border:1px solid #000000;'></canvas> &nbsp;&nbsp;"
        num_canvases++;
        if ((i + 1) % 4 == 0) {
            comicDOMelement.innerHTML += "<br><br>"
        }
    }

    draw_comic(num_canvases, comicSpec);

    console.log("Number of canvases: " + num_canvases);

}
