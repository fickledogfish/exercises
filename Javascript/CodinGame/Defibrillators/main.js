/*
Parse a string containing a floating-point number into a proper number.

This function is special because num may use a comma as a decimal separator.
*/
function parseCommaFloat(num) {
    return parseFloat(num.replace(',', '.'));
}

let userPos = {};

// Read the user's current position
{
    let userLon = parseCommaFloat(readline());
    let userLat = parseCommaFloat(readline());

    userPos = {
        lat: userLat,
        lon: userLon
    };
}

// Read the number of defibrillators will be passed to this program
let defibCount = parseInt(readline());

// Defining the data of the defibrillators
const DefibData = {
    id: 0,
    name: 1,
    addr: 2,
    phone: 3,
    lon: 4,
    lat: 5
};

// Somewhere to store the list of defibrillators
let defibLst = [];

// Parse and construct a single defibrillator from its properties
let Defibrillator = function(args) {
    let data = args.split(";");

    this.id    = data[DefibData.id];
    this.name  = data[DefibData.name];
    this.addr  = data[DefibData.addr];
    this.phone = data[DefibData.phone];
    this.lon   = parseCommaFloat(data[DefibData.lon]);
    this.lat   = parseCommaFloat(data[DefibData.lat]);
};

let defibs = [];

// Read and parse the list of defibrillators in the city
for (let i = 0; i < defibCount; i++) {
    let newDefib = readline();
    defibs.push(newDefib);
    defibLst.push(new Defibrillator(newDefib));
}

/*
Converts from degrees into radians.
*/
function toRad(deg) {
    return deg*Math.PI/180;
}

/*
Calculate the distance between two points.

Latitude and longitudes are assumed to be in degrees.
*/
function distance(latA, lonA, latB, lonB) {
    latA = toRad(latA);
    lonA = toRad(lonA);
    latB = toRad(latB);
    lonB = toRad(lonB);

    let x = (lonB - lonA)*Math.cos((latA + latB)/2);
    let y = latB - latA;

    return Math.sqrt(Math.pow(x, 2) + Math.pow(y, 2))*6371;
}

// Find the distance of all defibrillators in the city from the user
let distanceLst = [];

for(let el of defibLst) {
    distanceLst.push(distance(el.lat, el.lon, userPos.lat, userPos.lon));
}

/*
Finds the index of the smallest value in the array.
*/
function indexOfMin(arr) {
    let min = 0;

    for(let i = 0; i < arr.length; i++) {
        if(arr[i] < arr[min])
            min = i;
    }

    return min;
}

// Debug info
//for(let el of defibs) printErr(el);
//for(let el of defibLst) printErr(el.name);

// Print the solution (the name of the closest defibrillator)
print(defibLst[indexOfMin(distanceLst)].name);
