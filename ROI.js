"use strict";

// Represents a polygonal region-of-interest (ROI). 
class ROI {
    // Create a ROI, given an array of [x, y] values.
    constructor(coords) {
        this.coords = coords;
    }

    stroke(ctx) {
        ctx.beginPath();
        // start point
        ctx.moveTo(this.coords[0][0], this.coords[0][1]);
        // build path
        for(var i = 1, ln = this.coords.length; i < ln; i++) {
            ctx.lineTo(this.coords[i][0], this.coords[i][1]);
        }
        ctx.closePath();
    };

    draw(ctx, colour, width) {
        this.stroke(ctx);
        ctx.strokeStyle = colour;
        ctx.lineWidth = width;
        ctx.stroke();
    }

    /** Hit test whatever shape is defined in the context. */
    static hitTest(ctx, x, y) {
        return ctx.isPointInPath(x, y);
    };
};


// Returns a Promise object for fetching a ROI instance from a URL.
// The URL should reference a tab-separated x, y file,
// as saved by the ImageJ command: File -> Save As -> XY Coordinates.
const LoadROI = url => {
    return new Promise((resolve, reject) => {
    Papa.parse(url, {
	download: true,
        header: false,
        skipEmptyLines: true,
        dynamicTyping: true,
	complete: function(results) {
            if (results.errors.length > 0) {
                reject("Unable to read region-of-interest: " + results.errors[0].message + ", row " + results.errors[0].row);
            } else {
                // Convert rows to arrays of numbers
                resolve(new ROI(results.data));
            }
	}
    });
    })
}

/** Asynchronously fetches the image and ROI for a photo, and returns
 * a promise for both together.
*/
function LoadImageAndROI(photo) {
    const photoP = LoadImage(photo.URL);
    const RoiP = LoadROI(photo.ROI);
    return Promise.all([photoP, RoiP]);
}
