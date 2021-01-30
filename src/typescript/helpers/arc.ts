import { Arc } from "@thi.ng/geom";
import { Vec } from "@thi.ng/vectors";

/**
 * Get the slope of the tangent line at a specified angle for an arc
 * @param arc The arc to analyze.
 * @param angle The angle to look at.
 */
export const getTangentAtArcAngle = (arc: Arc, angle: number): number => {
    const startPos = [arc.r[0] * Math.cos(angle), arc.r[1] * Math.sin(angle)]; // Relative position of point on ellipse
    const v1 = (arc.r[1] / arc.r[0]) ** 2; // We know you always have to use the power rule here, so we can calulate a value that we need. The squared is because of the way the ellipse formula is made.
    const v2 = v1 * startPos[0] / startPos[1] * -1; // Get the slope based off of the derivative
    return v2;
};

/**
 * Get the midpoint (in Vec form) of a minor arc.
 * @param arc The arc to analyze.
 */
export const getArcMidpoint = (arc: Arc): Vec => {
    const startPos = [arc.r[0] * Math.cos(arc.start), arc.r[1] * Math.sin(arc.start)]; // Relative position of start point on ellipse
    const endPos = [arc.r[0] * Math.cos(arc.end), arc.r[1] * Math.sin(arc.end)]; // Relative position of end point on ellipse
    const heightDiff = arc.r[1] - startPos[1]; // Arc height
    const midpointFlat = [(startPos[0] + endPos[0]) / 2, (startPos[1] + endPos[1]) / 2]; // Midpoint of the flat line
    const angleFlat = Math.atan2((endPos[1] - startPos[1]) / (endPos[0] / startPos[0]), 1); // Angle of the flat line
    const angleModified = angleFlat + (Math.PI * 3 / 4) % (Math.PI * 2); // Modified angle accounting for modulo
    const trueMidpointDiff = [midpointFlat[0] + Math.cos(angleModified) * heightDiff, midpointFlat[1] + Math.sin(angleModified) * heightDiff];
    return [trueMidpointDiff[0] + arc.pos[0], trueMidpointDiff[1] + arc.pos[1]];
};

/**
 * Get the midpoint (in Vec form) of a minor arc with an offset.
 * @param arc The arc to analyze.
 * @param offset A number which specifies the offset distance
 */
export const getArcMidpointOffset = (arc: Arc, offset: number): Vec => {
    const offsetPos = [Math.sin(arc.axis) * (arc.r[1] + offset)* (arc.start < 0 ? 1 : -1), Math.cos(arc.axis) * (arc.r[1] + offset) * (arc.start < 0 ? -1 : 1)];
    return offsetPos.map((l, idx) => arc.pos[idx] + l);
};