import { arc, line } from "@thi.ng/geom";
import { fromEndPoints as fromEndPointsRaw } from "@thi.ng/geom-arc";
import { ReadonlyVec } from "@thi.ng/vectors";

export const fromEndPoints = (a: ReadonlyVec, b: ReadonlyVec, radii: ReadonlyVec, axis?: number | undefined, xl?: boolean | undefined, cw?: boolean | undefined) => {
    if (radii[1] === 0) {
        return line(a, b);
    }
    const rawData = fromEndPointsRaw(a, b, radii, axis, xl, cw)!;
    return arc(rawData.center, rawData.r, rawData.axis, rawData.start, rawData.end);
};