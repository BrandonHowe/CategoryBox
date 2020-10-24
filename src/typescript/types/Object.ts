import type { Vec } from "@thi.ng/vectors";
import type { Mat23Like } from "@thi.ng/matrices";
import { Circle } from "@thi.ng/geom";

export interface ObjectGeometry {
    position: Vec;
    name: string;
    shape: Circle;
}

export interface GeometryCache {
    objects: ObjectGeometry[];
    morphisms: ObjectGeometry[];
    camera: Mat23Like;
    dragging?: ObjectGeometry;
}