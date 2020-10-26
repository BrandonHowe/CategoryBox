import type { Vec } from "@thi.ng/vectors";
import type { Mat23Like } from "@thi.ng/matrices";
import { Circle } from "@thi.ng/geom";
import { MorphismGeometry } from "./Morphism";

export interface ObjectGeometry {
    id: number;
    position: Vec;
    name: string;
    shape: Circle;
}

export interface GeometryCache {
    objects: ObjectGeometry[];
    morphisms: MorphismGeometry[];
    camera: Mat23Like;
    mouseDown: boolean;
    dragging?: ObjectGeometry;
    morphismStart: ObjectGeometry;
}