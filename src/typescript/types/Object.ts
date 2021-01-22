import type { Vec } from "@thi.ng/vectors";
import type { Mat23Like } from "@thi.ng/matrices";
import { Circle } from "@thi.ng/geom";
import { Morphism2Geometry, MorphismGeometry } from "./Morphism";

export interface ObjectGeometry {
    id: number;
    position: Vec;
    name: string;
    shape: Circle;
}

export interface GeometryCache {
    objects: ObjectGeometry[];
    morphisms: MorphismGeometry[];
    naturalMorphisms: Map<{ from: Circle, to: Circle }, MorphismGeometry[]>;
    morphisms2: Morphism2Geometry[];
    camera: Mat23Like;
    dragging?: ObjectGeometry;
    morphismStart?: ObjectGeometry;
    composing?: MorphismGeometry;
}