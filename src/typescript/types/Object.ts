import type { Vec } from "@thi.ng/vectors";
import type { Mat23Like } from "@thi.ng/matrices";
import { Circle } from "@thi.ng/geom";
import { Morphism2Geometry, MorphismGeometry, MorphismGeometryStored } from "./Morphism";
import { Attribs } from "@thi.ng/geom-api";

export interface ObjectGeometry {
    id: number;
    position: Vec;
    name: string;
    attribs: Attribs;
}

export interface GeometryCache {
    objects: ObjectGeometry[];
    morphisms: MorphismGeometry[];
    naturalMorphisms: Map<{ from: ObjectGeometry, to: ObjectGeometry }, MorphismGeometry[]>;
    morphisms2: Morphism2Geometry[];
    camera: Mat23Like;
    dragging?: ObjectGeometry;
    morphismStart?: ObjectGeometry;
    composing?: MorphismGeometry;
}

export interface StoredCache {
    objects: ObjectGeometry[];
    morphisms: MorphismGeometryStored[];
    naturalMorphisms: [{ from: number, to: number }, MorphismGeometryStored[]][];
    morphisms2: Morphism2Geometry[];
    camera: Mat23Like;
    dragging?: ObjectGeometry;
    morphismStart?: ObjectGeometry;
    composing?: MorphismGeometry;
}