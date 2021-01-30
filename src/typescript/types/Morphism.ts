import { Arc, Circle, Cubic, Line, Path, Text as GText } from "@thi.ng/geom";
import { Attribs } from "@thi.ng/geom-api";
import { ObjectGeometry } from "./Object";

export type MorphismGeometryStored = {
    id: number;
    from: number;
    to: number;
    name: string;
    arcHeight: number;
    attribs: Attribs;
}

export type MorphismGeometry = {
    id: number;
    from: ObjectGeometry;
    to: ObjectGeometry;
    name: string;
    arcHeight: number;
    attribs: Attribs;
}

export interface Morphism2Geometry {
    id: number;
    from: MorphismGeometry;
    to: MorphismGeometry;
    name: string;
    arrowhead1: Line;
    arrowhead2: Line;
}