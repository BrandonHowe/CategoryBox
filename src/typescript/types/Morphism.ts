import { Arc, Circle, Cubic, Line, Path, Text as GText } from "@thi.ng/geom";
import { Attribs } from "@thi.ng/geom-api";

export interface MorphismGeometry {
    id: number;
    from: Circle;
    to: Circle;
    name: string;
    arrowhead1: Line;
    arrowhead2: Line;
    shape: Line | Arc;
    arcAttributes?: Attribs;
    text: GText;
}

export interface Morphism2Geometry {
    id: number;
    from: MorphismGeometry;
    to: MorphismGeometry;
    name: string;
    arrowhead1: Line;
    arrowhead2: Line;
    shape: Line;
    shape2: Line;
}