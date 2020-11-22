import { Circle, Line, Path } from "@thi.ng/geom";

export interface MorphismGeometry {
    id: number;
    from: Circle;
    to: Circle;
    name: string;
    arrowhead1: Line;
    arrowhead2: Line;
    shape: Line | Path;
}