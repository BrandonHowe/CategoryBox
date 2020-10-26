import { Circle } from "@thi.ng/geom";

export interface MorphismGeometry {
    id: number;
    from: Circle;
    to: Circle;
    name: string;
}