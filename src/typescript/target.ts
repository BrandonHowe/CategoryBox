import { Arc, circle, closestPoint, Line, Path, pointInside } from "@thi.ng/geom";
import { closestPoint as closestPointArc } from "@thi.ng/geom-arc";
import { Vec, dist } from "@thi.ng/vectors"
import { filterNaturalMorphisms, getMorphismShapes, getNaturalMorphismShapes } from ".";
import { findLast } from "./helpers/findLast";
import { minBy } from './helpers/minBy';
import { MorphismGeometry } from './types/Morphism';
import { GeometryCache, ObjectGeometry } from "./types/Object"

export enum MouseTargetKind {
    Morphism = "morphism",
    Object = "object",
    Nothing = "Nothing"
}

export type MouseTarget = {
    type: MouseTargetKind.Nothing
} | {
    type: MouseTargetKind.Object,
    target: ObjectGeometry
} | {
    type: MouseTargetKind.Morphism,
    target: MorphismGeometry
}

const getClosestPoint = (shape: Line | Arc, mouse: Vec): Vec | undefined => {
    if (shape instanceof Arc) {
        return closestPointArc(mouse, shape.pos, shape.r, shape.axis, shape.start, shape.end);
    } else {
        return closestPoint(shape, mouse);
    }
};

/**
 * Finds the object in the scene the mouse is hovering over
 *
 * @param mousePosition The position the mouse is at
 * @param cache The geometry cache to search trough
 */
export const getMouseTarget = (
    mousePosition: Vec,
    cache: GeometryCache
): MouseTarget => {
    if (cache.objects.length === 0) {
        return {
            type: MouseTargetKind.Nothing
        };
    }
    
    const distanceToMouse = (position: Vec) => dist(mousePosition, position);

    const objects = [...cache.objects];
    // const morphismShapes: (Line | Arc)[] = cache.morphisms.flatMap(l => ["arrowhead1", "arrowhead2", "shape"].map(j => getMorphismShapes(l)[j]));
    // const filteredNaturalMorphisms = [...cache.naturalMorphisms.values()].flat();
    // const naturalMorphismShapes: (Line | Arc)[] = filteredNaturalMorphisms.flatMap(l => ["arrowhead1", "arrowhead2", "shape"].map(j => getMorphismShapes(l)[j]));
    // const morphisms = [...morphismShapes, ...naturalMorphismShapes];
    const morphisms = [...cache.morphisms, ...[...cache.naturalMorphisms.values()].flat()];

    {
        const closestObject = findLast(objects, node => pointInside(circle(node.position, 15), mousePosition));

        if (closestObject) {
            return {
                type: MouseTargetKind.Object,
                target: closestObject
            }
        }
    }
    {
        const closestMorphism = minBy((a, b) => distanceToMouse(a.closest) < distanceToMouse(b.closest), morphisms.map(l => ({ geometry: l, closest: getClosestPoint(getMorphismShapes(l).shape, mousePosition)! })));

        if (closestMorphism && distanceToMouse(closestMorphism.closest) < 10) {
            return {
                type: MouseTargetKind.Morphism,
                target: closestMorphism.geometry
            }
        }
    }

    return {
        type: MouseTargetKind.Nothing
    };
}

/**
 * Finds the object in the scene the mouse is hovering over
 *
 * @param mousePosition The position the mouse is at
 * @param cache The geometry cache to search trough
 */
export const getAllTargets = (
    mousePosition: Vec,
    cache: GeometryCache
): MouseTarget[] => {
    if (cache.objects.length === 0) {
        return [];
    }
    
    const distanceToMouse = (position: Vec) => dist(mousePosition, position);

    const objects = [...cache.objects];
    const morphisms = [...cache.morphisms, ...[...cache.naturalMorphisms.values()].flat()];

    const output: MouseTarget[] = [];

    {
        const closestObjects = objects.filter(node => pointInside(circle(node.position, 10), mousePosition));

        output.push(...closestObjects.map(l => ({
            type: MouseTargetKind.Object,
            target: l
        } as MouseTarget)));
    }
    {
        const closestMorphism = minBy((a, b) => distanceToMouse(a.closest) < distanceToMouse(b.closest), morphisms.map(l => ({ geometry: l, closest: getClosestPoint(getMorphismShapes(l).shape, mousePosition)! })));

        if (closestMorphism && distanceToMouse(closestMorphism.closest) < 10) {
            output.push({
                type: MouseTargetKind.Morphism,
                target: closestMorphism.geometry
            });
        }
    }

    return output;
}