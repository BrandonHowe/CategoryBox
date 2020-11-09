import { closestPoint, pointInside } from "@thi.ng/geom";
import { Vec, dist } from "@thi.ng/vectors"
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
    const morphisms = [...cache.morphisms];

    {
        const closestObject = findLast(objects, node => pointInside(node.shape, mousePosition));

        if (closestObject) {
            return {
                type: MouseTargetKind.Object,
                target: closestObject
            }
        }
    }
    {
        const closestMorphism = minBy((a, b) => distanceToMouse(a.closest) < distanceToMouse(b.closest), morphisms.map(l => ({ geometry: l, closest: closestPoint(l.shape, mousePosition)! })));

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