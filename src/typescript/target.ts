import { pointInside } from "@thi.ng/geom";
import { Vec, dist } from "@thi.ng/vectors"
import { findLast } from "./helpers/findLast";
import { GeometryCache, ObjectGeometry } from "./types/Object"

export enum MouseTargetKind {
    Object = "object",
    Nothing = "Nothing"
}

export interface MouseTarget {
    type: MouseTargetKind;
    target?: ObjectGeometry;
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

    const nodes = [...cache.objects];

    {
        const closestObject = findLast(nodes, node => pointInside(node.shape, mousePosition));

        if (closestObject) {
            return {
                type: MouseTargetKind.Object,
                target: closestObject
            }
        }
    }

    return {
        type: MouseTargetKind.Nothing
    };
}