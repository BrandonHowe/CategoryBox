import { dist, Vec } from '@thi.ng/vectors';
import { GeometryCache } from '../types/Object';

export const mouseNearTarget = (
    mousePosition: Vec,
    cache: GeometryCache
) => {
    const distanceToMouse = (position: Vec) => dist(mousePosition, position);

    return cache.objects.reduce((acc, cur) => acc = distanceToMouse(cur.position) < 20, false);
};