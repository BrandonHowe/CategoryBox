import { dist, Vec } from '@thi.ng/vectors';
import { GeometryCache } from '../types/Object';

export const objectWithinRadius = (
    mousePosition: Vec,
    radius: number,
    cache: GeometryCache
) => {
    console.log(mousePosition);
    const distanceToMouse = (position: Vec) => dist(mousePosition, position);

    return cache.objects.reduce((acc, cur) => acc = distanceToMouse(cur.position) < radius, false);
};