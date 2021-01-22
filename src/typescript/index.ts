import {
    arc,
    asCubic,
    Circle,
    circle, Line, line, Path, pathFromCubics, text, Text as GText, Arc
} from "@thi.ng/geom";
import { fromEndPoints } from "@thi.ng/geom-arc"
import { draw } from "@thi.ng/hiccup-canvas";
import {
    concat,
    invert23,
    mulV23,
    transform23,
    translation23,
    Mat23Like
} from "@thi.ng/matrices";
import { dist, Vec } from "@thi.ng/vectors";
import { getArcMidpointOffset, getTangentAtArcAngle } from "./helpers/arc";
import { objectWithinRadius } from './helpers/objectWithinRadius';
import { isPressed, MouseButtons } from './mouse';
import { getAllTargets, getMouseTarget, MouseTarget, MouseTargetKind } from "./target";
import { ForeignAction, ForeignActionConfig } from "./types/ForeignAction";
import { MorphismGeometry } from './types/Morphism';
import { GeometryCache, ObjectGeometry } from "./types/Object";

const W = window.innerWidth / 2 - 50;
const H = window.innerHeight / 2 - 3;

export const emptyGeometryCache = (): GeometryCache => ({
    objects: [],
    morphisms: [],
    naturalMorphisms: new Map(),
    morphisms2: [],
    camera: transform23(null, [0, 0], 0, 1) as Mat23Like
});

const getMouseTransform = (
    ctx: CanvasRenderingContext2D,
    cache: GeometryCache
) => {
    const bounds = ctx.canvas.getBoundingClientRect();

    const transform = transform23(
        null,
        [-bounds.left - bounds.width / 2, -bounds.height / 2],
        0,
        1
    );

    concat(null, transform, invert23([], cache.camera)!);

    return transform;
};

/**
 * Returns a transform matrix moving the canvas by half it's size
 *
 * @param ctx The canvas rendering context to get the middle of.
 * @param cache The cache to get the camera from.
 */
const getTransform = (ctx: CanvasRenderingContext2D, cache: GeometryCache) => {
    const transform = transform23(
        null,
        [ctx.canvas.width / 2, ctx.canvas.height / 2],
        0,
        1
    );
  
    concat(null, transform, cache.camera);
  
    return transform;
};

const getMouseEventData = (
    ctx: CanvasRenderingContext2D,
    event: MouseEvent,
    cache: GeometryCache,
): {
    mousePosition: Vec
    transform: Vec
    mouse: Vec
    target: MouseTarget
} => {
    const mouse = [event.pageX, event.pageY];
    const transform = getMouseTransform(ctx, cache);
    const mousePosition = mulV23(null, transform, mouse);
    const target = getMouseTarget(mousePosition, cache);
    return { mousePosition, transform, mouse, target };
};

const getWheelEventData = (
    ctx: CanvasRenderingContext2D,
    event: WheelEvent,
    cache: GeometryCache,
): Pick<WheelEvent, "deltaMode" | "deltaX" | "deltaY" | "deltaZ"> => {
    return { deltaMode: event.deltaMode, deltaX: event.deltaX, deltaY: event.deltaY, deltaZ: event.deltaZ };
};

/**
 * Pan the camera by a certain amount.
 *
 * @param cache The cache to mutate.
 * @param offset The amount to move the camera by.
 */
const pan = (cache: GeometryCache, offset: Vec) => {
    offset[0] /= cache.camera[0];
    offset[1] /= cache.camera[3];
    concat(null, cache.camera, translation23([], offset));
};


/**
 * Zoom the camera by a certain amount.
 *
 * @param cache The cache to mutate.
 * @param offset The amount to zoom the camera by.
 */
const zoom = (ctx: CanvasRenderingContext2D, cache: GeometryCache, offset: number) => {
    const transform = transform23(
        null,
        [ctx.canvas.width / 2, ctx.canvas.height / 2],
        0,
        1
    );
    cache.camera[0] += offset;
    cache.camera[3] += offset;
};

/**
 * Handle a mouseMove event
 *
 * @param ctx The context to re-render to.
 * @param event The event to handle.
 * @param cache The cache to mutate.
 */
export const onMouseMove = (
    config: ForeignActionConfig,
    ctx: CanvasRenderingContext2D,
    event: MouseEvent,
    cache: GeometryCache
): () => ForeignAction => {
    const mouse = [event.pageX, event.pageY];
    const transform = getMouseTransform(ctx, cache);
    const mousePosition = mulV23(null, transform, mouse);

    const pressed = isPressed(event.buttons)
  
    const target = getMouseTarget(mousePosition, cache);

    cache.objects.map(l => {
        if (cache.morphismStart !== l) { 
            l.shape.attribs!.fill = "#000";
        }
    });
    cache.morphisms.map(l => {
        l.arrowhead1.attribs!.weight = 1
        l.arrowhead2.attribs!.weight = 1
        if (l.shape instanceof Line) {
            l.shape.attribs!.weight = 1;
        } else if (l.shape instanceof Arc) {
            l.arcAttributes!.weight = 1;
        }
    });
    
    cache.naturalMorphisms.forEach(l => {
        l.map(j => {
            j.arrowhead1.attribs!.weight = 1;
            j.arrowhead2.attribs!.weight = 1;
            if (j.shape instanceof Line) {
                j.shape.attribs!.weight = 1;
            } else if (j.shape instanceof Arc) {
                j.arcAttributes!.weight = 1;
            }
        });
    });

    if (target.type === MouseTargetKind.Object && !cache.dragging) {
        target.target.shape.attribs!.fill = "#999";
    }
    if (target.type === MouseTargetKind.Morphism && !cache.dragging) {
        console.log("Hovering over a morphism");
        if (target.target.shape instanceof Line) {
            target.target.shape.attribs!.weight = 3;
        } else if (target.target.shape instanceof Arc) {
            console.log("Hovering over an arc cuh");
            target.target.arcAttributes!.weight = 3;
        }
        target.target.arrowhead1.attribs!.weight = 3;
        target.target.arrowhead2.attribs!.weight = 3;
    }

    if (pressed(MouseButtons.LeftButton) && !cache.dragging && target.type === MouseTargetKind.Object) {
        return () => config.startDragging(cache.objects.indexOf(target.target));
    } else if (pressed(MouseButtons.RightButton)) {
        const mouseScreenOffset = [event.movementX, event.movementY]
        pan(cache, mouseScreenOffset);
    }

    if (cache.dragging) {
        const targets = getAllTargets(mouse, cache);
        cache.morphismStart = undefined;
        if (target.type === MouseTargetKind.Object && targets.some(l => l.type === MouseTargetKind.Object && l.target !== cache.dragging)) {
            const nonDraggingTarget = targets.find(l => l.type === MouseTargetKind.Object && l.target !== cache.dragging) as { type: MouseTargetKind.Object, target: ObjectGeometry };
            const angleRad = Math.atan2(mouse[1] - nonDraggingTarget.target.position[1], mouse[0] - nonDraggingTarget.target.position[0]);
            const distY = 20 * Math.sin(angleRad);
            const distX = 20 * Math.cos(angleRad);
            const newPoint = [nonDraggingTarget.target.position[0] + distX, nonDraggingTarget.target.position[1] + distY];
            cache.dragging.position = newPoint;
            cache.dragging.shape.pos = newPoint;
        } else {
            cache.dragging.position = mouse;
            cache.dragging.shape.pos = mouse;
        }
        cache.morphisms.map(l => {
            const matchingIsomorphism = cache.morphisms.some(j => j.from === l.to && j.to === l.from);
            const { arrowhead1, arrowhead2, shape, text } = getMorphismShapes(l.from, l.to, matchingIsomorphism, l.name);
            l.arrowhead1 = arrowhead1;
            l.arrowhead2 = arrowhead2;
            l.shape = shape;
            l.text = text;
        });
        const matchingNaturals = [...cache.naturalMorphisms.entries()].find(l => l[0].from === cache.dragging!.shape || l[0].to === cache.dragging!.shape);
        if (matchingNaturals) {
            cache.naturalMorphisms.set(matchingNaturals[0], getNaturalMorphismShapes(matchingNaturals[1].map(l => ({ from: l.from, to: l.to, name: l.name }))));
        }
    }

    return () => config.nothing;
}

export const createObject = (cache: GeometryCache, posX: number, posY: number, name: string): GeometryCache => {
    cache.objects.push({
        id: cache.objects.length + 1,
        position: [posX, posY],
        name,
        shape: circle([posX, posY], 10, { fill: "black" })
    });
    return cache;
}

const getTextLocation = (l: Pick<MorphismGeometry, "from" | "to" | "name">, attribs = { fill: "#000", align: "center", background: "#fff" }) => {
    const angle = (Math.atan2(l.to.pos[1] - l.from.pos[1], l.to.pos[0] - l.from.pos[0]) * (180 / Math.PI) + 360) % 360;
    const midpoint = [(l.from.pos[0] + l.to.pos[0]) / 2, (l.from.pos[1] + l.to.pos[1]) / 2];
    const modifiedAngle = (angle + 270) % 360;
    const textPos = [midpoint[0] + (20 * Math.cos(modifiedAngle * Math.PI / 360)), midpoint[1] + (20 * Math.sin(modifiedAngle * Math.PI / 360))];
    return text(textPos, l.name, attribs);
};

const getMorphismShapes = (from: Circle, to: Circle, isomorphismExists: boolean, name: string): Pick<MorphismGeometry, "arrowhead1" | "arrowhead2" | "shape" | "text"> => {
    if (from !== to) {
        const angle = (Math.atan2(to.pos[1] - from.pos[1], to.pos[0] - from.pos[0]) * (180 / Math.PI) + 360) % 360;
        const modifiedYDist = Math.sin(angle * Math.PI / 180) * 20;
        const modifiedXDist = Math.cos(angle * Math.PI / 180) * 20;
        const isomorphismModifierX = isomorphismExists ? Math.cos(((90 + angle) % 360) * Math.PI / 180) * 8 : 0;
        const isomorphismModifierY = isomorphismExists ? Math.sin(((90 + angle) % 360) * Math.PI / 180) * 8 : 0;
        const newEndpoint = [to.pos[0] - modifiedXDist + isomorphismModifierX, to.pos[1] - modifiedYDist + isomorphismModifierY];
        const arrowheadPoint1 = [newEndpoint[0] - Math.cos((angle + 45) * Math.PI / 180) * 10, newEndpoint[1] - Math.sin((angle + 45) * Math.PI / 180) * 10];
        const arrowheadPoint2 = [newEndpoint[0] - Math.cos((angle - 45) * Math.PI / 180) * 10, newEndpoint[1] - Math.sin((angle - 45) * Math.PI / 180) * 10];
        const arrowhead1 = line(newEndpoint, arrowheadPoint1, {});
        const arrowhead2 = line(newEndpoint, arrowheadPoint2, {});
        const shape = line([from.pos[0] + modifiedXDist + isomorphismModifierX, from.pos[1] + modifiedYDist + isomorphismModifierY], newEndpoint, { weight: 1 });
        return {
            arrowhead1,
            arrowhead2,
            shape,
            text: getTextLocation({ from, to, name })
        };
    } else {
        const morphismArc = arc([from.pos[0], from.pos[1] - 15], 10, Math.PI / 2 + 1, 0, Math.PI * 2 - 2);
        const radiusLength = [10 * Math.cos(Math.PI / 2 - 1), 10 * Math.sin(Math.PI / 2 - 1)];
        const endpoint = [from.pos[0] + radiusLength[0], from.pos[1] - 15 + radiusLength[1]];
        const rotationFactor = Math.PI * 2 - 2 - Math.PI / 4;
        const arrowheadPoint1 = [endpoint[0] + 8 * Math.cos(Math.PI - 1 + rotationFactor), endpoint[1] + 8 * Math.sin(Math.PI - 1 + rotationFactor)];
        const arrowheadPoint2 = [endpoint[0] + 8 * Math.cos(Math.PI / 2 - 1 + rotationFactor), endpoint[1] + 8 * Math.sin(Math.PI / 2 - 1 + rotationFactor)];
        const arrowhead1 = line(endpoint, arrowheadPoint1, { weight: 1, stroke: "#000" });
        const arrowhead2 = line(endpoint, arrowheadPoint2, { weight: 1, stroke: "#000" });
        const shape = morphismArc;
        const textPos = [from.pos[0], from.pos[1] - 30];
        return {
            arrowhead1,
            arrowhead2,
            shape,
            text: text(textPos, name, { fill: "#000", align: "center", background: "#fff" })
        };
    }
};

export const filterNaturalMorphisms = (morphisms: MorphismGeometry[]) => {
    const naturals: Map<{from: Circle, to: Circle}, string[]> = new Map();
    for (const morphism of morphisms) {
        if (morphisms.filter(l => l.to === morphism.to && l.from === morphism.from).length >= 2) {
            const matchingNatural = [...naturals.keys()].find(l => l.to === morphism.to && l.from === morphism.from);
            if (matchingNatural) {
                const matchingArr = naturals.get(matchingNatural)!;
                naturals.set(matchingNatural, matchingArr.length % 2 === 0 ? [morphism.name, ...matchingArr] : [...matchingArr, morphism.name]);
            } else {
                naturals.set({ from: morphism.from, to: morphism.to }, [morphism.name]);
            }
        }
    }
    return {
        duplicates: [...naturals.entries()].map(l => l[1].map(j => ({ ...l[0], name: j }))),
        regulars: morphisms.filter(morphism => morphisms.filter(l => l.to === morphism.to && l.from === morphism.from).length <= 1)
    };
};

export const getNaturalMorphismShapes = (morphisms: { from: Circle, to: Circle, name: string }[]): MorphismGeometry[] => {
    return morphisms.map((l, idx): MorphismGeometry => {
        const currIdx = idx - Math.ceil(morphisms.length / 2);
        const toPos = l.from.pos;
        const fromPos = l.to.pos;
        const distBetweenPos = dist(toPos, fromPos);
        if (currIdx >= 0) {
            const templateAngle = Math.atan2(toPos[1] - fromPos[1], toPos[0] - fromPos[0]);
            const templateArcData = fromEndPoints(fromPos, toPos, [distBetweenPos, (currIdx + 1) * 300], templateAngle)!;
            const offsetStartAngle = Math.atan2(getTangentAtArcAngle(arc(templateArcData.center!, templateArcData.r, templateArcData.axis, templateArcData.start, templateArcData.end), templateArcData.start), 1);
            const offsetEndAngle = Math.atan2(getTangentAtArcAngle(arc(templateArcData.center!, templateArcData.r, templateArcData.axis, templateArcData.start, templateArcData.end), templateArcData.end), 1);
            const startCoords = [Math.cos, Math.sin].map(l => l(offsetStartAngle + (templateAngle + Math.PI) % (Math.PI * 2)) * 15).map((j, idx) => fromPos[idx] - j);
            const endCoords = [Math.cos, Math.sin].map(l => l(offsetEndAngle + (templateAngle + Math.PI) % (Math.PI * 2)) * 15).map((j, idx) => toPos[idx] + j);
            const arcData = fromEndPoints(startCoords, endCoords, [dist(startCoords, endCoords), (currIdx + 1) * 300], templateAngle)!;
            const shape = arc(arcData.center!, arcData.r, arcData.axis, arcData.start, arcData.end);
            const newStartAngle = Math.atan2(getTangentAtArcAngle(arc(arcData.center!, arcData.r, arcData.axis, arcData.start, arcData.end), arcData.start), 1) + templateAngle % (Math.PI * 2);
            const arrowhead1Coords = [startCoords[0] + Math.cos(newStartAngle + (7 * Math.PI / 4) % (2 * Math.PI)) * 10, startCoords[1] + Math.sin(newStartAngle + (7 * Math.PI / 4) % (2 * Math.PI)) * 10];
            const arrowhead2Coords = [startCoords[0] + Math.cos(newStartAngle + (1 * Math.PI / 4) % (2 * Math.PI)) * 10, startCoords[1] + Math.sin(newStartAngle + (1 * Math.PI / 4) % (2 * Math.PI)) * 10];
            const arrowhead1 = line(startCoords, arrowhead1Coords, { weight: 1, stroke: "#000" });
            const arrowhead2 = line(startCoords, arrowhead2Coords, { weight: 1, stroke: "#000" });
            const textPos = getArcMidpointOffset(arc(arcData.center!, arcData.r, arcData.axis, arcData.start, arcData.end), -10);
            return {
                id: 0,
                from: l.from,
                to: l.to,
                shape,
                arrowhead1,
                arrowhead2,
                name: l.name,
                arcAttributes: { weight: 1, stroke: "#000" },
                text: text(textPos, l.name, { fill: "#000", align: "center", background: "#fff" })
            }
        } else if (currIdx === -1) {
            const templateAngle = Math.atan2(toPos[1] - fromPos[1], toPos[0] - fromPos[0]);
            const startCoords = [Math.cos, Math.sin].map(l => l(templateAngle) * 15).map((j, idx) => fromPos[idx] + j);
            const endCoords = [Math.cos, Math.sin].map(l => l((templateAngle + Math.PI) % (Math.PI * 2)) * 15).map((j, idx) => toPos[idx] + j);
            const shape = line(startCoords, endCoords, { weight: 1, stroke: "#000" });
            const arrowhead1Coords = [startCoords[0] + Math.cos(templateAngle + (7 * Math.PI / 4) % (2 * Math.PI)) * 10, startCoords[1] + Math.sin(templateAngle + (7 * Math.PI / 4) % (2 * Math.PI)) * 10];
            const arrowhead2Coords = [startCoords[0] + Math.cos(templateAngle + (1 * Math.PI / 4) % (2 * Math.PI)) * 10, startCoords[1] + Math.sin(templateAngle + (1 * Math.PI / 4) % (2 * Math.PI)) * 10];
            const arrowhead1 = line(startCoords, arrowhead1Coords, { weight: 1, stroke: "#000" });
            const arrowhead2 = line(startCoords, arrowhead2Coords, { weight: 1, stroke: "#000" });
            return {
                id: 0,
                from: l.from,
                to: l.to,
                shape,
                arrowhead1,
                arrowhead2,
                name: l.name,
                text: getTextLocation(l)
            }
        } else {
            const templateAngle = Math.atan2(toPos[1] - fromPos[1], toPos[0] - fromPos[0]); 
            const templateArcData = fromEndPoints(fromPos, toPos, [distBetweenPos, (currIdx * -1 - 1) * 300], templateAngle)!;
            const offsetStartAngle = Math.atan2(getTangentAtArcAngle(arc(templateArcData.center!, templateArcData.r, templateArcData.axis, templateArcData.start, templateArcData.end), templateArcData.start), 1) * -1;
            const offsetEndAngle = Math.atan2(getTangentAtArcAngle(arc(templateArcData.center!, templateArcData.r, templateArcData.axis, templateArcData.start, templateArcData.end), templateArcData.end), 1) * -1;
            const startCoords = [Math.cos, Math.sin].map(l => l(offsetStartAngle + (templateAngle + Math.PI) % (Math.PI * 2)) * 15).map((j, idx) => fromPos[idx] - j);
            const endCoords = [Math.cos, Math.sin].map(l => l(offsetEndAngle + (templateAngle + Math.PI) % (Math.PI * 2)) * 15).map((j, idx) => toPos[idx] + j);
            const arcData = fromEndPoints(startCoords, endCoords, [dist(startCoords, endCoords), (currIdx * -1 - 1) * 300], templateAngle, false, true)!;
            const shape = arc(arcData.center!, arcData.r, arcData.axis, arcData.start, arcData.end);
            const newStartAngle = Math.atan2(getTangentAtArcAngle(arc(arcData.center!, arcData.r, arcData.axis, arcData.start, arcData.end), arcData.start), 1) + templateAngle % (Math.PI * 2);
            const arrowhead1Coords = [startCoords[0] + Math.cos(newStartAngle + (7 * Math.PI / 4) % (2 * Math.PI)) * 10, startCoords[1] + Math.sin(newStartAngle + (7 * Math.PI / 4) % (2 * Math.PI)) * 10];
            const arrowhead2Coords = [startCoords[0] + Math.cos(newStartAngle + (1 * Math.PI / 4) % (2 * Math.PI)) * 10, startCoords[1] + Math.sin(newStartAngle + (1 * Math.PI / 4) % (2 * Math.PI)) * 10];
            const arrowhead1 = line(startCoords, arrowhead1Coords, { weight: 1, stroke: "#000" });
            const arrowhead2 = line(startCoords, arrowhead2Coords, { weight: 1, stroke: "#000" });
            const textPos = getArcMidpointOffset(arc(arcData.center!, arcData.r, arcData.axis, arcData.start, arcData.end), -10);
            return {
                id: 0,
                from: l.from,
                to: l.to,
                shape,
                arrowhead1,
                arrowhead2,
                name: l.name,
                arcAttributes: { weight: 1, stroke: "#000" },
                text: text(textPos, l.name, { fill: "#000", align: "center", background: "#fff" })
            }
        }
    });
};

// const getMorphism2Shapes = (from: MorphismGeometry, to: MorphismGeometry): Pick<MorphismGeometry, "arrowhead1" | "arrowhead2" | "shape"> => {
//     if (from !== to) {
//         const midpoint1 = getMidpointOfMorphism(from);
//         const midpoint2 = getMidpointOfMorphism(to);
//         const angle = (Math.atan2(midpoint2[1] - midpoint1[1], midpoint2[0] - midpoint1[0]) * (180 / Math.PI) + 360) % 360;
//         const modifiedYDist = Math.sin(angle * Math.PI / 180) * 20;
//         const modifiedXDist = Math.cos(angle * Math.PI / 180) * 20;
//         const newEndpoint = [to.pos[0] - modifiedXDist + isomorphismModifierX, to.pos[1] - modifiedYDist + isomorphismModifierY];
//         const arrowheadPoint1 = [newEndpoint[0] - Math.cos((angle + 45) * Math.PI / 180) * 10, newEndpoint[1] - Math.sin((angle + 45) * Math.PI / 180) * 10];
//         const arrowheadPoint2 = [newEndpoint[0] - Math.cos((angle - 45) * Math.PI / 180) * 10, newEndpoint[1] - Math.sin((angle - 45) * Math.PI / 180) * 10];
//         const arrowhead1 = line(newEndpoint, arrowheadPoint1, {});
//         const arrowhead2 = line(newEndpoint, arrowheadPoint2, {});
//         const shape = line([from.pos[0] + modifiedXDist + isomorphismModifierX, from.pos[1] + modifiedYDist + isomorphismModifierY], newEndpoint, { weight: 1 });
//         return {
//             arrowhead1,
//             arrowhead2,
//             shape
//         };
//     }
// };

const getMidpointOfMorphism = (morphism: MorphismGeometry): [number, number] => {
    const e1 = morphism.from.pos;
    const e2 = morphism.from.pos;
    return [(e1[0] + e2[0]) / 2, (e1[1] + e2[1]) / 2] as [number, number];
};

export const createMorphism = (cache: GeometryCache, idx1: number, idx2: number, name: string): GeometryCache => {
    console.log("Creating morphism");
    const from = cache.objects[idx1];
    const to = cache.objects[idx2];
    const { arrowhead1, arrowhead2, shape, text } = getMorphismShapes(from.shape, to.shape, false, name);
    cache.morphisms.push({
        id: cache.morphisms.length + 1,
        from: cache.objects[idx1].shape,
        to: cache.objects[idx2]!.shape,
        name,
        arrowhead1,
        arrowhead2,
        shape,
        text
    });
    const matchingNaturalArr = ([...cache.naturalMorphisms.entries()].find(l => l[0].from === from.shape && l[0].to === to.shape) || [[], [] as MorphismGeometry[]])[1];
    const filteredNaturalMorphisms = filterNaturalMorphisms([...new Set([...cache.morphisms, ...matchingNaturalArr])]);
    const matchingNaturals = filteredNaturalMorphisms.duplicates.find(l => l[0].from === from.shape && l[0].to === to.shape);
    const matchingNaturalGeoms = matchingNaturals && getNaturalMorphismShapes(matchingNaturals);
    console.log(matchingNaturalGeoms);
    delete cache.morphismStart;
    delete cache.composing;
    if (matchingNaturalGeoms) {
        cache.morphisms.pop();
        cache.morphisms = cache.morphisms.filter(l => l.from !== from.shape || l.to !== to.shape);
        for (const natMorSet of cache.naturalMorphisms.entries()) {
            if (natMorSet[0].from === from.shape && natMorSet[0].to === to.shape) {
                cache.naturalMorphisms.set(natMorSet[0], matchingNaturalGeoms);
                return cache;
            }
        }
        cache.naturalMorphisms.set({ from: from.shape, to: to.shape }, matchingNaturalGeoms);
    }
    return cache;
}

export const startMorphism = (cache: GeometryCache, idx: number): GeometryCache => {
    cache.morphismStart = cache.objects[idx];
    return cache;
};

export const startDragging = (cache: GeometryCache, idx: number): GeometryCache => {
    cache.dragging = cache.objects[idx];
    return cache;
};

export const startComposition = (cache: GeometryCache, idx: number): GeometryCache => {
    cache.composing = cache.morphisms[idx];
    return cache;
};

export const stopDragging = (cache: GeometryCache): GeometryCache => {
    delete cache.dragging;
    return cache;
};

/**
 * Handle a mouseDown event
 *
 * @param ctx The context to re-render to.
 * @param event The event to handle.
 * @param cache The cache to mutate.
 */
export const onMouseDown = (
    config: ForeignActionConfig,
    ctx: CanvasRenderingContext2D,
    event: MouseEvent,
    cache: GeometryCache,
): () => ForeignAction => {
    const { mousePosition, target } = getMouseEventData(ctx, event, cache);

    event.preventDefault();

    const pressed = isPressed(event.buttons);

    if (pressed(MouseButtons.LeftButton)) {
        if (target?.type === MouseTargetKind.Nothing && !objectWithinRadius(mousePosition, 20, cache)) {
            render(ctx)(cache);
            return () => config.getObjectName(mousePosition[0], mousePosition[1]);
        } else if (target?.type === MouseTargetKind.Object) {
            console.log("clicking an object");
            if (cache.morphismStart) {
                console.log("clicked AND we have a morphism");
                const matchingObject = cache.objects.indexOf(cache.morphismStart!);
                delete cache.morphismStart;
                render(ctx)(cache);
                return () => config.getMorphismName(matchingObject, cache.objects.indexOf(target.target));
            } else {
                cache.morphismStart = target.target;
            }
        } else if (target?.type === MouseTargetKind.Morphism) {
            if (cache.composing) {
                const matchingMorphism = cache.morphisms.indexOf(cache.composing!);
                delete cache.composing;
                return () => config.getCompositionName(matchingMorphism, cache.morphisms.indexOf(target.target));
            } else {
                cache.composing = target.target;
            }
        }
    }
    render(ctx)(cache);
    return () => config.nothing;
};

/**
 * Handle a mouseUp event
 *
 * @param ctx The context to re-render to.
 * @param event The event to handle.
 * @param cache The cache to mutate.
 */
export const onMouseUp = (
    config: ForeignActionConfig,
    ctx: CanvasRenderingContext2D,
    event: MouseEvent,
    cache: GeometryCache,
): () => ForeignAction => {
    const pressed = isPressed(event.buttons);

    const { target } = getMouseEventData(ctx, event, cache);

    if (pressed(MouseButtons.LeftButton) && target.type === MouseTargetKind.Object && !cache.dragging) {
        target.target!.shape.attribs!.fill = "#f00";
        return () => config.startMorphism(cache.objects.indexOf(target.target!));
    }
    if (cache.dragging) {
        return () => config.stopDragging;
    }
    render(ctx)(cache);
    return () => config.nothing;
};

/**
 * Handle a onScroll event
 *
 * @param ctx The context to re-render to.
 * @param event The event to handle.
 * @param cache The cache to mutate.
 */
export const onScroll = (
    config: ForeignActionConfig,
    ctx: CanvasRenderingContext2D,
    event: WheelEvent,
    cache: GeometryCache,
): () => ForeignAction => {

    return () => config.nothing;
};

export const render = (ctx: CanvasRenderingContext2D) => (cache: GeometryCache) => {
    console.log("Rendering!");
    ctx.resetTransform();
    ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height);

    const matrix = getTransform(ctx, cache);

    const objectsShapes = cache.objects.flatMap(l => {
        const modifiedPos = [l.position[0], l.position[1] + 20];
        return [l.shape, text(modifiedPos, l.name, { fill: "#000", align: "center" })];
    });

    const morphismShapes = cache.morphisms.flatMap(l => [l.shape, l.arrowhead1, l.arrowhead2, l.text]);

    const filteredNaturalMorphisms = [...cache.naturalMorphisms.values()].flat();
    const naturalMorphismShapes = filteredNaturalMorphisms.flatMap(l => [l.arrowhead1, l.arrowhead2, l.shape, l.text]);
    const naturalMorphismRenders = filteredNaturalMorphisms.map(l => l.shape instanceof Arc ? pathFromCubics(asCubic(l.shape), l.arcAttributes) : l.shape);
    const morphism2Shapes = cache.morphisms2.flatMap(l => {
        if (l.from !== l.to) {
            const midpoint1 = getMidpointOfMorphism(l.from);
            const midpoint2 = getMidpointOfMorphism(l.to);
            const angle = (Math.atan2(midpoint2[1] - midpoint1[1], midpoint2[0] - midpoint1[0]) * (180 / Math.PI) + 360) % 360;
            const modifiedAngle = (angle + 270) % 360;
            const textPos = [midpoint1[0] + (20 * Math.cos(modifiedAngle * Math.PI / 360)), midpoint1[1] + (20 * Math.sin(modifiedAngle * Math.PI / 360))];
            return [l.arrowhead1, l.arrowhead2, l.shape, l.shape2, text(textPos, l.name, { fill: "#000", align: "center", background: "#fff" })];
        }
    });
    const shapes = ["g", { transform: matrix }, ...objectsShapes, ...morphismShapes, ...naturalMorphismShapes, ...naturalMorphismRenders, ...morphism2Shapes];

    draw(ctx, [shapes], {
        attribs: {},
        edits: []
    });
    
    return () => {};
};
