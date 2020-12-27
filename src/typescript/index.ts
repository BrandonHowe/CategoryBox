import {
    arc,
    asCubic,
    Circle,
    circle, line, pathFromCubics, text,
} from "@thi.ng/geom";
import { draw } from "@thi.ng/hiccup-canvas";
import {
    concat,
    invert23,
    mulV23,
    transform23,
    translation23,
    Mat23Like
} from "@thi.ng/matrices";
import { Vec } from "@thi.ng/vectors";
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
        l.shape.attribs!.weight = 1
    });

    if (target.type === MouseTargetKind.Object && !cache.dragging) {
        target.target.shape.attribs!.fill = "#999";
    }
    if (target.type === MouseTargetKind.Morphism && !cache.dragging) {
        target.target.shape.attribs!.weight = 3;
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
            const { arrowhead1, arrowhead2, shape } = getMorphismShapes(l.from, l.to, matchingIsomorphism);
            l.arrowhead1 = arrowhead1;
            l.arrowhead2 = arrowhead2;
            l.shape = shape;
        });
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

const getMorphismShapes = (from: Circle, to: Circle, isomorphismExists: boolean): Pick<MorphismGeometry, "arrowhead1" | "arrowhead2" | "shape"> => {
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
            shape
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
        const shape = pathFromCubics(asCubic(morphismArc), { weight: 1, stroke: "#000" });
        return {
            arrowhead1,
            arrowhead2,
            shape
        };
    }
};

export const createMorphism = (cache: GeometryCache, idx1: number, idx2: number, name: string): GeometryCache => {
    console.log("Creating morphism");
    const from = cache.objects[idx1];
    const to = cache.objects[idx2];
    const { arrowhead1, arrowhead2, shape } = getMorphismShapes(from.shape, to.shape, false);
    cache.morphisms.push({
        id: cache.objects.length + 1,
        from: cache.objects[idx1].shape,
        to: cache.objects[idx2]!.shape,
        name,
        arrowhead1,
        arrowhead2,
        shape
    });
    delete cache.morphismStart;
    if (cache.composing) {
        delete cache.composing;
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
                console.log("clicked AND we have a morphism")
                render(ctx)(cache);
                return () => config.getMorphismName(cache.objects.indexOf(cache.morphismStart!), cache.objects.indexOf(target.target));
            } else {
                cache.morphismStart = target.target;
            }
        } else if (target?.type === MouseTargetKind.Morphism) {
            if (cache.composing) {
                return () => config.getCompositionName(cache.morphisms.indexOf(cache.composing!), cache.morphisms.indexOf(target.target));
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
    ctx.resetTransform();
    ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height);

    const matrix = getTransform(ctx, cache);

    const objectsShapes = cache.objects.map(l => {
        const modifiedPos = [l.position[0], l.position[1] + 20];
        return [l.shape, text(modifiedPos, l.name, { fill: "#000", align: "center" })];
    }).flat();
    const morphismShapes = cache.morphisms.map(l => {
        if (l.from !== l.to) {
            const angle = (Math.atan2(l.to.pos[1] - l.from.pos[1], l.to.pos[0] - l.from.pos[0]) * (180 / Math.PI) + 360) % 360;
            const midpoint = [(l.from.pos[0] + l.to.pos[0]) / 2, (l.from.pos[1] + l.to.pos[1]) / 2];
            const modifiedAngle = (angle + 270) % 360;
            const textPos = [midpoint[0] + (20 * Math.cos(modifiedAngle * Math.PI / 360)), midpoint[1] + (20 * Math.sin(modifiedAngle * Math.PI / 360))];
            return [l.arrowhead1, l.arrowhead2, l.shape, text(textPos, l.name, { fill: "#000", align: "center", background: "#fff" })];
        } else {
            const textPos = [l.from.pos[0], l.from.pos[1] - 30];
            return [l.arrowhead1, l.arrowhead2, l.shape, text(textPos, l.name, { fill: "#000", align: "center", background: "#fff" })]
        }
    }).flat();
    const shapes = ["g", { transform: matrix }, ...objectsShapes, ...morphismShapes];

    draw(ctx, [shapes], {
        attribs: {},
        edits: []
    });

    return () => {};
};