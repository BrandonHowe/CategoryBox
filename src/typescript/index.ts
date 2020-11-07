import {
    arc,
    Circle,
    circle, line, text,
} from "@thi.ng/geom";
import { draw } from "@thi.ng/hiccup-canvas";
import {
    concat,
    invert23,
    mulV23,
    transform23,
    Mat23Like
} from "@thi.ng/matrices";
import { sin, Vec } from "@thi.ng/vectors";
import { getMouseTarget, MouseTarget, MouseTargetKind } from "./target";
import { ForeignAction, ForeignActionConfig } from "./types/ForeignAction";
import { MorphismGeometry } from './types/Morphism';
import { GeometryCache, ObjectGeometry } from "./types/Object";

const W = 300;
const H = 300;

export const emptyGeometryCache = (): GeometryCache => ({
    objects: [],
    morphisms: [],
    camera: transform23(null, [0, 0], 0, 1) as Mat23Like,
    mouseDown: false
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

const getEventData = (
    ctx: CanvasRenderingContext2D,
    event: MouseEvent,
    cache: GeometryCache,
): {
    mousePosition: Vec
    transform: Vec
    mouse: Vec
    target: MouseTarget
} => {
    const mouse = [event.pageX + W, event.pageY + H];
    const transform = getMouseTransform(ctx, cache);
    const mousePosition = mulV23(null, transform, mouse);
    const target = getMouseTarget(mousePosition, cache);
    return { mousePosition, transform, mouse, target };
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
    const mouse = [event.pageX + W, event.pageY + H];
    const transform = getMouseTransform(ctx, cache);
    const mousePosition = mulV23(null, transform, mouse);
  
    const target = getMouseTarget(mousePosition, cache);

    cache.objects.map(l => l.shape.attribs!.fill = "#000");

    if (target && target.type === MouseTargetKind.Object && !cache.dragging) {
        target.target.shape.attribs!.fill = "#999";
    }

    if (cache.mouseDown && !cache.dragging && target.type === MouseTargetKind.Object) {
        return () => config.startDragging(cache.objects.indexOf(target.target));
    }

    if (cache.dragging) {
        cache.dragging.position = mouse;
        cache.dragging.shape.pos = mouse;
        cache.morphisms.map(l => {
            const { arrowhead1, arrowhead2, shape } = getMorphismShapes(l.from, l.to);
            l.arrowhead1 = arrowhead1;
            l.arrowhead2 = arrowhead2;
            l.shape = shape;
        })
    }
    
    return () => config.nothing;
}

export const createObject = (cache: GeometryCache, posX: number, posY: number, name: string): GeometryCache => {
    cache.objects.push({
        id: cache.objects.length + 1,
        position: [posX, posY],
        name: prompt("What is the name of this object?") || "",
        shape: circle([posX, posY], 10, { fill: "black" })
    });
    return cache;
}

const getMorphismShapes = (from: Circle, to: Circle): Pick<MorphismGeometry, "arrowhead1" | "arrowhead2" | "shape"> => {
    const angle = (Math.atan2(to.pos[1] - from.pos[1], to.pos[0] - from.pos[0]) * (180 / Math.PI) + 360) % 360;
    const modifiedYDist = Math.sin(angle * Math.PI / 180) * 20;
    const modifiedXDist = Math.cos(angle * Math.PI / 180) * 20;
    const newEndpoint = [to.pos[0] - modifiedXDist, to.pos[1] - modifiedYDist];
    const arrowheadPoint1 = [newEndpoint[0] - Math.cos((angle + 45) * Math.PI / 180) * 10, newEndpoint[1] - Math.sin((angle + 45) * Math.PI / 180) * 10];
    const arrowheadPoint2 = [newEndpoint[0] - Math.cos((angle - 45) * Math.PI / 180) * 10, newEndpoint[1] - Math.sin((angle - 45) * Math.PI / 180) * 10];
    const arrowhead1 = line(newEndpoint, arrowheadPoint1);
    const arrowhead2 = line(newEndpoint, arrowheadPoint2);
    const shape = line([from.pos[0] + modifiedXDist, from.pos[1] + modifiedYDist], newEndpoint);
    return {
        arrowhead1,
        arrowhead2,
        shape
    };
};

export const createMorphism = (cache: GeometryCache, idx1: number, idx2: number): GeometryCache => {
    console.log(`Creating morphism: ${idx1}|${idx2}`);
    const from = cache.objects[idx1];
    const to = cache.objects[idx2];
    const name = prompt("What is the name of this morphism?") || "";
    const { arrowhead1, arrowhead2, shape } = getMorphismShapes(from.shape, to.shape);
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
    return cache;
}

export const startMorphism = (cache: GeometryCache, idx: number): GeometryCache => {
    cache.morphismStart = cache.objects[idx];
    return cache;
};

export const startDragging = (cache: GeometryCache, idx: number): GeometryCache => {
    console.log("We called a start dragging boyzzz");
    cache.dragging = cache.objects[idx];
    return cache;
};

export const stopDragging = (cache: GeometryCache): GeometryCache => {
    cache.mouseDown = false;
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
    const { mousePosition, target } = getEventData(ctx, event, cache);
    if (target?.type === MouseTargetKind.Nothing) {
        console.log("We are creating an object on the TS side.");
        render(ctx)(cache);
        return () => config.createObject(mousePosition[0], mousePosition[1], "blah");
    } else if (target?.type === MouseTargetKind.Object) {
        if (cache.morphismStart) {
            render(ctx)(cache);
            return () => config.createMorphism(cache.objects.indexOf(cache.morphismStart!), cache.objects.indexOf(target.target!));
        } else {
            cache.mouseDown = true;
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
    const { target } = getEventData(ctx, event, cache);
    if (cache.mouseDown && target.type === MouseTargetKind.Object && !cache.dragging) {
        target.target!.shape.attribs!.fill = "#f00";
        cache.mouseDown = false;
        return () => config.startMorphism(cache.objects.indexOf(target.target!));
    }
    if (cache.dragging) {
        return () => config.stopDragging;
    }
    cache.mouseDown = false;
    render(ctx)(cache);
    return () => config.nothing;
};

export const render = (ctx: CanvasRenderingContext2D) => (cache: GeometryCache) => {
    ctx.resetTransform();
    ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height);
    cache.objects.map(l => {
        draw(ctx, l.shape);
        const modifiedPos = [l.position[0], l.position[1] + 20];
        draw(ctx, text(modifiedPos, l.name, { fill: "#000", align: "center" }));
    });
    cache.morphisms.map(l => {
        if (l.from !== l.to) {
            const angle = (Math.atan2(l.to.pos[1] - l.from.pos[1], l.to.pos[0] - l.from.pos[0]) * (180 / Math.PI) + 360) % 360;
            const midpoint = [(l.from.pos[0] + l.to.pos[0]) / 2, (l.from.pos[1] + l.to.pos[1]) / 2];
            const modifiedAngle = (angle + 270) % 360;
            const textPos = [midpoint[0] + (20 * Math.cos(modifiedAngle * Math.PI / 360)), midpoint[1] + (20 * Math.sin(modifiedAngle * Math.PI / 360))];
            draw(ctx, l.arrowhead1);
            draw(ctx, l.arrowhead2);
            draw(ctx, l.shape);
            draw(ctx, text(textPos, l.name, { fill: "#000", align: "center", background: "#fff" }));
        }
    });
    return () => {};
};