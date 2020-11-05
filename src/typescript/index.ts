import {
    arc,
    circle, line,
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
import { GeometryCache } from "./types/Object";

const W = 300;
const H = 300;

export const emptyGeometryCache = (): GeometryCache => ({
    objects: [{
        name: "blah",
        position: [100, 100],
        shape: circle([100, 100], 10, { fill: "black" }),
        id: 1
    }],
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

    if (target.type === MouseTargetKind.Object && !cache.dragging) {
        target.target!.shape.attribs!.fill = "#999";
    }

    if (cache.mouseDown && !cache.dragging && target) {
        return () => config.startDragging(cache.objects.indexOf(target.target!));
    }

    if (cache.dragging) {
        cache.dragging.position = mouse;
        cache.dragging.shape.pos = mouse;
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

export const createMorphism = (cache: GeometryCache, idx1: number, idx2: number): GeometryCache => {
    console.log(`Creating morphism: ${idx1}|${idx2}`);
    cache.morphisms.push({
        id: cache.objects.length + 1,
        from: cache.objects[idx1].shape,
        to: cache.objects[idx2]!.shape,
        name
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
    if (target.type === MouseTargetKind.Nothing) {
        console.log("We are creating an object on the TS side.");
        render(ctx)(cache);
        return () => config.createObject(mousePosition[0], mousePosition[1]);
    } else if (target.type === MouseTargetKind.Object) {
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
    if (cache.mouseDown && target && !cache.dragging) {
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
    });
    cache.morphisms.map(l => {
        if (l.from !== l.to) {
            const angle = (Math.atan2(l.to.pos[1] - l.from.pos[1], l.to.pos[0] - l.from.pos[0]) * (180 / Math.PI) + 360) % 360;
            const modifiedYDist = Math.sin(angle * Math.PI / 180) * 20;
            const modifiedXDist = Math.cos(angle * Math.PI / 180) * 20;
            const newEndpoint = [l.to.pos[0] - modifiedXDist, l.to.pos[1] - modifiedYDist];
            const arrowheadPoint1 = [newEndpoint[0] - Math.cos((angle + 45) * Math.PI / 180) * 10, newEndpoint[1] - Math.sin((angle + 45) * Math.PI / 180) * 10];
            const arrowheadPoint2 = [newEndpoint[0] - Math.cos((angle - 45) * Math.PI / 180) * 10, newEndpoint[1] - Math.sin((angle - 45) * Math.PI / 180) * 10];
            draw(ctx, line(newEndpoint, arrowheadPoint1));
            draw(ctx, line(newEndpoint, arrowheadPoint2));
            draw(ctx, line([l.from.pos[0] + modifiedXDist, l.from.pos[1] + modifiedYDist], newEndpoint));
        }
    });
    return () => {};
};