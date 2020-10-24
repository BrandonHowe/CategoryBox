import {
    circle,
} from "@thi.ng/geom";
import { draw } from "@thi.ng/hiccup-canvas";
import {
    concat,
    invert23,
    mulV23,
    transform23,
    Mat23Like
} from "@thi.ng/matrices";
import { Vec } from "@thi.ng/vectors";
import { getMouseTarget, MouseTargetKind } from "./target";
import { GeometryCache } from "./types/Object";

const W = 300;
const H = 300;

export const emptyGeometryCache = () => ({
    objects: [{
        name: "blah",
        position: [100, 100],
        shape: circle([100, 100], 10, { fill: "black" })
    }],
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

const getEventData = (
    ctx: CanvasRenderingContext2D,
    event: MouseEvent,
    cache: GeometryCache,
): {
    mousePosition: Vec
    transform: Vec
    mouse: Vec
    target: unknown
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
 * @param config The config for what we want to return
 * @param ctx The context to re-render to.
 * @param event The event to handle.
 * @param cache The cache to mutate.
 */
export const onMouseMove = (
    ctx: CanvasRenderingContext2D,
    event: MouseEvent,
    cache: GeometryCache
): () => void => {
    const mouse = [event.pageX + W, event.pageY + H];
    const transform = getMouseTransform(ctx, cache);
    const mousePosition = mulV23(null, transform, mouse);
  
    const target = getMouseTarget(mousePosition, cache);

    cache.objects.map(l => l.shape.attribs!.fill = "#000");

    if (target.type === MouseTargetKind.Object) {
        target.target!.shape.attribs!.fill = "#999";
    }
    
    return () => {};
  
}

export const onMouseDown = (
    ctx: CanvasRenderingContext2D,
    event: MouseEvent,
    cache: GeometryCache,
): () => void => {
    const { mousePosition } = getEventData(ctx, event, cache);
    cache.objects.push({
        position: mousePosition,
        name: "blah",
        shape: circle(mousePosition, 10, { fill: "black" })
    });
    return () => {};
};

export const render = (ctx: CanvasRenderingContext2D) => (cache: GeometryCache) => {
    ctx.resetTransform();
    ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height);
    cache.objects.map(l => {
        draw(ctx, l.shape);
    });
    return () => {};
};