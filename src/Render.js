const render = require("../../src/typescript/index.ts");

exports.renderCanvas = render.render;
exports.handleMouseUpImpl = render.onMouseUp;
exports.handleMouseDownImpl = render.onMouseDown;
exports.handleMouseMoveImpl = render.onMouseMove;
exports.emptyGeometryCache = render.emptyGeometryCache;

// Scale a canvas to its bounding box
exports.resizeCanvas = canvas => () => {
    const { width, height } = canvas.getBoundingClientRect();
  
    canvas.width = width;
    canvas.height = height;
};

// To be able to get contexts from purescript
exports.getContext = canvas => () => canvas.getContext("2d");