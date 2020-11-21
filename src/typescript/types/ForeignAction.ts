import { Fn3, Fn2, Fn } from "@thi.ng/api"

// Those are here so we can do purescript interop properly
export type ForeignAction = { readonly foreignAction: unique symbol }

export interface ForeignActionConfig {
    createObject: Fn3<number, number, string, ForeignAction>;
    getObjectName: Fn2<number, number, ForeignAction>;
    createMorphism: Fn3<number, number, string, ForeignAction>;
    getMorphismName: Fn2<number, number, ForeignAction>;
    composeMorphisms: Fn3<number, number, string, ForeignAction>;
    getCompositionName: Fn2<number, number, ForeignAction>;
    startMorphism: Fn<number, ForeignAction>;
    startDragging: Fn<number, ForeignAction>;
    startComposing: Fn<number, ForeignAction>;
    stopDragging: ForeignAction;
    nothing: ForeignAction;
}