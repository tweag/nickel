import React from "react"
import Loadable from "@loadable/component";

/**
 * Wrapper around the Playground component to use it on the client side only (and not via server-side rendering).
 * This is made necessary by the code editor, based on react-ace and ace-builds, which use `window` and don't seem to fully support SSR yet.
 * @type {React.ForwardRefExoticComponent<React.PropsWithoutRef<{}> & React.RefAttributes<unknown>>}
 */
const LoadablePlayground = Loadable(() => import("./playground/playground"));
export default LoadablePlayground