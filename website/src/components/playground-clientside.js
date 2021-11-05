import Loadable from "@loadable/component";
import React, { Suspense } from 'react'
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';
import {
    faSpinner,
} from '@fortawesome/free-solid-svg-icons';

/**
 * Wrapper around the Playground component to use it on the client side only (and not via server-side rendering).
 * This is made necessary by the code editor, based on react-ace and ace-builds, which use `window` and don't seem to fully support SSR yet.
 * @type {React.ForwardRefExoticComponent<React.PropsWithoutRef<{}> & React.RefAttributes<unknown>>}
 */
const LoadablePlayground = Loadable(() => import("./playground/playground"));

export default function ClientSidePlayground(props) {
    return (
        <React.Suspense fallback={<FontAwesomeIcon icon={faSpinner} spin/>}>
            <div>
                <LoadablePlayground {...props}/>
            </div>
        </React.Suspense>
    );
}
