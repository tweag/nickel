import React from "react";
import { Link } from "gatsby";
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';
import {
    faGithub,
    faTwitter
} from "@fortawesome/free-brands-svg-icons";
import {
    faArrowUp,
    faComments
} from '@fortawesome/free-solid-svg-icons';

const Footer = () => (
    <footer className="bg-secondary text-center">
        <div className="container pt-3">
            <section>
                <Link className="btn btn-outline-light btn-floating m-1" to="https://twitter.com/nickel_lang" role="button"
                ><FontAwesomeIcon color="black" icon={faTwitter}/></Link>

                <Link className="btn btn-outline-light btn-floating m-1" to="https://github.com/tweag/nickel" role="button"
                ><FontAwesomeIcon color="black" icon={faGithub}/></Link>

                <Link className="btn btn-outline-light btn-floating m-1" to="https://github.com/tweag/nickel/discussions" role="button"
                ><FontAwesomeIcon color="black" icon={faComments}/></Link>

                <Link className="btn btn-outline-light btn-floating m-1 ml-4" to="#" role="button"
                ><FontAwesomeIcon color="black" icon={faArrowUp}/></Link>
            </section>
        </div>
        <hr/>
        <div className="text-center pb-3">
            © 2021 Copyright: <Link to={'https://github.com/tweag/nickel/graphs/contributors'} className={'link-footer'}>Nickel contributors</Link>
        </div>
    </footer>
)

export default Footer
