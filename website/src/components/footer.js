import React from "react";
import { Link } from "gatsby";
import PropTypes from "prop-types";
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
    <footer className="bg-secondary text-center text-white">
        <div className="container pt-3">
            <section>
                <Link className="btn btn-outline-light btn-floating m-1" to="https://twitter.com/nickel_lang" role="button"
                ><FontAwesomeIcon icon={faTwitter}/></Link>

                <Link className="btn btn-outline-light btn-floating m-1" to="https://github.com/tweag/nickel" role="button"
                ><FontAwesomeIcon icon={faGithub}/></Link>

                <Link className="btn btn-outline-light btn-floating m-1" to="https://github.com/tweag/nickel/discussions" role="button"
                ><FontAwesomeIcon icon={faComments}/></Link>

                <Link className="btn btn-outline-light btn-floating m-1 ml-4" to="#" role="button"
                ><FontAwesomeIcon icon={faArrowUp}/></Link>
            </section>
        </div>
        <hr/>
        <div className="text-center pb-3">
            © 2021 Copyright:
            Nickel contributors
        </div>
    </footer>
)

export default Footer
