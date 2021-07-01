import React from "react"
import { Link } from "gatsby"
import PropTypes from "prop-types"
import {StaticImage} from "gatsby-plugin-image";

const Header = ({ menuLinks }) => (
    <header>
        <nav className="navbar navbar-expand-lg navbar-light bg-primary">
            <div className="container-fluid justify-content-center">
                <Link className="navbar-brand flex-fill w-100" to="/">
                    <StaticImage className={"logo-navbar"} src="../images/nickel-logo-2.svg" alt="logo"/><span className="nickel">Nickel</span>
                </Link>
                <button className="navbar-toggler" type="button" data-bs-toggle="collapse"
                        data-bs-target="#navbarNavAltMarkup"
                        aria-controls="navbarNavAltMarkup" aria-expanded="false" aria-label="Toggle navigation">
                    <span className="navbar-toggler-icon"/>
                </button>
                <div className="collapse navbar-collapse justify-content-center flex-fill w-100" id="navbarNavAltMarkup">
                    <div className="navbar-nav">
                        {menuLinks.map(link => (
                            <Link key={link.name} className="nav-link" activeClassName="active" to={link.link}>{link.name}</Link>
                        ))}
                    </div>
                </div>
                <ul className={'nav navbar-nav w-100 justify-contend-end'}/>
            </div>
        </nav>
    </header>
);

Header.propTypes = {
    siteTitle: PropTypes.string,
};

Header.defaultProps = {
    siteTitle: `Nickel`,
};

export default Header