import React from "react"
import { Link } from "gatsby"
import {StaticImage} from "gatsby-plugin-image";

const Header = ({ menuLinks }) => (
    <header>
        <nav className="navbar navbar-expand-md navbar-light bg-primary">
            <div className="container-fluid">
                <Link className="navbar-brand flex-md-fill w-md-100" to="/">
                    <StaticImage className={"logo-navbar"} src="../images/nickel-logo-2.svg" alt="logo"/><span className="nickel">Nickel</span>
                </Link>
                <button className="navbar-toggler" type="button" data-toggle="collapse"
                        data-target="#navbarNavAltMarkup"
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
                <ul className={'d-none d-md-block w-100 justify-contend-end'}/>
            </div>
        </nav>
    </header>
);

export default Header