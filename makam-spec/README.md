### Using Makam

Makam is a dialect of lambda Prolog created and maintained by Antonis Stampoulis, more information [here](https:#github.com/astampoulis/makam).
This is an attempt to use it to define the semantics of the **Nickel** language.

He distributes it through [NPM](https:#www.npmjs.com/package/makam), and on the [repo](https:#github.com/astampoulis/makam) there's information on how to get it working.

#### Using it on Nix

We use the `node2nix` helper, there's a [PR](https:#github.com/NixOS/nixpkgs/pull/67703) to add it to nixpkgs, but for now this simple config should help.

Just `nix-build -A makam` and then `result/bin/makam ./src/init.makam -`. Or run `result/bin/makam src/init.makam src/examples.makam` to run the examples.

###### To update it

Run `node2nix --nodejs-12 -c makam-composition.nix` on the `makam-spec` directory
