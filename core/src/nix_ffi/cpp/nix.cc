#include "nix.hh"

using namespace nix;

#include <nix/config.h>
#include <nix/store-api.hh>
#include <nix/eval.hh>
#include <nix/shared.hh>
#include <nix/canon-path.hh>
#include <nix/nixexpr.hh>
#include <nix/value-to-json.hh>
#include <nix/command.hh>
#include <nix/value.hh>

#include "nickel-lang-core/src/nix_ffi/mod.rs.h"

struct DummyEvalCommand : virtual EvalCommand {

  void run(ref<Store> store) override
  {
    (void)store;
  }

};

// XXX: properly propagating errors from nix code to nickel. Malformed nix code shouldn't cause a crash.
//      can we even get span information?

rust::String eval_to_json(rust::Str nixCode)
{
  auto dummy = DummyEvalCommand({});
  initNix();
  initGC();
  auto & state = *dummy.getEvalState();

  auto vRes = state.allocValue();
  state.eval(state.parseExprFromString(std::string(nixCode), state.rootPath(CanonPath::root)), *vRes);

  std::stringstream out;
  NixStringContext context;
  // TODO: don't force the value. figure out a way to return thunks.
  //       (second argument)
  printValueAsJSON(state, true, *vRes, noPos, out, context, false);

  return out.str();
}
