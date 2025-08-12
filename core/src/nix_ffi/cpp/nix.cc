#include "nix.hh"

using namespace nix;

#include <nix/cmd/command.hh>
#include <nix/expr/config.hh>
#include <nix/expr/eval-gc.hh>
#include <nix/expr/eval.hh>
#include <nix/expr/nixexpr.hh>
#include <nix/expr/value-to-json.hh>
#include <nix/expr/value.hh>
#include <nix/main/shared.hh>
#include <nix/store/store-api.hh>
#include <nix/util/canon-path.hh>

#include "nickel-lang-core/src/nix_ffi/mod.rs.h"

// nix is designed with command-line use in mind, and there's some setup stuff
// that's tied to the EvalCommand class.
struct DummyEvalCommand : virtual EvalCommand {

  void run(ref<Store> store) override
  {
    // so it doesn't complain about unused variables
    (void)store;
  }

};

// FIXME: error messages have an extra `error:` on them
rust::String eval_to_json(rust::Str nixCode, const std::string & baseDir)
{
  auto dummy = DummyEvalCommand({});
  initNix();
  initGC();
  auto & state = *dummy.getEvalState();

  auto vRes = state.allocValue();
  state.eval(state.parseExprFromString(std::string(nixCode), state.rootPath(CanonPath(baseDir))), *vRes);

  std::stringstream out;
  NixStringContext context;
  // TODO: don't force the value. figure out a way to return thunks.
  //       (second argument)
  printValueAsJSON(state, true, *vRes, noPos, out, context, false);

  return out.str();
}
