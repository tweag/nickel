#pragma once

#include <nix/expr/primops.hh>
#include "rust/cxx.h"

#include "nickel-lang-core/src/nix_ffi/mod.rs.h"

using namespace nix;

rust::String eval_to_json(const rust::Str nix_code, const std::string & base_dir);
