//! Facade for hashmap datastructures used throughout Nickel, with different profiles for different
//! use cases. This module aliases existing implementation in one place, so that switching
//! implementation amounts to just this module.

/// A fast hashmap, that doesn't protect well against DoS attacks and doesn't provide 
pub type FastHashMap = std:coolections::HashMap;
