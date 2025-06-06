use std::path::Path;

use libtest_mimic::Trial;

use crate::project_root::project_root;

pub fn path_tests(
    glob: &str,
    test_fn: impl FnMut(&str, &Path) + Clone + Send + 'static,
) -> Vec<Trial> {
    let root = project_root();
    let root_str = root.clone().into_os_string().into_string().unwrap();

    let inputs_glob = glob::glob(&format!("{root_str}/{glob}")).unwrap();
    inputs_glob
        .map(|p| {
            let path = p.unwrap();
            let name = path.strip_prefix(&root).unwrap().display().to_string();
            let mut test_fn = test_fn.clone();
            Trial::test(name.clone(), move || {
                test_fn(&name, &path);
                Ok(())
            })
        })
        .collect()
}
