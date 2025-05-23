# capture = 'stdout'
# command = ['export', 'imports/mixed.ncl', 'imports/mixed.json', 'imports/mixed.toml', '--apply-contract', 'imports/mixed_contract.ncl', '--import-path', '.']

# TODO: For some reason, we need to explicitly add the current working directory
# to the import path for this test to work on the CI. Maybe we should always
# look in the current directory as a fallback during import resolution?
{}
